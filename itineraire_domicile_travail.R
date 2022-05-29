library("rjson")
library("sp")
library("sf")
library("httr")
library("dplyr")
library("googlePolylines")
library("stringr")
library("curl")
library("plyr")
library("raster")
library("fasterize")
library("zip")
library("archive")
library("stringr")
library("stringi")
library("data.table")
library("gdistance")
library("reldist")
library("rgdal")
library("smoothr")
library("collapse")
library("rvest")

options(scipen=15)
setwd("/Users/corentintrevien/Dropbox/Travail/itin_map")
source('Z-fonctions-v3.R')

#TO DO :
#Stats récapitulatives sur les trajets 
#Ajouter la largeur des autoroutes (et autres routes ?)
#Programmer le retry

extract_rp_data <- function(code_zone,file_zone,type_zone = "EPCI",max_dist_vo=80000,year=2018,all_combi = FALSE,max_itin_tranche=50000){
  if(type_zone=="EPCI"){var_zone <- "CODE_EPCI"}
  if(type_zone=="DEP"){var_zone <- "INSEE_DEP"}
  if(type_zone=="REG"){var_zone <- "INSEE_REG"}
  print("Chargement des informations communales")
  deb <- Sys.time()
  #Chargement de l'emplacement des chefs-lieux
  chef_lieu <- read_admin_express_ign("CHEF_LIEU")
  chef_lieu_arr <- read_admin_express_ign("CHFLIEU_ARRONDISSEMENT_MUNICIPAL")
  chef_lieu <- rbind(chef_lieu[,c("NOM_CHF","INSEE_COM")],chef_lieu_arr[,c("NOM_CHF","INSEE_COM")])
  
  #Communes de l'EPCI
  commune <- read_admin_express_ign("COMMUNE")
  commune <- subset(commune,select= c("ID","NOM_COM","INSEE_COM","INSEE_CAN","INSEE_ARR","INSEE_DEP","INSEE_REG","CODE_EPCI"))
  commune_arr <- read_admin_express_ign("ARRONDISSEMENT_MUNICIPAL")
  commune_arr <- subset(commune_arr,select= c("ID","NOM_COM","INSEE_COM","INSEE_RATT"))
  commune_arr[,c("INSEE_CAN","INSEE_ARR","INSEE_DEP","INSEE_REG","CODE_EPCI")] <- 
    st_drop_geometry(commune)[match(commune_arr$INSEE_RATT,commune$INSEE_COM),
                              c("INSEE_CAN","INSEE_ARR","INSEE_DEP","INSEE_REG","CODE_EPCI")]
  commune_arr <- subset(commune_arr,select= c("ID","NOM_COM","INSEE_COM","INSEE_CAN","INSEE_ARR","INSEE_DEP","INSEE_REG","CODE_EPCI"))
  commune <- rbind(commune,commune_arr)
  
  list_com_zone <- st_drop_geometry(commune[commune[[var_zone]]==code_zone,])$INSEE_COM
  #chef_lieu_zone <- chef_lieu[chef_lieu$INSEE_COM %in% list_com_zone,]
  
  print("Chargement des flux domcile-travail du RP")
  print(Sys.time() - deb)
  dir.create("Insee",showWarnings = FALSE)
  read_rp_data <- function(year){
    
    list_url_mobpro <- c("2018"="https://www.insee.fr/fr/statistiques/fichier/5395749/RP2018_mobpro_csv.zip",
                         "2017"="https://www.insee.fr/fr/statistiques/fichier/4507890/RP2017_mobpro_csv.zip",
                         "2016"="https://www.insee.fr/fr/statistiques/fichier/4171531/RP2016_mobpro_csv.zip",
                         "2015"="https://www.insee.fr/fr/statistiques/fichier/3566008/rp2015_mobpro_txt.zip",
                         "2014"="https://www.insee.fr/fr/statistiques/fichier/2866308/rp2014_mobpro_txt.zip",
                         "2013"="https://www.insee.fr/fr/statistiques/fichier/2409494/rp2013_mobpro_txt.zip",
                         "2012"="https://www.insee.fr/fr/statistiques/fichier/1913213/rp2012_mobpro_txt.zip",
                         "2011"="https://www.insee.fr/fr/statistiques/fichier/2011277/rp2011_mobpro_txt.zip",
                         "2010"="https://www.insee.fr/fr/statistiques/fichier/2411532/rp2010_mobpro_txt.zip",
                         "2009"="https://www.insee.fr/fr/statistiques/fichier/2411392/rp2009_mobpro_txt.zip",
                         "2008"="https://www.insee.fr/fr/statistiques/fichier/2408676/rp2008_mobpro_txt.zip",
                         "2007"="https://www.insee.fr/fr/statistiques/fichier/2408167/rp2007_mobpro_txt.zip",
                         "2006"="https://www.insee.fr/fr/statistiques/fichier/2407634/rp2006_mobpro_txt.zip")
    
    year <- as.character(year)
    url <- list_url_mobpro[as.character(year)]
    file_url <- str_locate_all(url,"/")[[1]]
    file_url <- file_url[nrow(file_url),"end"]
    file_url <- substr(url,file_url[[1]]+1,nchar(url))
    if(!file.exists(paste0("Insee/",file_url))){
      curl_download(url, paste0("Insee/",file_url), mode="wb")
    }
    list_files <- utils::unzip(paste0("Insee/",file_url), list = TRUE)
    data_file <- list_files[which.max(list_files$Length),"Name"]
    #Lecture du fichier
    unzip(paste0("Insee/",file_url),files=data_file,exdir="Insee")
    dom_trav_rp <- fread(paste0("Insee/",data_file))
    file.remove(paste0("Insee/",data_file))
    #Arrondissements municipaux
    dom_trav_rp[ARM!="ZZZZZ","COMMUNE"] <- dom_trav_rp[ARM!="ZZZZZ","ARM"] 
    #Uniformisation des variable
    dom_trav_rp$IPONDI <- as.numeric(gsub(",",".",dom_trav_rp$IPONDI))
    dom_trav_rp$TRANS <- as.character(dom_trav_rp$TRANS)
    if(mean(dom_trav_rp$TRANS=="6")==0){dom_trav_rp$TRANS <- revalue(dom_trav_rp$TRANS,c("3"="4","4"="5","5"="6"))}
    dom_trav_rp <- plyr::rename(dom_trav_rp,c("COMMUNE"="codgeo_dep","DCLT"="codgeo_arr"))
    return(dom_trav_rp)
  }
 
  data_rp <- read_rp_data(year)
  
  print("Mise en forme des flux domcile-travail du RP")
  print(Sys.time() - deb)
  #Flux entrants et sortants de la zone 
  dom_trav_rp <- subset(data_rp,(codgeo_dep %in% list_com_zone | codgeo_arr %in% list_com_zone) & TRANS == "5" & IPONDI>0) 

  #Aggregation des flux 
  dom_trav_rp <- dcast(data=dom_trav_rp,codgeo_dep+codgeo_arr~1,value.var="IPONDI",fun.aggregate=sum)
  colnames(dom_trav_rp)[3] <- paste0("flux_voit_",year)
  dom_trav_rp$sens <- "Domicile-travail"
  
  #Toutes les combinaisons à l'intérieur de la zone
  setDF(dom_trav_rp)
  if(all_combi == TRUE){
    combi_com_zone <- expand.grid(codgeo_dep= list_com_zone,codgeo_arr=list_com_zone)
    dom_trav_rp <- full_join(dom_trav_rp,combi_com_zone, by = c("codgeo_dep", "codgeo_arr"))
    dom_trav_rp[is.na(dom_trav_rp$sens),"sens"] <- "Flux interne nul"
    dom_trav_rp[is.na(dom_trav_rp)] <- 0
  }

  #Distance à vol d'oiseau 
  dom_trav_rp$dist_vo <- as.numeric(st_distance(chef_lieu[match(dom_trav_rp$codgeo_dep,chef_lieu$INSEE_COM),],
                                                chef_lieu[match(dom_trav_rp$codgeo_arr,chef_lieu$INSEE_COM),],
                                                by_element=TRUE))
  
  #Sélection des trajets de moins de "max_dist_vo" m à vol d'oiseau
  if(all_combi == FALSE){dom_trav_rp <- subset(dom_trav_rp,dist_vo<=max_dist_vo)}
  if(all_combi == TRUE){dom_trav_rp <- subset(dom_trav_rp,dist_vo<=max_dist_vo | (codgeo_dep %in% list_com_zone & codgeo_arr %in% list_com_zone))}
  
  #Ajout des coordonnées des communes de départ et d'arrivée
  dom_trav_rp[,c("lon_dep","lat_dep")] <- st_coordinates(chef_lieu)[match(dom_trav_rp$codgeo_dep,chef_lieu$INSEE_COM),]
  dom_trav_rp[,c("lon_arr","lat_arr")] <- st_coordinates(chef_lieu)[match(dom_trav_rp$codgeo_arr,chef_lieu$INSEE_COM),]
  
  trav_dom_rp <- plyr::rename(dom_trav_rp[dom_trav_rp$sens=="Domicile-travail",],
                              c("codgeo_dep"="codgeo_arr","codgeo_arr"="codgeo_dep",
                                "lon_dep"="lon_arr","lat_dep"="lat_arr",
                                "lon_arr"="lon_dep","lat_arr"="lat_dep"))
  trav_dom_rp$sens <- "Travail-domicile"
  data_itin_zone <- rbind(dom_trav_rp,trav_dom_rp)
  #Flux inverses (travail domicile)
  trav_dom_rp <- plyr::rename(dom_trav_rp[dom_trav_rp$sens=="Domicile-travail",],
                              c("codgeo_dep"="codgeo_arr","codgeo_arr"="codgeo_dep",
                                "lon_dep"="lon_arr","lat_dep"="lat_arr",
                                "lon_arr"="lon_dep","lat_arr"="lat_dep"))
  trav_dom_rp$sens <- "Travail-domicile"
  data_itin_zone <- rbind(dom_trav_rp,trav_dom_rp)

  #Identifiant itinéraire
  data_itin_zone$itin_id <- with(data_itin_zone,paste(codgeo_dep,codgeo_arr,sep="_"))
  #Zone de départ 
  data_itin_zone$zone_dep <- as.numeric(data_itin_zone$codgeo_dep %in% list_com_zone)
  
  #Zonage administratif de départ 
  data_itin_zone[,c("can_dep","arr_dep","dep_dep")] <- 
    st_drop_geometry(commune)[match(data_itin_zone$codgeo_dep,commune$INSEE_COM), c("INSEE_CAN","INSEE_ARR","INSEE_DEP")]
  
  print("Tranches de chargement")
  print(Sys.time() - deb)
  
  #Tranches de chargement
  setDT(data_itin_zone)
  data_itin_zone$tranche <- file_zone
  data_itin_zone[,nb_itin := sum(!duplicated(itin_id)) ,by = "tranche"]
  data_itin_zone$tranche <- with(data_itin_zone,ifelse(nb_itin>max_itin_tranche,paste(tranche,dep_dep,sep="_"),tranche))
  data_itin_zone[,nb_itin := .N,by = "tranche"]
  data_itin_zone$tranche <- with(data_itin_zone,ifelse(nb_itin>max_itin_tranche,paste(tranche,arr_dep,sep="_"),tranche))
  data_itin_zone[,nb_itin := .N,by = "tranche"]
  data_itin_zone$tranche <- with(data_itin_zone,ifelse(nb_itin>max_itin_tranche,paste(tranche,can_dep,sep="_"),tranche))
  data_itin_zone[,nb_itin := .N,by = "tranche"]
  data_itin_zone$tranche <- with(data_itin_zone,ifelse(nb_itin>max_itin_tranche,paste(tranche,codgeo_dep,sep="_"),tranche))
  print(plyr::count(data_itin_zone[,"tranche"]))
  data_itin_zone <- subset(data_itin_zone,select=-nb_itin)
    
  
  dir.create(path=file_zone,showWarnings = FALSE)
  fwrite(data_itin_zone,paste0(file_zone,"/data_itin_",file_zone,".csv.gz"))
  print(Sys.time() - deb)
  
}

#Suppression des trajets nuls de plus de 20 km (pour limiter le nombre d'itinéraires à charger)
#extract_rp_data(code_zone="11",file_zone="idf",type_zone = "REG",all_combi = TRUE,year=2014,max_itin_tranche=200000)
#data_itin_idf <- fread("idf/data_itin_idf.csv.gz")
#fwrite(subset(data_itin_idf,dist_vo<=20000 | flux_voit_2014>0),"idf/data_itin_idf.csv.gz")

###CHARGEMENT DES ITINERAIRES###
#Programmer la compression en cas d'arrêt
#API itinéraire 

#file_zone <- "saint_brieuc_armor"
download_itin <- function(file_zone,modep="car",retry=FALSE,optim ="fastest",peage=TRUE){
  deb <- Sys.time()

  #data_dep_arr <- fread(paste0(file_zone,"/data_itin_",file_zone,".csv.gz"),colClasses = list(character=c("codgeo_dep","codgeo_arr")))
  data_dep_arr <- fread(paste0(file_zone,"/data_itin_",file_zone,".csv.gz"))
  
  #Suppression des doublons
  data_dep_arr <- subset(data_dep_arr,select=c("itin_id","lon_dep","lat_dep","lon_arr","lat_arr","tranche"))
  data_dep_arr <- data_dep_arr[!duplicated(data_dep_arr),]
  setDF(data_dep_arr)
  
  dir.create(paste0(file_zone,"/Itin"),showWarnings = FALSE)
  
  #Zippage des tables ouvertes 
  # list_files <- list.files(paste0(file_zone,"/Itin"))
  # unziped_tranche <- NULL
  # if(length(grep("_coord.csv",list_files))>0){
  #   file_coord <- list_files[grep("_coord.csv",list_files)]
  #   unziped_tranche <- gsub("_coord.csv","",file_coord[1])
  #   zipr(paste0(file_zone,"/Itin/",unziped_tranche,".zip"),include_directories = F,
  #        files = paste0(file_zone,"/Itin/",unziped_tranche,"_",c("info","coord","step"),".csv"))
  #   file.remove(paste0(file_zone,"/Itin/",unziped_tranche,"_",c("info","coord","step"),".csv"))
  # }
  # 
  #Recherche des tranches déjà chargés 
  list_tranche <- unique(data_dep_arr$tranche)
  liste_charge <- list.files(paste0(file_zone,"/Itin"))
  liste_charge <- gsub("_coord_seg.csv.gz","",liste_charge)
  list_tranche <- setdiff(list_tranche,liste_charge) 

  #tranche <- list_tranche[1]
  for(tranche in list_tranche){
    print(paste("Tranche",tranche))
    print(Sys.time()-deb)
    data_dep_arr_tr <- data_dep_arr[data_dep_arr$tranche == tranche,]

    #Décompression des itinéraires déjà chargés
    if(file.exists(paste0(file_zone,"/Itin/",tranche,"_coord.csv"))){
       deja_charge <- fread(paste0(file_zone,"/Itin/",tranche,"_coord.csv"))
       data_dep_arr_tr <- data_dep_arr_tr[!data_dep_arr_tr$itin_id %in% deja_charge$itin_id,]
    }
    
    #Chargement des itinéraires
    print(paste("Nombre d'itinéraires à charger :",nrow(data_dep_arr_tr)))
    
    c <- 1
    while(c<=nrow(data_dep_arr_tr)){
      #print(c)
      try({itin <- get_itin(depart = data_dep_arr_tr[c,c("lon_dep","lat_dep")],arrivee= data_dep_arr_tr[c,c("lon_arr","lat_arr")],
                            modep=modep,itin_id=data_dep_arr_tr[c,"itin_id"],optim=optim,peage=peage) 
      #Ecriture 
      write.table(itin[["info"]], paste0(file_zone,"/Itin/",tranche,"_info.csv"),row.names = FALSE, append=T,sep=",",
                  col.names=!file.exists(paste0(file_zone,"/Itin/",tranche,"_info.csv")))
      write.table(itin[["coord"]], paste0(file_zone,"/Itin/",tranche,"_coord.csv"),row.names = FALSE, append=T,sep=",",
                  col.names=!file.exists(paste0(file_zone,"/Itin/",tranche,"_coord.csv")))
      write.table(itin[["step"]], paste0(file_zone,"/Itin/",tranche,"_step.csv"),row.names = FALSE, append=T,sep=",",
                  col.names=!file.exists(paste0(file_zone,"/Itin/",tranche,"_step.csv")))
      })
      c <- c + 1
      if(c %%1000 == 0){
        print(paste(c,"/",nrow(data_dep_arr_tr)))
        print(Sys.time()-deb)}
    }
    
    print("Simplification des coordonnées")
    print(Sys.time() - deb)
  
    # file_zone = tranche = "saint_brieuc_armor"
    #Coordonnées
    coord <- fread(paste0(file_zone,"/Itin/",tranche,"_coord.csv"),colClasses = list(character=c("lon","lat")))
    coord$seg_id <- paste(coord$itin_id,coord$step,sep="_")
    #Etapes 
    step <- fread(paste0(file_zone,"/Itin/",tranche,"_step.csv"))
    step$step_id <- paste(step$itin_id,step$step,sep="_")
    step$seg_id <- step$step_id
    step$order <- 1 
    step$nstep <- 1
    
    #Simplification
    coord_seg_itin <- simpli_coord(coord_seg=subset(coord,select=c(lat,lon,seg_id)) ,
                                   seg_itin=subset(step,select=c(step_id,seg_id,order,nstep)),
                                   init_identifier='step_id')
    
    print("Enregistrement des données")
    print(Sys.time() - deb)
    
    fwrite(coord_seg_itin[[1]],paste0(file_zone,"/Itin/",tranche,"_coord_seg.csv.gz"))
    fwrite(coord_seg_itin[[2]],paste0(file_zone,"/Itin/",tranche,"_seg_itin.csv.gz"))
    fwrite(subset(step,select=c("nom_1_gauche","nom_1_droite","cpx_numero","cpx_toponyme","distance","duration","step_id")),
           paste0(file_zone,"/Itin/",tranche,"_step.csv.gz"))
    itin <- fread(paste0(file_zone,"/Itin/",tranche,"_info.csv"))
    fwrite(subset(itin,select=c("itin_id","distance","duration","lon_dep","lat_dep","lon_end","lat_end")),
           paste0(file_zone,"/Itin/",tranche,"_itin.csv.gz"))
    #Suppression des données chargées
    file.remove(paste0(file_zone,"/Itin/",tranche,"_",c("info","coord","step"),".csv"))
    print(Sys.time() - deb)
    
  }
}
#download_itin("idf")
download_itin("ile_de_france")
#download_itin("ile_de_france_bus",peage=FALSE,optim="shortest")

file_zone <- "ile_de_france"
test <- fread(paste0(file_zone,"/data_itin_",file_zone,".csv.gz"))

###MISE EN FORME DE LA CARTE###
#file_zone <- "ile_de_france_bus"
make_map_itin <- function(file_zone){
  deb <- Sys.time()
  
  #Liste des tranches 
  list_files <- list.files(paste0(file_zone,"/Itin"))
  list_files <- list_files[ grepl("_step.csv.gz", list_files, fixed = TRUE)]
  tranche_all <- gsub("_step.csv.gz","",list_files)
  tranche_all <- tranche_all[tranche_all != file_zone]
  
  #Si plusieurs tranches, lancement de l'aggregation 
  if(length(tranche_all) > 0){
    #Nombre d'underscore dans le nom du fichier
    nb_underscore <- nrow(str_locate_all(file_zone,"_")[[1]])
    
    #Niveau d'aggregation intermédiaire de chaque tranche 
    tranche_init <- lapply(tranche_all,function(t){
      #niv <- str_locate_all(t,pattern='_')[[1]][-(1:nb_underscore),"start"]
      niv <- str_locate_all(t,pattern='_')[[1]]
      niv <- niv[(nb_underscore+1):nrow(niv),"start"]
      niv <- c(lapply(niv,function(p) substr(t,1,p-1)),t)
      return(as.character(unlist(niv)))
    }) 
    
    #Agregation à réaliser à chaque niveau
    nb_niveau_tranche <- unlist(lapply(tranche_init,length))
    nb_niveau <- max(nb_niveau_tranche)-1
    
    list_tranche_niveau <-  lapply(1:nb_niveau,function(n){
      tranche_niveau <- lapply(tranche_init[nb_niveau_tranche>n],function(t) c(t[n],t[n+1]))
      tranche_niveau <- unique(tranche_niveau)
      tranche_niveau <- do.call(rbind,tranche_niveau)
      tranche_niveau <- split(tranche_niveau[,2],tranche_niveau[,1])
      return(tranche_niveau)
    })
    
    #Agrégation
    #n <- 1
    for(n in nb_niveau:1){
      print(paste("Niveau de consolidation",n))
      list_aggreg <- list_tranche_niveau[[n]]
      print(paste(length(list_aggreg),"consolidation(s)"))
      #a <- 1
      for(a in 1:length(list_aggreg)){
        print(paste("Consolidation",names(list_aggreg)[a]))
        list_tranche <- list_aggreg[[a]]
        print(list_tranche)
        
        print("Simplification des coordonnées")
        coord_seg_complet <- lapply(list_tranche,function(tranche){
          coord_seg <- fread(paste0(file_zone,"/Itin/",tranche,"_coord_seg.csv.gz"),colClasses = list(character=c("lon","lat")))
          coord_seg$seg_id <- paste(tranche,coord_seg$seg_id,sep="_")
          return(coord_seg)})
        
        #Coordonnées d'entrée
        coord_seg_complet <- rbindlist(coord_seg_complet)
        print(paste(nrow(coord_seg_complet),"coordonnées en entrée"))
        coord_seg_complet <-plyr::rename(coord_seg_complet,c("seg_id"="seg_id_init"))
        #Simplification
        seg_simpl <- deduplicate_lines(coord_seg_complet,"seg_id_init")
        rm("coord_seg_complet")
        
        #Suppression des doublons
        seg_itin_simpl <- seg_simpl[[1]]
        coord_seg_simpl <- seg_simpl[[2]]
        rm("seg_simpl")
        
        #Enregistrement des données
        fwrite(coord_seg_simpl,paste0(file_zone,"/Itin/",names(list_aggreg)[a],"_coord_seg.csv.gz"))
        print(paste(nrow(coord_seg_simpl),"coordonnées en sortie"))
        rm("coord_seg_simpl")
        print(Sys.time()-deb)
        #Lien segment/itin
        print("Lien segment/itinéraire")

        #tranche <- list_tranche[[1]]
        for(tranche in list_tranche){
          print(tranche)
          print(Sys.time() - deb)
          seg_itin_tranche <- fread(paste0(file_zone,"/Itin/",tranche,"_seg_itin.csv.gz"))
          seg_itin_tranche$seg_id_init <- paste(tranche,seg_itin_tranche$seg_id,sep="_")
          seg_itin_tranche <- subset(seg_itin_tranche,select=-seg_id)
          seg_itin_tranche <- inner_join(plyr::rename(seg_itin_simpl,c("order"="order_simpl","nstep"="nstep_simpl")),
                                         plyr::rename(seg_itin_tranche,c("order"="order_init","nstep"="nstep_init")),
                                         by="seg_id_init")
          seg_itin_tranche <- seg_itin_tranche[order(step_id,nstep_init,order_init*nstep_simpl),]
          seg_itin_tranche[,nstep := 1:.N,by="step_id"]
          seg_itin_tranche$order <- with(seg_itin_tranche,order_init*order_simpl)
          seg_itin_tranche <- subset(seg_itin_tranche,select=c("step_id","seg_id","nstep","order"))
          dir.create(paste0(file_zone,"/Temp/"),showWarnings=FALSE)
          fwrite(seg_itin_tranche,paste0(file_zone,"/Temp/",tranche,"_seg_itin_consolid.csv.gz"))
        }
        rm("seg_itin_simpl")
        
        print("Fusion des données")
        print(Sys.time()-deb)
        seg_itin_complet <- lapply(list_tranche,function(tranche){
          return(fread(paste0(file_zone,"/Temp/",tranche,"_seg_itin_consolid.csv.gz")))
          file.remove(paste0(file_zone,"/Temp/",tranche,"_seg_itin_consolid.csv.gz"))
        })
        seg_itin_complet <- rbindlist(seg_itin_complet)
        
        print("Enregistrement des des données")
        print(Sys.time()-deb)
        fwrite(seg_itin_complet,paste0(file_zone,"/Itin/",names(list_aggreg)[a],"_seg_itin.csv.gz"))
        print(Sys.time()-deb)
      }
    }
      
  }
  
  #Carte
  print("Carte")
  from_coord_to_map <- function(coords,id){
    coords <- as.data.frame(coords)
    name_lines <- as.matrix(coords[!duplicated(coords[,id]),id])
    colnames(name_lines) <- id
    coords <- split(coords[,c("lon","lat")],coords[,id])
    spatial_lines <- lapply(coords,function(l) st_linestring(as.matrix(l)))
    spatial_lines <- st_sfc(spatial_lines,crs=4326)
    spatial_lines <- st_sf(name_lines,geometry=spatial_lines)
    return(spatial_lines)
  }
  
  coord_seg_final <- fread(paste0(file_zone,"/Itin/",file_zone,"_coord_seg.csv.gz"))
  map_itin <- from_coord_to_map(coord_seg_final,"seg_id")
  st_write(map_itin,paste0(file_zone,"/",file_zone,"_map.shp"),delete_dsn =TRUE)
  print(Sys.time()-deb)
  
  #Etapes et itinéraire 
  if(length(tranche_all) > 1){
    
    print("Consolidation des données d'itinéraire et d'étape")
    
    list_files_init <- list.files(paste0(file_zone,"/Itin"))
    list_files_init <- list_files_init[ grepl("_step.csv.gz", list_files_init, fixed = TRUE)]
    tranche_init <- gsub("_step.csv.gz","",list_files_init)
    tranche_init <- tranche_init[tranche_init != file_zone]
    
    itin_complet <- lapply(tranche_init,function(tranche){
      print(tranche)
      return(fread(paste0(file_zone,"/Itin/",tranche,"_itin.csv.gz")))})
    itin_complet <-  rbindlist(itin_complet)
    fwrite(itin_complet,paste0(file_zone,"/Itin/",file_zone,"_itin.csv.gz"))
    
    for(tranche in tranche_init){
      print(paste("Etape",tranche))
      tranche_step <- fread(paste0(file_zone,"/Itin/",tranche,"_step.csv.gz"))
      write.table(tranche_step,paste0(file_zone,"/Itin/",file_zone,"_step.csv"),row.names = FALSE, append=TRUE,sep=",",
                  col.names=!file.exists(paste0(file_zone,"/Itin/",file_zone,"_step.csv")))
    }

    print("Compression des données d'étape")
    step_complet <- fread(paste0(file_zone,"/Itin/",file_zone,"_step.csv"))
    fwrite(step_complet,paste0(file_zone,"/Itin/",file_zone,"_step.csv.gz"))
    
    }
  
  
  print(Sys.time()-deb)

}
make_map_itin("ile_de_france")
make_map_itin("ile_de_france_bus")
#make_map_itin("saint_brieuc_armor")
#make_map_itin("sba2")

###ENRICHISSEMENT DE LA CARTE###
info_complem <- function(file_zone){
  deb <- Sys.time()
  print("CHARGMENT DES DONNEES")
  
  map_itin <- st_read(paste0(file_zone,"/",file_zone,"_map.shp"))
  map_itin$length <- as.numeric(st_length(map_itin))
  
  #Chargement des données
  step <- fread(paste0(file_zone,"/Itin/",file_zone,"_step.csv.gz"))
  step$itin_id <- substr(step$step_id,1,11)
  data_itin <- fread(paste0(file_zone,"/data_itin_",file_zone,".csv.gz"))
  seg_itin <- fread(paste0(file_zone,"/Itin/",file_zone,"_seg_itin.csv.gz"))
  print(Sys.time()-deb)
  
  ###COMMUNE DE DEPART ET D'ARRIVEE
  print("COMMUNE DEPART ET ARRIVEE")
  
  #Carte des communes
  map_itin <- st_transform(map_itin,crs=2154)
  commune <- read_admin_express_ign("COMMUNE")
  commune <- st_transform(commune,crs=2154)
  commune <- st_crop(commune,map_itin)
  #Points de départ et d'arrivée
  point_itin <- st_cast(map_itin, "POINT")
  point_itin[!duplicated(point_itin$seg_id),"dep_arr"] <- "dep"
  point_itin[!duplicated(point_itin$seg_id,fromLast = TRUE),"dep_arr"] <- "arr"
  point_itin <- point_itin[!is.na(point_itin$dep_arr),]
  #Commune de départ et d'arrivée
  point_itin <- st_intersection(point_itin,commune)
  colnames(point_itin) <- tolower(colnames(point_itin))
  point_itin <- dcast(data=setDT(st_drop_geometry(point_itin)),seg_id~dep_arr,value.var = c("insee_com","insee_reg"))
  #Ajoutà la carte
  map_itin <- left_join(map_itin,point_itin, by = "seg_id")
  print(Sys.time()-deb)
  
  ####SEGMENTS BIDIRECTIONNELS####
  print("SENS")
  
  order_seg <- dcast(data=seg_itin,seg_id~order)
  order_seg$bidir <- as.numeric(order_seg$`1` > 0 & order_seg$`-1`>0)
  map_itin$bidir  <- order_seg[match(map_itin$seg_id,order_seg$seg_id),]$bidir
  map_itin[is.na(map_itin$bidir),'bidir'] <- 0
  print(Sys.time()-deb)

  ####FLUX####
  print("FLUX")
  
  #Données flux
  var_flux <- colnames(data_itin)
  var_flux <- var_flux[substr(var_flux,1,5)=="flux_"]
  data_flux <- inner_join(step[,c("itin_id","step_id")],subset(data_itin,select=c("itin_id",var_flux)))
  #data_flux$sens <- revalue(data_flux$sens,c("Domicile-travail"="dt","Travail-domicile"="td"))
  data_flux <- inner_join(data_flux,seg_itin[,c("seg_id","step_id","order")])
  data_flux$order <- revalue(as.factor(data_flux$order),c("1"="dir","-1"="inv"))
  data_flux <- dcast(data=data_flux,seg_id~order,value.var=var_flux,fun.aggregate = sum,na.rm=TRUE)
  #data_flux <- dcast(data=data_flux,seg_id~sens+order,value.var="flux",fun.aggregate = sum)
  map_itin[,paste0(var_flux,c("_inv","_dir"))] <- data_flux[match(map_itin$seg_id,data_flux$seg_id),c("inv","dir")]
  
  print(Sys.time()-deb)
  
  ####DUREE, VITESSE###
  print("VITESSE")
  
  # seg_itin$lenght_seg <- map_itin[match(seg_itin$seg_id,map_itin$seg_id),]$length  
  # step_seg <- dcast(data=seg_itin,step_id~1,value.var="lenght_seg",fun.aggregate = sum)
  # step_seg <- plyr::rename(step_seg,c("."="lenght_seg"))
  # step_seg <- inner_join(step,step_seg)
  # quantile(with(step_seg,distance/lenght_seg),probs=0:100/100,na.rm=TRUE)
  # step$vitesse <- with(step,(distance/1000)/(duration/60))
  # quantile(step$vitesse,probs=0:100/100,na.rm=TRUE)
  
  seg_itin_step <- left_join(seg_itin,
                   plyr::rename(step[,c("step_id","itin_id",'distance',"duration")],
                                c("distance"="distance_step","duration"="duration_step")),
                   by="step_id")
  seg_itin_step$vitesse_step <- with(seg_itin_step,(distance_step/1000)/(duration_step/60))
  seg_itin_step <- left_join(seg_itin_step,
                             plyr::rename(st_drop_geometry(map_itin)[,c("seg_id","length")],c("length"="length_seg")),
                             by="seg_id")
  vitesse_seg <-dcast(data=seg_itin_step,seg_id~1,value.var = "vitesse_step",fun.aggregate = median)
  vitesse_seg <- plyr::rename(vitesse_seg,c("."="vitesse"))
  vitesse_seg$vitesse <- pmax(vitesse_seg$vitesse,10)
  vitesse_seg$vitesse <- pmin(vitesse_seg$vitesse,130)
  map_itin <- left_join(map_itin,vitesse_seg)
  print(Sys.time()-deb)
  
  ####NOMS DES SEGMENTS####  
  print("NOMS ROUTES")

  info_seg <- join(seg_itin,step,by="step_id")
  
  #Nom de rue
  info_seg_name <- rbind(plyr::rename(info_seg[cpx_numero != "",c("seg_id","nom_1_gauche")],c("nom_1_gauche"="nom")),
                         plyr::rename(info_seg[cpx_numero != "",c("seg_id","nom_1_droite")],c("nom_1_droite"="nom")))
  info_seg_name <- plyr::count(info_seg_name[nom != "",c("seg_id","nom")])
  info_seg_name <- info_seg_name[order(info_seg_name$seg_id,info_seg_name$freq),]
  info_seg_name <- info_seg_name[!duplicated(info_seg_name$seg_id,fromLast = T),]
  map_itin <- left_join(map_itin,info_seg_name[,c("seg_id","nom")], by = "seg_id")
  
  #Numéro de route
  info_seg_cpx <- plyr::count(info_seg[cpx_numero != "",c("seg_id","cpx_numero")])
  info_seg_cpx <- info_seg_cpx[order(info_seg_cpx$seg_id,info_seg_cpx$freq),]
  info_seg_cpx <- info_seg_cpx[!duplicated(info_seg_cpx$seg_id,fromLast = T),]
  map_itin <- left_join(map_itin,info_seg_cpx[,c("seg_id","cpx_numero")], by = "seg_id")
  print(Sys.time()-deb)
  rm("seg_itin")
  
  
  ####OCCUPATION DES SOLS####
  print("OCCUPATION DES SOLS")
  
  list_reg_itin <- unique(c(map_itin$insee_reg_arr,map_itin$insee_reg_dep))
  #Chargement de la BD CARTO 
  bd_carto_occup <- read_bd_carto_ign("HABILLAGE/ZONE_OCCUPATION_SOL",list_reg_itin)
  bd_carto_occup <- bd_carto_occup[!duplicated(bd_carto_occup$ID),]
  bd_carto_occup <- st_crop(bd_carto_occup,st_bbox(map_itin))
  #Sélection des zones traversées par un segment
  intersects_itin <- st_intersects(bd_carto_occup,map_itin)
  bd_carto_occup <- bd_carto_occup[unlist(lapply(intersects_itin,length))>0,]
  #Libéllé d'occupation des sols 
  bd_carto_occup$NATURE <- gsub("[^[:alnum:][:space:]]","",
                                iconv(bd_carto_occup$NATURE,
                                      from = 'UTF-8', to= "ASCII//TRANSLIT//IGNORE"))
  bd_carto_occup$NATURE <- gsub(" ","_",bd_carto_occup$NATURE)
  bd_carto_occup$NATURE <- tolower(bd_carto_occup$NATURE)
  bd_carto_occup$NATURE <- paste0("os_",bd_carto_occup$NATURE)
  
  #Sols traversés par les segments
  itin_occup <- st_intersection(bd_carto_occup,map_itin)
  #VERIFIER LES DOUBLONS 
  itin_occup$length_intersect <- as.numeric(st_length(itin_occup))
  itin_occup <- dcast(data=setDT(st_drop_geometry(itin_occup)),
                      seg_id~NATURE,value.var = "length_intersect",
                      fun.aggregate = sum)
  itin_occup$os_total <- rowSums(itin_occup[,-1])
  map_itin <- left_join(map_itin,itin_occup, by = "seg_id")
  map_itin$urbain <- with(map_itin,os_bati/os_total>0.5)
  
  print(Sys.time()-deb)
  
  ####TYPES DE ROUTES####
  print("AUTOROUTES")
  
  #Carte des vitesses 
  bd_carto_route <- read_bd_carto_ign("RESEAU_ROUTIER/TRONCON_ROUTE",list_reg_itin)
  bd_carto_route <- st_crop(bd_carto_route,st_bbox(map_itin))
  bd_carto_route <- bd_carto_route[!is.na(bd_carto_route$geometry),]
  
  #Type de route
  bd_carto_route$type_route <- 
    with(bd_carto_route,ifelse(NB_CHAUSSE == "2 chaussées" & NB_VOIES_D != "1 voie" & NB_VOIES_M != "1 voie",
                               "Voie rapide","Autre route"))
  bd_carto_route[bd_carto_route$VOCATION=="Type autoroutier","type_route"] <- "Type autoroutier"
  bd_carto_route$length <-  as.numeric(st_length(bd_carto_route))
  
  #plot(st_geometry(bd_carto_route[bd_carto_route$VOCATION %in% c("Type autoroutier"),]),col="red",lwd=0.2)
  #plot(st_geometry(bd_carto_route[bd_carto_route$type_route %in% c("Voie rapide"),]),col="blue",lwd=0.2,add=T)
  #plot(st_geometry(bd_carto_route[bd_carto_route$NB_VOIES %in% c("4 voies","Plus de 4 voies"),]), add=T,col="green",lwd=0.2)
  #plot(st_geometry(bd_carto_route[bd_carto_route$NB_VOIES %in% c("3 voies"),]),add=T,col="grey",lwd=0.2)
  #plyr::count(bd_carto_route$VOCATION)
  
  #Repérage des segments autoroute ou voie rapide 
  buffer_autoroute <- st_buffer(bd_carto_route[bd_carto_route$type_route == "Type autoroutier",],100)
  #buffer_autoroute <- st_union(buffer_autoroute)
  map_itin_auto <- st_intersection(map_itin[map_itin$bidir==0,c("seg_id","length","vitesse","urbain","cpx_numero")],
                                   buffer_autoroute[,c("NB_VOIES_M","NB_VOIES_D","NUM_ROUTE")])
  map_itin_auto$length_autoroute <- as.numeric(st_length(map_itin_auto))
  
  length_itin_auto <- dcast(data=setDT(st_drop_geometry(map_itin_auto)),seg_id~1,
                value.var = "length_autoroute",fun.aggregate =sum )
  map_itin_auto$length_autoroute <- length_itin_auto[match(map_itin_auto$seg_id,length_itin_auto$seg_id),]$.
  map_itin_auto$autoroute <-
    with(map_itin_auto,(length_autoroute/length>0.5 & (vitesse>=60 & urbain==TRUE)|(vitesse>= 80 & urbain==FALSE)|
                          (cpx_numero == NUM_ROUTE & !is.na(cpx_numero) & !is.na(NUM_ROUTE) & vitesse>=50)))
  map_itin_auto <- setDT(st_drop_geometry(map_itin_auto))
  map_itin_auto <- map_itin_auto[order(seg_id,-length_autoroute),]
  map_itin_auto <- map_itin_auto[!duplicated(seg_id),]
  colnames(map_itin_auto) <- tolower(colnames(map_itin_auto))
  map_itin_auto$nb_voies <-  pmin(map_itin_auto$nb_voies_m,map_itin_auto$nb_voies_d)
  #Ajout dans la carte
  map_itin <- left_join(map_itin,subset(map_itin_auto,select=c("seg_id","autoroute","num_route","nb_voies")),by = c("seg_id"))
  map_itin[is.na(map_itin$autoroute),"autoroute"] <- FALSE
  map_itin[is.na(map_itin$num_route),"num_route"] <- map_itin[is.na(map_itin$num_route),]$cpx_numero
  map_itin <- subset(map_itin,select=- cpx_numero)

  ####DECLIVITE####
  print("RELIEF")

  #Points des segments 
  pts_map_itin <- lapply(1:nrow(map_itin),function(s){
    coord <- st_coordinates(map_itin[s,])
    coord <- cbind(coord,seg_id=rep(map_itin[s,]$seg_id,nrow(coord)))
    return(coord)
  })
  #Densification des segments 
  pts_map_itin <- lapply(pts_map_itin, smooth_densify,max_distance=25)
  pts_map_itin <- do.call(rbind,pts_map_itin)
  pts_map_itin <- as.data.table(pts_map_itin)
  pts_map_itin[,rang_seg := 1:.N,by = "seg_id"]
  
  #Quatres "coins" du carreau à 25 m 
  pts_map_itin$X1 <- floor(pts_map_itin$X/25)*25
  pts_map_itin$Y1 <- floor(pts_map_itin$Y/25)*25
  pts_map_itin$X2 <- pts_map_itin$X1 + 25
  pts_map_itin$Y2 <- pts_map_itin$Y1
  pts_map_itin$X3 <- pts_map_itin$X1 + 25
  pts_map_itin$Y3 <- pts_map_itin$Y1 + 25
  pts_map_itin$X4 <- pts_map_itin$X1 
  pts_map_itin$Y4 <- pts_map_itin$Y1 + 25
  
  #Inverse de la distance à chaque coin
  for(i in 1:4){
    pts_map_itin[[paste0("inv_dist",i)]] <- 1/sqrt((pts_map_itin[[paste0("X",i)]]- pts_map_itin$X)**2
                                                   + (pts_map_itin[[paste0("Y",i)]]- pts_map_itin$Y)**2)
  }
  
  #Chargement des données d'altitude
  #Bbox carte
  bbox_itin <- st_bbox(map_itin)
  bbox_itin[1:2] <- floor(bbox_itin[1:2]/25)*25
  bbox_itin[3:4] <- ceiling(bbox_itin[3:4]/25)*25

  #Ensemble des points arrondis à 25 de la carte
  all_x_y_25 <- cbind(X=c(pts_map_itin$X1,pts_map_itin$X2,pts_map_itin$X3,pts_map_itin$X4),
                      Y=c(pts_map_itin$Y1,pts_map_itin$Y2,pts_map_itin$Y3,pts_map_itin$Y4))
  all_x_y_25 <- all_x_y_25[!duplicated(all_x_y_25),]
  list_x_y_25  <- all_x_y_25[,"X"]*100000000+all_x_y_25[,"Y"]
  
  #Liste des départements à charger 
  list_dep_itin <- unique(c(substr(map_itin$insee_com_dep,1,2),substr(map_itin$insee_com_arr,1,2)))
  print(Sys.time()-deb)
  
  #Chargement des départements
  data_alti <- lapply(list_dep_itin,function(d){
    alti_dep <- read_bd_alti_ign(d)
    alti_dep <- alti_dep[(alti_dep$X*100000000+alti_dep$Y) %in% list_x_y_25,]
    print(Sys.time()-deb)
    return(alti_dep)
  })
  data_alti <- rbindlist(data_alti)
  
  #Appariement avec l'altidue
  for(i in 1:4){
    pts_map_itin[[paste0("alti",i)]] <- 
      data_alti[match(pts_map_itin[[paste0("X",i)]]*10000000+pts_map_itin[[paste0("Y",i)]],
                    data_alti$X*10000000+data_alti$Y),]$alti
  }

  #Altitude du point
  pts_map_itin <- pts_map_itin[order(seg_id,rang_seg),]
  pts_map_itin$alti <- with(pts_map_itin,(alti1*inv_dist1+alti2*inv_dist2+alti3*inv_dist3+alti4*inv_dist4)/
                              (inv_dist1+inv_dist2+inv_dist3+inv_dist4))
  pts_map_itin$alti <- round(pts_map_itin$alti,3)
    
  #Altitude départ/arrivée
  pts_map_itin <- pts_map_itin[order(seg_id,rang_seg),]
  pts_map_itin[!duplicated(pts_map_itin$seg_id),"dep_arr"] <- "dep"
  pts_map_itin[!duplicated(pts_map_itin$seg_id,fromLast = TRUE),"dep_arr"] <- "arr"
  alti_dep_arr <- dcast(data = pts_map_itin[!is.na(dep_arr),],seg_id~dep_arr,value.var = "alti")
  colnames(alti_dep_arr) <- c("seg_id","alti_arr","alti_dep")
  alti_dep_arr$alti_arr <- with(alti_dep_arr,ifelse(is.na(alti_arr),alti_dep,alti_arr))
  alti_dep_arr$alti_dep <- with(alti_dep_arr,ifelse(is.na(alti_dep),alti_arr,alti_dep))
  #Variations altimetrique
  alti_var <- cbind(pts_map_itin[duplicated(seg_id),c("seg_id","X","Y","alti")],
        plyr::rename(pts_map_itin[duplicated(seg_id,fromLast=TRUE),c("X","Y","alti")],
                     c("X"="X_1","Y"="Y_1","alti"="alti_1")))
  alti_var$dist <- with(alti_var,sqrt((X_1-X)**2+(Y_1-Y)**2)) 
  alti_var$valti <- with(alti_var,alti-alti_1) 
  alti_var$pct <- with(alti_var,ifelse(dist>0,valti/dist,0))
  #Dénivelé positif 
  deniv_pos <- plyr::rename(dcast(data=alti_var[valti>0,],seg_id~1,value.var="valti",fun.aggregate = sum),
                            c("."="deniv_pos"))
  deniv_neg <- plyr::rename(dcast(data=alti_var[valti<0,],seg_id~1,value.var="valti",fun.aggregate = sum),
                            c("."="deniv_neg"))
  #TRanches de dénivellé 
  quantile(alti_var$pct,1:50/50)
  alti_var$deniv_tr <- cut(alti_var$pct,c(-Inf,-0.05,-0.025,-0.01,0.01,0.025,0.05,Inf),
      c("forte_desc","moyen_desc","faible_desc","plat","faible_montee","moyen_montee","forte_montee"))
  deniv_tr <- dcast(data=alti_var,seg_id~deniv_tr,value.var="dist",fun.aggregate = sum)
  #Fusion des données 
  seg_deniv <- full_join(deniv_pos,deniv_neg, by = "seg_id")
  seg_deniv <- full_join(seg_deniv,deniv_tr, by = "seg_id")
  seg_deniv <- full_join(seg_deniv,alti_dep_arr, by = "seg_id")
  seg_deniv[is.na(seg_deniv)] <- 0
  
  map_itin <- left_join(map_itin,seg_deniv, by = "seg_id")
  
  print(Sys.time()-deb)
  #Enregistrement des enrichissements 
  fwrite(st_drop_geometry(map_itin), paste0(file_zone,"/map_complements_",file_zone,".csv.gz"))
}
#info_complem("ile_de_france_bus")
info_complem("ile_de_france")

###STATISTIQUES SUR LES ITINERAIRES : Declivité, flux min, max, mean, med, occupation des sols, part autoroute
file_zone <- "ile_de_france"

###CARTES###
draw_map <- function(file_zone){
  
  map_itin <- st_read(paste0(file_zone,"/",file_zone,"_map.shp"))
  data_map_itin <- fread(paste0(file_zone,"/map_complements_",file_zone,".csv.gz"))
  map_itin <- left_join(map_itin,data_map_itin,by='seg_id')
  
  #Contour de la zone d'intérêt
  data_itin <- fread(paste0(file_zone,"/data_itin_",file_zone,".csv.gz"))
  commune <- read_admin_express_ign("COMMUNE")
  commune <- commune[commune$INSEE_COM %in% unique(data_itin[zone_dep==1,]$codgeo_dep),]
  map_contour <- st_union(commune)
  
  dir.create(paste0(file_zone,"/Map"),showWarnings = FALSE)
  
  pdf(paste0(file_zone,"/Map/",file_zone,'_map.pdf'))
  
  #Mono/bidirectionnel 
  plot(st_geometry(map_contour),border=NA,col="lightgrey",main="Direction",
       xlim=st_bbox(map_itin)[c(1, 3)], ylim = st_bbox(map_itin)[c(2, 4)])

  plot(st_geometry(map_itin[map_itin$bidir==1,]),add=TRUE,lwd=0.5,col="red") 
  plot(st_geometry(map_itin[map_itin$bidir==0,]),add=TRUE,lwd=0.5,col="blue") 
  
  legend("bottomleft", legend=c("Segment bidirectionnel", "Segment unidirectionnel"),
         col=c("red", "blue"), lty=1, cex=0.8)
  
  #Autoroutes 
  plot(st_geometry(map_contour),border=NA,col="lightgrey",main="Autoroute",
       xlim=st_bbox(map_itin)[c(1, 3)], ylim = st_bbox(map_itin)[c(2, 4)])
  
  plot(st_geometry(map_itin[map_itin$autoroute==FALSE,]),add=TRUE,lwd=0.5,col="purple") 
  plot(st_geometry(map_itin[map_itin$autoroute==TRUE,]),add=TRUE,lwd=0.5) 
  
  legend("bottomleft", legend=c("Type autoroutier", "Autre"),
         col=c("purple", "black"), lty=1, cex=0.8)
  
  #Vitesse 
  plot(st_geometry(map_contour),border=NA,col="lightgrey",main="Vitesse",
       xlim=st_bbox(map_itin)[c(1, 3)], ylim = st_bbox(map_itin)[c(2, 4)])
  
  plot(st_geometry(map_itin[map_itin$vitesse<=50,]),add=TRUE,col="darkgrey",lwd=0.5)
  plot(st_geometry(map_itin[map_itin$vitesse>50 & map_itin$vitesse<=60,]),col="blue",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$vitesse>60 & map_itin$vitesse<=70,]),col="green",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$vitesse>70 & map_itin$vitesse<=80,]),col="gold",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$vitesse>80 & map_itin$vitesse<=90,]),col="orange",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$vitesse>90 & map_itin$vitesse<=100,]),col="red",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$vitesse>100 ,]),col="brown",add=TRUE,lwd=0.5)

  legend("bottomleft", legend=c("Moins de 50 km/h", "50-60 km/h", "60-70 km/h", "70-80 km/h", 
                                "80-90 km/h","90-100 km/h","Plus de 100 km/h"),
         col=c("darkgrey","blue","green","gold","orange","red","brown"), lty=1, cex=0.8)
  
  #Flux 
  var_flux <- colnames(map_itin)
  var_flux <- var_flux[substr(var_flux,1,5)=="flux_"]
  map_itin$flux_max <-apply(st_drop_geometry(map_itin)[,var_flux],1,max)
  
  tr_flux <- c(quantile( map_itin$flux_max,probs= 2:3/4,na.rm=T),
               wtd.quantile(map_itin$flux_max,q= c(0.25,0.5,0.75,0.9),w= map_itin$flux_max,na.rm=T))
  tr_flux <- round(tr_flux)
  tr_flux <- sort(tr_flux)
  
  plot(st_geometry(map_contour),border=NA,col="lightgrey",main="Flux",
       xlim=st_bbox(map_itin)[c(1, 3)], ylim = st_bbox(map_itin)[c(2, 4)])
  
  plot(st_geometry(map_itin[map_itin$flux_max<=tr_flux[1],]),add=TRUE,col="darkgrey",lwd=0.5)
  plot(st_geometry(map_itin[map_itin$flux_max>tr_flux[1] & map_itin$flux_max<=tr_flux[2],]),col="blue",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$flux_max>tr_flux[2] & map_itin$flux_max<=tr_flux[3],]),col="green",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$flux_max>tr_flux[3] & map_itin$flux_max<=tr_flux[4],]),col="gold",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$flux_max>tr_flux[4] & map_itin$flux_max<=tr_flux[5],]),col="orange",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$flux_max>tr_flux[5] & map_itin$flux_max<=tr_flux[6],]),col="red",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$flux_max>tr_flux[6] ,]),col="purple",add=TRUE,lwd=0.5)
  
  name_tr_flux <-c(0,tr_flux)
  name_tr_flux <- lapply(2:7,function(i) paste0(name_tr_flux[i-1],"-",name_tr_flux[i]))
  name_tr_flux <- c(name_tr_flux,paste0(">",tr_flux[6]))
  
  legend("bottomleft", legend=name_tr_flux,
         col=c("darkgrey","blue","green","gold","orange","red","purple"), lty=1, cex=0.8)
  
  #Occupation des zones 
  plot(st_geometry(map_contour),border=NA,col="lightgrey",main="Urbain",
       xlim=st_bbox(map_itin)[c(1, 3)], ylim = st_bbox(map_itin)[c(2, 4)])

  map_itin$part_urbain <- with(map_itin,(os_bati + os_carriere_decharge + os_zone_dactivites)/length)
  
  plot(st_geometry(map_itin[map_itin$part_urbain>0.9,]),add=TRUE,lwd=0.5,col="red") 
  plot(st_geometry(map_itin[map_itin$part_urbain>0.1 & map_itin$part_urbain <= 0.9,]),add=TRUE,lwd=0.5,col="orange") 
  plot(st_geometry(map_itin[map_itin$part_urbain<=0.1,]),add=TRUE,lwd=0.5,col="green") 
  
  legend("bottomleft", legend=c("Urbain", "Partiellement urbain","Extra urbain"),
         col=c("red", "orange","green"), lty=1, cex=0.8)
  
  #Altitude 
  map_itin$alti_mean <-with(map_itin,(alti_arr+alti_dep)/2) 
  tr_alti <- round(quantile(map_itin$alti_mean,probs= 1:4/5))
  
  plot(st_geometry(map_contour),border=NA,col="lightgrey",main="Altitude",
       xlim=st_bbox(map_itin)[c(1, 3)], ylim = st_bbox(map_itin)[c(2, 4)])
  
  plot(st_geometry(map_itin[map_itin$alti_mean<=tr_alti[1],]),col="darkgreen",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$alti_mean>tr_alti[1] & map_itin$alti_mean<=tr_alti[2],]),col="green",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$alti_mean>tr_alti[2] & map_itin$alti_mean<=tr_alti[3],]),col="gold",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$alti_mean>tr_alti[3] & map_itin$alti_mean<=tr_alti[4],]),col="orange",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$alti_mean>=tr_alti[4],]),col="red",add=TRUE,lwd=0.5)
  
  name_tr_alti <-c(0,tr_alti)
  name_tr_alti <- lapply(2:5,function(i) paste0(name_tr_alti[i-1],"-",name_tr_alti[i]," m."))
  name_tr_alti <- c(name_tr_alti,paste0(">",tr_alti[4]," m."))
  
  legend("bottomleft", legend=name_tr_alti,
         col=c("darkgreen","green","gold","orange","red"), lty=1, cex=0.8)
  
  #Dénivellé 
  var_deniv <- c("forte_desc","moyen_desc","faible_desc","plat","faible_montee","moyen_montee","forte_montee")
  map_itin$type_deniv <- var_deniv[apply(st_drop_geometry(map_itin)[,var_deniv],1,which.max)]
  
  plot(st_geometry(map_contour),border=NA,col="lightgrey",main="Dénivelé",
       xlim=st_bbox(map_itin)[c(1, 3)], ylim = st_bbox(map_itin)[c(2, 4)])
  
  plot(st_geometry(map_itin[map_itin$type_deniv %in% "plat",]),col="darkgrey",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$type_deniv %in% c("faible_desc","faible_montee"),]),col="darkgrey",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$type_deniv %in% c("moyen_desc","moyen_montee"),]),col="blue",add=TRUE,lwd=0.5)
  plot(st_geometry(map_itin[map_itin$type_deniv %in% c("forte_desc","forte_montee"),]),col="purple",add=TRUE,lwd=0.5)

  # 
  # map_itin$deniv_tot <- (abs(map_itin$deniv_neg) + abs(map_itin$deniv_pos))/map_itin$length
  # map_itin[is.na(map_itin$deniv_tot),"deniv_tot"] <- 0
  # tr_deniv <- round(quantile(map_itin$deniv_tot,probs= 1:4/5),3)
  # 
  # plot(st_geometry(map_contour),border=NA,col="lightgrey",main="Dénivelé",
  #      xlim=st_bbox(map_itin)[c(1, 3)], ylim = st_bbox(map_itin)[c(2, 4)])
  # 
  # plot(st_geometry(map_itin[map_itin$deniv_tot<=tr_deniv[1],]),col="darkgrey",add=TRUE,lwd=0.5)
  # plot(st_geometry(map_itin[map_itin$deniv_tot>tr_deniv[1] & map_itin$deniv_tot<=tr_deniv[2],]),col="deepskyblue",add=TRUE,lwd=0.5)
  # plot(st_geometry(map_itin[map_itin$deniv_tot>tr_deniv[2] & map_itin$deniv_tot<=tr_deniv[3],]),col="cyan",add=TRUE,lwd=0.5)
  # plot(st_geometry(map_itin[map_itin$deniv_tot>tr_deniv[3] & map_itin$deniv_tot<=tr_deniv[4],]),col="blue",add=TRUE,lwd=0.5)
  # plot(st_geometry(map_itin[map_itin$deniv_tot>=tr_deniv[4],]),col="purple",add=TRUE,lwd=0.5)
  # 
  # name_tr_deniv <-c(0,tr_deniv*100)
  # name_tr_deniv <- lapply(2:5,function(i) paste0(name_tr_deniv[i-1],"-",name_tr_deniv[i]," %"))
  # name_tr_deniv <- c(name_tr_deniv,paste0(">",tr_deniv[4]*100," %"))
  # 
  # legend("bottomleft", legend=name_tr_deniv,
  #        col=c("darkgrey","deepskyblue","cyan","blue","purple"), lty=1, cex=0.8)
  
  dev.off()
  
  
}
draw_map("ile_de_france")
#draw_map("ile_de_france_bus")

file_zone <- "idf"
#Mise en forme des itinéraires (sous forme de fichiers cartographique)
itin_complem <- function(file_zone){

  data_itin_idf <- fread(paste0(file_zone,"/data_itin_",file_zone,".csv.gz"))
  data_seg <- fread(paste0(file_zone,"/map_complements_",file_zone,".csv.gz"))
  seg_itin <- fread(paste0(file_zone,"/Itin/",file_zone,"_seg_itin.csv.gz"))
  seg_itin$itin_id <- substr(seg_itin$step_id,1,11)
  data_itin <- fread(paste0(file_zone,"/Itin/",file_zone,"_itin.csv.gz"))
  
  #Var flux
  var_flux <- colnames(data_map_itin)
  var_flux <- var_flux[substr(var_flux,1,5)=="flux_"]
  data_seg$flux_voit <-apply(subset(data_seg,select=var_flux),1,max)
  data_seg[is.na(flux_voit),"flux_voit"] <- 0
  #Type voie
  data_seg$autoroute <- as.numeric(data_seg$autoroute)
  data_seg$urbain <- with(data_seg,os_bati/os_total)
  #Pente
  data_seg$lenght_pente <- with(data_seg,plat+faible_desc + faible_montee+moyen_desc +
                                  moyen_montee+forte_desc + forte_montee)
  data_seg$plat_pente <- with(data_seg,plat/lenght_pente)
  data_seg$faible_pente <- with(data_seg,(faible_desc + faible_montee)/lenght_pente)
  data_seg$moyen_pente <- with(data_seg,(moyen_desc + moyen_montee)/lenght_pente)
  data_seg$forte_pente <- with(data_seg,(forte_desc + forte_montee)/lenght_pente)
  #Durée 
  data_seg$duree_seg <- with(data_seg,60*(length/1000)/vitesse)
  
  #Fusion données 
  data_seg <- subset(data_seg, select = c("seg_id","length","flux_voit","urbain","autoroute","nb_voies", 
                                          "deniv_pos","deniv_neg","plat_pente","faible_pente","moyen_pente",
                                          "forte_pente","duree_seg"))
  seg_itin <- left_join(seg_itin,data_seg, by = "seg_id")

  #Stat 
  stat_itin <- collap(seg_itin, autoroute+urbain+plat_pente+faible_pente+moyen_pente+forte_pente~  itin_id, 
                           fmean , w = ~ length) 
  stat_itin <- full_join(stat_itin,collap(seg_itin,  deniv_pos+deniv_neg+duree_seg~ itin_id, fsum))
  
  #Stat flux voit
  stat_itin <- full_join(stat_itin,collap(seg_itin, flux_voit ~  itin_id, list(fmedian,fmean,fmax) , w = ~ length))
  
  quantiles_flux_voit <- lapply(c(0.5,0.75,0.9),function(p){
    q <- fnth(seg_itin$flux_voit,n=p,w=seg_itin$length,g=seg_itin$itin_id)
    q <- data.frame(names(q),q)
    colnames(q) <- c("itin_id",paste0("q",p*100,".flux_voit"))
    return(q)})
  quantiles_flux_voit <- Reduce(full_join,quantiles_flux_voit)
  stat_flux_voit <- full_join(stat_flux_voit,quantiles_flux_voit)
  stat_itin <- full_join(stat_itin,quantiles_flux_voit)
  
  #Données initiales
  stat_itin <- full_join(data_itin,stat_itin)
  fwrite(stat_itin,paste0(file_zone,"/itin_complements_",file_zone,".csv.gz"))
}
itin_complem("idf")

plot(st_geometry(st_read(paste0(file_zone,"/","gam","_map.shp"))),lwd = 0.2)
