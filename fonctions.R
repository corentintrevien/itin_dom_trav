library("curl")
library("httr")
library("archive")


read_admin_express_ign <- function(map){
  #map <- "CHEF_LIEU"
  
  path_map_ign <- "ADMIN-EXPRESS-COG_2-1__SHP__FRA_2020-11-20/ADMIN-EXPRESS-COG/1_DONNEES_LIVRAISON_2020-11-20/ADE-COG_2-1_SHP_WGS84G_FRA"
  
  
  if(!file.exists(paste0("IGN/ADMIN-EXPRESS-COG_2-1__SHP__FRA_WGS84G_2020-11-20.7z"))){
    dir.create("IGN")
    
    curl_download("ftp://Admin_Express_ext:Dahnoh0eigheeFok@ftp3.ign.fr/ADMIN-EXPRESS-COG_2-1__SHP__FRA_WGS84G_2020-11-20.7z",
                  
                  "IGN/ADMIN-EXPRESS-COG_2-1__SHP__FRA_WGS84G_2020-11-20.7z", mode="wb")
    
  }
  
  archive_extract("IGN/ADMIN-EXPRESS-COG_2-1__SHP__FRA_WGS84G_2020-11-20.7z",dir="IGN",
                  files = paste0(path_map_ign,"/",map,".",c("shp","shx","dbf","prj","cpg")))
  
  map_st <- st_read(paste0("IGN/",path_map_ign,"/",map,".shp"))
  
  unlink("IGN/ADMIN-EXPRESS-COG_2-1__SHP__FRA_2020-11-20",recursive=TRUE)
  return(map_st)
}

read_bd_carto_ign <- function(couche,list_reg){
  #couche =  "RESEAU_ROUTIER/TRONCON_ROUTE" "HABILLAGE/ZONE_OCCUPATION_SOL"
  #Chargement des régions
  bd_carto<- lapply(list_reg,function(r){
    print(paste("Région",r))
    if(!file.exists(paste0("IGN/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R",r,"_2021-05-10.7z"))){
      print("Téléchargement du fichier vectoriel")
      dir.create("IGN",showWarnings = FALSE)
      curl_download(paste0("ftp://BD_CARTO_ext:Oor4el8aeM3io0Ji@ftp3.ign.fr/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R",r,"_2021-05-10.7z"),
                    paste0("IGN/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R",r,"_2021-05-10.7z"), mode="wb")
    }
    #Décompression
    print("Dézipage du fichier vectoriel")
    path_bb_carto <- paste0("BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R",r,"_2021-05-10/BDCARTO/1_DONNEES_LIVRAISON_2021-05-00119/BDC_4-0_SHP_LAMB93_R",r,"-ED211/")
    archive_extract(paste0("IGN/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R",r,"_2021-05-10.7z"),dir="IGN",
                    files = paste0(path_bb_carto,couche,".",c("shp","shx","dbf","prj","cpg")))
    #Chargement
    print("Chargement du fichier vectoriel")
    map_bd_carto <- st_read(paste0("IGN/",path_bb_carto,couche,".shp"), options = "ENCODING=UTF8")
    try(unlink(paste0("IGN/BDCARTO_4-0_TOUSTHEMES_SHP_LAMB93_R",r,"_2021-05-10/"),recursive=TRUE))
    return(map_bd_carto)
  })
  bd_carto <- do.call(rbind,bd_carto)
  return(bd_carto)
}

read_bd_alti_ign <- function(list_dep){
  
  #Liste des fichiers BDALTI départementaux
  if(!file.exists("IGN/url_bdalti.csv.gz")){
    page_bdalti <- read_html("https://geoservices.ign.fr/bdalti")
    list_url_bdalti <- html_attr(html_nodes(page_bdalti,xpath = "//ul/li/a"),"href")
    list_url_bdalti <- 
      list_url_bdalti[substr(list_url_bdalti,1,68) == "ftp://BD_ALTI_ext:docoazeecoosh1Ai@ftp3.ign.fr/BDALTIV2_2-0_25M_ASC_"]
    url_bdalti <- data.table(url = list_url_bdalti)
    url_bdalti$pos <- str_locate(url_bdalti$url,"_D")[,"start"]
    url_bdalti$dep <- substr(url_bdalti$url, url_bdalti$pos+2,url_bdalti$pos+4)
    url_bdalti$file <- gsub("ftp://BD_ALTI_ext:docoazeecoosh1Ai@ftp3.ign.fr/","",url_bdalti$url)
    fwrite(url_bdalti[,c("url","file","dep")],"IGN/url_bdalti.csv.gz")
  }
  url_bdalti <- fread("IGN/url_bdalti.csv.gz")
  
  d <- "29"
  alti_data <- lapply(list_dep,function(d){
    d <- str_pad(d,3,pad="0")
    file_alti <- url_bdalti[dep==d,]$file
    url_alti <- url_bdalti[dep==d,]$url
    print(paste("Département",d))
    if(!file.exists(paste0("IGN/",file_alti))){
      print("Téléchargement des fichier MNT")
      dir.create("IGN",showWarnings = FALSE)
      curl_download(url_alti, paste0("IGN/",file_alti), mode="wb")
    }
    
    print("Dézipage des fichiers MNT")
    
    list_file_alti <- archive(paste0("IGN/",file_alti))
    list_file_alti <- list_file_alti[ substr(stri_reverse(list_file_alti$path),1,4) == "csa.",]$path
    archive_extract(paste0("IGN/",file_alti),dir="IGN",files = list_file_alti)
    
    print("Chargement des fichiers MNT")
    
    alti_dep <- lapply(list_file_alti,function(f){
      #print(f)
      raster_alti <- raster(paste0("IGN/",f)) 
      data_alti <- cbind(coordinates(raster_alti),as.data.frame(raster_alti))
      colnames(data_alti) <- c("X","Y","alti")
      setDT(data_alti)
      return(data_alti)
    })
    alti_dep <- rbindlist(alti_dep)
    try(unlink(paste0("IGN/",gsub(".7z","",file_alti)),recursive=TRUE))
    
    return(alti_dep)
  })
  alti_data  <- rbindlist(alti_data)
  return(alti_data)
}

get_itin <- function(depart,arrivee,modep="car",itin_id="itin",optim ="fastest",peage=TRUE){
  depart <- as.numeric(depart)
  depart <- round(depart,digits=5)
  arrivee <- as.numeric(arrivee)
  arrivee <- round(arrivee,digits=5)
  
  url_api <- paste0("https://itineraire.ign.fr/simple/1.0.0/route?resource=bdtopo-osrm&profile=",modep,
                    "&optimization=",optim,"&start=",depart[1],",",depart[2],"&end=",arrivee[1],",",arrivee[2],
                    "&geometryFormat=polyline&getSteps=true&getBbox=true")
  if(peage==FALSE){url_api <- paste0(url_api,'&constraints={"constraintType":"banned","key":"wayType","operator":"=","value":"autoroute"}')}
  #Interrogation de l'API (avec arrêt temporaire en cas d'échec)
  try_GET <- try(resp <- GET(url_api, encoding = "UTF-8"))
  if (class(try_GET) == "try-error") {
    print("Erreur API, nouvel essai dans 3 minutes")
    Sys.sleep(90)
    print("Nouvel essai")
    try(resp <- GET(url_api, encoding = "UTF-8"))
  }
    
  itin_json <- fromJSON(content(resp, "text", encoding = "UTF-8"))
  #Infos sur l'ensemble de l'itinéraire 
  if(is.null(itin_json$error)==TRUE){
    info_itin <- c(itin_json$bbox,itin_json$distance,itin_json$duration, str_split(itin_json$start,",")[[1]],str_split(itin_json$end,",")[[1]])
    names(info_itin) <- c("xmin","ymin","xmax","ymax","distance","duration","lon_dep","lat_dep","lon_end","lat_end")
    info_itin <- c(info_itin,itin_id=itin_id)
    info_itin <- as.data.frame(t(info_itin))
    #Etapes
    step <- itin_json$portions[[1]]$steps
    #Informations sur chaque étape
    info_step <- lapply(1:length(step),function(s){ 
      info <- c(step[[s]]$attributes$name,distance = step[[s]]$distance,duration = step[[s]]$duration)
      info$step <- s
      info$itin_id <- itin_id
      return(info)})
    info_step <- do.call(rbind,info_step)
    #info_step <- as.data.frame(info_step)
    #Coordonnées sur chaque étape
    coord_step <- lapply(1:length(step),function(s){ 
      coord <- decode(step[[s]]$geometry)[[1]]
      coord$lon <- as.character(coord$lon)
      coord$lat <- as.character(coord$lat)
      coord$step <- s
      coord$itin_id <- itin_id
      return(coord)})
    coord_step <- do.call(rbind,coord_step)
  }else{
    info_itin <- data.frame(xmin =NA,ymin=NA,xmax=NA,ymax=NA,distance=NA,duration=NA,
                            lon_dep=NA,lat_dep=NA,lon_end=NA,lat_end=NA,itin_id=itin_id)
    info_step <- NULL
    coord_step <- NULL
  }
  return(list(info=info_itin,step=info_step,coord=coord_step))
}

#get_itin(depart=c(2,48),arrivee=c(3,48))
#Déduplication des lignes
deduplicate_lines <- function(coord_init,id,TEST=TRUE){
  #coord_init <- coords_sba
  coord_init <- subset(as.data.table(coord_init),select=c(id,"lon","lat"))
  colnames(coord_init) <- c("old_id","lon","lat")
  coord_init=cbind(coord_init[duplicated(subset(coord_init,select=old_id),fromLast = TRUE),],
               plyr::rename(coord_init[duplicated(subset(coord_init,select=old_id)),c("lon","lat")],
                            replace=c("lon"="lon2","lat"="lat2")))
  #Identifiant unique de segment
  seg_id <- subset(coord_init,select=c(lon,lat,lon2,lat2))
  seg_id[,order := (lon>lon2 |(lon==lon2 & lat<lat2))]
  setDF(seg_id)
  seg_id[seg_id$order==FALSE,c("lon","lat","lon2","lat2")] <- seg_id[seg_id$order==FALSE,c("lon2","lat2","lon","lat")] 
  setDT(seg_id)
  seg_id[,seg_id := .GRP,by= c("lon","lat","lon2","lat2")]
  coord_init[,c("seg_id","order")]<- seg_id[,c("seg_id","order")]
  coord_init[,"order"] <- ifelse(coord_init$order,1,-1)
  #Rang du point dans l'itinéraire
  coord_init[,ncoord := seq_len(.N), by="old_id"]
  #Segment immédiatement prédécent 
  coord_init$seg_id_prev <- lag(coord_init$seg_id)
  coord_init[!duplicated(old_id),"seg_id_prev"] <- NA
  #Segments uniques
  #coord_unique <- subset(coord_init,!duplicated(seg_id),select=c(seg_id,lon,lat,lon2,lat2,old_id,order,ncoord))
  coord_unique <- subset(coord_init,!duplicated(seg_id))
  #Repérage des points de partage des segments
  #Bifurcation 
  bifurc <- rbindlist(list(coord_init[,c("lon","lat","seg_id_prev","seg_id")],
                           coord_init[,c("lon","lat","seg_id","seg_id_prev")]),
                      use.names = FALSE)
  bifurc <- bifurc[!is.na(seg_id) & !is.na(seg_id_prev),]
  bifurc <- subset(bifurc,!duplicated(bifurc))
  bifurc[,bifurc:= .N ,by=c("lon","lat","seg_id_prev")]
  bifurc <- bifurc[bifurc > 1,]
  bifurc <- rbindlist(list(bifurc[,c("lon","lat","seg_id")],
                           bifurc[,c("lon","lat","seg_id_prev")]),
                      use.names = FALSE) 
  #Début et fin de segments initiaux 
  deb_fin <- rbindlist(list(coord_init[!duplicated(old_id),c("lon","lat","seg_id")],
                            coord_init[!duplicated(old_id,fromLast=TRUE),c("lon2","lat2","seg_id")]),
                       use.names = FALSE)
  #Ensemble des points de partage des segments 
  break_point <- rbindlist(list(bifurc,deb_fin))
  break_point <- break_point[!duplicated(break_point),]
  break_point$brk <- 1 
  #Repérage dans la table des coordonnées uniques 
  coord_unique <- left_join(coord_unique,plyr::rename(break_point,c("seg_id"="seg_id_prev","brk"="brk_left")),
                            by = c("lon", "lat", "seg_id_prev"))
  coord_unique <- left_join(coord_unique,plyr::rename(break_point,c("brk"="brk_right")),
                            by = c("lon", "lat", "seg_id"))
  coord_unique[is.na(brk_left),'brk_left'] <- 0
  coord_unique[is.na(brk_right),'brk_right'] <- 0
  coord_unique$brk <- with(coord_unique,brk_left+brk_right)
  #Nouvel identifiant par somme cumulée 
  coord_unique <- coord_unique[order(old_id,ncoord),]
  coord_unique[,new_id := .GRP*10000000000,by= "old_id"]
  coord_unique$new_id <- coord_unique$new_id + cumsum(coord_unique$brk)
  coord_unique[,new_id := .GRP,by= "new_id"]
  #Table de passage avec les anciens segments 
  coord_init[,c("new_id","new_order")]<- coord_unique[match(coord_init$seg_id,coord_unique$seg_id),c("new_id","order")]
  coord_init$order <- with(coord_init,order*new_order)
  #Vérification que les nouveaux segments sont bien identiques au sein de chaque ancien segment
  if(TEST==TRUE){
    sum_test <- dcast(data=coord_init,old_id+new_id~1,value.var="seg_id",fun.aggregate = sum)
    colnames(sum_test) <- c("old_id","new_id","sum_test")
    sum_test[,test := length(unique(sum_test)),by="new_id"]
    length_test <- dcast(data=coord_init,old_id+new_id~1,value.var="seg_id",fun.aggregate = length)
    colnames(length_test) <- c("old_id","new_id","length_test")
    length_test[,test := length(unique(length_test)),by="new_id"]
    if( max(length_test$test)>1 | max(sum_test$test)>1){"PROBLEME DE COHERENCE DES NOUVEAUX SEGMENTS"}
  }
  #Table de passage
  passage_init_new <- subset(coord_init,
                             !duplicated(coord_init[,c("old_id","new_id","order")]),
                             select=c("old_id","new_id","order","lat","lon"))
  passage_init_new[,nstep := 1:.N,by = "old_id"]
  #Vérification que l'ordre est bien uniforme
  if(TEST==TRUE){
    passage_init_new[,c("lat2","lon2")]  <- subset(coord_init,
                                                   !duplicated(coord_init[,c("old_id","new_id","order")],fromLast=TRUE),
                                                   select=c("lat2","lon2"))
    passage_init_new[,c("lag_lat","lag_lon")] <- lag(passage_init_new[,c("lat2","lon2")])
    if(nrow(passage_init_new[nstep != 1 & lat != lag_lat & lon != lag_lon,])){"PROBLEME DEBUT/FIN NOUVEAUX SEGMENTS"}
    if(mean(duplicated(passage_init_new[,c("old_id","new_id")])) > 0){"PROBLEME ORDRE NOUVEAUX SEGMENTS"}
    }
  #Passage à une coordonnée par ligne 
  coord_unique[,rank := 1:.N,by= "new_id"]
  coord_new <- subset(coord_unique,select=c("new_id","lon","lat","rank"))
  last_point <- subset(coord_unique,!duplicated(new_id,fromLast=TRUE),select=c("new_id","lon2","lat2","rank"))
  last_point$rank <- last_point$rank+1
  coord_new <- rbindlist(list(coord_new,last_point),use.names = FALSE)
  coord_new <- coord_new[order(new_id,rank),]
  return(list(plyr::rename(subset(passage_init_new,select=c("old_id","new_id","nstep","order")),
                           c("old_id"=id,"new_id"="seg_id")),
              plyr::rename(subset(coord_new,select=c("lon","lat","new_id")),c("new_id"="seg_id"))))}

deduplicate_lines_old <- function(coords,id){
  #save_coords<-coords 
  #coords <- save_coords
  #id <- "itin_ident_dep"
  coords <- subset(data.table(coords),select=c(id,"lon","lat"))
  colnames(coords) <- c("old_id","lon","lat")
  coords=cbind(coords[duplicated(subset(coords,select=old_id),fromLast = T),],
               plyr::rename(coords[duplicated(subset(coords,select=old_id)),c("lon","lat")],
                            replace=c("lon"="lon2","lat"="lat2")))
  #Identifiant unique de segment
  coords[,order := (lon>lon2 |(lon==lon2 & lat<lat2))]
  coords$seg <- with(coords,ifelse(order,paste(lon,lat,lon2,lat2),paste(lon2,lat2,lon,lat)))
  coords[,seg_id := .GRP,by= "seg"]
  #Rang du point dans l'itinéraire
  coords[,ncoord := seq_len(.N), by="old_id"]
  #Segments uniques
  unique_seg <- subset(coords,!duplicated(seg_id),select=c(seg_id,lon,lat,lon2,lat2,old_id,order))
  coords <- coords[,c("old_id","seg_id","ncoord","order")] 
  #Frequence d'apparition des points
  count_pt <- plyr::count(data.frame(rbind(unique_seg[,c("lon","lat")],
                                           plyr::rename(unique_seg[,c("lon2","lat2")],replace=c("lon2"="lon","lat2"="lat")))))
  unique_seg$count_pt <- count_pt[match(paste0(unique_seg$lon,unique_seg$lat),
                                        paste0(count_pt$lon,count_pt$lat)),"freq"]
  #Points de s?paration des lignes
  unique_seg$first <- (unique_seg$count_pt!=2 | !duplicated(subset(unique_seg,select=old_id)))
  #Limites des nouveaux segments
  id_first <- which(unique_seg$first==1)
  id_last <- append(id_first[2:length(id_first)]-1,nrow(unique_seg))
  #Nouvel identifiant
  length_line <- id_last - id_first + 1
  new_id <- lapply(1:length(id_first),function(x) rep(x,length_line[x]))
  unique_seg$new_id <- unlist(new_id)
  #Nouvelle table des coordonnees
  unique_seg$rang <- 1:nrow(unique_seg)*10
  new_coord <- subset(unique_seg,select=c("lon","lat","new_id","rang"))
  new_coord_last <- plyr::rename(unique_seg[id_last,c("lon2","lat2","new_id","rang")],replace=c("lon2"="lon","lat2"="lat"))
  new_coord_last$rang <- new_coord_last$rang + 1
  new_coord <- rbind(new_coord,new_coord_last)
  new_coord <- subset(new_coord[order(new_coord$rang),],select=-rang)
  #Correspondance ancien et nouvel identifiant 
  passage_new_itin <- coords[,c("old_id","seg_id","ncoord","order")]
  rm(coords)
  passage_new_itin[,c("new_id","new_order")]  <- unique_seg[match(passage_new_itin$seg_id,unique_seg$seg_id),c("new_id","order")]
  passage_new_itin[,"order"]  <- ifelse(passage_new_itin$order,1,-1)*ifelse(passage_new_itin$new_order,1,-1)
  passage_new_itin <- passage_new_itin[!duplicated(passage_new_itin[,c("old_id","new_id")]),]
  passage_new_itin[,nstep := rank(ncoord),by="old_id"]
  passage_new_itin <- passage_new_itin[,c("old_id","new_id","order","nstep")]
  return(list(plyr::rename(passage_new_itin,c("old_id"=id,"new_id"="seg_id")),
              plyr::rename(new_coord,c("new_id"="seg_id"))))}

#Simplification des segments 
simpli_coord <- function(coord_seg,seg_itin,init_identifier){
  print(paste(nrow(coord_seg),"coordonnées en entrée"))
  coord_seg <-plyr::rename(coord_seg,c("seg_id"="seg_id_init"))
  seg_itin <-plyr::rename(seg_itin,c("seg_id"="seg_id_init"))
  
  seg_simpl <- deduplicate_lines(coord_init=coord_seg,id="seg_id_init")
  
  #Suppression des doublons
  seg_itin_simpl <- seg_simpl[[1]]
  coord_seg_simpl <- seg_simpl[[2]]
  
  #Suppression des segments de longueur nulle
  #coord_seg_simpl[,n_diff_pt := sum(!duplicated(lon,lat)),by="seg_id"]
  #coord_seg_simpl <- subset(coord_seg_simpl,n_diff_pt>1,select = -n_diff_pt)
  #seg_itin_simpl <- subset(seg_itin_simpl,seg_id %in% coord_seg_simpl$seg_id)

  #Lien avec l'identifiant itinéraires
  seg_itin_simpl <- left_join(plyr::rename(seg_itin_simpl,c("order"="order_simpl","nstep"="nstep_simpl")),
                              plyr::rename(seg_itin,c("order"="order_init","nstep"="nstep_init")),
                              by="seg_id_init")
  seg_itin_simpl <- seg_itin_simpl[order(seg_itin_simpl[[init_identifier]],nstep_init,nstep_simpl),]
  seg_itin_simpl[,nstep := 1:.N,by=init_identifier]
  seg_itin_simpl$order <- with(seg_itin_simpl,order_init*order_simpl)
  seg_itin_simpl <- subset(seg_itin_simpl,select=c(init_identifier,"seg_id","nstep","order"))
  
  print(paste(nrow(coord_seg_simpl),"coordonnées en sortie"))
  return(list(coord_seg_simpl,seg_itin_simpl))
}

#Sortie 
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

st_fasterize_map <- function(map_linestring){
  map_linestring <- st_transform(map_linestring,crs=3035)
  map_linestring$value <- 1
  raster_line <- lapply(1:nrow(map_linestring), function(l){
    first_last <- st_coordinates(map_linestring[l,])
    first_last <- first_last[c(1,nrow(first_last)),1:2]
    #bbox de la ligne
    bbox <- st_bbox(map_linestring[l,])
    bbox[1:2] <- floor(bbox[1:2]/200)*200
    bbox[3:4] <- ceiling(bbox[3:4]/200)*200
    raster <- raster(xmn=bbox["xmin"],ymn=bbox["ymin"],xmx=bbox["xmax"],ymx=bbox["ymax"],resolution=200,crs=3035)
    if(dim(raster)[1]<=2 & dim(raster)[2]<=2){
      route <- floor(first_last/200)*200+100
      route <- st_linestring(route[,c("X","Y")])
      route <- st_sfc(route,crs=3035)
      route <- st_sf(geometry=route,seg_id=map_linestring[l,]$seg_id)
      st_crs(route) <- 3035
    }else{
      raster <- fasterize(st_buffer(map_linestring[l,],142),raster,field="value")
      raster <- transition(raster,function(x) min(x),directions = 8)
      first_last <- st_coordinates(map_linestring[l,])
      first_last <- first_last[c(1,nrow(first_last)),]
      path <- shortestPath(raster, first_last[1,c("X","Y")], first_last[2,c("X","Y")], output = "SpatialLines")
      route <- st_as_sf(path)
      route$seg_id <- map_linestring[l,]$seg_id
      st_crs(route) <- 3035
    }
    return(route)
  })
  raster_line <- do.call(rbind,raster_line)
  return(raster_line)
}
