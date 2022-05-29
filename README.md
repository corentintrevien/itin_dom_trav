# itin_dom_trav

Permet de déterminer les caractéristiques des itinéraires routiers pour l'ensemble des déplacements domicile-travail d'un territoire donné. 

Exemple cartographique des indicateurs obtenus pour les flux domicile-travail de la communauté d'agglomération Saint-Brieuc Armor en 2018 : 
[saint_brieuc_armor_map.pdf](https://github.com/corentintrevien/itin_dom_trav/files/8793960/saint_brieuc_armor_map.pdf)

## Programme  

Le programme itineraire_domicile_travail.R contient six fonctions principales à lancer successivement ainsi qu'un exemple d'utilisation sur l'EPCI Saint-Brieuc Armor en 2018
- extract_rp_data : télécharge les flux domicile-travail sur un territoire donné pour une année donnée sur le site de l'Insee (résultats du recensement)
- download_itin : calcul les itinéraires routiers pour les ensemble des flux domicile-travail 
- simplify_itin : simplification des itinéraires routiers, c'est-à-dire, reconstruction du réseau routier et recodage des intéraires comme un enchainement de segments élémentaires du réseau routier
- map_complem : calcul d'informations complémentaires sur les segments élémentaires (sens de circulation, flux de navetteurs, vitesse, altitude, dénivé, caractère urbain ou autoroutier)
- stat_complem : calcul des caractéristiques des itinéraires domicile-travail à partir des informations complémentaire des segments 
- draw_map : génération de la carte des caractéristiques des itinéraires domicile-travail du territoire sélectionné

Le programme fonctions.R contient un ensemble de fonctions secondaires utiles (téléchargement de fichier, lecture des API, traitement des coordonnées géographiques) 

## Output   

Les six fonctions produisent les fichiers suivants, dans le dossier [zone] défini par l'utilisateur pour le territoire choisi : 
- [zone]_map.shp : fichier cartographique contenant l'ensemble des segments élémentaires du réseau routier, empruntés pour les déplacements domicile-travail sur le territoire choisi 
- map_complements_[zone].csv.gz : informations complémentaires de chaque segement routier (sens de circulation, flux de navetteurs, vitesse, altitude, dénivé, caractère urbain ou autoroutier)
- map_init_[zone].csv.gz : enchainement de segments élémentaires de chaque itinéraire domicile-travail 
- stat_init_[zone].csv.gz : caractéristiques de chaque itinéraire domicile-travail (commune de départ, commune d'arrivée, nombre de travailleurs concernés, distance et temps routiers, trafic moyen, médian et maximum sur l'itinéraire, dénivelé, part du trajet en ville ou sur autoroute)  
- Map/[zone]_map.pdf : carte des caractéristiques des itinéraires domicile-travail du territoire sélectionné
