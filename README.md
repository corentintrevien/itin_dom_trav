# itin_dom_trav

Permet de déterminer les caractéristiques des itinéraires routiers pour les déplacements domicile-travail pour un ensemble d'origines-destinations.

Exemple cartographique des indicateurs obtenus pour les flux domicile-travail de la communauté d'agglomération Saint-Brieuc Armor en 2018 : 
[saint_brieuc_armor_map.pdf](https://github.com/corentintrevien/itin_dom_trav/files/8793960/saint_brieuc_armor_map.pdf)

## Programme  

Le programme itineraire_domicile_travail.R contient six fonctions principales à lancer successivement ainsi qu'un exemple d'utilisation sur l'EPCI Saint-Brieuc Armor en 2018
- extract_rp_data : télécharge les flux domicile-travail sur un territoire donné et une année donnée sur le site de l'Insee (résultats du recensement)
- download_itin : calcul les intinéraires routiers pour les ensemble des flux domicile-travail 
- simplify_itin : simplification des intinéraires routiers c'est-à-dire, reconstruction du réseau routier et recodage des intéraires comme un enchainement de segments
- map_complem : calcul d'informations complémentaiers 
- stat_complem
- draw_map

Le programme fonctions.R contient un ensemble de fonctions secondaires utiles (téléchargement de fichier, lecture des API, traitement des coordonnées géographiques) 

## Output   
