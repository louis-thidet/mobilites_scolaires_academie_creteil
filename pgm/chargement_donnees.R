library(data.table)
library(sf)
library(dplyr)

#######################################################################
### CHARGEMENT DES DONNEES DES ADRESSES DES ETABLISSEMENT SCOLAIRES ###
#######################################################################

don_ADRESSES <- fread("data/don_ADRESSES.csv") # données ADRESSES

don_ADRESSES$latitude <- as.numeric(don_ADRESSES$latitude)
don_ADRESSES$longitude <- as.numeric(don_ADRESSES$longitude)

don_ADRESSES <- don_ADRESSES[complete.cases(don_ADRESSES$latitude), ] # Supprimer les coordonnées inexistantes
don_ADRESSES <- subset(don_ADRESSES, don_ADRESSES$libelle_academie %in% "Créteil") # restriction aux académies d'Île-de-France

don_ADRESSES <- don_ADRESSES %>% filter(date_ouverture <= "2020-01-01")

liste_communes <- unique(substr(don_ADRESSES$code_commune, 1, 2)) # liste des communes à retenir

#####################################
### CHARGEMENT DES DONNEES RP2019 ###
#####################################

don_RP2019_MOBSCO <- fread("data/FD_MOBSCO_2019.csv") # chargement  données RP2019 MOBSCO
meta_RP2019_MOBSCO <- fread("data/Varmod_MOBSCO_2019.csv") # chargement metadonnées RP2019 MOBSCO
don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% select(COMMUNE, DCETUF, ARM, CSM, DIPL, ILETUD, ILETUU, STOCD) # sélection des colonnes à retenir

don_RP2019_MOBSCO <- subset(don_RP2019_MOBSCO, substr(COMMUNE, 1, 2) %in% liste_communes) # restriction aux communes d'Île-de-France

#don_RP2019_MOBSCOe <- subset(don_RP2019_MOBSCO, COMMUNE %in% liste_communes) # restriction aux communes d'Île-de-France

nb_colonnes <- ncol(don_RP2019_MOBSCO) # comptage nombre de colonnes
noms_colonnes = c() # récupération du nom des colonnes

for (i in 1:nb_colonnes)
{
  nom_colonne <- colnames(don_RP2019_MOBSCO)[i]  # récupération du nom de la colonne à l'indice i
  noms_colonnes <- c(noms_colonnes, nom_colonne)  # ajout du nom de la colonne au vecteur
}

# correction du type des variables pour éviter les conflits lors de la boucle à venir
don_RP2019_MOBSCO$ILETUD <- as.character(don_RP2019_MOBSCO$ILETUD)
don_RP2019_MOBSCO$ILETUU <- as.character(don_RP2019_MOBSCO$ILETUU)

# ajout des metadonnées au tableau
for(i in 1:nb_colonnes)
{
  metadonnees_var_i <- subset(meta_RP2019_MOBSCO, COD_VAR %in% noms_colonnes[i]) # filtrage des metadonnées sur une variable

  metadonnees_var_i <- metadonnees_var_i %>%   # renommage des variables utiles des métadonnées et sélection de celles-ci
    rename(!!noms_colonnes[i] := COD_MOD, !!paste0("LIB_", noms_colonnes[i]) := LIB_MOD) %>% select(!!noms_colonnes[i], !!paste0("LIB_", noms_colonnes[i]))
  
  don_RP2019_MOBSCO <-left_join(don_RP2019_MOBSCO,metadonnees_var_i) # jointure des metadonnées à la table principale
}

# retirer les hors-sujet (ce qui est au-delà du lycée)
don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(LIB_DIPL != "Doctorat de recherche (hors santé)")
don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(LIB_DIPL != "Licence, licence pro, maîtrise, diplôme équivalent de niveau bac+3 ou bac+4")
don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(LIB_DIPL != "Baccalauréat professionnel, brevet professionnel, de technicien ou d’enseignement, diplôme équivalent de niveau bac+3 ou bac+4")
don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(LIB_DIPL != "Licence, licence pro, maîtrise, diplôme équivalent de niveau bac+3 ou bac+4")
don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(LIB_DIPL != "BTS, DUT, Deug, Deust, diplôme de la santé ou du social de niveau bac+2, diplôme équivalent" )
don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(LIB_DIPL != "Master, DEA, DESS, diplôme grande école niveau bac+5, doctorat de santé")





###################################
### CHARGEMENT DU FOND DE CARTE ###
###################################

com <- st_read("geom/communes-20220101.shp") # fond de carte des communes

#com <- subset(com, com$insee %in% liste_communes) %>% rename("COMMUNE" = "insee")
com <- subset(com, substr(insee, 1, 2) %in% liste_communes) %>% rename("COMMUNE" = "insee")

# extraction latitude et longitude du centre des communes
colonnes = c("longitude","latitude","COMMUNE")
tab_long_lat = data.frame(matrix(nrow = 0, ncol = length(colonnes)))
colnames(tab_long_lat) = colonnes

i <- 1
while(i < nrow(com))
{
  long_lat <- as.data.frame(com[[5]][[i]][[1]][[1]])
  
  ligne = c(longitude = long_lat$V1[length(long_lat$V1)/2], latitude = long_lat$V2[length(long_lat$V1)/2], COMMUNE = com$COMMUNE[i])
  
  tab_long_lat = rbind(tab_long_lat,ligne)
  i <- i+1
}

names(tab_long_lat)[1] <- "longitude"
names(tab_long_lat)[2] <- "latitude"
names(tab_long_lat)[3] <- "COMMUNE"

com <- com %>% left_join(tab_long_lat)

com <- com %>% select(COMMUNE, longitude, latitude, geometry) # restriction à la liste des communes d'Île-de-France

com <- com %>% left_join(unique(don_RP2019_MOBSCO[, c("LIB_COMMUNE", "COMMUNE")]), by = "COMMUNE")

# libération de la mémoire
rm(meta_RP2019_MOBSCO)
rm(i)
rm(nb_colonnes)
rm(nom_colonne)
rm(noms_colonnes)
rm(metadonnees_var_i)
rm(colonnes)
rm(ligne)
rm(liste_communes)
rm(long_lat)
rm(tab_long_lat)

#
don_RP2019_MOBSCO_couche <- couche_proportion()
