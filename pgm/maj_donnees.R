library(httr)
library(jsonlite)
library(dplyr)

# TELECHARGEMENT BASE DE DONNEES 1 : RP2019 (fichier détail MOBSCO)

GET("https://www.insee.fr/fr/statistiques/fichier/6456052/RP2019_mobsco_csv.zip", write_disk("data/don_RP2019.zip", overwrite = TRUE))

unzip(zipfile = "data/don_RP2019.zip", files = "FD_MOBSCO_2019.csv", exdir = "data/")
unzip(zipfile = "data/don_RP2019.zip", files = "Varmod_MOBSCO_2019.csv", exdir = "data/")

file.remove("data/don_RP2019.zip")

# TELECHARGEMENT BASE DE DONNEES 2 : Adresse et géolocalisation des établissements d'enseignement du premier et second degrés

don_ADRESSES <- GET("https://www.data.gouv.fr/fr/datasets/r/85da76fc-a4b1-458a-96bd-d7df92ccd32c")
don_ADRESSES <- fromJSON(rawToChar((don_ADRESSES$content)))
don_ADRESSES <- don_ADRESSES$features$properties

don_ADRESSES <- apply(don_ADRESSES,2,as.character)
write.csv(don_ADRESSES, "data/don_ADRESSES.csv", row.names=FALSE)
