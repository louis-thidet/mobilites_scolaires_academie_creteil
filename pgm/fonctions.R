nom_dep <- function(sel_dep)
{
  if (sel_dep == "77")
  {
    nom_dep = "en Seine-et-Marne"
  }
  else if (sel_dep == "94")
  {
    nom_dep = "dans le Val-de-Marne"
  }
  else
  {
    nom_dep = "en Seine-Saint-Denis"
  }
  return(nom_dep)
}

couche_proportion <- function()
{
  # COMPTAGE DES ELEVES ETUDIANT DANS LA COMMUNE FILTREE
  don_RP2019_MOBSCO_couche <- don_RP2019_MOBSCO %>% group_by(COMMUNE, LIB_COMMUNE, ILETUD, LIB_ILETUD) %>% # agrégation par la variable
    summarize(nbeleves = n()) %>% 
    ungroup() %>%
    as.data.frame()
  
  # TRANSFORMATION DU TABLEAU : Les modalités deviennent des colonnes
  # le nombre d'élèves de la commune sélectionnée étudiant et résident dans celle-ci : la valeur "Dans la commune de résidence
  # les autres colonnes, ce sont le nombre d'élèves étudiant dans la commune sélectionnée mais vivant dans une autre commune.
  # Les colonnes varient en fonction de la nature de la commune où vit l'élève d'une autre commune : elle est dans le même département
  # ou pas, dans une autre région...
  don_RP2019_MOBSCO_couche <- don_RP2019_MOBSCO_couche %>%
    pivot_wider(
      id_cols = c(COMMUNE, LIB_COMMUNE),
      names_from = LIB_ILETUD,
      values_from = nbeleves,
      values_fn = list(nbeleves = sum),
      values_fill = 0
    )
  
  nb_moda = 7
  noms_colonnes = c()
  for (i in 3:(2 + nb_moda))
  {
    nom_colonne <- colnames(don_RP2019_MOBSCO_couche)[i]  # récupération du nom de la colonne à l'indice i
    noms_colonnes <- c(noms_colonnes, nom_colonne)  # ajout du nom de la colonne au vecteur
  }
  
  don_RP2019_MOBSCO_couche$total <- rowSums(don_RP2019_MOBSCO_couche[noms_colonnes])
  don_RP2019_MOBSCO_couche$nb_nonresidents <- don_RP2019_MOBSCO_couche$total-don_RP2019_MOBSCO_couche$`Dans la commune de résidence actuelle`
  don_RP2019_MOBSCO_couche$part_residents <- 100*don_RP2019_MOBSCO_couche$`Dans la commune de résidence actuelle`/don_RP2019_MOBSCO_couche$total
  don_RP2019_MOBSCO_couche$part_non_residents <- 100-don_RP2019_MOBSCO_couche$part_residents

  
  don_RP2019_MOBSCO_couche <- com %>% left_join(don_RP2019_MOBSCO_couche)
  
  return(don_RP2019_MOBSCO_couche)
}


