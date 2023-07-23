library(shiny)

# manipulation de données
library(dplyr)
library(sf)
library(DT)
library(tidyr)

# cartographie statique
library(mapsf)
library(RColorBrewer) # palettes de couleurs

# cartographie dynamique
library(leaflet)
library(leaflet.minicharts)

# HTML
library(base64enc) # charger images

# API
library(httr)
library(jsonlite)

################ CHARGEMENT DES DONNEES ###########################################

#source("pgm/maj_donnees.R") # script de mise à jour des donnees
source("pgm/fonctions.R") # chargement des fonctions
#source("data/temp_shinyapps_io/script.R") # chargement de fichiers déjà prêts, pour contourner la limite de mémoire de shinyapps.io
source("pgm/chargement_donnees.R") # script de chargement des données

# chargement de l'image de fond de l'application
image_path <- "img/fond.jpg"
image_data <- readBin(image_path, "raw", file.info(image_path)$size)
image_fond <- base64encode(image_data)
# chargement des images de fond des boutons
image_path <- "img/visu_nbventes_oui.PNG" # bouton pour activer la visualisation des ventes sur la carte
image_data <- readBin(image_path, "raw", file.info(image_path)$size)
visu_nbventes_oui <- base64encode(image_data)
image_path <- "img/visu_nbventes_non.PNG" # bouton pour désactiver la visualisation des ventes sur la carte
image_data <- readBin(image_path, "raw", file.info(image_path)$size)
visu_nbventes_non <- base64encode(image_data)

################ PARTIE INTERFACE #################################################

###########################
### BARRE DE NAVIGATION ###
###########################
ui <- navbarPage("Les mobilités scolaires intercommunales dans l'Académie de Créteil en 2019",
                 # CREATION DES STYLES CSS CONFIGURANT LES IMAGES DE FOND
                 tags$style
                 (
                   HTML
                   ("
      .image_fond {
        position: fixed;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        z-index: -1;
        background-image: linear-gradient(to bottom, rgba(255, 255, 255, 0) 0%, rgba(255, 255, 255, 1) 110%), url('data:image/png;base64,", image_fond, "');
        background-repeat: no-repeat;
        background-size: cover;
      }
    .style_btn_oui {
      background-image: url('data:image/png;base64,", visu_nbventes_oui, "')!important;
      padding:20px;
    }
    .style_btn_non {
      background-image: url('data:image/png;base64,", visu_nbventes_non, "')!important;
      padding:20px;
    }
    "),
                   #########################################
                   ### INTEGRATION DES AUTRES STYLES CSS ###
                   #########################################
                   includeCSS("styles.css"),
                 ),
                 div(class = "image_fond"),
                 #####################
                 ### ONGLET CARTES ###
                 #####################
                 # ================================================ CARTES INTERACTIVES ================================================
                 tabPanel
                 ("Carte interactive",
                   fluidPage
                   (
                     sidebarLayout
                     (
                       sidebarPanel
                       (
                         div(
                           paste("Paramètres des cartes"), class = "case1"
                         ),
                         ################################
                         ### SELECTION DU DEPARTEMENT ###
                         ################################
                         selectInput(
                           inputId = "sel_dep",
                           label = "Sélection du département : ",
                           choices = NULL,
                         ),
                         ###############################
                         ### SELECTION DE LA COMMUNE ###
                         ###############################
                         selectInput(
                           inputId = "sel_commune",
                           label = "Sélection de la commune : ",
                           choices = NULL,
                         ),
                         ###########################
                         ### SENS DE LA MOBILITE ###
                         ###########################
                         selectInput(
                           inputId = "sel_mobi",
                           label = "Sélection des mobilités à afficher :",
                           choices = c("Déplacements vers la commune" = "vers",
                                       "Déplacements en-dehors de la commune" = "endehors"
                           )
                         ),
                         #################################
                         ### SELECTION DU TYPE D'ELEVE ###
                         #################################
                         conditionalPanel(
                           condition = "input.sel_commune != 'Toutes les communes'",
                           selectInput(
                             inputId = "sel_type",
                             label = "Type d'élève :",
                             choices = NULL
                           ),
                         ),
                         #############################################
                         ### SELECTION DE LA CATEGORIE DES PARENTS ###
                         #############################################
                         conditionalPanel(
                           condition = "input.sel_commune != 'Toutes les communes'",
                           selectInput(
                             inputId = "sel_categorie",
                             label = "Catégorie des parents :",
                             choices = NULL
                           ),
                         ),
                         div(
                           paste("Chiffre clé :"), class = "case1"
                         ),
                         ################################
                         ### AFFICHAGE DU CHIFFRE CLÉ ###
                         ################################
                         div(
                           htmlOutput("chiffre_cle"),class = "case2"
                         ),
                         div(
                           htmlOutput("remarque_chiffre_cle"), class = "remarque"
                          ),
                         style = "background-color:#ffeedb; border:1px solid orange;", width=3
                       ),
                       ##############################
                       ### INTEGRATION DES CARTES ###
                       ##############################      
                       mainPanel
                       (
                         class = "custom-main-panel",
                         tags$div(
                           style = "width: 100%; display: grid; grid-template-columns: 1fr 1fr;",
                           div(
                             plotOutput("carte_statique", height = "650px", width = "100%"),
                           ),
                           div(
                             leafletOutput("carte_dynamique",height = "650px", width = "100%"),
                           ),
                         ),
                         div(
                           sidebarPanel(
                             ##########################
                             ### SELECTION DU THEME ###
                             ##########################  
                             selectInput(
                               inputId = "sel_theme",
                               label = "Thème carte :",
                               choices = c("Thème 1" = "default",
                                           "Thème 2" = "agolalight",
                                           "Thème 3" = "iceberg",
                                           "Thème 4" = "brutal",
                                           "Thème 5" = "nevermind"),
                               selected = "default"
                             ),
                             ###############################
                             ### SELECTION DE LA COULEUR ###
                             ###############################
                             div(paste("Couleur carte : "),
                                 id = "couleurBouton",
                                 actionButton(inputId = "bouton_orange", label = "", class = " style_btn_orange"),
                                 actionButton(inputId = "bouton_rouge", label = "", class = "style_btn_rouge"),
                                 actionButton(inputId = "bouton_vert", label = "", class = "style_btn_vert"),
                                 actionButton(inputId = "bouton_bleu", label = "", class = "style_btn_bleu"), style = "margin-bottom:30px; font-weight: bold;"
                             ),
                             tags$input(type = "text", id = "couleur_legende", value = "Oranges"), # input de la couleur de la légende
                             tags$input(type = "text", id = "couleur_contour", value = "#4298f5"), # input de la couleur des départements
                             style = "margin-top:20px; height:200px; background-color:#ffeedb; border:1px solid orange;"
                           ),
                           sidebarPanel(
                             #########################################
                             ### SELECTION DE LA METHODE DE CLASSE ###
                             #########################################
                             selectInput(
                               inputId = "methode",
                               label = "Type de classes :",
                               choices = c("Effectifs égaux" = "quantile",
                                           "Amplitudes égales" = "equal",
                                           "Jenks" = "jenks"),
                               selected = "quantile"
                             ),
                             ######################################
                             ### SELECTION DU NOMBRE DE CLASSES ###
                             ######################################
                             sliderInput(
                               inputId = "nb_classes",
                               label = "Nombres de classes :",
                               min = 2,
                               max = 8,
                               value = 5
                             ),
                             style = "margin-top:20px; height:200px; background-color:#ffeedb; border:1px solid orange;"
                           ),
                         ),
                         width = 9
                       )
                     )
                   ),
                 ),
                 # =================================================== TABLEAUX DE DONNEES =================================================== 
                 tabPanel("Tableaux de données",
                          tabsetPanel(
                            id = 'dataset',
                            tabPanel("Données paramétrées Adresses", DT::dataTableOutput("donadresses"), style = "margin-top:20px; padding:15px; background-color:white;"),
                            tabPanel("Données paramétrées RP2019", DT::dataTableOutput("donrp2019"), style = "margin-top:20px; padding:15px; background-color:white;"),
                            tabPanel("Données brutes RP2019", DT::dataTableOutput("don_RP2019_MOBSCO"), style = "margin-top:20px; padding:15px; background-color:white;"),
                            tabPanel("Données brutes ADRESSES", DT::dataTableOutput("don_ADRESSES"), style = "margin-top:20px; padding:15px; background-color:white;"),
                            div(downloadButton("don_para_adresses", "Données paramétrées Adresses", class = "telechargement_donnees"),
                                downloadButton("don_para_RP2019", "Données paramétrées RP2019", class = "telechargement_donnees"),
                                downloadButton("donnees_RP2019", "Données RP2019", class = "telechargement_donnees"),
                                downloadButton("donnees_ADRESSES", "Données ADRESSES", class = "telechargement_donnees")
                            ),
                            div(
                              HTML(paste("<span style='color:red;;font-weight:bold;'> Définitions </span> <br>
                                             DCRA : Dans la commune de résidence actuelle<br>
                                             DACDR : Dans une autre commune du département de résidence<br>
                                             DADRR : Dans un autre département de la région de résidence<br>")), style ="font:18px; margin-top:10px;"
                            ),
                          )
                 ),
                 # =================================================== A PROPOS =================================================== 
                 tabPanel("A propos",
                          div(
                            paste("A propos "), class = "a_propos_titre"
                          ),
                          div(
                            paste("Cette application Shiny a été réalisée par Louis THIDET dans le cadre du cours Datamining du master PISE
                               de l'Université Paris-Cité, dirigé par Claude GRASLAND et Camille SIGNORETTO"), class ="a_propos"
                          ),
                          div(
                            HTML(paste("Sources : <br>
                                       <a href='https://www.data.gouv.fr/fr/datasets/adresse-et-geolocalisation-des-etablissements-denseignement-du-premier-et-second-degres-1/'>Adresse et géolocalisation des établissements d'enseignement du premier et second degrés</a> <br>
                                       <a href='https://www.insee.fr/fr/statistiques/6456052?sommaire=6456104'>Logements, individus, activité, mobilités scolaires et professionnelles, migrations résidentielles en 2019 
Recensement de la population - Fichier détail</a> <br>
                                       <a href='https://www.data.gouv.fr/fr/datasets/decoupage-administratif-communal-francais-issu-d-openstreetmap/'>Découpage administratif communal français issu d'OpenStreetMap
</a>")), class ="a_propos"
                          ),
                          div(
                            HTML(paste("Contact : <a href='mailto: louis.thidet@gmail.com'>louis.thidet@gmail.com</a>")), class ="a_propos"
                          ),
                 )
)

################ PARTIE SERVEUR ###################################################
server <- function(input, output, session) {
  
  # ======================================================================= CHOIX ======================================================================= 
  ##################################################
  ### CHARGEMENT DES CHOIX SELECTION DEPARTEMENT ###
  ##################################################
  observe({
    
    choices <- unique(substr(don_ADRESSES$code_postal_uai, 1, 2))
    updateSelectInput(session, "sel_dep", choices = choices)
    
  })
  
  ###############################################
  ### CHARGEMENT DES CHOIX SELECTION COMMMUNE ###
  ###############################################
  observe({
    
    choices <- unique(don_ADRESSES[, c("code_commune", "libelle_commune")])
    choices <- choices[substr(choices$code_commune, 1, 2) == input$sel_dep, ]
    choices <- choices$libelle_commune
    choices <- sort(choices, decreasing = FALSE) # tri par ordre alphabétique
    choices <- append(choices, "Toutes les communes")
    temp <- choices[length(choices)]
    choices <- choices[-length(choices)]
    choices <- c(temp, choices)
    
    updateSelectInput(session, "sel_commune", choices = choices)
    
  })
  
  
  #################################################
  ### CHARGEMENT DES CHOIX SELECTION TYPE ELEVE ###
  #################################################
  observe({
    
    choices <- unique(don_RP2019_MOBSCO$LIB_DIPL)
    choices <- append(choices, "Pas de filtre")
    temp <- choices[length(choices)]
    choices <- choices[-length(choices)]
    choices <- c(temp, choices)
    
    updateSelectInput(session, "sel_type", choices = choices) 
    
  })
  
  ########################################################
  ### CHARGEMENT DES CHOIX SELECTION CATEGORIE PARENTS ###
  ########################################################
  observe({
    
    choices <- unique(don_RP2019_MOBSCO$LIB_CSM)
    choices <- append(choices, "Pas de filtre")
    temp <- choices[length(choices)]
    choices <- choices[-length(choices)]
    choices <- c(temp, choices)
    
    updateSelectInput(session, "sel_categorie", choices = choices) 
    
  })
  
  # ======================================================================= PREPARATION DONNEES ======================================================================= 
  ####################################
  ### PREPARATION DONNEES ADRESSES ###
  ####################################
  don_ADRESSES_parametrees <- reactive({
    
    if(input$sel_commune != "Toutes les communes") # affichage d'une commune
    {
      don_ADRESSES_pretes <- subset(don_ADRESSES, don_ADRESSES$libelle_commune %in% input$sel_commune)
    }
    else # affichage d'un département
    {
      don_ADRESSES_pretes <- subset(don_ADRESSES, substr(don_ADRESSES$code_commune, 1, 2) %in% input$sel_dep)
    }
    
    return(don_ADRESSES_pretes)
    
  })
  
  ##################################
  ### PREPARATION DONNEES RP2019 ###
  ##################################
  don_RP2019_MOBSCO_parametrees <- reactive({
    
    req(don_ADRESSES_parametrees()) # la fonction est exécutée après la production de don_ADRESSES_parametrees()
    
    code_commune <- unique(don_ADRESSES_parametrees()$code_commune)
    
    if(input$sel_type != "Pas de filtre"){
      don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(don_RP2019_MOBSCO$LIB_DIPL == input$sel_type)
    }
    
    if(input$sel_categorie != "Pas de filtre"){
      don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(don_RP2019_MOBSCO$LIB_CSM == input$sel_categorie)
    }
    
    
    if(input$sel_mobi == "vers"){ ### VONT ETUDIER VERS ###
      
      # FILTRAGE SUR LES COMMUNES CONCERNEES
      don_RP2019_MOBSCO <- subset(don_RP2019_MOBSCO, DCETUF %in% code_commune)
      
      # COMPTAGE DES ELEVES ETUDIANT DANS LA COMMUNE FILTREE
      don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% group_by(COMMUNE, LIB_COMMUNE, ILETUD, LIB_ILETUD) %>% # agrégation par la variable
        summarize(nbeleves = n()) %>% 
        ungroup() %>%
        as.data.frame()
      
      # TRANSFORMATION DU TABLEAU : Les modalités deviennent des colonnes
      don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>%
        pivot_wider(
          id_cols = c(COMMUNE, LIB_COMMUNE),
          names_from = LIB_ILETUD,
          values_from = nbeleves,
          values_fn = list(nbeleves = sum),
          values_fill = 0
        )
    }
    else
    {
      don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(COMMUNE == code_commune)
      
      don_RP2019_MOBSCO <- subset(don_RP2019_MOBSCO, substr(don_RP2019_MOBSCO$DCETUF, 1, 2) %in% c("77","93","94")) # L'application ne traite que de 
      # l'intérieur de l'académie de Créteil.
      # Dans le sens "vers", on n'a pas à faire ce filtre
      # car dans chargement_donnees.R a été filtrée la colonne
      # COMMUNE sur les communes de l'académie de Créteil
      #don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(COMMUNE == code_commune)
      don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(COMMUNE != DCETUF)
      
      # COMPTAGE DES ELEVES ETUDIANT DANS LA COMMUNE FILTREE
      don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% group_by(DCETUF, LIB_COMMUNE, ILETUD, LIB_ILETUD) %>% # agrégation par la variable
        summarize(nbeleves = n()) %>% 
        ungroup() %>%
        as.data.frame()
      
      # TRANSFORMATION DU TABLEAU : Les modalités deviennent des colonnes
      don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>%
        pivot_wider(
          id_cols = c(DCETUF, LIB_COMMUNE),
          names_from = LIB_ILETUD,
          values_from = nbeleves,
          values_fn = list(nbeleves = sum),
          values_fill = 0
        )
      
      
    }
    
    # RENOMMAGE DES VARIABLES
    if(input$sel_mobi == "vers"){
      don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>%
        rename(DCRA = `Dans la commune de résidence actuelle`) # DCRA
    }
    
    if('Dans une autre commune du département de résidence' %in% names(don_RP2019_MOBSCO)){
      don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>%
        rename(DACDR = `Dans une autre commune du département de résidence`) # DACDR
    }
    
    if('Dans un autre département de la région de résidence' %in% names(don_RP2019_MOBSCO)){
      don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>%
        rename(DADRR = `Dans un autre département de la région de résidence`) # DADRR
    }
    
    
    if(("DADRR" %in% names(don_RP2019_MOBSCO)) && ("DACDR" %in% names(don_RP2019_MOBSCO))){
      don_RP2019_MOBSCO$var_unique <- ifelse(don_RP2019_MOBSCO$DADRR != 0, don_RP2019_MOBSCO$DADRR, don_RP2019_MOBSCO$DACDR)
    }
    else if("DACDR" %in% names(don_RP2019_MOBSCO)){
      don_RP2019_MOBSCO$var_unique <- don_RP2019_MOBSCO$DACDR
    }
    else if("DADRR" %in% names(don_RP2019_MOBSCO)){
      don_RP2019_MOBSCO$var_unique <- don_RP2019_MOBSCO$DADRR
    }
    
    
    
    
    
    
    
    don_RP2019_MOBSCO_parametrees <- don_RP2019_MOBSCO
    
    return(don_RP2019_MOBSCO_parametrees)
    
  })
  
  # ======================================================================= PREPARATION CARTES ======================================================================= 
  ##################################
  ### PREPARATION CARTE STATIQUE ###
  ##################################
  prepa_carte_statique <- reactive({
    
    req(don_ADRESSES_parametrees()) # la fonction est exécutée après la production de don_ADRESSES_parametrees()
    
    # CREATION D'UNE PALETTE DE COULEURS POUR LES CLASSES
    palette <- brewer.pal(name = input$couleur_legende, n = input$nb_classes)
    
    # =============================================== #
    # ============== CARTE DEPARTEMENT ============== #
    # =============================================== #
    if(input$sel_commune == "Toutes les communes")
    {
      # FILTRAGE SUR LES COMMUNES DU DEPARTEMENT
      don_RP2019_MOBSCO_couche <- don_RP2019_MOBSCO_couche %>% filter(substr(don_RP2019_MOBSCO_couche$COMMUNE, 1, 2) == input$sel_dep)
      
      # INITIALISATION DE LA CARTE
      mf_init(don_RP2019_MOBSCO_couche)
      
      # APPLICATION DU THEME
      if(input$sel_theme == "default" || input$sel_theme == "agolalight" || input$sel_theme == "nevermind" || input$sel_theme == "green"){
        mf_theme(input$sel_theme, pos = "left", cex = 1.5, font = 3, tab = FALSE)
      }
      else{
        mf_theme(input$sel_theme, pos = "center", cex = 1.4, font = 1, tab = FALSE)
      }
      
      # AJOUT D'UNE OMBRE
      mf_shadow(don_RP2019_MOBSCO_couche, col = "grey50", cex = 3)
      
      # SELECTION DE LA VARIABLE EN FONCTION DU SENS DE MOBILITE SELECTIONNE
      if(input$sel_mobi == "endehors"){ ####################
        var <- "part_residents"
        titre <- "Part d'élèves-résidents \nde la commune"
      }
      else{
        var <- "part_non_residents"
        titre <- "Part d'élèves venant d'une \nautre commune"
      }
      
      # CREATION DE LA COUCHE MONTRANT LA PART D'ELEVES RESIDANT OU NE RESIDANT PAS DANS LA COMMUNE
      mf_map(don_RP2019_MOBSCO_couche,
             leg_title = titre,
             var = var,
             type = "choro",
             nbreaks = input$nb_classes,
             breaks = input$methode,
             pal = palette,
             leg_pos = "bottomleft2",
             leg_title_cex = 1.2,
             leg_val_cex = 1.2,
             add = TRUE)
      
      # RECUPERATION DU NOM DU DEPARTEMENT
      nom_dep <- nom_dep(input$sel_dep)
      
      if(input$sel_mobi == "endehors"){ #####################
        titre <- paste("Part d'élèves qui résident dans leur commune de scolarisation", nom_dep)
      }
      else{
        titre <- paste("Part d'élèves n'étant pas scolarisés dans leur commune de résidence", nom_dep)
      }
      
      # TITRE DE LA CARTE
      mf_layout(title = titre,
                arrow = FALSE, credits= "", scale = FALSE)
      
      # CREDITS DE LA CARTE
      mf_credits(
        txt = paste("Source : Base RP2019\nfichier détail MOBSCO"),
        pos = "bottomleft",
        col = "black",
        cex = 1,
        font = 3,
        bg = NA
      )
    }
    # =========================================== #
    # ============== CARTE COMMUNE ============== #
    # =========================================== #
    else
    {
      # RECUPERATION DU CODE DE LA COMMUNE SELECTIONNEE
      code_commune <- unique(don_ADRESSES_parametrees()$code_commune)
      
      # FILTRAGE SUR UN CERTAIN TYPE D'ELEVE

      
      # MISE EN MEMOIRE DU FOND DE CARTE AU NIVEAU DE L'ACADEMIE
      com_acad <- com
      
      # FILTRAGE DES COMMUNES EN FONCTION DU SENS DE MOBILITE SELECTIONNE
      if(input$sel_mobi == "vers"){ ### VONT ETUDIER VERS ###
        
        # FILTRAGE SUR LES COMMUNES CONCERNEES
        don_RP2019_MOBSCO <- subset(don_RP2019_MOBSCO, DCETUF %in% code_commune)
        
        # FILTRAGE DES COMMUNES CONCERNEES SUR LE FOND DE CARTE
        com <- subset(com, COMMUNE %in% don_RP2019_MOBSCO$COMMUNE)
      }
      else{ ### VONT ETUDIER EN-DEHORS ###
        don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(COMMUNE == code_commune)
        
        don_RP2019_MOBSCO <- subset(don_RP2019_MOBSCO, substr(don_RP2019_MOBSCO$DCETUF, 1, 2) %in% c("77","93","94")) # L'application ne traite que de 
        # l'intérieur de l'académie de Créteil.
        # Dans le sens "vers", on n'a pas à faire ce filtre
        # car dans chargement_donnees.R a été filtrée la colonne
        # COMMUNE sur les communes de l'académie de Créteil
        don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(COMMUNE == code_commune)
        don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(COMMUNE != DCETUF)
        
        # MISE EN MEMOIRE DU FOND DE CARTE SUR LA COMMUNE SELECTIONNEE
        com_selec <- com %>% filter(COMMUNE == code_commune)
        
        # FILTRAGE DES COMMUNES CONCERNEES SUR LE FOND DE CARTE
        com <- subset(com, COMMUNE %in% don_RP2019_MOBSCO$DCETUF)
        
      }
      
      
      
      don_RP2019_MOBSCO <- don_RP2019_MOBSCO_parametrees()
      
      
      
      # JOINTURE DE LA CARTE ET DES DONNEES
      if(input$sel_mobi == "endehors"){
        don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% rename("COMMUNE" = "DCETUF")
        don_RP2019_MOBSCO <- com %>% left_join(don_RP2019_MOBSCO, by = "COMMUNE")
      }
      else
      {
        don_RP2019_MOBSCO <- com %>% left_join(don_RP2019_MOBSCO)
      }
      
      
      
      # INITIALISATION DE LA CARTE ET AJOUT DE L'OMBRE DE LA CARTE
      
      mf_init(don_RP2019_MOBSCO)
      
      if(input$sel_theme == "default" || input$sel_theme == "agolalight" || input$sel_theme == "green")
      {
        mf_theme(input$sel_theme, pos = "left", cex = 1.5, font = 3, tab = FALSE)
      }
      else
      {
        mf_theme(input$sel_theme, pos = "center", cex = 1.4, font = 1, tab = FALSE)
      }
      
      
      # COUCHE CONTOUR DES DEPARTEMENTS
      mf_map(com_acad,
             type = "base",
             col = "grey",
             border=input$couleur_contour,
             lwd=1.5,
             add = TRUE)
      
      classe_milieu <- floor(input$nb_classes / 2+2)
      couleur <- palette[classe_milieu]
      
      
      # COUCHE CONTOUR DES DEPARTEMENTS
      mf_map(don_RP2019_MOBSCO,
             type = "base",
             col = "white",
             border= input$couleur_contour,
             lwd=1.5,
             add = TRUE)
      
      
      # COMMUNES DONT DES ELEVES VONT VERS LA COMMUNE SELECTIONNEE
      mf_map(don_RP2019_MOBSCO,
             leg_title = "Nombre d'élèves",
             var = "var_unique",
             type = "prop",
             col = couleur,
             border = "black",
             leg_pos = "bottomleft2",
             leg_title_cex = 1.5,
             inches = 0.3,
             leg_val_cex = 1.3,
             add = TRUE)
      
      # COMMUNE SELECTIONEE
      if(input$sel_mobi == "vers")
      {
        don_RP2019_MOBSCO <- don_RP2019_MOBSCO %>% filter(sub(" \\(.*", "", don_RP2019_MOBSCO$LIB_COMMUNE) == input$sel_commune)
        
        mf_map(don_RP2019_MOBSCO,
               type = "base",
               col = "#dc9c84",
               border = "black",
               lwd = 2,
               leg_pos = "NA",
               add = TRUE)
        
        val <- don_RP2019_MOBSCO$DCRA
        
        mf_legend(
          type = "typo", pos = "topleft", val = "", title = "Commune sélectionnée", val_cex = 1.3, title_cex = 1.5, pal = c("#dc9c84")
        )
        
        mf_layout(title = paste("Les élèves de", input$sel_commune,"provenant d'autres communes"),
                  arrow = FALSE, scale = FALSE, credits= "")
      }
      else
      {
        mf_map(com_selec,
               type = "base",
               col = "#dc9c84",
               border = "black",
               lwd = 2,
               leg_pos = "NA",
               add = TRUE)
        
        mf_legend(
          type = "typo", pos = "topleft", val = "", title = "Commune sélectionnée", val_cex = 1.3, title_cex = 1.5, pal = c("#dc9c84")
        )
        
        mf_layout(title = paste("Les élèves de", input$sel_commune,"scolarisés dans d'autres communes"),
                  arrow = FALSE, scale = FALSE, credits= "")
      }
      
      mf_credits(
        txt = paste("Source : Base RP2019\nfichier détail MOBSCO"),
        pos = "bottomleft",
        col = "black",
        cex = 1,
        font = 3,
        bg = NA
      )
      
      
      
    }
    
  })
  
  ###################################
  ### PREPARATION CARTE DYNAMIQUE ###
  ###################################
  prepa_carte_dynamique <- reactive({
    
    req(input$sel_commune)
    req(don_RP2019_MOBSCO_parametrees())
    
    # ======================================================= #
    # ============== PREPARATION DE LA CARTE ============== #
    # ======================================================= #
    if(input$sel_commune != "Toutes les communes") # UNE COMMUNE
    {
      code_commune <- unique(don_ADRESSES_parametrees()$code_commune)
      
      # PREPARATION COUCHE 1
      if(input$sel_mobi == "vers") ### VONT ETUDIER VERS ###
      {

        com <- subset(com, COMMUNE %in% don_RP2019_MOBSCO_parametrees()$COMMUNE)
        don_RP2019_MOBSCO_parametrees <- don_RP2019_MOBSCO_parametrees()
        com <- com %>% left_join(don_RP2019_MOBSCO_parametrees)
      }
      else ### VONT ETUDIER EN-DEHORS ###
      {

        don_RP2019_MOBSCO_parametrees <- don_RP2019_MOBSCO_parametrees()
        don_RP2019_MOBSCO_parametrees <- don_RP2019_MOBSCO_parametrees %>% rename(COMMUNE = DCETUF)
        com <- subset(com, COMMUNE %in% don_RP2019_MOBSCO_parametrees$COMMUNE)
        
        com <- com %>% left_join(don_RP2019_MOBSCO_parametrees, by = "COMMUNE")
        
      }
      
      # PREPARATION COUCHE 2
      don_RP2019_MOBSCO_couche <- don_RP2019_MOBSCO_couche %>% filter(sub(" \\(.*", "", don_RP2019_MOBSCO_couche$LIB_COMMUNE) == input$sel_commune)
      
      # OBTENTION DES COORDONNEES DE LA COMMUNE SELECTIONNEE
      long_lat <- unique(don_ADRESSES[, c("libelle_commune", "code_commune", "latitude", "longitude")])
      long_lat <- subset(long_lat, long_lat$libelle_commune %in% input$sel_commune)
      latitude <- long_lat$latitude[1]
      longitude <- long_lat$longitude[1]
      zoom <- 12
      
    }
    else # UN DEPARTEMENT
    {
      # OBTENTION DES COORDONNEES DU DEPARTEMENT SELECTIONNE
      if(input$sel_dep == "94")
      {
        latitude <- 48.7838849656266
        longitude <- 2.46694397340301
        zoom <- 10
      }
      else if(input$sel_dep == "77")
      {
        latitude <- 48.6132378641319
        longitude <- 3.01937960766232
        zoom <- 9
      }
      else
      {
        latitude <- 48.9090054809632
        longitude <- 2.47087148467765
        zoom <- 10
      }
      
      # PREPARATION COUCHE DES COMMUNES DU DEPARTEMENT SELECTIONNE
      don_RP2019_MOBSCO_couche <- don_RP2019_MOBSCO_couche %>% filter(substr(don_RP2019_MOBSCO_couche$COMMUNE, 1, 2) == input$sel_dep)
      
    }
    
    # CREATION D'UNE PALETTE
    palette <- brewer.pal(name = input$couleur_legende, n = input$nb_classes) # On récupère la palette générée pour la légende
    bins <- seq(0, 100, length.out = input$nb_classes+1)
    
    if(input$sel_mobi == "vers")
    {
      pal <- colorBin(palette, domain = don_RP2019_MOBSCO_couche$part_residents, bins = bins)
    }
    else
    {
      pal <- colorBin(palette, domain = don_RP2019_MOBSCO_couche$part_non_residents, bins = bins)
    }
    
    # ================================================== #
    # ============== CREATION DE LA CARTE ============== #
    # ================================================== #
    
    carte <- leaflet() %>%
      # TUILES
      addTiles() %>%
      addTiles(group = "ESRI topo.") %>%
      addProviderTiles('Esri.WorldTopoMap', group = "ESRI topo.") %>%
      addProviderTiles('Esri.WorldImagery', group = "ESRI photo.") %>%
      # CONTROLE DES TUILES
      addLayersControl( baseGroups = c("ESRI topo.","ESRI photo.","OSM"),
                        position = "bottomright") %>%
      # CONTROLE DE LA VUE
      setView(lat = latitude, lng = longitude, zoom = zoom)
    
    # COUCHE AFFICHAGE DES COMMUNES D'OU VIENNENT LES ELEVES
    if(input$sel_commune != "Toutes les communes")
    {
      ##################################################
      if(input$sel_mobi == "vers")
      {
        monpop <- paste(
          "<span style='color:red;;font-weight:bold;'>Nombre d'élèves scolarisés à", input$sel_commune, "</span> :", com$var_unique,"<br>")
        monlab <- com$LIB_COMMUNE
      }
      else
      {
        monpop <- paste(
          "<span style='color:red;;font-weight:bold;'>Nombre d'élèves provenant de", input$sel_commune,"</span> :", com$var_unique,"<br>")
        
        monlab <- com$LIB_COMMUNE.x
      }
      carte <- carte %>%
        addPolygons(
          data = com,
          fillColor = input$couleur_contour,
          color = "white",
          fillOpacity = 0.7,
          weight = 2,
          opacity = 1,
          dashArray = "3",
          label = ~monlab,
          popup = monpop
        )
      ##################################################
    }
    
    if(input$sel_mobi == "vers")
    {
      # COUCHE COMMUNES AVEC LES DONNEES
      carte <- carte %>%
        addPolygons(
          data = don_RP2019_MOBSCO_couche,
          fillColor = ~pal(part_non_residents),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          label = ~LIB_COMMUNE,
          popup = paste(
            "<span style='color:red;;font-weight:bold;'>Part d'élèves scolarisés résidents</span> :", round(don_RP2019_MOBSCO_couche$part_residents,0),"%","<br>",
            "<span style='color:red;;font-weight:bold;'>Part d'élèves scolarisés venant d'une autre commune</span> :", round(don_RP2019_MOBSCO_couche$part_non_residents,0),"%","<br>",
            "<span style='color:blue;font-weight:bold;'>Nombre d'élèves scolarisés résidents</span> :", don_RP2019_MOBSCO_couche$`Dans la commune de résidence actuelle`,"<br>",
            "<span style='color:blue;font-weight:bold;'>Nombre d'élèves scolarisés venant d'une autre commune </span> :", don_RP2019_MOBSCO_couche$nb_nonresidents,"<br>")
        )
      
      # LEGENDE DE LA COUCHE DONNEES
      carte <- carte %>%
        addLegend(data = don_RP2019_MOBSCO_couche,
                  pal = pal, 
                  title = "Part d'élèves venant d'une autre commune",
                  values =~part_non_residents, 
                  position = 'topright')
    }
    else
    {
      # COUCHE COMMUNES AVEC LES DONNEES
      carte <- carte %>%
        addPolygons(
          data = don_RP2019_MOBSCO_couche,
          fillColor = ~pal(part_residents),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          label = ~LIB_COMMUNE,
          popup = paste(
            "<span style='color:red;;font-weight:bold;'>Part d'élèves scolarisés résidents</span> :", round(don_RP2019_MOBSCO_couche$part_residents,0),"%","<br>",
            "<span style='color:red;;font-weight:bold;'>Part d'élèves scolarisés venant d'une autre commune</span> :", round(don_RP2019_MOBSCO_couche$part_non_residents,0),"%","<br>",
            "<span style='color:blue;font-weight:bold;'>Nombre d'élèves scolarisés résidents</span> :", don_RP2019_MOBSCO_couche$`Dans la commune de résidence actuelle`,"<br>",
            "<span style='color:blue;font-weight:bold;'>Nombre d'élèves scolarisés venant d'une autre commune </span> :", don_RP2019_MOBSCO_couche$nb_nonresidents,"<br>")
        )
      
      # LEGENDE DE LA COUCHE DONNEES
      carte <- carte %>%
        addLegend(data = don_RP2019_MOBSCO_couche,
                  pal = pal, 
                  title = "Part d'élèves-résidents de la commune",
                  values =~part_residents, 
                  position = 'topright')
    }
    
    #if()
    # COUCHE POINTS REPRESENTANT LES ECOLES
    carte <- carte %>%
      addCircleMarkers(data=don_ADRESSES_parametrees(),
                       lat = ~latitude,
                       lng = ~longitude,
                       weight = 2.5,
                       color = "#3366FF",
                       fillColor = "red",
                       fillOpacity = 0.9,
                       radius = 5,
                       popup = paste(
                         "<span style='color:red;'>Nom</span> :", don_ADRESSES_parametrees()$appellation_officielle, "<br>",
                         "<span style='color:blue;font-weight:bold;'>Type</span> :", don_ADRESSES_parametrees()$nature_uai_libe, "<br>",
                         "<span style='color:green;'>Adresse</span> :", don_ADRESSES_parametrees()$adresse_uai, "<br>",
                         "<span style='color:orange;'>Latitude</span> :", don_ADRESSES_parametrees()$latitude, "<br>",
                         "<span style='color:orange;'>Longitude</span> :", don_ADRESSES_parametrees()$longitude)
      )
    
    
    
    
    
  })
  
  # ======================================================================= LANCEMENT DES CARTES ======================================================================= 
  
  ################################
  ### LANCEMENT CARTE STATIQUE ###
  ################################
  output$carte_statique <-renderPlot({
    
    prepa_carte_statique()
    
  })
  
  #################################
  ### LANCEMENT CARTE DYNAMIQUE ###
  #################################
  output$carte_dynamique <- renderLeaflet({
    
    prepa_carte_dynamique()
    
  })
  
  
  # ==================================================================== TABLEAUX / BOUTONS / CHIFFRE CLE ================================================================== 
  
  ###############
  ### BOUTONS ###
  ###############
  
  ## ============ BOUTONS DE SELECTION DE LA COULEUR ============
  observeEvent(input$bouton_orange, {
    updateSelectInput(inputId = "couleur_legende", selected = "Oranges")
    updateSelectInput(inputId = "couleur_contour", selected = "#4298f5")
  })
  observeEvent(input$bouton_rouge, {
    updateSelectInput(inputId = "couleur_legende", selected = "Reds")
    updateSelectInput(inputId = "couleur_contour", selected = "#31a354")
  })
  observeEvent(input$bouton_vert, {
    updateSelectInput(inputId = "couleur_legende", selected = "Greens")
    updateSelectInput(inputId = "couleur_contour", selected = "Red")
  })
  observeEvent(input$bouton_bleu, {
    updateSelectInput(inputId = "couleur_legende", selected = "Blues")
    updateSelectInput(inputId = "couleur_contour", selected = "Orange")
  })
  output$selectedColor <- renderPrint({ ## MISE A JOUR DES SORTIES EN FONCTION DE L'ACTION EFFECTUEE
    input$couleur_legende
  })
  
  
  ###################
  ### CHIFFRE CLE ###
  ###################
  output$chiffre_cle <- renderText({
    
    palette <- brewer.pal(name = input$couleur_legende, n = input$nb_classes) # On récupère la palette générée pour la légende
    classe_milieu <- floor(input$nb_classes / 2)                              # On calcul l'indice de la couleur de la classe médiane
    couleur <- palette[classe_milieu]                                         # On récupère la couleur de la classe médiane
    
    if(input$sel_commune != "Toutes les communes") # Si on ne choisit pas de considérer tous les types de logements, alors...
    {
      chiffre_cle <- don_RP2019_MOBSCO_couche %>% filter(sub(" \\(.*", "", com$LIB_COMMUNE) == input$sel_commune)
      
      if(input$sel_mobi == "vers")
      {
        chiffre_cle <- chiffre_cle$part_non_residents
        
        HTML(paste("Part des élèves scolarisés à", input$sel_commune, " mais n'y résidant pas : <br>",
                   "<span style='color:", couleur, "; font-size: 35px;'>", round(chiffre_cle,0), "%</span>"))
      }
      else
      {
        filtre <- don_RP2019_MOBSCO_couche %>% filter(sub(" \\(.*", "", com$LIB_COMMUNE) == input$sel_commune)
        
        filtre <- filtre$`Dans la commune de résidence actuelle`
        
        chiffre_cle <- 100*sum(don_RP2019_MOBSCO_parametrees()$var_unique)/(sum(don_RP2019_MOBSCO_parametrees()$var_unique)+filtre)
        
        HTML(paste("Part des élèves résidant de", input$sel_commune, "scolarisés ailleurs : <br>",
                   "<span style='color:", couleur, "; font-size: 35px;'>", round(chiffre_cle,0), "%</span>"))
      }
      
    }
    else
    {
      chiffre_cle <- don_RP2019_MOBSCO_couche %>% filter(substr(don_RP2019_MOBSCO_couche$COMMUNE, 1, 2) == input$sel_dep)
      nom_dep <- nom_dep(input$sel_dep)
      
      if(input$sel_mobi == "vers")
      {
        chiffre_cle <- mean(chiffre_cle$part_non_residents, na.rm = TRUE)
        
        HTML(paste("Part moyenne d'élèves scolarisés ailleurs que dans leur commune de résidence", nom_dep, ": <br>",
                   "<span style='color:", couleur, "; font-size: 35px;'>", round(chiffre_cle,0), "%</span>"))
      }
      else
      {
        chiffre_cle <- mean(chiffre_cle$part_residents, na.rm = TRUE)
        
        HTML(paste("Part moyenne d'élèves scolarisés dans leur commune de résidence", nom_dep, ": <br>",
                   "<span style='color:", couleur, "; font-size: 35px;'>", round(chiffre_cle,0), "%</span>"))
      }
      
    }

    
    
  })
  
  
  ############################
  ### REMARQUE CHIFFRE CLE ###
  ############################
  output$remarque_chiffre_cle <- renderText({
    
    if(input$sel_commune != "Toutes les communes") # Si on ne choisit pas de considérer tous les types de logements, alors...
    {
      if(input$sel_mobi == "vers")
      {
        HTML(paste("<span style= 'font-weight:bold;'> Remarque </span> : Ce chiffre concerne les élèves scolarisés dans la commune.
                   La part restante comprend donc les élèves qui sont à la fois scolarisés et résidants de la commune, mais pas les élèves résidant dans la commune et scolarisés
                   ailleurs."))
      }
      else
      {
        HTML(paste("<span style= 'font-weight:bold;'> Remarque </span> : Ce chiffre concerne les élèves résidant dans la commune.
                   La part restante comprend donc les élèves qui sont à la fois scolarisés et résidants de la commune, mais pas les élèves scolarisés dans la commune et résidant
                   ailleurs."))
        # filtre <- don_RP2019_MOBSCO_couche %>% filter(sub(" \\(.*", "", com$LIB_COMMUNE) == input$sel_commune)
        # filtre <- filtre$`Dans la commune de résidence actuelle`
        # chiffre_cle <- 100*sum(don_RP2019_MOBSCO_parametrees()$var_unique)/(sum(don_RP2019_MOBSCO_parametrees()$var_unique)+filtre)
        # 
        # HTML(paste("Cela signifie que", round(chiffre_cle,0) ,"% des élèves résidant dans la commune sont scolarisés en-dehors de celle-ci"))
      }
      
    }
    else
    {
      if(input$sel_mobi == "vers")
      {
        chiffre_cle <- don_RP2019_MOBSCO_couche %>% filter(substr(don_RP2019_MOBSCO_couche$COMMUNE, 1, 2) == input$sel_dep)
        nom_dep <- nom_dep(input$sel_dep)
        chiffre_cle <- mean(chiffre_cle$part_non_residents, na.rm = TRUE)
        
        HTML(paste("<span style= 'font-weight:bold;'> Remarque </span> : Il s'agit d'une moyenne sur l'ensemble des communes du département, et non 
                                        du pourcentage d'élèves scolarisés dans leur propre commune à l'échelle du département. Cela signifie que la commune moyenne du département a",
                   round(chiffre_cle,0),"% d'élèves scolarisés ailleurs que dans leur commune de résidence."))
      }
      else
      {
        chiffre_cle <- don_RP2019_MOBSCO_couche %>% filter(substr(don_RP2019_MOBSCO_couche$COMMUNE, 1, 2) == input$sel_dep)
        nom_dep <- nom_dep(input$sel_dep)
        chiffre_cle <- mean(chiffre_cle$part_residents, na.rm = TRUE)
        
        HTML(paste("<span style= 'font-weight:bold;'> Remarque </span> : Il s'agit d'une moyenne sur l'ensemble des communes du département, et non 
                                        du pourcentage d'élèves scolarisés dans leur propre commune à l'échelle du département. Cela signifie que la commune moyenne du département a",
                   round(chiffre_cle,0),"% d'élèves scolarisés dans leur commune de résidence."))
      }
      
    }
    
  })
  
  #################################
  ### BOUTONS DE TELECHARGEMENT ###
  #################################
  
  # =================================================================================== #
  # =============== BOUTON DE TELECHARGEMENT DES DONNEES CARTES 1 ET 2 ================ #
  # =================================================================================== #
  output$don_para_adresses <- downloadHandler(
    filename = function() {
      paste("don_para_adresses-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(don_ADRESSES_parametrees(), file)
    }
  )
  output$don_para_RP2019 <- downloadHandler(
    filename = function() {
      paste("don_para_RP2019-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(don_RP2019_MOBSCO_parametrees(), file)
    }
  )
  
  # =================================================================================== #
  # ================== BOUTON DE TELECHARGEMENT DES DONNEES BRUTES ==================== #
  # =================================================================================== #
  output$donnees_RP2019 <- downloadHandler(
    filename = function() {
      paste("donnees_RP2019-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(don_RP2019_MOBSCO, file) # map = la données ùùùùù
    }
  )
  output$donnees_ADRESSES <- downloadHandler(
    filename = function() {
      paste("donnees_ADRESSES-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(don_ADRESSES, file)
    }
  )
  
  ###########################
  ### TABLEAUX DE DONNEES ###
  ###########################
  
  # ============================================================= #
  # ============ TABLEAU DONNEES PARAMETREES RP2019 ============= #
  # ============================================================= #
  output$donadresses = DT::renderDataTable({
    don_ADRESSES_parametrees()
  })
  # ============================================================= #
  # =========== TABLEAU DONNEES PARAMETREES ADRESSES ============ #
  # ============================================================= #
  output$donrp2019 = DT::renderDataTable({
    don_RP2019_MOBSCO_parametrees()
  })
  # ================================================================== #
  # ============ TABLEAU DES DONNEES BRUTES RP2019_MOBSCO ============ #
  # ================================================================== #
  output$don_RP2019_MOBSCO = DT::renderDataTable({
    don_RP2019_MOBSCO
  })
  # ============================================================= #
  # ============ TABLEAU DES DONNEES BRUTES ADRESSES ============ #
  # ============================================================= #
  output$don_ADRESSES = DT::renderDataTable({
    don_ADRESSES
  })
}

# lancement de l'application
shinyApp(ui = ui, server = server)