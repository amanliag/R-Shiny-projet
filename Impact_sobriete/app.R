# Charger les dépendances
source('../dependencies.R')
source('../fonctions.R')

# Charger les paquets
lapply(required_packages, require, character.only = TRUE)

# UI
ui <- fluidPage(
  ######## DASHBOARD ########
  dashboardPage(
    dashboardHeader(title = "Coût - garde partagé"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Simulateur", tabName = "simulateur", icon = icon("chart-pie")),
        menuItem("Etudes de cas", tabName = "etude_cas", icon = icon("user"))
      )
    ),
    
    dashboardBody(
      tabItems(
        ######## ONGLET INFORMATIONS POUR SIMULATEUR ########
        tabItem(tabName = "simulateur",
                h2("Informations nécessaires à la simulation"),
                
                # Informations pour les deux familles
                fluidRow(
                  column(6,
                         wellPanel(
                           style = "background-color: #ffcdba; border: 2px solid #150a0a;",
                           h3("Famille 1"),
                           
                           # Salaire brut horaire
                           wellPanel(
                             style = "background-color: #fff; border: 1px solid #333;",
                             numericInput("salaire_brut_f1", "Salaire brut horaire (€) :", value = 12.26, min = 12.26)
                           ),
                           # Heures par jour de la semaine
                           wellPanel(
                             style = "background-color: #fff; border: 1px solid #333;",
                             h4("Heures travaillées par jour :"),
                             lapply(c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche"), function(jour) {
                               tagList(
                                 h5(jour),
                                 fluidRow(
                                   column(6, timeInput(paste0("debut_matin_", jour, "_f1"), "Début matin :", value = strptime("08:00", format = "%H:%M"))),
                                   column(6, timeInput(paste0("fin_matin_", jour, "_f1"), "Fin matin :", value = strptime("12:00", format = "%H:%M")))
                                 ),
                                 fluidRow(
                                   column(6, timeInput(paste0("debut_aprem_", jour, "_f1"), "Début après-midi :", value = strptime("14:00", format = "%H:%M"))),
                                   column(6, timeInput(paste0("fin_aprem_", jour, "_f1"), "Fin après-midi :", value = strptime("18:00", format = "%H:%M")))
                                 )
                               )
                             })
                           )
                         )
                  ),
                  column(6,
                         wellPanel(
                           style = "background-color: #ffcdba; border: 2px solid #150a0a;",
                           h3("Famille 2"),
                           
                           # Salaire brut horaire
                           wellPanel(
                             style = "background-color: #fff; border: 1px solid #333;",
                             numericInput("salaire_brut_f2", "Salaire brut horaire (€) :", value = 12.26, min = 12.26)
                           ),
                           # Heures par jour de la semaine
                           wellPanel(
                             style = "background-color: #fff; border: 1px solid #333;",
                             h4("Heures travaillées par jour :"),
                             lapply(c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche"), function(jour) {
                               tagList(
                                 h5(jour),
                                 fluidRow(
                                   column(6, timeInput(paste0("debut_matin_", jour, "_f2"), "Début matin :", value = strptime("08:00", format = "%H:%M"))),
                                   column(6, timeInput(paste0("fin_matin_", jour, "_f2"), "Fin matin :", value = strptime("12:00", format = "%H:%M")))
                                 ),
                                 fluidRow(
                                   column(6, timeInput(paste0("debut_aprem_", jour, "_f2"), "Début après-midi :", value = strptime("14:00", format = "%H:%M"))),
                                   column(6, timeInput(paste0("fin_aprem_", jour, "_f2"), "Fin après-midi :", value = strptime("18:00", format = "%H:%M")))
                                 )
                               )
                             })
                           )
                         )
                  )
                ),
                
                fluidRow(
                  column(12,
                         actionButton("calcul_global", "Calculer pour les deux familles"),
                         verbatimTextOutput("resultats_combines") 
                  )
                )
        ),
        
        # Onglet Etudes de cas
        tabItem(tabName = "etude_cas",
                h2("Contenu des études de cas"),
                selectInput("test", "Test :", choices = NULL)
        )
      )
    )
  )
)

### SERVER
server <- function(input, output, session) {
  
  #### Calcul des heures totales travaillées par semaine pour une famille donnée ####
  calcul_heures_semaine <- function(input_suffixe) {
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    total_minutes <- 0
    
    for (jour in jours) {
      matin_debut <- input[[paste0("debut_matin_", jour, input_suffixe)]]
      matin_fin <- input[[paste0("fin_matin_", jour, input_suffixe)]]
      aprem_debut <- input[[paste0("debut_aprem_", jour, input_suffixe)]]
      aprem_fin <- input[[paste0("fin_aprem_", jour, input_suffixe)]]
      
      matin_debut <- if (!is.null(matin_debut) && matin_debut != "") as.POSIXct(matin_debut, format = "%H:%M") else NULL
      matin_fin <- if (!is.null(matin_fin) && matin_fin != "") as.POSIXct(matin_fin, format = "%H:%M") else NULL
      aprem_debut <- if (!is.null(aprem_debut) && aprem_debut != "") as.POSIXct(aprem_debut, format = "%H:%M") else NULL
      aprem_fin <- if (!is.null(aprem_fin) && aprem_fin != "") as.POSIXct(aprem_fin, format = "%H:%M") else NULL
      
      if (!is.null(matin_debut) && !is.null(matin_fin)) {
        total_minutes <- total_minutes + as.numeric(difftime(matin_fin, matin_debut, units = "mins"))
      }
      
      if (!is.null(aprem_debut) && !is.null(aprem_fin)) {
        total_minutes <- total_minutes + as.numeric(difftime(aprem_fin, aprem_debut, units = "mins"))
      }
    }
    
    return(total_minutes / 60) # Convertir les minutes en heures
  }
  
  #### Observer le bouton pour calculer les résultats ####
  observeEvent(input$calcul_global, {
    heures_f1 <- calcul_heures_semaine("_f1") #Préfixe Famille 1
    salaire_brut_f1 <- input$salaire_brut_f1
    salaire_annuel_f1 <- salaire_brut_f1 * heures_f1 * 52
    
    heures_f2 <- calcul_heures_semaine("_f2") # Préfixe pour Famille 2
    salaire_brut_f2 <- input$salaire_brut_f2
    salaire_annuel_f2 <- salaire_brut_f2 * heures_f2 * 52
    
    heures_totales <- heures_f1 + heures_f2
    salaire_annuel_total <- salaire_annuel_f1 + salaire_annuel_f2
    
    output$resultats_combines <- renderText({
      paste(
        "Résultats pour la Famille 1 :\n",
        "- Salaire brut horaire : ", salaire_brut_f1, "€\n",
        "- Salaire net horaire :", calcul_net(salaire_brut_f1), "€\n",
        "- Heures par semaine : ", round(heures_f1, 2), "h\n",
        "- Salaire annualisé : ", round(salaire_annuel_f1, 2), "€\n\n",
        
        "Résultats pour la Famille 2 :\n",
        "- Salaire brut horaire : ", salaire_brut_f2, "€\n",
        "- Salaire net horaire :", calcul_net(salaire_brut_f2), "€\n",
        "- Heures par semaine : ", round(heures_f2, 2), "h\n",
        "- Salaire annualisé : ", round(salaire_annuel_f2, 2), "€\n\n",
        
        "Résultats combinés :\n",
        "- Heures totales par semaine : ", round(heures_totales, 2), "h\n", #A CORRIGER SELON LES PLAGES HORAIRES DES FAMILLES
        "- Salaire annualisé total brut pour la famille 1: ", calcul_salaire_mensualise_partage(round(heures_f1, 2), round(heures_f2, 2), salaire_brut_f1, salaire_brut_f2, 52)[1],"€\n",
        "- Salaire annualisé total brut pour la famille 2: ", calcul_salaire_mensualise_partage(round(heures_f1, 2), round(heures_f2, 2), salaire_brut_f1, salaire_brut_f2, 52)[2],"€\n",
        "- Salaire annualisé total brut pour les deux familles: ", calcul_salaire_mensualise_partage(round(heures_f1, 2), round(heures_f2, 2), salaire_brut_f1, salaire_brut_f2, 52)[3],"€\n\n",
        
        "- Salaire annualisé total net pour la famille 1: ", repartition_salaire_net_mensualise(round(heures_f1, 2), round(heures_f2, 2), salaire_brut_f1, salaire_brut_f2, 52)[1],"€\n",
        "- Salaire annualisé total net pour la famille 2: ", repartition_salaire_net_mensualise(round(heures_f1, 2), round(heures_f2, 2), salaire_brut_f1, salaire_brut_f2, 52)[2],"€\n",
        "- Salaire annualisé total net pour les deux familles: ", repartition_salaire_net_mensualise(round(heures_f1, 2), round(heures_f2, 2), salaire_brut_f1, salaire_brut_f2, 52)[3],"€\n",
        "- Part en pourcentage de la famille 1: ", repartition_salaire_net_mensualise(round(heures_f1, 2), round(heures_f2, 2), salaire_brut_f1, salaire_brut_f2, 52)[4],"%\n",
        "- Part en pourcentage de la famille 2: ", repartition_salaire_net_mensualise(round(heures_f1, 2), round(heures_f2, 2), salaire_brut_f1, salaire_brut_f2, 52)[5],"%\n\n"
      )
    })
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
