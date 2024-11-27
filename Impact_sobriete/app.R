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
                                   column(6, timeInput(paste0("debut_matin_", jour), "Début matin :", value = strptime("08:00", format = "%H:%M"))),
                                   column(6, timeInput(paste0("fin_matin_", jour), "Fin matin :", value = strptime("12:00", format = "%H:%M")))
                                 ),
                                 fluidRow(
                                   column(6, timeInput(paste0("debut_aprem_", jour), "Début après-midi :", value = strptime("14:00", format = "%H:%M"))),
                                   column(6, timeInput(paste0("fin_aprem_", jour), "Fin après-midi :", value = strptime("18:00", format = "%H:%M")))
                                 ),
                                 checkboxGroupInput(paste0("repas_", jour), "Repas pris en compte :", 
                                                    choices = c("Petit-déjeuner", "Midi", "Goûter", "Soir"))
                               )
                             })
                           ),
                           
                           # Dates des jours non travaillés et vacances
                           wellPanel(
                             style = "background-color: #fff; border: 1px solid #333;",
                             h4("Dates non travaillées :"),
                             airDatepickerInput("jours_non_travailles_f1", 
                                                "Jours non travaillés :", 
                                                multiple = TRUE, value = Sys.Date()), 
                             
                             h4("Périodes de vacances :"),
                             actionButton("ajout_vacances", "Ajouter une période de vacances"),
                             uiOutput("vacances_f1") 
                           ),
                           #Prime de panier/remboursement du transport
                           wellPanel(
                             style = "background-color: #fff; border: 1px solid #333;",
                             h4("Prime de panier :"),
                             numericInput("prime_panier_f1", "Prime de panier (€) :", value = 7.30, min = 7.30),
                             h4("Remboursement du titre de transport :"),
                             numericInput("remb_transport_f1", "Remboursement (€) :", value = 0),
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
                                 ),
                                 checkboxGroupInput(paste0("repas_", jour, "_f2"), "Repas pris en compte :", 
                                                    choices = c("Petit-déjeuner", "Midi", "Goûter", "Soir"))
                               )
                             })
                           ),
                           
                           # Dates des jours non travaillés et vacances
                           wellPanel(
                             style = "background-color: #fff; border: 1px solid #333;",
                             h4("Dates non travaillées :"),
                             airDatepickerInput("jours_non_travailles_f2", 
                                                "Jours non travaillés :", 
                                                multiple = TRUE, 
                                                value = Sys.Date()), 
                             
                             h4("Périodes de vacances :"),
                             actionButton("ajout_vacances_f2", "Ajouter une période de vacances"),
                             uiOutput("vacances_f2") 
                           ),
                           #Prime de panier/remboursement du transport
                           wellPanel(
                             style = "background-color: #fff; border: 1px solid #333;",
                             h4("Prime de panier :"),
                             numericInput("prime_panier_f2", "Prime de panier (€) :", value = 7.30),
                             h4("Remboursement du titre de transport :"),
                             numericInput("remb_transport_f2", "Remboursement (€) :", value = 0),
                           )
                         )
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
  
#### Période vacances
  compteurs_vacances_f1 <- reactiveVal(0)
  observeEvent(input$ajout_vacances, {
    count <- compteurs_vacances_f1() + 1
    vacations_counter_f1(count)
    insertUI(
      selector = "#ajout_vacances",
      
      where = "afterEnd",
      ui = dateRangeInput(paste0("vacation_f1_", count),
                          label = paste("Période de vacances", count),
                          start = Sys.Date(), end = Sys.Date() + 7)
    )
  })
  
  compteur_vacances_f2 <- reactiveVal(0)
  observeEvent(input$ajout_vacances_f2, {
    count <- compteur_vacances_f2() + 1
    vacations_counter_f2(count)
    insertUI(selector = "#ajout_vacances_f2",
      where = "afterEnd",
      ui = dateRangeInput(paste0("vacation_f2_", count),
                          label = paste("Période de vacances", count),
                          start = Sys.Date(), end = Sys.Date() + 7)
    )
  })
  
#### ----- Vérification heures ----- ###
  
  observe({ 
  jours <- c("Lundi","Mardi","Mercredi","Jeudi","Vendredi","Samedi")
  heures <- list()
  for (jour in jours){
      heures[[jour]]<-list(input[[paste0("debut_matin_", jour)]],
                           input[[paste0("fin_matin_", jour)]],
                           input[[paste0("debut_aprem_", jour)]],
                           input[[paste0("fin_aprem_", jour)]])

  }
  
  res <- calcul_total_heure(heures)
  if (res >= 2400 & res <= 2880){
    showNotification(paste("ATTENTION :", conversion_entier_to_heure(res-2400), "seront payés 25% de plus"), type = "warning",duration = 5)
  }else if (res >2880){
    showNotification(paste("Vous dépassez le cadre légal (48h) de :", conversion_entier_to_heure(res-2880), "h, veuillez changer votre saisie"), type = "warning",duration = 5)
  }
  })
}
# Lancer l'application
shinyApp(ui = ui, server = server)
