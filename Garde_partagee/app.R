library(shiny)
library(DT)
library(shinydashboard)

# Charger les dépendances
source('../dependencies.R')
source('../fonctions.R')

# Charger les paquets
lapply(required_packages, require, character.only = TRUE)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Coût - garde partagé"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Simulateur", tabName = "simulateur", icon = icon("chart-pie")),
      menuItem("Etudes de cas", tabName = "etude_cas", icon = icon("user")),
      menuItem("Législation à respecter", tabName = "legislation", icon = icon("book")),
      menuItem("Droits et aides de l'employeur", tabName = "droits_aides", icon = icon("hand-holding-usd"))
      
    )
  ),
  dashboardBody(
    tabItems(
      ######## ONGLET SIMULATEUR ########
      tabItem(tabName = "simulateur",
              h2("Informations nécessaires à la simulation"),
              fluidRow(
                column(6,
                       wellPanel(
                         style = "background-color: #ffcdba; border: 2px solid #150a0a;",
                         h3("Famille 1"),
                         wellPanel(
                           style = "background-color: #fff; border: 1px solid #333;",
                           numericInput("salaire_brut_f1", "Salaire brut horaire (€) :", value = 12.26, min = 12.26)
                         ),
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
                               ),
                               fluidRow(
                                 column(12, checkboxGroupInput(paste0("repas_", jour, "_f1"), "Repas pris en charge :", choices = c("Petit-déjeuner", "Déjeuner", "Goûter", "Dîner"), selected = NULL))
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
                         wellPanel(
                           style = "background-color: #fff; border: 1px solid #333;",
                           numericInput("salaire_brut_f2", "Salaire brut horaire (€) :", value = 12.26, min = 12.26)
                         ),
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
                               fluidRow(
                                 column(12, checkboxGroupInput(paste0("repas_", jour, "_f2"), "Repas pris en charge :", choices = c("Petit-déjeuner", "Déjeuner", "Goûter", "Dîner"), selected = NULL))
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
      ######## ONGLET LÉGISLATION ########
      tabItem(tabName = "legislation",
              h3("Législation à respecter concernant l'assistante maternelle", align = "center"),
              fluidRow(
                box(
                  title = "Charges sociales et salaire minimum", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  DTOutput("table_charges")
                ),
                box(
                  title = "Durée et organisation du travail", 
                  status = "info", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  DTOutput("table_duree")
                ),
                box(
                  title = "Repos et vacances", 
                  status = "success", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  tags$ul(
                    tags$li("Repos hebdomadaire : Minimum 24 heures consécutives"),
                    tags$li("Coordination des repos : Si plusieurs employeurs, le jour de repos est le même pour tous"),
                    tags$li("Vacances : Doivent être les mêmes pour toutes les familles employeuses")
                  )
                ),
                box(
                  title = "Indemnités", 
                  status = "warning", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  tags$ul(
                    tags$li("Indemnités repas : 1.50€ petit-déjeuner et goûter, 4.50€ déjeuner et dîner"),
                    tags$li("Indemnités transport : À préciser")
                  )
                ),
                box(
                  title = "Salaire annualisé", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  tags$p("Salaire hebdomadaire × Nombre de semaines travaillées ÷ 12")
                ),
                box(
                  title = "Calcul des congés payés", 
                  status = "info", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  tags$p("Nombre de semaines de travail effectif × 2,5 jours de congés payés ÷ 4 semaines = nombre de jours de congés payés ouvrables acquis par le salarié.")
                ),
                box(
                  title = "Travail de nuit", 
                  status = "warning", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  tags$p("Les heures de 20h30 à 6h30 seront payées au même tarif que le jour.")
                ),
                box(
                  title = "Précision sur les jours fériés", 
                  status = "danger", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  tags$ul(
                    tags$li("Si le jour férié tombe pendant une semaine non travaillée prévue alors le jour n'est pas rémunéré."),
                    tags$li("Si le jour férié tombe sur une semaine prévue au travail, alors pour qu’il soit rémunéré, l’assistante maternelle doit avoir travaillé habituellement le jour d’accueil qui précède et celui qui suit le jour férié."),
                    tags$li("Le 1er Mai, le salaire est doublé, pour les autres jours fériés ordinaires, majorés à 10% du salaire de base.")
                  )
                )
              )
      ),
      ######## ONGLET DROITS ET AIDES DE L'EMPLOYEUR ########
      tabItem(tabName = "droits_aides",
              h3("Droits et aides de l'employeur", align = "center"),
              fluidRow(
                box(
                  title = "Crédit d'impôt", 
                  status = "info", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  tags$ul(
                    tags$li("Conditions: l'enfant doit avoir moins de 6 ans le 1er janvier de l'année d'imposition et être à la charge de la personne recevant le crédit d'impôt."),
                    tags$li("Crédit d'impôt égal à 50% des dépenses"),
                    tags$li("Plafond des dépenses : 3500 € par an (déduire la CMG des dépenses total pour connaître la valeur à déclarer) et par enfant gardé (1750€ en cas de garde alternée)."),
                    tags$li("Crédit d'impôt différent de déduction d'impôt."),
                    tags$li(tags$a(href = "https://www.service-public.fr/particuliers/vosdroits/F8.", "Informations en cliquant ici - site du service public"))
                  )
                  
                ),
                box(
                  title = "Complément de libre choix du mode de garde (CMG)", 
                  status = "warning", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  tags$ul(
                    tags$li("Prise en charge partielle de la rémunération d'une assistante maternelle agréée."),
                    tags$li("Conditions: rémunération brute ne doit pas dépasser 59,40 € par jour et par enfant gardé, le complément prend en charge jusqu'à 85 % de la rémunération et l'enfant doit avoir moins de 6 ans."),
                    tags$li(tags$a(href = "https://www.service-public.fr/particuliers/vosdroits/F345", 
                                   "Simulateur disponible ici - site du service public"))
                  )
                )
                
              )
        )
      )
    )
  )


# Server
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
  
  #### Calcul des indemnités repas ####
  calcul_indemnite_repas <- function(input_suffixe) {
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    total_indemnite <- 0
    
    for (jour in jours) {
      repas_selectionnes <- input[[paste0("repas_", jour, input_suffixe)]]
      
      if (!is.null(repas_selectionnes)) {
        total_indemnite <- total_indemnite + 
          sum(1.5 * (repas_selectionnes %in% c("Petit-déjeuner", "Goûter"))) +
          sum(4.5 * (repas_selectionnes %in% c("Déjeuner", "Dîner")))
      }
    }
    
    return(total_indemnite)
  }
  
  #### Observer le bouton pour calculer les résultats ####
  observeEvent(input$calcul_global, {
    salaire_brut_f1 <- as.numeric(input$salaire_brut_f1)
    salaire_brut_f2 <- as.numeric(input$salaire_brut_f2)
    
    heures_f2 <- calcul_heures_semaine("_f2")
    heures_f1 <- calcul_heures_semaine("_f1")
    
    # Famille 1
    salaire_annuel_f1 <- calcul_salaire_mensualise_partage(heures_f1, heures_f2, salaire_brut_f1, salaire_brut_f2, 52)$salaire_f1
    charges_f1 <- charges_patronales(salaire_brut_f1)    
    indemnite_f1 <- calcul_indemnite_repas("_f1")
    
    # Famille 2
    salaire_annuel_f2 <- calcul_salaire_mensualise_partage(heures_f1, heures_f2, salaire_brut_f1, salaire_brut_f2, 52)$salaire_f2
    charges_f2 <- charges_patronales(salaire_brut_f2)    
    indemnite_f2 <- calcul_indemnite_repas("_f2")
    
    # Total global
    heures_totales <- heures_f1 + heures_f2
    salaire_annuel_total <- salaire_annuel_f1 + salaire_annuel_f2
    
    output$resultats_combines <- renderText({
      paste(
        "Résultats pour la Famille 1 :\n",
        "- Salaire brut horaire : ", salaire_brut_f1, "€\n",
        "- Heures par semaine : ", round(heures_f1, 2), "h\n",
        "- Salaire annualisé brut : ", round(salaire_annuel_f1, 2), "€\n",
        "- Indemnités repas: ", round((indemnite_f1*52)/12,2), "€\n", #à mieux calculer selon le nombre de semaines travaillé etc
        "- Charges patronales par mois:", charges_patronales(salaire_annuel_f1), "€\n",
        "- Coût total par mois :", round(salaire_annuel_f1, 2) + round((indemnite_f1*52)/12, 2) + charges_f1, "€\n\n",
        
        
        "Résultats pour la Famille 2 :\n",
        "- Salaire brut horaire : ", salaire_brut_f2, "€\n",
        "- Heures par semaine : ", round(heures_f2, 2), "h\n",
        "- Salaire annualisé : ", round(salaire_annuel_f2, 2), "€\n",
        "- Indemnités repas : ", round((indemnite_f2*52)/12, 2), "€\n", #à mieux calculer selon le nombre de semaines travaillé etc
        "- Charges patronales :", charges_patronales(salaire_annuel_f2), "€\n",
        "- Coût total par mois :", round(salaire_annuel_f2, 2) + round((indemnite_f2*52)/12, 2) + charges_f2, "€\n\n",
        
        "Résultats combinés :\n",
        "- Heures totales par semaine : ", round(heures_totales, 2), "h\n",
        "- Salaire annualisé total : ", round(salaire_annuel_total, 2), "€\n",
        "- Indemnités repas totales : ", round(indemnite_f1 + indemnite_f2, 2), "€\n\n"
        
      )
    })
  })
  
  # Charges sociales et salaire minimum
  output$table_charges <- renderDT({
    datatable(
      data.frame(
        Catégorie = c(
          "Charges sociales salariales", 
          "Charges sociales patronales", 
          "Salaire brut minimum", 
          "Salaire net minimum"
        ),
        Valeur = c(
          "21,88% du brut", 
          "44,69% du brut", 
          "12,26 €/heure", 
          "9,56 €/heure"
        )
      ),
      options = list(dom = 't', paging = FALSE),
      rownames = FALSE
    )
  })
  
  # Durée et organisation du travail
  output$table_duree <- renderDT({
    datatable(
      data.frame(
        Catégorie = c(
          "Heures majorées", 
          "Durée maximale par jour", 
          "Durée maximale par semaine", 
          "Durée de travail hebdomadaire normale", 
          "Temps de repos obligatoire"
        ),
        Valeur = c(
          "+25% à partir de la 40ᵉ heure", 
          "9 heures", 
          "48 heures", 
          "45 heures", 
          "11 heures après une journée de travail"
        )
      ),
      options = list(dom = 't', paging = FALSE),
      rownames = FALSE
    )
  })
}

# Lancer l'application
shinyApp(ui, server)
