library(shiny)
library(DT)
library(shinydashboard)

# Charger les dépendances
source('../dependencies.R')
source('../fonctions.R')

# Charger les paquets
lapply(required_packages, require, character.only = TRUE)


sidebar <-     sidebarMenu(
  menuItem("Simulateur", tabName = "simulateur", icon = icon("chart-pie")),
  menuItem("Etudes de cas", tabName = "etude_cas", icon = icon("user")),
  menuItem("Législation à respecter", tabName = "legislation", icon = icon("book")),
  menuItem("Droits et aides de l'employeur", tabName = "droits_aides", icon = icon("hand-holding-usd"))
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Coût - garde partagé"),
  dashboardSidebar(
    sidebar
  ),
  dashboardBody(
    useShinyFeedback(),
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
                                 column(12, 
                                        checkboxInput(paste0("jour_travaille_", jour, "_f1"), 
                                                      label = paste(jour, "travaillé ?"), 
                                                      value = FALSE)
                                 )
                               ),
                               conditionalPanel(
                                 condition = paste0("input.jour_travaille_", jour, "_f1 == true"),
                                 fluidRow(
                                   column(6, timeInput(paste0("debut_matin_", jour, "_f1"), "Début matin :", value = strptime("08:00", format = "%H:%M"))),
                                   column(6, timeInput(paste0("fin_matin_", jour, "_f1"), "Fin matin :", value = strptime("12:00", format = "%H:%M")))
                                 ),
                                 fluidRow(
                                   column(6, timeInput(paste0("debut_aprem_", jour, "_f1"), "Début après-midi :", value = strptime("14:00", format = "%H:%M"))),
                                   column(6, timeInput(paste0("fin_aprem_", jour, "_f1"), "Fin après-midi :", value = strptime("18:00", format = "%H:%M")))
                                 ),
                                 fluidRow(
                                   column(12, checkboxGroupInput(paste0("repas_", jour, "_f1"), 
                                                                 "Repas pris en charge :", 
                                                                 choices = c("Petit-déjeuner", "Déjeuner", "Goûter", "Dîner"), 
                                                                 selected = NULL))
                                 )
                               )
                             )
                           })
                           ),
                         wellPanel(
                           style = "background-color: #fff; border: 1px solid #333;",
                           h4("Aides financières :"),
                           checkboxInput("cmg_f1", "Complément de libre choix du mode de garde (CMG)", value = FALSE),
                           conditionalPanel(
                             condition = "input.cmg_f1 == true",
                             numericInput("valeur_cmg_f1", "Montant CMG (€) :", value = NULL, min = 0)
                           ),
                           
                           h5("Droit au crédit d'impôt :"),
                           textOutput("credit_impot_f1")
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
                                 column(12, 
                                        checkboxInput(paste0("jour_travaille_", jour, "_f2"), 
                                                      label = paste(jour, "travaillé ?"), 
                                                      value = FALSE))),
                               conditionalPanel(
                                 condition = paste0("input.jour_travaille_", jour, "_f2 == true"),
                                 fluidRow(
                                   column(6, timeInput(paste0("debut_matin_", jour, "_f2"), "Début matin :", value = strptime("08:00", format = "%H:%M"))),
                                   column(6, timeInput(paste0("fin_matin_", jour, "_f2"), "Fin matin :", value = strptime("12:00", format = "%H:%M")))
                                 ),
                                 fluidRow(
                                   column(6, timeInput(paste0("debut_aprem_", jour, "_f2"), "Début après-midi :", value = strptime("14:00", format = "%H:%M"))),
                                   column(6, timeInput(paste0("fin_aprem_", jour, "_f2"), "Fin après-midi :", value = strptime("18:00", format = "%H:%M")))
                                 ),
                                 fluidRow(
                                   column(12, checkboxGroupInput(paste0("repas_", jour, "_f2"), 
                                                                 "Repas pris en charge :", 
                                                                 choices = c("Petit-déjeuner", "Déjeuner", "Goûter", "Dîner"), 
                                                                 selected = NULL)))
                               )
                             )
                         })
                    )
                    ,
                    wellPanel(
                      style = "background-color: #fff; border: 1px solid #333;",
                      h4("Aides financières :"),
                      checkboxInput("cmg_f2", "Complément de libre choix du mode de garde (CMG)", value = FALSE),
                      conditionalPanel(
                        condition = "input.cmg_f2 == true",
                        numericInput("valeur_cmg_f2", "Montant CMG (€) :", value = NULL, min = 0)
                      ),
                      h5("Droit au crédit d'impôt :"),
                      textOutput("credit_impot_f2")
                    )
                  )
                )
              ),
              fluidRow(
                column(12,
                       actionButton("calcul_global", "Calculer pour les deux familles"),
                       verbatimTextOutput("resultats_combines")
                )
              ),
              conditionalPanel(
                condition = "output.resultats_combines != null",
                fluidRow(
                  column(6,
                         box(
                           title = "Répartition des heures travaillées",
                           status = "primary",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           plotlyOutput("plot_repartition_heures")
                         )
                  ),
                  column(6,
                         box(
                           title = "Revenus par famille",
                           status = "success",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           plotlyOutput("plot_revenu_famille")
                         )
                  )
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
                ),
                box(
                  title = "Répartition des charges sociales", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  plotOutput("plot_charges")
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
                
                
                
                
                
               
         

server <- function(input, output, session) {
  
  # Vérification salaire brut horaire
  observe({
    # Famille 1
    shinyFeedback::feedbackWarning(
      inputId = "salaire_brut_f1",
      show = input$salaire_brut_f1 < 12.26,
      text = "Le salaire brut horaire doit être supérieur ou égal à 12,26 €."
    )
    
    # Famille 2
    shinyFeedback::feedbackWarning(
      inputId = "salaire_brut_f2",
      show = input$salaire_brut_f2 < 12.26,
      text = "Le salaire brut horaire doit être supérieur ou égal à 12,26 €."
    )
  })
  
  #Vérification du non dépassement des 13heures de travail par jour
  observeEvent({
    unlist(lapply(c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche"), function(jour) {
      c(input[[paste0("debut_matin_", jour, "_f1")]], input[[paste0("fin_matin_", jour, "_f1")]], 
        input[[paste0("debut_aprem_", jour, "_f1")]], input[[paste0("fin_aprem_", jour, "_f1")]],
        input[[paste0("debut_matin_", jour, "_f2")]], input[[paste0("fin_matin_", jour, "_f2")]],
        input[[paste0("debut_aprem_", jour, "_f2")]], input[[paste0("fin_aprem_", jour, "_f2")]])}))}, 
    {
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    for (jour in jours) {
      horaires_f1 <- c()
      horaires_f2 <- c()
      #heures f1
      if (isTRUE(input[[paste0("jour_travaille_", jour, "_f1")]])) {
        matin_debut_f1 <- input[[paste0("debut_matin_", jour, "_f1")]]
        matin_fin_f1 <- input[[paste0("fin_matin_", jour, "_f1")]]
        aprem_debut_f1 <- input[[paste0("debut_aprem_", jour, "_f1")]]
        aprem_fin_f1 <- input[[paste0("fin_aprem_", jour, "_f1")]]
        
        horaires_f1 <- c(matin_debut_f1, matin_fin_f1, aprem_debut_f1, aprem_fin_f1) }
      #heures f2
      if (isTRUE(input[[paste0("jour_travaille_", jour, "_f2")]])) {
        matin_debut_f2 <- input[[paste0("debut_matin_", jour, "_f2")]]
        matin_fin_f2 <- input[[paste0("fin_matin_", jour, "_f2")]]
        aprem_debut_f2 <- input[[paste0("debut_aprem_", jour, "_f2")]]
        aprem_fin_f2 <- input[[paste0("fin_aprem_", jour, "_f2")]]
        
        horaires_f2 <- c(matin_debut_f2, matin_fin_f2, aprem_debut_f2, aprem_fin_f2) }
      horaires_combines <- c(horaires_f1, horaires_f2)
      horaires_combines <- horaires_combines[!is.null(horaires_combines) & horaires_combines != ""]
      
      if (length(horaires_combines) > 0) {
        horaires_combines <- as.POSIXct(horaires_combines, format = "%H:%M")
        heure_min <- min(horaires_combines)
        heure_max <- max(horaires_combines)
        
        amplitude_total <- as.numeric(difftime(heure_max, heure_min, units = "hours"))
        if (amplitude_total > 13) {
          showModal(modalDialog(
            title = paste("⚠️ Avertissement ⚠️ : 13 heures de travail ont été dépassées -", jour),
            paste("Actuellement :", round(amplitude_total, 2), "h."),
            easyClose = FALSE, 
            footer = modalButton("OK")))}}
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  #Vérification qu'il y a bien un jour de repos si 6 jours consécutifs 
  
 # famille 1 et famille 2, si elle une des deux a 6 jours cochées, on vérifie que lautre a les 6 mêmes jours cochés ou que le jour non coché 
  observeEvent({ jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
      compteur_jours_f1 <- 0
      compteur_jours_f2 <- 0
      variable_bool <- FALSE
      jours_travailles_f1 <- list()
      jours_travailles_f2 <- list()

      for (jour in jours) {
        if (isTRUE(input[[paste0("jour_travaille_", jour, "_f1")]])) {
          compteur_jours_f1 = compteur_jours_f1 +1 }
          jours_travailles_f1 <- append(jours_travailles_f1, jour)

        if (isTRUE(input[[paste0("jour_travaille_", jour, "_f2")]])) {
          compteur_jours_f1 = compteur_jours_f1 +1 }
          jours_travailles_f2 <- append(jours_travailles_f2, jour)
        }

        if (length(compteurs_jours_f1) == 6 | length(compteurs_jours_f2) == 6) {
          if (length(compteurs_jours_f1) == length(compteurs_jours_f2)){
            variable_bool <- setequal(compteurs_jours_f1)}
        }

        if (length(compteurs_jours_f1) == 7 | length(compteurs_jours_f2) == 7) {
              variable_bool <- FALSE
        }


        if (variable_bool == FALSE) {
            showModal(modalDialog(
              title = paste("⚠️ Avertissement ⚠️ Le jour de repos n'est pas respecté -", jour),
              paste("Voir LEGISLATION"),
              easyClose = FALSE,
              footer = modalButton("OK")))
        }
          
}, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  

  #### Calcul des heures totales travaillées par semaine pour une famille donnée ####
  calcul_heures_semaine <- function(input_suffixe) {
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    total_minutes <- 0
    
    for (jour in jours) {
      if (isTRUE(input[[paste0("jour_travaille_", jour, input_suffixe)]])) {
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
    }
    
    return(total_minutes / 60) # Convertir les minutes en heures
  }
  

  
  #### Calcul des indemnités repas ####
  calcul_indemnite_repas <- function(input_suffixe) {
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    total_indemnite <- 0
    
    for (jour in jours) {
      if (isTRUE(input[[paste0("jour_travaille_", jour, input_suffixe)]])) {
        repas_selectionnes <- input[[paste0("repas_", jour, input_suffixe)]]
        
        if (!is.null(repas_selectionnes)) {
          total_indemnite <- total_indemnite + 
            sum(1.5 * (repas_selectionnes %in% c("Petit-déjeuner", "Goûter"))) +
            sum(4.5 * (repas_selectionnes %in% c("Déjeuner", "Dîner")))
        }
      }
    }
    
    return(total_indemnite)
  }
    

    observeEvent(input$calcul_global, {
    salaire_brut_f1 <- as.numeric(input$salaire_brut_f1) #horaire
    salaire_brut_f2 <- as.numeric(input$salaire_brut_f2) #horaire
    
    heures_f2 <- calcul_heures_semaine("_f2")
    heures_f1 <- calcul_heures_semaine("_f1")
    
    # Famille 1
    salaire_annuel_f1 <- calcul_salaire_mensualise_partage(heures_f1, heures_f2, salaire_brut_f1, salaire_brut_f2, 52)$salaire_f1 #mensualise
    charges_f1 <- charges_patronales(salaire_brut_f1)    
    indemnite_f1 <- calcul_indemnite_repas("_f1")
    salaire_brut_an_f1 <- 52 * heures_f1 * salaire_brut_f1 #il faudra changer 52 pour le nombre de semaines travaillées 
    
    # Famille 2
    salaire_annuel_f2 <- calcul_salaire_mensualise_partage(heures_f1, heures_f2, salaire_brut_f1, salaire_brut_f2, 52)$salaire_f2
    charges_f2 <- charges_patronales(salaire_brut_f2)    
    indemnite_f2 <- calcul_indemnite_repas("_f2")
    salaire_brut_an_f2 <- 52 * heures_f2 * salaire_brut_f2 #il faudra changer 52 pour le nombre de semaines travaillées 
    
    # Total global
    heures_totales <- heures_f1 + heures_f2
    salaire_net_annuel_mensualise <- repartition_salaire_net_mensualise(heures_f1, heures_f2, salaire_brut_f1, salaire_brut_f2, semaines_travaillees = 52)$total_contribution_net

    
    
    #CMG Complément libre choix de garde
    cmg_f1 <- if (input$cmg_f1) input$valeur_cmg_f1 else 0
    cmg_f2 <- if (input$cmg_f2) input$valeur_cmg_f2 else 0
    cmg_f1_mensuel <- if (input$cmg_f1) input$valeur_cmg_f1 else 0
    cmg_f2_mensuel <- if (input$cmg_f2) input$valeur_cmg_f2 else 0
    
    salaire_annualise_brut_apresCMG_f1 <- salaire_annuel_f1 - cmg_f1_mensuel
    salaire_annualise_brut_apresCMG_f2 <- salaire_annuel_f2 - cmg_f2_mensuel
    
    #CI Crédit d'impôt
    credit_impot_f1 <- {
      plafond_deduction <- 3500
      frais_garde <- salaire_annualise_brut_apresCMG_f1
      frais_deductibles <- max(0, frais_garde)
      frais_eligibles <- min(plafond_deduction, frais_deductibles)
      credit_impot <- 0.5 * frais_eligibles
      credit_impot
    }
    
    output$credit_impot_f1 <- renderText({
      if (credit_impot_f1 > 0) {
        paste("OUI :", round(credit_impot_f1, 2), "€")
      } else {
        "NON"
      }
    })
    
    
    credit_impot_f2 <- {
      plafond_deduction <- 3500
      frais_garde <- salaire_annualise_brut_apresCMG_f2
      frais_deductibles <- max(0, frais_garde)
      frais_eligibles <- min(plafond_deduction, frais_deductibles)
      credit_impot <- 0.5 * frais_eligibles
      credit_impot
    }
    
    output$credit_impot_f2 <- renderText({
      if (credit_impot_f2 > 0) {
        paste("OUI :", round(credit_impot_f2, 2), "€")
      } else {
        "NON"
      }
    })
    
    
    # Calcul des salaires après crédit d'impôt
    salaire_mensualise_f1 <- salaire_annualise_brut_apresCMG_f1 - credit_impot_f1 / 12
    salaire_mensualise_f2 <- salaire_annualise_brut_apresCMG_f2 - credit_impot_f2 / 12

    
    output$resultats_combines <- renderText({
      paste(
        "Résultats pour la Famille 1 :\n",
        "- Salaire brut horaire : ", salaire_brut_f1, "€\n",
        "- Heures par semaine : ", round(heures_f1, 2), "h\n",
        "- Montant annuel et mensuel CMG : ", ifelse(cmg_f1 > 0, paste(cmg_f1, "€ ;"), "Non applicable ;"), ifelse(cmg_f1 !=0 , paste(cmg_f1_mensuel, "€\n"), "\n"),
        "- Droit au crédit d'impôt : ", ifelse(credit_impot_f1 > 0, paste("OUI :", round(credit_impot_f1, 2), "€"), "NON"), "\n",
        "- Salaire annualisé brut (après déduction de la CMG si applicable): ", round(salaire_annualise_brut_apresCMG_f1, 2), "€\n",
        "- Indemnités repas: ", round((indemnite_f1*52)/12,2), "€\n",
        "- Charges patronales par mois:", charges_patronales(salaire_annuel_f1), "€\n",
        "- Coût total par mois :", round(salaire_mensualise_f1, 2) + round((indemnite_f1*52)/12, 2) + charges_f1, "€\n\n",
        
        "Résultats pour la Famille 2 :\n",
        "- Salaire brut horaire : ", salaire_brut_f2, "€\n",
        "- Heures par semaine : ", round(heures_f2, 2), "h\n",
        " -Montant annuel et mensuel CMG : ", ifelse(cmg_f2 > 0, paste(cmg_f2, "€ ;"), "Non applicable ;"), ifelse(cmg_f2 !=0 , paste(cmg_f2_mensuel, "€\n"), "\n"),
        "- Droit au crédit d'impôt : ", ifelse(credit_impot_f2 > 0, paste("OUI :", round(credit_impot_f2, 2), "€"), "NON"), "\n",
        "- Salaire annualisé brut (après déduction de la CMG si applicable): ", round(salaire_annualise_brut_apresCMG_f2, 2), "€\n",
        "- Indemnités repas : ", round((indemnite_f2*52)/12, 2), "€\n",
        "- Charges patronales :", charges_patronales(salaire_annuel_f2), "€\n",
        "- Coût total par mois :", round(salaire_mensualise_f2, 2) + round((indemnite_f2*52)/12, 2) + charges_f2, "€\n\n",
        
        "Résultats combinés :\n",
        "- Heures totales par semaine : ", round(heures_totales, 2), "h\n",
        "- Salaire annualisé total net : ", round(salaire_net_annuel_mensualise, 2), "€\n",
        "- Indemnités repas totales : ", round(indemnite_f1 + indemnite_f2, 2), "€\n"
      )
    })
  })
  
  # Graphiques
  output$plot_repartition_heures <- renderPlotly({
    
    req(input$calcul_global)
    
    heures_f1 <- calcul_heures_semaine("_f1")
    heures_f2 <- calcul_heures_semaine("_f2")
    data <- data.frame(
      Famille = c("Famille 1", "Famille 2"),
      Heures = c(heures_f1, heures_f2)
    )
    
    p <- ggplot(data, aes(x = Famille, y = Heures, fill = Famille)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "Répartition des heures travaillées", y = "Heures par semaine", x = "Famille") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$plot_revenu_famille <- renderPlotly({
    
    req(input$calcul_global)
    
    heures_f1 <- calcul_heures_semaine("_f1")
    heures_f2 <- calcul_heures_semaine("_f2")
    salaire_brut_f1 <- as.numeric(input$salaire_brut_f1)
    salaire_brut_f2 <- as.numeric(input$salaire_brut_f2)
    
    revenu_f1 <- calcul_salaire_mensualise_partage(heures_f1, 0, salaire_brut_f1,0, 52)$salaire_f1
    revenu_f2 <- calcul_salaire_mensualise_partage(0, heures_f2,0, salaire_brut_f2, 52)$salaire_f2
    
    data <- data.frame(
      Famille = c("Famille 1", "Famille 2"),
      Revenus = c(revenu_f1, revenu_f2)
    )

    p <- ggplot(data, aes(x = Famille, y = Revenus, fill = Famille)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "Tarif brut à payer par famille", y = "Revenus (€)", x = "Famille") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$plot_charges <- renderPlot({
    data <- data.frame(
      Type = c("Salariales", "Patronales"),
      Pourcentage = c(21.88, 44.69)
    )
    
    ggplot(data, aes(x = "", y = Pourcentage, fill = Type)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      labs(title = "Répartition des charges sociales", x = NULL, y = NULL) +
      theme_void() +
      theme(legend.title = element_blank())
    

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
