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
    useShinyjs(),
    useShinyFeedback(),
    tabItems(
      ######## ONGLET SIMULATEUR ########
      tabItem(tabName = "simulateur",
              h2("Informations nécessaires à la simulation"),
              fluidRow(
                column(12,
                       wellPanel(
                         style = "background-color: #ffcdba; border: 2px solid #150a0a;",
                         h3("Salaire brut horaire pour Famille 1 et Famille 2"),
                         numericInput("salaire_brut", "Salaire brut horaire (€) :", value = 12.26, min = 12.26)
                       )
                )
              ),
              fluidRow(
                column(6,
                       wellPanel(
                         style = "background-color: #ffcdba; border: 2px solid #150a0a;",
                         h3("Famille 1"),
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
                                   column(6, timeInput(paste0("debut_matin_", jour, "_f1"), "Début:", value = "08:00")),
                              
                                   column(6, timeInput(paste0("fin_aprem_", jour, "_f1"), "Fin:",  value = "18:00"))
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
                                   column(6, timeInput(paste0("debut_matin_", jour, "_f2"), "Début:", value = "08:00")),
                          
                                   column(6, timeInput(paste0("fin_aprem_", jour, "_f2"), "Fin:", value = "18:00"))
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
                       wellPanel(
                         style = "background-color: #ffcdba; border: 2px solid #150a0a;",
                       wellPanel(
                         style = "background-color: #fff; border: 1px solid #333;",
                         h4("Forfait déplacement :"),
                         
                         checkboxGroupInput(
                           inputId = "moyens_de_deplacement",
                           label = NULL,
                           choiceNames = list(
                             tagList(icon("xmark"), " Aucun"),
                             tagList(icon("bicycle"), " Vélo"),
                             tagList(icon("bus"), " Transports en commun"),
                             tagList(icon("car"), " Voiture")
                           ),
                           choiceValues = list("Aucun", "Vélo", "Transports en commun", "Voiture - 6CV")
                         ),
                         
                         conditionalPanel(
                           condition = "input.moyens_de_deplacement.includes('Vélo')",
                           numericInput(
                             inputId = "km_velo_par_mois",
                             label = "Nombre de kilomètres en vélo par mois (0.41cts/km):",
                             value = NULL,
                             min = 0
                           )
                         ),
                         conditionalPanel(
                           condition = "input.moyens_de_deplacement.includes('Transports en commun')",
                           numericInput(
                             inputId = "Tarif_transports_communs",
                             label = "50% du tarif des tickets OU de l'abonnement par mois pris en charge:",
                             value = NULL,
                             min = 0
                           )
                         ),
                         conditionalPanel(
                           condition = "input.moyens_de_deplacement.includes('Voiture - 6CV')",
                           numericInput(
                             inputId = "km_voiture_par_mois",
                             label = "Nombre de kilomètres en voiture par mois :",
                             value = NULL,
                             min = 0
                           )
                         ),
                         h4("Sélection des semaines de congés :"),
                         
                         fluidRow(
                           column(6, dateInput("semaine1_debut", "Début Semaine 1 :", value = NA, format = "dd-mm-yyyy")),
                           column(6, dateInput("semaine1_fin", "Fin Semaine 1 :", value = NA, format = "dd-mm-yyyy"))
                         ),
                         fluidRow(
                           column(6, dateInput("semaine2_debut", "Début Semaine 2 :", value = NA, format = "dd-mm-yyyy")),
                           column(6, dateInput("semaine2_fin", "Fin Semaine 2 :", value = NA, format = "dd-mm-yyyy"))
                         ),
                         fluidRow(
                           column(6, dateInput("semaine3_debut", "Début Semaine 3 :", value = NA, format = "dd-mm-yyyy")),
                           column(6, dateInput("semaine3_fin", "Fin Semaine 3 :", value = NA, format = "dd-mm-yyyy"))
                         ),
                         fluidRow(
                           column(6, dateInput("semaine4_debut", "Début Semaine 4 :", value = NA, format = "dd-mm-yyyy")),
                           column(6, dateInput("semaine4_fin", "Fin Semaine 4 :", value = NA, format = "dd-mm-yyyy"))
                         ),
                         fluidRow(
                           column(6, dateInput("semaine5_debut", "Début Semaine 5 :", value = NA, format = "dd-mm-yyyy")),
                           column(6, dateInput("semaine5_fin", "Fin Semaine 5 :", value = NA, format = "dd-mm-yyyy"))
                         ),
                         fluidRow(
                           column(6, dateInput("semaine6_debut", "Début Semaine 6 :", value = NA, format = "dd-mm-yyyy")),
                           column(6, dateInput("semaine6_fin", "Fin Semaine 6 :", value = NA, format = "dd-mm-yyyy"))
                         )
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
                  column(12,
                         box(
                           title = "Répartition des heures travaillées",
                           status = "primary",
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           plotlyOutput("plot_repartition_heures")
                         )
                  ),
                  column(12,
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
                    tags$li("Indemnités transport par mois: 5 euros par mois à partir de 20km en vélo ; 0.41cts le km en voiture ; 50% de prise en charge des tarifs de transport en commun")
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
                ),
                box(
                  title = "Document - Contrat de travail à durée indéterminée (CDI)", 
                  status = "warning", 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  tags$ul(tags$li(tags$a(href = "https://www.pajemploi.urssaf.fr/pajewebinfo/files/live/sites/pajewebinfo/files/contributed/pdf/contrats-de-travail/6237-PE-Contrat-CDI-Pajemploi-Ama-FORMULAIRE.pdf", 
                                   "Contrat de travail - Document disponible en suivant ce lien"))
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
      inputId = "salaire_brut",
      show = input$salaire_brut < 12.26,
      text = "Le salaire brut horaire doit être supérieur ou égal à 12,26 €."
    )})
  
  observeEvent({
    unlist(lapply(c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche"), function(jour) {
      c(input[[paste0("debut_matin_", jour, "_f1")]], input[[paste0("fin_aprem_", jour, "_f1")]], 
        input[[paste0("debut_matin_", jour, "_f2")]], input[[paste0("fin_aprem_", jour, "_f2")]])}))
  }, 
  {
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    for (jour in jours) {
      horaires_f1 <- c()
      horaires_f2 <- c()
      
      # Heures f1
      if (isTRUE(input[[paste0("jour_travaille_", jour, "_f1")]])) {
        debut_f1 <- input[[paste0("debut_matin_", jour, "_f1")]]
        fin_f1 <- input[[paste0("fin_aprem_", jour, "_f1")]]
        horaires_f1 <- c(debut_f1, fin_f1)
      }
      
      # Heures f2
      if (isTRUE(input[[paste0("jour_travaille_", jour, "_f2")]])) {
        debut_f2 <- input[[paste0("debut_matin_", jour, "_f2")]]
        fin_f2 <- input[[paste0("fin_aprem_", jour, "_f2")]]
        horaires_f2 <- c(debut_f2, fin_f2)
      }
      
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
            footer = modalButton("OK")))
        }
      }
    }
  }, ignoreNULL = FALSE, ignoreInit = TRUE)
  
  
  
  #Vérification qu'il y a bien un jour de repos si 6 jours consécutifs 
  observe({
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    
    jours_travailles_f1 <- c()
    jours_travailles_f2 <- c()
    
    for (jour in jours) {
      if (isTRUE(input[[paste0("jour_travaille_", jour, "_f1")]])) {
        jours_travailles_f1 <- c(jours_travailles_f1, jour)
      }
      
      if (isTRUE(input[[paste0("jour_travaille_", jour, "_f2")]])) {
        jours_travailles_f2 <- c(jours_travailles_f2, jour)
      }
    }
    
    f1_7_jours <- length(jours_travailles_f1) == 7
    f2_7_jours <- length(jours_travailles_f2) == 7
    jours_total <- unique(c(jours_travailles_f1, jours_travailles_f2))
    couverture <- length(jours_total) == 7
    
    if (f1_7_jours || f2_7_jours || couverture) {
      showModal(modalDialog(
        title = "⚠️ Avertissement ⚠️",
        "Une des familles demande la garde de son enfant 7 jours ou toutes les journées de la semaine sont couvertes par les deux familles.",
        easyClose = TRUE,
        footer = modalButton("OK")
      ))
    }
  })
  
  
  
  

  #### Calcul des heures totales travaillées par semaine pour une famille donnée ####
  calcul_heures_semaine <- function(input_suffixe_f1, input_suffixe_f2) {
    jours <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
    
    total_minutes_f1 <- 0
    total_minutes_f2 <- 0
    total_minutes_communes <- 0
    
    for (jour in jours) {
      
      if (isTRUE(input[[paste0("jour_travaille_", jour, input_suffixe_f1)]])) {
        matin_debut_f1 <- input[[paste0("debut_matin_", jour, input_suffixe_f1)]]
        aprem_fin_f1 <- input[[paste0("fin_aprem_", jour, input_suffixe_f1)]]
        
        matin_debut_f1 <- ifelse(!is.null(matin_debut_f1) && matin_debut_f1 != "", as.POSIXct(matin_debut_f1, format = "%H:%M"), NA)
        aprem_fin_f1 <- ifelse(!is.null(aprem_fin_f1) && aprem_fin_f1 != "", as.POSIXct(aprem_fin_f1, format = "%H:%M"), NA)
      } else {
        matin_debut_f1 <- NA
        aprem_fin_f1 <- NA
      }
      
      if (isTRUE(input[[paste0("jour_travaille_", jour, input_suffixe_f2)]])) {
        matin_debut_f2 <- input[[paste0("debut_matin_", jour, input_suffixe_f2)]]
        aprem_fin_f2 <- input[[paste0("fin_aprem_", jour, input_suffixe_f2)]]
        
        matin_debut_f2 <- ifelse(!is.null(matin_debut_f2) && matin_debut_f2 != "", as.POSIXct(matin_debut_f2, format = "%H:%M"), NA)
        aprem_fin_f2 <- ifelse(!is.null(aprem_fin_f2) && aprem_fin_f2 != "", as.POSIXct(aprem_fin_f2, format = "%H:%M"), NA)
      } else {
        matin_debut_f2 <- NA
        aprem_fin_f2 <- NA
      }
      
      if (!is.na(matin_debut_f1) && !is.na(aprem_fin_f1)) {
        total_minutes_f1 <- total_minutes_f1 + as.numeric(difftime(aprem_fin_f1, matin_debut_f1, units = "mins"))
      }
      
      if (!is.na(matin_debut_f2) && !is.na(aprem_fin_f2)) {
        total_minutes_f2 <- total_minutes_f2 + as.numeric(difftime(aprem_fin_f2, matin_debut_f2, units = "mins"))
      }
      
      if (!is.na(matin_debut_f1) && !is.na(aprem_fin_f1) && !is.na(matin_debut_f2) && !is.na(aprem_fin_f2)) {
        debut_commune <- max(matin_debut_f1, matin_debut_f2, na.rm = TRUE)
        fin_commune <- min(aprem_fin_f1, aprem_fin_f2, na.rm = TRUE)
        
        if (debut_commune < fin_commune) {  # Si il y a un chevauchement
          total_minutes_communes <- total_minutes_communes + as.numeric(difftime(fin_commune, debut_commune, units = "mins"))
        }
      }
    }
    
    total_minutes_assistante <- total_minutes_f1 + total_minutes_f2 - total_minutes_communes
    
    return(list(
      heures_communes = total_minutes_communes / 60,
      heures_f1 = total_minutes_f1 / 60,
      heures_f2 = total_minutes_f2 / 60,
      heures_assistante = total_minutes_assistante / 60
    ))
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

  #Indemnite deplacement
  calcul_indemnite_deplacement <- function() {
    
    tarif_km_voiture <- 0.41 
    moyens_de_transport <- input[[paste0("moyens_de_deplacement")]]
    
    km_velo <- input[[paste0("km_velo_par_mois")]]
    km_voiture <- input[[paste0("km_voiture_par_mois")]]
    tarif_transport <- input[[paste0("Tarif_transports_communs")]]
    
    if (is.na(km_velo)) km_velo <- 0
    if (is.na(km_voiture)) km_voiture <- 0
    if (is.na(tarif_transport)) tarif_transport <- 0
    tarif_velo <- 0
    tarif_voiture <- 0
    tarif_transport_reduction <- 0
    
    if (!is.null(moyens_de_transport)) {
      
      if ("Vélo" %in% moyens_de_transport && km_velo > 20) {
        tarif_velo <- 5  
      }
      
      if ("Voiture - 6CV" %in% moyens_de_transport) {
        tarif_voiture <- km_voiture * tarif_km_voiture  
      }
      
      if ("Transports en commun" %in% moyens_de_transport) {
        tarif_transport_reduction <- tarif_transport 
      }
    }
    
    total_indemnite <- tarif_velo + tarif_voiture + tarif_transport_reduction
    
        return(list(
      tarif_indemnite_velo = tarif_velo/2,
      tarif_indemnite_voiture = tarif_voiture/2,
      tarif_indemnite_transport_reduction = tarif_transport_reduction/2,
      tarif_total_assistante = total_indemnite,
      tarif_total_famille = total_indemnite/2,
      km_velo = km_velo,
      km_voiture = km_voiture
    ))
  }
  
  
  
    observeEvent(input$calcul_global, {
    salaire_brut <- as.numeric(input$salaire_brut) #horaire

    heures_f2 <- calcul_heures_semaine("_f1", "_f2")$heures_f2 
    heures_f1 <- calcul_heures_semaine("_f1", "_f2")$heures_f1 
    heures_communes <- calcul_heures_semaine("_f1", "_f2")$heures_communes
  
    
    # Famille 1
    salaire_annuel_f1 <- calcul_salaire_mensualise_partage(heures_communes, heures_f1, heures_f2, salaire_brut, 52)$salaire_f1 #mensualise
    charges_f1 <- charges_patronales(salaire_brut)    
    indemnite_f1 <- calcul_indemnite_repas("_f1")
    salaire_brut_an_f1 <- 52 * heures_f1 * salaire_brut #il faudra changer 52 pour le nombre de semaines travaillées 
    
    # Famille 2
    salaire_annuel_f2 <- calcul_salaire_mensualise_partage(heures_communes, heures_f1, heures_f2, salaire_brut, 52)$salaire_f2
    charges_f2 <- charges_patronales(salaire_brut)    
    indemnite_f2 <- calcul_indemnite_repas("_f2")
    salaire_brut_an_f2 <- 52 * heures_f2 * salaire_brut #il faudra changer 52 pour le nombre de semaines travaillées 
    
    # Total global
    heures_totales <- calcul_heures_semaine("_f1", "_f2")$heures_asssistante
    salaire_net_annuel_mensualise <- repartition_salaire_net_mensualise(heures_communes, heures_f1, heures_f2, salaire_brut, semaines_travaillees = 52)$total_contribution_net

    
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

    #Indemnites déplacements 
    indemnite_km <- reactive({calcul_indemnite_deplacement()})

    
    
    output$resultats_combines <- renderText({
      paste(
        "Résultats pour la Famille 1 :\n",
        "- Salaire brut horaire : ", salaire_brut, "€\n",
        "- Heures par semaine : ", heures_f1, "h\n",
        "- Montant annuel et mensuel CMG : ", ifelse(cmg_f1 > 0, paste(cmg_f1, "€ ;"), "Non applicable ;"), ifelse(cmg_f1 !=0 , paste(cmg_f1_mensuel, "€\n"), "\n"),
        "- Droit au crédit d'impôt : ", ifelse(credit_impot_f1 > 0, paste("OUI :", round(credit_impot_f1, 2), "€"), "NON"), "\n",
        "- Salaire annualisé brut (après déduction de la CMG si applicable): ", round(salaire_annualise_brut_apresCMG_f1, 2), "€\n",
        "- Indemnités repas: ", round((indemnite_f1*52)/12,2), "€\n",
        "- Indemnité déplacement vélo: ", indemnite_km()$tarif_indemnite_velo, "€ pour", indemnite_km()$km_velo ,"kilomètres\n",
        "- Indemnité déplacement voiture: ", indemnite_km()$tarif_indemnite_voiture, "€ pour", indemnite_km()$km_voiture ," kilomètres \n",
        "- Indemnité déplacement transports en commun: ",indemnite_km()$tarif_indemnite_transport, "€ \n",
        "- Charges patronales par mois:", charges_patronales(salaire_annuel_f1), "€\n",
        "- Coût total par mois :", round(salaire_mensualise_f1, 2) + round((indemnite_f1*52)/12, 2) + charges_f1 + indemnite_km()$tarif_total_famille, "€\n\n",
        
        "Résultats pour la Famille 2 :\n",
        "- Salaire brut horaire : ", salaire_brut, "€\n",
        "- Heures par semaine : ", heures_f2, "h\n",
        " -Montant annuel et mensuel CMG : ", ifelse(cmg_f2 > 0, paste(cmg_f2, "€ ;"), "Non applicable ;"), ifelse(cmg_f2 !=0 , paste(cmg_f2_mensuel, "€\n"), "\n"),
        "- Droit au crédit d'impôt : ", ifelse(credit_impot_f2 > 0, paste("OUI :", round(credit_impot_f2, 2), "€"), "NON"), "\n",
        "- Salaire annualisé brut (après déduction de la CMG si applicable): ", round(salaire_annualise_brut_apresCMG_f2, 2), "€\n",
        "- Indemnités repas : ", round((indemnite_f2*52)/12, 2), "€\n",
        "- Indemnité déplacement vélo: ", indemnite_km()$tarif_indemnite_velo, "€ pour", indemnite_km()$km_velo ,"kilomètres\n",
        "- Indemnité déplacement voiture: ", indemnite_km()$tarif_indemnite_voiture, "€ pour", indemnite_km()$km_voiture ," kilomètres \n",
        "- Indemnité déplacement transports en commun: ",indemnite_km()$tarif_indemnite_transport, "€ \n",
        "- Charges patronales :", charges_patronales(salaire_annuel_f2), "€\n",
        "- Coût total par mois :", round(salaire_mensualise_f2, 2) + round((indemnite_f2*52)/12, 2) + charges_f2 + indemnite_km()$tarif_total_famille , "€\n\n",
      
        "Résultats combinés :\n",
        "- Heures totales par semaine : ", heures_f1 + heures_f2 - heures_communes, "h\n",
        "- Salaire annualisé total net : ", round(salaire_net_annuel_mensualise, 2), "€\n",
        "- Indemnités repas totales : ", round(indemnite_f1 + indemnite_f2, 2), "€\n",
        "- Indemnités déplacement : ", indemnite_km()$tarif_total_assistante, "€\n",
        "- Semaines semaines de congéss :\n",
        paste0(
          "  - ", format(input$semaine1_debut, "%d/%m/%Y"), " au ", format(input$semaine1_fin, "%d/%m/%Y"), "\n",
          "  - ", format(input$semaine2_debut, "%d/%m/%Y"), " au ", format(input$semaine2_fin, "%d/%m/%Y"), "\n",
          "  - ", format(input$semaine3_debut, "%d/%m/%Y"), " au ", format(input$semaine3_fin, "%d/%m/%Y"), "\n",
          "  - ", format(input$semaine4_debut, "%d/%m/%Y"), " au ", format(input$semaine4_fin, "%d/%m/%Y"), "\n",
          "  - ", format(input$semaine5_debut, "%d/%m/%Y"), " au ", format(input$semaine5_fin, "%d/%m/%Y"), "\n",
          "  - ", format(input$semaine6_debut, "%d/%m/%Y"), " au ", format(input$semaine6_fin, "%d/%m/%Y"), "\n"
        )
        
      )
    })
  })

  

    
    
  # Graphiques
  output$plot_repartition_heures <- renderPlotly({
    
    req(input$calcul_global)
    
    heures_f1 <- calcul_heures_semaine("_f1", "_f2")$heures_f1 
    heures_f2 <- calcul_heures_semaine("_f1", "_f2")$heures_f2
    data <- data.frame(
      Famille = c("Famille 1", "Famille 2"),
      Heures = c(heures_f1, heures_f2)
    )
    
    p <- ggplot(data, aes(x = Famille, y = Heures, fill = Famille)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "Répartition des heures travaillées", y = "Heures par semaine", x = "Famille") +
      theme_minimal()
  })
  
  output$plot_revenu_famille <- renderPlotly({
    
    req(input$calcul_global)
    
    heures_f1 <- calcul_heures_semaine("_f1", "_f2")$heures_f1
    heures_f2 <- calcul_heures_semaine("_f1", "_f2")$heures_f2
    heures_communes <- calcul_heures_semaine("_f1", "_f2")$heures_communes
    
    salaire_brut<- as.numeric(input$salaire_brut)

    revenu_f1 <- calcul_salaire_mensualise_partage(heures_communes, heures_f1, heures_f2, salaire_brut, 52)$salaire_f1
    revenu_f2 <- calcul_salaire_mensualise_partage(heures_communes, heures_f1, heures_f2, salaire_brut, 52)$salaire_f2
    
    data <- data.frame(
      Famille = c("Famille 1", "Famille 2"),
      Revenus = c(revenu_f1, revenu_f2)
    )

    p <- ggplot(data, aes(x = Famille, y = Revenus, fill = Famille)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "Tarif brut à payer par famille", y = "Revenus (€)", x = "Famille") +
      theme_minimal()
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
          "+4.10€ à partir de la 32 à 45ème heure et +4.48€ dès la 45ème heure", 
          "13 heures", 
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
