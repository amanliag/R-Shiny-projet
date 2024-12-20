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
                )
              )
      )
    )
  )
)
                
                
                
                
                
               
         

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
        "- Indemnités repas: ", round((indemnite_f1*52)/12,2), "€\n",
        "- Charges patronales par mois:", charges_patronales(salaire_annuel_f1), "€\n",
        "- Coût total par mois :", round(salaire_annuel_f1, 2) + round((indemnite_f1*52)/12, 2) + charges_f1, "€\n\n",
        
        "Résultats pour la Famille 2 :\n",
        "- Salaire brut horaire : ", salaire_brut_f2, "€\n",
        "- Heures par semaine : ", round(heures_f2, 2), "h\n",
        "- Salaire annualisé : ", round(salaire_annuel_f2, 2), "€\n",
        "- Indemnités repas : ", round((indemnite_f2*52)/12, 2), "€\n",
        "- Charges patronales :", charges_patronales(salaire_annuel_f2), "€\n",
        "- Coût total par mois :", round(salaire_annuel_f2, 2) + round((indemnite_f2*52)/12, 2) + charges_f2, "€\n\n",
        
        "Résultats combinés :\n",
        "- Heures totales par semaine : ", round(heures_totales, 2), "h\n",
        "- Salaire annualisé total : ", round(salaire_annuel_total, 2), "€\n",
        "- Indemnités repas totales : ", round(indemnite_f1 + indemnite_f2, 2), "€\n\n"
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
    
    revenu_f1 <- calcul_salaire_mensualise_partage(heures_f1, 0, salaire_brut_f1, 52)$salaire_f1
    revenu_f2 <- calcul_salaire_mensualise_partage(0, heures_f2, salaire_brut_f2, 52)$salaire_f2
    
    data <- data.frame(
      Famille = c("Famille 1", "Famille 2"),
      Revenus = c(revenu_f1, revenu_f2)
    )
    
    p <- ggplot(data, aes(x = Famille, y = Revenus, fill = Famille)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "Revenus par famille", y = "Revenus (€)", x = "Famille") +
      theme_minimal()
    
    ggplotly(p)
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
