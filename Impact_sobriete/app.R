# INSTALLATION DEPENDANCES ----------------------------------------------------

source('../dependencies.R')
source('../fonctions.R')

# charger les paquets
lapply(required_packages, require, character.only = TRUE)

# ui ----------------------------------------------------
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
        # Onglet Simulateur
        tabItem(tabName = "simulateur",
                h2("Contenu du simulateur"),
                
                # Disposition des filtres (côte à côte) et du graphique en dessous
                fluidRow(
                  column(6,
                         wellPanel(
                           style = "background-color: #ffcdba; border: 2px solid #150a0a;",
                           h3("Famille 1"),
                           sliderInput("salaire_f1", "Salaire (net) :", 0, 300, 15, step = 1),
                           numericInput("prime_f1", "Montant de la prime/remboursement :", value=20),
                           numericInput("heure_garde_f1", "Nombre d’heures de garde par jour de la semaine :", value=35),
                           numericInput("sem_vacances_f1", "Nombre de semaines de vacances :", value=25)
                         )
                  ),
                  column(6,
                         wellPanel(
                           style = "background-color: #ffcdba; border: 2px solid #150a0a;",
                           h3("Famille 2"),
                           numericInput("salaire_f2", "Salaire (net) :", value=2000),
                           numericInput("prime_f2", "Montant de la prime/remboursement :", value=20),
                           numericInput("heure_garde_f2", "Nombre d’heures de garde par jour de la semaine :", value=35),
                           numericInput("sem_vacances_f2", "Nombre de semaines de vacances :", value=25)
                         )
                  )
                ),
                fluidRow(
                  column(12,  
                         plotOutput("barplot_salaire")
                  )
                )
        ),
        
        # Onglet Etudes de cas
        tabItem(tabName = "etude_cas",
                h2("Contenu des études de cas"),
                selectInput("test", "Test :", choices=NULL)
        )
      )
    )
  )
)


server <- function(input, output) {
  
  # Calcul du salaire brut et affichage
  output$salaire_brut <- renderText({
    sal_brut <- calcul_brut(input$salaire_f1)
    emp_cal <- calcul_emp(input$salaire_f1)
    
    # Affichage du résultat dans l'UI
    paste("Salaire brut estimé : ", sal_brut, "€\n",
          "L'employeur versera : ", emp_cal, "€")
  })
  
  # Générer le diagramme en barres avec ggplot2
  output$barplot_salaire <- renderPlot({
    sal_brut <- calcul_brut(input$salaire_f1)
    
    # Créer un barplot avec salaire brut et net
    bar_data <- c(input$salaire_f1, sal_brut)
    bar_names <- c("Salaire net", "Salaire Brut")
    
    salaire_data <- data.frame(
      type = bar_names,
      montant = bar_data
    )
    

    ggplot(salaire_data, aes(x = type, y = montant, fill = type)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "Comparaison Salaire Net / Salaire Brut", y = "Montant (€)", x = "") +
      theme_minimal() +
      scale_fill_manual(values = c("blue", "red"))  
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
