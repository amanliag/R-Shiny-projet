# INSTALLATION DEPENDANCES ----------------------------------------------------

source('../dependencies.R')
source('../fonctions.R')

# charger les paquets
lapply(required_packages, require, character.only = TRUE)


# ui ----------------------------------------------------

ui <- fluidPage (
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
      tabItem(tabName = "simulateur",
              h2("Contenu du simulateur")
      ),
      tabItem(tabName = "etude_cas",
              h2("Contenu des études de cas")
      )
    )
  )
)




)


server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)

