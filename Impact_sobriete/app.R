# INSTALL DEPENDENCIES ----------------------------------------------------

source('../dependencies.R')
# load all packages
lapply(required_packages, require, character.only = TRUE)

ui <- fluidPage(
  
)

server <- function(input, output) {
  
}

# Run the application 
shinyApp(ui = ui, server = server)

