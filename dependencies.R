# LIST OF REQUIRED PACKAGES -----------------------------------------------

required_packages <- c(
  "shiny", 
  "shinydashboard", 
  "tidyr", 
  "ggplot2", 
  "shinyTime", 
  "shinyWidgets", 
  "plotly", 
  "shinyFeedback"
)


new.packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if (length(new.packages)) {
  install.packages(new.packages)
}

lapply(required_packages, library, character.only = TRUE)
