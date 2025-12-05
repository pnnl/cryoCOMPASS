suppressPackageStartupMessages({
   ## R stuff

   ## Shiny stuff
   library(shiny)
   library(shinyjs)
   library(shinyBS)
   library(shinyWidgets)
   library(shinycssloaders)
   library(shinyalert)
   library(prompter)
   library(shinydashboardPlus)
   library(shinyFiles)
   library(shinyjqui)
   library(rintrojs)
   library(shinybusy)
   library(shinythemes)
   library(shiny.blueprint)
  
  ## Desirability Functions
  library(AlgDesign)
  library(dplyr)
  library(ggplot2)
  library(car)
  library(patchwork)
  library(desirability)
  library(gt)
  library(stringr)
  library(readxl)
  library(plotly)
  library(writexl)
  library(purrr)
  
})

# load files
# Load files
file_loads <- c(
   list.files("./Helpers", recursive = T, full.names = T),
   list.files("./Frontend_Static_UI", recursive = T, full.names = T)
)

for (f in grep(".R$", file_loads, value = T)) source(f, local = TRUE)
