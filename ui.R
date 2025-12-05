ui <- fluidPage(
  shinyjs::useShinyjs(),
  theme = shinythemes::shinytheme(theme = "cerulean"),
  titlePanel("CryoCOMPASS: Condition Optimization by Modelling and Parameter Analysis for Sample Screening"),
  # Navigation bar to switch between analysis pages
 navbarPage(
    title = tags$div(
      style = "text-align: center; width: 100%;",
      tags$img(src = "cryoCOMPASS.png", height = "30px")
    ),
    tabPanel("Home", home_tab()),
    tabPanel("Experimental Design Setup", factorial_tab()),
    tabPanel("Desirability Analysis", desirability_tab())
 )
 # div(style="position:fixed;z-index:9999;bottom:10px;right:10px;",
 #     actionButton("Browser","whats wrong")
 # )
)
