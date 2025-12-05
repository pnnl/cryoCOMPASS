home_tab <- function(){
   div(
      fluidRow(
         tabPanel("Home",
                  p("CryoCOMPASS reduces the amount of trial and error in the process of grid vitrification, minimizing the
amount of time and effort needed to obtain high-quality grids for CryoEM data collection."),
                  p("Use the Experimental Design tab to create a list of conditions for screening your sample grids. When screening is complete, use the Desirability Analysis tab to build a regression model of your parameter space and calculate optimized input conditions."),
                  p("View or download the CryoCOMPASS user guide below."),
                  actionButton("view_userguide", "Open User Guide"),
                  downloadButton("download_userguide", "Download User Guide"),
                  p(""),
                  p("Created by Rose Marie Yasuda and Damon Leach"),
                  actionButton("email_button", "Contact Us")
         ),
         bsModal("modal", "CryoCOMPASS User Guide", "view_userguide", size = "large",
                 htmltools::tags$iframe(src = "CryoCOMPASS-userguide.pdf", width = '100%',  height = 1000,  style = "border:none;"))
      )
   )
}