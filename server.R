library(shiny)

# Define server logic
server <- function(input, output) {
   file_loads <- c(
      list.files("./Frontend_render_UI", recursive = T, full.names = T)
   )

   for (f in grep(".R$", file_loads, value = T)) source(f, local = TRUE)

   make_table_good <- reactiveVal(FALSE)
   make_stats_good <- reactiveVal(FALSE)

   # output$developer_buttons <- renderUI({
   #    div(style="position:fixed;z-index:9999;bottom:10px;right:10px;",
   #        actionButton("Browser", "whats wrong!?!?", style = "background:deepskyblue")      )
   # })
   # 
   # # Button to do browser things for debug
   # observeEvent(input$Browser, {
   #    browser()
   # })
}
