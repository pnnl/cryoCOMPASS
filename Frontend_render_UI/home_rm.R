# Download handler for static PDF
output$download_userguide <- downloadHandler(
  filename = function() {
    "CryoCOMPASS-userguide.pdf"
  },
  content = function(file) {
    file.copy("www/CryoCOMPASS-userguide.pdf", file)
  }
)

# Show the PDF in an iframe when the button is clicked
observeEvent(input$view_userguide, {
  output$pdf_viewer <- renderUI({
    tags$iframe(style = "height:600px; width:100%;",
                src = "CryoCOMPASS-userguide.pdf")  # Relative to www/
  })
})

observeEvent(input$email_button, {
  shinyjs::runjs("window.location.href = 'mailto:rose.yasuda@pnnl.gov';")
})