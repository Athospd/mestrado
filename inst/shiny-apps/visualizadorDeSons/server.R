library(shiny)
library(httr)

addResourcePath("data_raw", "/media/athos/DATA/OneDrive/Documents/mestrado/data-raw")
addResourcePath("data", "/media/athos/DATA/OneDrive/Documents/mestrado/data")


function(input, output, session) {

  arq <- reactive((
    readr::read_rds(paste0("/media/athos/DATA/OneDrive/Documents/mestrado/data/Glaucidium-minutissimum/", input$arquivo))
  ))

  output$grafico <- renderPlot({
    arq()$grafico
  })

  output$segmentacao <- renderReactable({
    reactable::reactable(arq()$segmentacao)
  })

  output$audio <- renderUI({
    wav_name = stringr::str_replace(input$arquivo, "rds", "wav")
    column(
      width = 12,
      tags$audio(src = glue::glue("data_raw/wav_12khz/{wav_name}"), type = "audio/wav", autoplay = FALSE, controls = NA)
    )
  })
}
