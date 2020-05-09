library(shiny)
library(wavesurfer)

annotate_audio <- function(folder_of_the_audios = "", folder_to_save_annotations = "") {
  require(shiny)
  require(wavesurfer)

  folder_of_the_audios <- "/media/athos/DATA/OneDrive/Documents/mestrado/data-raw/wav_12khz/"
  shiny::addResourcePath("wavs", folder_of_the_audios)
  folder_to_save_annotations <- "/media/athos/DATA/OneDrive/Documents/mestrado/data/anotacoes/"

  ui <- fluidPage(

    titlePanel("Audio Annotator"),
    tabsetPanel(
      tabPanel(
        title = "Annotator",

        fluidRow(
          column(
            width = 3,
            uiOutput("especies"),
            uiOutput("arquivo_wav")
          )
        ),
        fluidRow(
          column(
            width = 12,
            h3("Plugins"),
            actionButton("minimap", "Minimap", icon = icon("map")),
            actionButton("spectrogram", "spectrogram", icon = icon("chart")),
            actionButton("cursor", "Cursor", icon = icon("pointer")),
            actionButton("timeline", "Timeline", icon = icon("time"))
          )
        ),
        tags$br(),
        fluidRow(
          column(
            width = 12,
            wavesurferOutput("meu_ws")
          )
        ),
        fluidRow(
          column(
            width = 12,
            tags$div(
              class="btn-group",
              role="group",
              actionButton("play", "Play", icon = icon("play")),
              actionButton("pause", "Pause", icon = icon("pause")),
              actionButton("mute", "Mute", icon = icon("mute")),
              actionButton("stop", "Stop", icon = icon("stop")),
              actionButton("save", "Save", icon = icon("save")),
              actionButton("sugerir_regioes", "Sugerir regiões", icon = icon("cut"))
            )
          ),
          column(
            width = 4,
            sliderInput("zoom", "Zoom", min = 1, max = 1000, value = 50),
          )
        ),
        verbatimTextOutput("current_region"),
        verbatimTextOutput("regions"),
      ),
      tabPanel(
        title = "inputs feedback",
        verbatimTextOutput("regions_class")
      )
    )

  )




  server <- function(input, output, session) {

    passaros <- readr::read_rds("/media/athos/DATA/OneDrive/Documents/mestrado/data/passaros.rds")
    wav_name <- reactive({stringr::str_replace(input$audio, "^wavs/", "")})
    output$especies <- renderUI({
      selectizeInput(
        "especie",
        "Espécie: ",
        choices = passaros$especie,
        width = "100%"
      )
    })

    output$arquivo_wav <- renderUI({
      req(!is.null(input$especie))

      audios_nao_anotados <- paste0("wavs/", list.files(folder_of_the_audios, pattern = input$especie))
      audios_anotados <- paste0("wavs/", stringr::str_replace(list.files(paste0(folder_to_save_annotations)), "rds$", "wav"))
      audios_nao_anotados <- audios_nao_anotados[!audios_nao_anotados %in% audios_anotados]

      selectizeInput(
        "audio",
        "Audio: ",
        choices = audios_nao_anotados,
        width = "100%"
      )
    })

    output$meu_ws <- renderWavesurfer({
      req(!is.null(input$audio))
      annotations_path <- stringr::str_replace_all(stringr::str_replace_all(input$audio, "wav$", "rds"), "^wavs/", "")
      annotations_path <- paste0(folder_to_save_annotations, annotations_path)

      if(file.exists(annotations_path)) {
        annotations_df <- readr::read_rds(annotations_path) %>% dplyr::mutate(label = character(0))
      } else {
        annotations_df <- NULL
      }
      wavesurfer(visualization = "spectrogram",
                 input$audio,
                 annotations = annotations_df
      ) %>%
        ws_set_wave_color(color = "#aa88ff") %>%
        ws_region_labeller() %>%
        ws_regions()
    })

    observeEvent(input$play, {
      ws_play("meu_ws")
    })

    observeEvent(input$pause, {
      ws_pause("meu_ws")
    })

    observeEvent(input$mute, {
      ws_toggle_mute("meu_ws")
    })

    observe({
      ws_zoom("meu_ws", input$zoom)
    })

    observeEvent(input$stop, {
      ws_stop("meu_ws")
    })

    observeEvent(input$minimap, {
      ws_minimap("meu_ws")
    })

    observeEvent(input$spectrogram, {
      ws_spectrogram("meu_ws")
    })

    observeEvent(input$timeline, {
      ws_timeline("meu_ws")
    })

    observeEvent(input$cursor, {
      ws_cursor("meu_ws")
    })

    observeEvent(input$elan, {
      ws_elan("meu_ws")
    })

    observeEvent(input$regions, {
      ws_regions("meu_ws")
    })

    observeEvent(input$microphone, {
      ws_minimap("meu_ws")
    })


    observeEvent(input$save, {
      req(!is.null(wav_name()))
      annotations <- stringr::str_replace_all(stringr::str_replace_all(input$audio, "wav$", "rds"), "^wavs/", "")
      regions <- input$meu_ws_regions %>% dplyr::mutate(sound_id = wav_name())
      readr::write_rds(x = regions, path = paste0(folder_to_save_annotations, annotations))
    })

    observeEvent(input$sugerir_regioes, {
      browser()

      wav <- tuneR::readWave(paste0(folder_of_the_audios, wav_name()))

      ## funcao do auto detector
      auto_detect_partial <- purrr::partial(
        warbleR::auto_detec,
        X = data.frame(sound.files = wav_name(), selec = 1, start = 0, end = Inf),
        path = folder_of_the_audios,
        pb = FALSE
      )
      params_do_auto_detec <- passaros$parametros_do_auto_detect[[input$especie]]
      ## segmentacoes encontradas
      suggested_annotations <- do.call(auto_detect_partial, params_do_auto_detec)
      suggested_annotations$sound.files <- wav_name()
      if(is.null(suggested_annotations$label)) {
        suggested_annotations$label <- "(unlabeled)"
      }
      suggested_annotations <- suggested_annotations %>% dplyr::mutate(label = sample(letters, size = length(label), replace = TRUE))
      names(suggested_annotations) <- c("sound_id", "segmentation_id", "start", "end", "label")
      ws_add_regions("meu_ws", suggested_annotations)
    })

    output$regions_class <- renderPrint({
      reactiveValuesToList(input)
    })

    output$current_region <- renderPrint({
      input$meu_ws_selected_region
    })
    output$regions <- renderPrint({
      input$meu_ws_regions
    })
  }

  shinyApp(ui = ui, server = server)
}


annotate_audio()

