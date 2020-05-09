library(reactable)
library(shiny)
library(shinydashboard)
library(httr)
library(shinycssloaders)
library(magrittr)

addResourcePath("data_raw", "/media/athos/DATA/OneDrive/Documents/mestrado/data-raw")
addResourcePath("data", "/media/athos/DATA/OneDrive/Documents/mestrado/data")

dashboardPage(
  dashboardHeader(title = "Visualizador de Sons"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(
        text = "Corujinhas do Mato",
        tabName = "corujinhas_do_mato"
      )
    )
  ),

  dashboardBody(
    tabItems(
      tabItem(
        "corujinhas_do_mato",
        fluidRow(
          box(width = 12,
              selectizeInput("arquivo", label = "Arquivo", choices = list.files("/media/athos/DATA/OneDrive/Documents/mestrado/data/Glaucidium-minutissimum")),
              uiOutput("audio") %>% withSpinner(color="#0dc5c1", proxy.height = 158, size = 0.3)
          ),

          box(width = 12,
              plotOutput("grafico")  %>% withSpinner(color="#0dc5c1")
          ),
          box(width = 12,
              reactableOutput("segmentacao")  %>% withSpinner(color="#0dc5c1")
          )
        )
      )
    )
  )
)
