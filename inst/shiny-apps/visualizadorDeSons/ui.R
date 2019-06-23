library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(httr)
library(shinyhttr)

source("modules/pb.R")

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
        "corujinhas_do_mato"
      )
    )
  )
)
