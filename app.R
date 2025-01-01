library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(plotly)
library(dashboardthemes)


options(browser = "/usr/bin/firefox")
options(shiny.port = 3838)
options(shiny.host = "0.0.0.0")

source("ui.R")
source("server.R")
shinyApp(ui = ui, server = server)