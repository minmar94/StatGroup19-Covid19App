# Packages
require(tidyverse)
require(magrittr)
require(shiny)
require(shinythemes)
require(ggiraph)


ui <- navbarPage(theme = shinytheme("united"),
                title = "Analysis of the outbreak of CoviD-19 syndrome in Italy",
      
                tabPanel("Overview", 
                         fluidRow(
                           column(4, tags$strong("What is CoviD-19?"), textOutput(outputId = "COV19")),
                           column(4, tags$strong("Vocabulary"), htmlOutput(outputId = "Defins")),
                           column(4, tags$strong("Statistical quantities"), htmlOutput(outputId = "Statquant"))
                           ), tags$hr(),
                         fluidRow(
                           column(6, tags$strong("Current situation"), htmlOutput(outputId = "CurrentSitua")),
                           column(6, tags$strong("Map"), ggiraphOutput(outputId = "MapIta")))
                         )
                ,
                tabPanel("Modeling"),
                tabPanel("Credits")
  
)

server <- function(input, output){
  output$COV19 <- renderText({"COronaVIrus Diseas 2019 (CoviD-19) is an infectious disease caused by severe acute respiratory  syndrome coronavirus 2 (SARS-CoV-2). The virus is spread mainly through close contact and via respiratory droplets produced when people cough or sneeze. Respiratory droplets may be produced during breathing but the virus is not generally airborne. People may also contract COVID-19 by touching a contaminated surface and then their face.[14][15] It is most contagious when people are symptomatic, although spread may be possible before symptoms appear."})
  
  output$Defins <- renderUI(HTML("<ul><li>Currently positive patients</li><li>Deceased patients</li></ul>"))
  
  output$Statquant <- renderUI(HTML("<ul><li>Mortality</li><li>Deadliness</li></ul>"))
  
  output$MapIta <- renderggiraph({
    italy_map <- map_data(map = "italy")
    italy_plot <- italy_map %>%
      ggplot(aes(x = long, y = lat)) +
      geom_path_interactive(aes(group = region)) +
      ggtitle("Map example")
    ggiraph(ggobj = italy_plot)
  })
  
}

shinyApp(ui = ui, server = server)