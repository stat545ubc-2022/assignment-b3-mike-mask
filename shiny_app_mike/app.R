library(shiny)
library(tidyverse)
library(gapminder)

gap <- gapminder

#COMMENT ALL CODE BEFORE YOU SUBMIT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

ui <- fluidPage(
  img(src= "images.jpeg"),
  titlePanel("Gapminder Dataset"),
  h3("Exploring the Gapminder dataset"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("COUNTRY"),
      verbatimTextOutput("value")
    ),
    mainPanel(
      tableOutput("results")
    )
  )
)

server <- function(input, output) {
  filtered <- reactive({
    gap %>%
      filter(country == input$COUNTRY)
  })
  output$value <- renderText({input$caption})
  output$results <- renderTable({
    filtered()
  })
  output$COUNTRY <- renderUI({
    selectInput("COUNTRY", "Country",
                sort(unique(gap$country)),
                selected = "Canada")
  })
 
}

shinyApp(ui = ui, server = server)

