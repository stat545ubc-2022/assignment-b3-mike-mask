library(shiny) #loading shiny package
library(tidyverse) #loading tidyverse package
library(gapminder) #loading gapminder data package

gap <- gapminder

#COMMENT ALL CODE BEFORE YOU SUBMIT!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ADD REFERENCE TO GAPMINDER DATA!!!!!!!!!

ui <- fluidPage( #Our user interface
  img(src= "images.jpeg"), #A beautiful relevant image to make app more visually appealing
  titlePanel("Gapminder Dataset"), #The title of our app
  h3("Exploring the Gapminder dataset"), #Brief description of our app
  sidebarLayout( #Defining sidebar
    sidebarPanel( #What goes in the sidebar
      uiOutput("COUNTRY") #Adds box to UI to allow users to easily select a country
    ),
    mainPanel( #Defining main panel of app
      tableOutput("results"), #Output for the table of data
      downloadButton("dl_data", "Download Data") #Adding a download button so users can download the filtered dataset
    )
  )
)

server <- function(input, output) { #Defining our server
  filtered <- reactive({ #Creating a reactive filter as its own variable in this function
    gap %>% #Start with gapminder dataset
      filter(country == input$COUNTRY) #Filters for whichever country users select from sidebar box
  })
  output$results <- renderTable({ #output for our table
    filtered() #using the filtered dataset
  })
  output$COUNTRY <- renderUI({ #Making input box dynamic
    selectInput("COUNTRY", "Country", #Creating the input box that goes in UI
                sort(unique(gap$country)), #Sort options in input box
                selected = "Canada") #Default option is Canada
  })
  output$dl_data <- downloadHandler( #Defining output for our download box
    filename = function() { #filename function
      paste('data-', '.csv', sep = "-") #how file will be named
    },
    content = function(file) { #function for writing the file data
      write_csv(filtered(), file) #call to write the data
    }
  )
}

shinyApp(ui = ui, server = server) #runs our application :)

