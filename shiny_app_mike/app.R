library(shiny) #loading shiny package
library(tidyverse) #loading tidyverse package
library(gapminder) #loading gapminder data package
library(bslib)

gap <- gapminder
thematic::thematic_shiny()

ui <- navbarPage("Application", #Our user interface
  theme = bs_theme(bg = "#0b3d91", fg = "white", primary = "#FCC780",
                   base_font = font_google("Impact"),
                   code_font = font_google("Space Mono")),
  tabPanel("Table",
           sidebarLayout( #Defining sidebar
             sidebarPanel( #What goes in the sidebar
               titlePanel("Gapminder Dataset"), #The title of our app
               h3("Explore the World!"), #Brief description of our app
               img(src= "images.jpeg"),
               uiOutput("COUNTRY"),
               ),
             mainPanel(
               h4(textOutput("my_text"), "is a lovely country to explore!", inline=FALSE),
               tableOutput("results"),
               downloadButton("dl_data", "Download Data")
             )
             )
          ),
  
  tabPanel("Plot",
           sidebarLayout( #Defining sidebar
             sidebarPanel( #What goes in the sidebar
               titlePanel("Gapminder Dataset"), #The title of our app
               h3("Explore the World!"), #Brief description of our app
               img(src= "images.jpeg"),
               uiOutput("COUNTRY_2"),
               radioButtons("predictor", "Select predictor variable",
                            choices = c("lifeExp", "pop", "gdpPercap", "year"),
                            selected = "gdpPercap"),
               radioButtons("criterion", "Select criterion variable",
                            choices = c("lifeExp", "pop", "gdpPercap"),
                            selected = "lifeExp")
               ),
             mainPanel(
             h3("can i say something?"),
             plotOutput("plot_1")
             )
    )
  )
)


server <- function(input, output) { #Defining our server
  filtered <- reactive({ #Creating a reactive filter as its own variable in this function
    gap %>% #Start with gapminder dataset
      filter(country == input$COUNTRY) #Filters for whichever country users select from sidebar box
  })
  output$my_text <- renderText({
    input$COUNTRY
  })
  output$plot_1 <- renderPlot({
    ggplot(filtered(), aes_string(x = input$predictor, y = input$criterion))+
      geom_point()
  })
  output$results <- renderTable({ #output for our table
    filtered() #using the filtered dataset
  })
  output$COUNTRY <- renderUI({ #Making input box dynamic
    selectInput("COUNTRY", "Country", #Creating the input box that goes in UI
                sort(unique(gap$country)), #Sort options in input box
                selected = "Canada") #Default option is Canada
  })
  output$COUNTRY_2 <- renderUI({ #Making input box dynamic
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

