library(shiny) #loading shiny package
library(tidyverse) #loading tidyverse package
library(gapminder) #loading gapminder data package
library(bslib) #loading bslib package for theme

gap <- gapminder #set our dataframe as variable gap
thematic::thematic_shiny() #this function renders our theme to also affect plots

ui <- navbarPage("Application", #Our user interface, navbarPage let's us create diff pages for the app
                 #the function below this lets us set some theme options!
  theme = bs_theme(bg = "#0b3d91", fg = "white", primary = "#FCC780",
                   base_font = font_google("Impact"),
                   code_font = font_google("Space Mono")),
  tabPanel("About", #create a page talking about the app with use instructions
           sidebarLayout( #Defining sidebar
             sidebarPanel( #What goes in the sidebar
               titlePanel("Gapminder Dataset"), #The title 
               h3("Explore the World!"), #Brief description
               img(src= "images.jpeg") #inserts image of world
           ),
           mainPanel( #main panel of the about page
             HTML("Welcome!
             <br>
             <br>
             This application will help you learn about the World by exploring
             the",
                ),
             tags$a(href="https://gapminder.org/data/", "Gapminder dataset", target="_blank"),
             HTML(". <br><br> On the <strong>Table</strong> page, you will be able to select the country
                  of your choosing and explore some of the interesting facts about that country, such as its
                  continent as well as life expectancy, GDP per capita and population by year. There is also
                  a feature to download the data as a csv file if you'd like!<br><br>
                  On the <b>Plot</b> page, you will be able to select the country as well as
                  a criterion and predictor variable and plot the results visually!")
           ))),
  tabPanel("Table", #creates new page for the table
           sidebarLayout( #Defining sidebar
             sidebarPanel( #What goes in the sidebar
               titlePanel("Gapminder Dataset"), #The title of our app
               h3("Explore the World!"), #Brief description of our app
               img(src= "images.jpeg"), #inserts the image of world
               uiOutput("COUNTRY") #refers to selectInput in server to select country to view
               ),
             mainPanel( #main panel of Table page
               #whichever country is selected that country will be output as part of the text!
               h4(textOutput("my_text"), "is a lovely country to explore!"),
               tableOutput("results"), #displays the table of selected country
               downloadButton("dl_data", "Download Data") #enables users to download selected data as csv file
             )
             )
          ),
  
  tabPanel("Plot", #creates a third page that shows plot
           sidebarLayout( #Defining sidebar
             sidebarPanel( #What goes in the sidebar
               titlePanel("Gapminder Dataset"), #The title 
               h3("Explore the World!"), #Brief description 
               img(src= "images.jpeg"), #insert image of world
               uiOutput("COUNTRY_2"), #refers to selectInput in server to select country to view
               radioButtons("predictor", "Select predictor variable",
                            choices = c("lifeExp", "pop", "gdpPercap", "year"),
                            selected = "gdpPercap"), #lets users select predictor variable for plot, with default option being gdpPercap
               radioButtons("criterion", "Select criterion variable",
                            choices = c("lifeExp", "pop", "gdpPercap"),
                            selected = "lifeExp") #lets users select criterion variable for plot, with default option being lifeExp
               ),
             mainPanel( #main panel of plot page
             h5("Select the country and desired variables and plot the results!"),
             plotOutput("plot_1") #displays the plot
             )
    )
  )
)


server <- function(input, output) { #Defining our server
  filtered <- reactive({ #Creating a reactive filter as its own variable in this function
    gap %>% #Start with gapminder dataset
      filter(country == input$COUNTRY) #Filters for whichever country users select from sidebar box
  })
  #below is what creates the text to display selected country
  output$my_text <- renderText({
    input$COUNTRY
  })
  #below creates the plot displayed on plot page with data filtered for selected country and x and y being selected inputs via radiobuttons
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

