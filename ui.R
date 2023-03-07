#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(shiny)
library(plotly)
library(readr)

car_data <- read_delim("car_data.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Used and New Car Data"),
  tabsetPanel(
    tabPanel("Welcome",
             h2("Welcome to the Used and New Car Dataset"),
             h4("In this dataset, there are over 100,000 cars for you to look at to figure out
                which car fits your needs perfectly!"),
             p("This web app shows information regarding", em("various car data from", min(car_data$Year), "to", max(car_data$Year))),
             p("There are", strong(format(nrow(car_data), big.mark = ","), "cars and", ncol(car_data), "variables") , "we will be looking at in this dataset."),
             p("This data consists of several variables necessary for buyers to look at before buying cars, such as the",
               em("Model, Year, Status, Mileage, Price, and MSRP.")),
             p("This data is found on", strong("Kaggle"), "and was updated by", strong("George Baffour.")),
             h5("Below is a", em("random sample of 10 car data values:")),
             dataTableOutput("welcome_table"),
             ),
    
    tabPanel("Plot",
             sidebarLayout(
               sidebarPanel(
                 textOutput("plot_summary"),
                 selectInput("model",
                             "Select Car Model:",
                             choices = unique(car_data$Model)),
                 radioButtons("color", "Pick a color",
                             choices = c("red", "orange", "green",
                                              "blue","purple"))
                 ),
               mainPanel(
                 plotOutput("plot")
                 )
               )
             ),
    tabPanel("Table",
             sidebarLayout(
               sidebarPanel(
                 textOutput("table_summary"),
                 sliderInput("year", "Select your earliest year preference.",
                             min = 1949,
                             max = 2023,
                             value = 2000,
                             sep = ""),
                             ),
               mainPanel(
                 dataTableOutput("table")
                 )
               )
             )
    )
)

server <- function(input, output){
  
  output$welcome_table <- renderDataTable({
    car_data %>% 
      sample_n(10)
  })
  
  output$plot <- renderPlot({
    model_data <- car_data %>%
      filter(!grepl('Not available', Mileage)) %>%
      filter(Model == input$model) %>% 
      mutate(Mileage = as.numeric(str_replace_all(Mileage, "[^[:digit:]]", "")),
             Price = as.numeric(str_replace_all(Price, "[^[:digit:]]", ""))) %>%
      na.omit() %>% 
      ggplot(aes(x = as.integer(gsub(" mi.", "", Mileage)), y = as.integer(gsub("$", "", Price)))) +
      geom_point(col = input$color) +
      labs(x = "Mileage of Car (in miles)", y = "Price of Car (in $)", title = "Car Price vs Mileage")
    model_data
  })
  
  output$plot_summary <- renderText({
    model_data <- car_data %>%
      filter(!grepl('Not available', Mileage)) %>% 
      filter(Model == input$model)
    n_total <- nrow(model_data)
    paste0("There are ", n_total, " " , input$model, "s that have available data.")
  })
  
  output$table <- renderDataTable({
    car_data %>% 
      filter(Year >= input$year) %>% 
      arrange(Year)
  })
  
  output$table_summary <- renderText({
    year_data <- car_data %>% 
      filter(Year >= input$year)
    n_format <- format(nrow(year_data), big.mark = ",")
    paste0("There are ", n_format, " cars that were manufactured later than ", input$year, ".")
  })
}
shinyApp(ui = ui, server = server, options = list(height = 1080))