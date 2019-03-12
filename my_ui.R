library("dplyr")
library("ggplot2")
library("maps")
library("shiny")

wa_crime_report <- read.csv("Criminal_Justice_Data_Book.csv")
ny_crime_report <- read.csv("Index__Violent__Property__and_Firearm_Rates_By_County__Beginning_1990.csv")

my_ui <- fluidPage( 
  titlePanel("Eastern and Western Criminal Behavior Comparsion"),
  h1("The crime rate of representative state in Eastern and Western States of 
     U.S.A in past three decades"),
  p("We are focusing on researching Washington State and New York State which  
can represent most for East and West in United States. There are a lot of 
important changes happened in past three decades. We will explore those 
changes in history along with criminal rates."),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(inputId = "dataset", 
                  label = "Choose the area(s) you want to search:",
                  choices = c("New York", "Washington", "Both")),
      
      radioButtons(inputId = "year", 
                   label = "choose the time period you want to search:",
                   choices = list("1990's" = 1990, 
                                  "2000's" = 2000, 
                                  "2010's" = 2010), 
                   selected = 1990)
      
                ),
    
    mainPanel(

      tabsetPanel(type = "tabs",
                
                tabPanel("Violent Crime", 
                         h2("How does violent crime relate to changes of history
                            within 1990 to 2016"),
                         textOutput("vio_text"),
                         br(),
                         dataTableOutput("vio_table")),
                
                tabPanel("Property Crime", 
                         h4("How does property crime relate to changes of history
                            within 1990 to 2016"),
                         textOutput("pro_text"),
                         br(),
                         dataTableOutput("pro_table")),

                tabPanel("Rate by County", 
                         h3("How are crime incidents distributed in the state"),
                         textOutput("text1"),
                         plotOutput("plot"), 
                         plotOutput("plot2")),

                tabPanel("Demographic", 
                         h2("Population Movement -- Crime Acts"),
                         textOutput("intro_main"), 
                         textOutput("intro_spe"),
                         hr(),
                         h2("The Big Picture of What Happened"),
                         imageOutput("demo_map"), 
                         textOutput("population"),
                         br(),
                         imageOutput("demo_map1"),
                         textOutput("crime"),
                         hr(),
                         h2("More Specific Explorations"),
                         plotOutput("demo_cor"), 
                         plotOutput("demo_cor1"), 
                         plotOutput("demo_cor2"))
              )
            )
          )
        )


