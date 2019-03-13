library("dplyr")
library("ggplot2")
library("maps")
library("shiny")

wa_crime_report <- read.csv("Criminal_Justice_Data_Book.csv")
ny_crime_report <- read.csv("Index__Violent__Property__and_Firearm_Rates_By_County__Beginning_1990.csv")

my_ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("@import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    p{
                    color: #3399CC;
                    }
                    h3{
                    color: #33CCFF;
                    }
                    "))
    ),
  
  headerPanel(strong("Eastern and Western Criminal Behavior Comparison")),
  br(),
  h5(em("The crime rate of representative state in Eastern and Western States of 
     U.S.A in past three decades")),
  br(),
  p("We are focusing on researching", strong("Washington State"), 
"and", strong("New York State"), "which  
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
                         h3("How does violent crime relate to changes of history
                            within 1990 to 2016?"),
                         textOutput("vio_text"),
                         br(),
                         dataTableOutput("vio_table")),
                
                tabPanel("Property Crime", 
                         h3("How does property crime relate to changes of history
                            within 1990 to 2016?"),
                         textOutput("pro_text"),
                         br(),
                         dataTableOutput("pro_table")),

                tabPanel("Rate by County", 
                         h3("How are crime incidents distributed in the state?"),
                         textOutput("text1"),
                         br(),
                         plotOutput("plot"), 
                         plotOutput("plot2")),

                tabPanel("Demographic", 
                         h3("Population Movement -- Crime Acts"),
                         textOutput("intro_main"), 
                         br(),
                         textOutput("intro_spe"),
                         hr(),
                         h3("The Big Picture of What Happened"),
                         htmlOutput("conclusion1"),
                         htmlOutput("slogan1"),
                         htmlOutput("conclusion2"),
                         htmlOutput("slogan2"),
                         imageOutput("demo_map"), 
                         textOutput("population"),
                         br(),
                         imageOutput("demo_map1"),
                         textOutput("crime"),
                         hr(),
                         htmlOutput("subtitle"),
                         plotOutput("demo_cor"), 
                         plotOutput("demo_cor1"), 
                         plotOutput("demo_cor2"),
                         htmlOutput("number1")
                         )
              )
          )
      ), 
  helpText(
    h2("References"),
    tagList(
      tags$a(href = "https://newyork.cbslocal.com/top-lists/biggest-new-york-events-of-the-2000s/",
             "New York in 2000's"), 
      br(),
      tags$a(href = "https://ephemeralnewyork.wordpress.com/tag/new-york-in-1900/",
             "New York in 1990's"),
      br(),
      tags$a(href = "https://newyork.cbslocal.com/top-lists/biggest-new-york-events-of-the-2010s/",
             "New York in 2010's"),
      br(),
      tags$a(href = "http://www.ereferencedesk.com/resources/state-history-timeline/washington.html",
             "Washington History"),
      br(),
      tags$a(href = "https://www.villagevoice.com/2014/08/07/the-rise-and-fall-of-crime-in-new-york-city-a-timeline/",
             "TimeLine of New York State"),
      br(),
      tags$a(href = "http://www.timelines.ws/states/WASHINGTON.HTML", 
             "TimeLine of Washington State"),
      br(),
      tags$a(href = "https://www.neighborhoodscout.com/wa/crime",
             "WA crime rate by county"),
      br(),
      tags$a(href = "https://www.areavibes.com/new+york-ny/crime/",
            "NY crime rate by county"),
      br(),
      tags$a(href = "https://www-journals-uchicago-edu.offcampus.lib.washington.edu/doi/10.1086/230753",
             "Claude Fischer's Subcultural Theory")
    )
  )
)


