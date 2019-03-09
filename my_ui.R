wa_crime_report <- read.csv("Criminal_Justice_Data_Book.csv")
ny_crime_report <- read.csv("Index__Violent__Property__and_Firearm_Rates_By_County__Beginning_1990.csv")

my_ui <- fluidPage( 
  titlePanel("Eastern and Western Crime Rate"),
  h1("The crime rate of representive state in Eastern and Western States of 
     U.S.A in past three decades"),
  p("We are focusing on researching Washington State and New York State which  
can represent most for East and West in United States. There are a lot of 
important changes happened in past threee decades. We will explore those 
changes in history along with criminal rates."),
  wa_crime_df <- wa_crime_report %>%
    filter(year ==c(1990:2016))
    
)
