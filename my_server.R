library("dplyr")
library("ggplot2")
library("maps")

county_map <- map_data("county")

wa_crime_report <- read.csv("Criminal_Justice_Data_Book.csv")
ny_crime_report <- read.csv("Index__Violent__Property__and_Firearm_Rates_By_County__Beginning_1990.csv")


wa_crime_report$county <- tolower(wa_crime_report$county)
filtered_wa <- select(wa_crime_report, year, county, SRS_TOTAL)
wa_county_map <- filter(county_map, region == "washington")

ny_crime_report$County <- tolower(ny_crime_report$County)
filtered_ny <- select(ny_crime_report, Year, County, SRS_TOTAL)
wa_county_map <- filter(county_map, region == "washington")
joined_wa <- left_join(filtered_wa, wa_county_map, by = c("county" = "subregion"))

wa_crime_df <- wa_crime_report %>%
  filter(year == c(1990:2016)) %>%
  select(year, SRS_AG_ASSLT, SRS_ARSON, SRS_BURGLARY, SRS_MURDER, SRS_MVT, 
         SRS_RAPE, SRS_ROBBERY, SRS_THEFT, SRS_TOTAL) %>%
  group_by(year) %>%
  summarize(wa_ave_violent = mean((SRS_MURDER + SRS_BURGLARY + SRS_AG_ASSLT + 
                                     SRS_RAPE) / SRS_TOTAL * 100),
            wa_ave_proporty = mean((SRS_ROBBERY + SRS_THEFT + SRS_MVT + 
                                      SRS_ARSON) / SRS_TOTAL * 100))
ny_crime_df <- ny_crime_report %>%
  filter(Year == c(1990:2016)) %>%
  select(Year, Violent.Rate, Property.Rate) %>%
  group_by(Year) %>%
  summarize(ny_ave_violent = mean(Violent.Rate / 10),
            ny_ave_property = mean(Property.Rate / 100))
wa_ny_crime_df <- left_join(ny_crime_df, wa_crime_df, by = c("Year" = "year"))
View(wa_ny_crime_df)

my_server <- function(input, output) {
  
    ouput$vio_table <- renderDataTable({
      filtered_table <- wa_ny_crime_df
      if(input$dataset == "New York") {
        filtered_vio_ny <- filtered_table %>% 
          select(Year, ny_ave_violent)
      } else if(input$dataset == "Washington") {
        filter_vio_wa <- filtered_table %>%
          select(Year, wa_ave_violent)
      } else if(input$dataset == "Both") {
        filter_vio_both <- filtered_table %>%
          select(Year, wa_ave_violent, ny_ave_violent)
      }
      if(input$year == "2010's") {
        filtered_table <- filtered_table %>%
          filter(Year == c(input$year : 2016))
      } else{
        filtered_table <- filtered_table %>%
          filter(Year == c(input$year : (input$year + 9)))
      }
      filtered_table
    })
    
    output$pro_table <- renderDataTable({
      filtered_table <- wa_ny_crime_df
      if(input$dataset == "New York") {
        filtered_pro_ny <- filtered_table %>% 
          select(Year, ny_ave_proporty)
      } else if(input$dataset == "Washington") {
        filter_pro_wa <- filtered_table %>%
          select(Year, wa_ave_proporty)
      } else if(input$dataset == "Both") {
        filter_pro_both <- filtered_table %>%
          select(Year, wa_ave_proporty, ny_ave_proporty)
      }
      if(input$year == "2010's") {
        filtered_table <- filtered_table %>%
          filter(Year == c(input$year : 2016))
      } else{
        filtered_table <- filtered_table %>%
          filter(Year == c(input$year : (input$year + 9)))
      }
      filtered_table
    })
    
    output$plot <- renderPlot({
    ggplot(data = joined_wa) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = SRS_TOTAL)) +
      coord_quickmap() +
      scale_fill_gradient(limits = range(joined_wa$SRS_TOTAL),
                          low = "pink", high = "red")
    })
}


