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


my_server <- function(input, output) {
  
  output$plot <- renderPlot({
    ggplot(data = joined_wa) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = SRS_TOTAL)) +
      coord_quickmap() +
      scale_fill_gradient(limits = range(joined_wa$SRS_TOTAL),
                          low = "pink", high = "red")
    })
}


