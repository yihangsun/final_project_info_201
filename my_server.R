library("dplyr")
library("ggplot2")
library("maps")
library("shiny")
library("tidyr")

county_map <- map_data("county")

wa_crime_report <- read.csv("Criminal_Justice_Data_Book.csv")
ny_crime_report <- read.csv("Index__Violent__Property__and_Firearm_Rates_By_County__Beginning_1990.csv")


wa_crime_report$county <- tolower(wa_crime_report$county)
filtered_wa <- select(wa_crime_report, year, county, SRS_TOTAL)
wa_county_map <- filter(county_map, region == "washington")

ny_crime_report$County <- tolower(ny_crime_report$County)
filtered_ny <- select(ny_crime_report, Year, County, Violent.Rate)
colnames(filtered_ny)[1] <- "year"

wa_county_map <- filter(county_map, region == "washington")
ny_county_map <- filter(county_map, region == "new york")

joined_wa <- left_join(filtered_wa, wa_county_map, by = c("county" = "subregion"))
joined_ny <- left_join(filtered_ny, ny_county_map, by = c("County" = "subregion"))

View(joined_wa_ny)
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

selected_data_wa <- wa_crime_report %>%
  filter(county == "spokane" | county == "king" |
           county == "walla walla" | county == "pierce" |
           county == "clark" | county == "benton" |
           county == "snohomish" | county == "cowlitz")

selected_data_ny <- ny_crime_report  %>%
  filter(County == "chautauqua" | County == "erie" |
           County == "niagara" | County == "new york" |
           County == "kings" | County == "queens" |
           County == "bronx" | County == "hamilton" |
           County == "orange")

coors_wa <- data.frame(
  cities = c("Seattle", "Spokane", "Walla walla", "Tacoma"),
  latc = c(47.608013, 47.658779, 46.055645, 47.258728),
  longc = c(-122.335167, -117.426048, -118.346359, -122.465973),
  stringsAsFactors = FALSE
)

coors_ny <- data.frame(
  cities = c("Buffalo", "New York", "Albany"),
  latc = c(42.880230, 40.730610, 42.652580),
  longc = c(-78.878738, -73.935242, -73.756233),
  stringsAsFactors = FALSE
)



my_server <- function(input, output) {
  
    output$vio_table <- renderDataTable({
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
    dataset <- input$dataset
    year <- as.numeric(input$year)
    if (dataset == "New York") {
      selected_data <- joined_ny
    } else if (dataset == "Washington" | dataset == "Both") {
      selected_data <- joined_wa
    }
    selected_data <- selected_data[selected_data$year > year & selected_data < (year + 10),]
    ggplot(data = selected_data) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = selected_data[,3])) +
      coord_quickmap() +
      scale_fill_gradient(limits = range(selected_data[3]),
                          low = "pink", high = "red")
    })
    
    output$text1 <- renderText({
      dataset <- input$dataset
      year <- input$year
      if (dataset == "New York") {
        selected_data <- "New York"
        msg <- paste0("The map above shows the distribution of crime rate in ",
                      selected_data, " in the ", year, "'s. We can see that
                      crime rates are almost the same in New york. Based on the
                      legend on the right, crime is diminshing over the decades.")
      } else if (dataset == "Washington") {
        selected_data <- "Washington"
        msg <- paste0("The map above shows the distribution of crime rate in ",
                      selected_data, " in the ", year, "'s. From the graph, we
                       can see that King's county is where the crime rate is
                      highest. The distribution and frequency of crime does not
                      change dramatically over the year.")
      } else if (dataset == "Both") {
        msg <- "You can check crime rate data for each state from the drop down
        menu on the left. In this page we are only showing you the stats for
        Washington state. Generally crime distribution is more spreaded out in
        New York, and it's more concentrated in Washington."
      }
      
    })
    
    
    
    output$demo_map <- renderPlot({
      dataset <- input$dataset
      if (dataset == "New York") {
        selected_data <- selected_data_ny %>%
          select(Year, County, Population) %>%
          filter(Year == 1990 | Year == 1999 |
                   Year == 2000 | Year == 2009 |
                   Year == 2010 | Year == 2016) 
        selected_data <- mutate(
          selected_data,
          percentage = 0
        )
        for(i in 54:10)
          selected_data$percentage[i] =  
          (selected_data$Population[i - 9] -  selected_data$Population[i]) / 
          selected_data$Population[i] * 100
        selected_data <- selected_data %>%
          filter(Year == input$year) 
        names(selected_data)[2] <- "subregion"
        
        selected_data <- left_join(selected_data, ny_county_map, by = "subregion")
        county_map <- ny_county_map
        coors <- coors_ny
      }
       
      
        
        if (dataset == "Washington") {
        selected_data <- selected_data_wa %>%
          select(year, county, POP_TOTAL) %>%
         filter(year == 1990 | year == 1999 |
                year == 2000 | year == 2009 |
                year == 2010 | year == 2016)
        
        selected_data <- mutate(
          selected_data,
          percentage = 0
        )
         for(i in 1:47)
          selected_data$percentage[i] =  
  (selected_data$POP_TOTAL[i + 1] -  selected_data$POP_TOTAL[i]) / 
                                  selected_data$POP_TOTAL[i] * 100
        
        selected_data <- selected_data %>%
          filter(year == input$year) 
        names(selected_data)[2] <- "subregion"
  selected_data <- left_join(selected_data, wa_county_map, by = "subregion")
  county_map <- wa_county_map
  coors <- coors_wa
      }
      
      
      ggplot(selected_data, aes(x = long, y = lat, group = group, fill = percentage)) +
        geom_polygon() +
        scale_fill_gradient(limits = range(selected_data$percentage), 
                            low = "pink", high = "red") +
        ggtitle("Here are the population rate change in the given time period:") +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        geom_polygon(data = county_map, aes(x=long, y = lat, group = group), fill = NA, color = "grey") +
        coord_fixed(1.3)  +
        geom_point(data = coors, aes(x=longc, y=latc,group = NULL, fill = NULL), color="black", size=2) +
        geom_text(data = coors, aes(x = longc +0.35, y = latc+0.15, label = cities, group = NULL, fill = NULL), 
                  size = 3.9, col = "black", fontface = "italic") 
      
      
    })
    
    output$demo_map1 <- renderPlot({
      dataset <- input$dataset
      if (dataset == "New York") {
        selected_data <- selected_data_ny %>%
          select(Year, County, Index.Count) %>%
          filter(Year == 1990 | Year == 1999 |
                   Year == 2000 | Year == 2009 |
                   Year == 2010 | Year == 2016) 
        selected_data <- mutate(
          selected_data,
          percentage = 0
        )
        for(i in 54:10)
          selected_data$percentage[i] =  
          (selected_data$Index.Count[i - 9] -  selected_data$Index.Count[i]) / 
          selected_data$Index.Count[i] * 100
        selected_data <- selected_data %>%
          filter(Year == input$year) 
        names(selected_data)[2] <- "subregion"
        selected_data <- left_join(selected_data, ny_county_map, by = "subregion")
        county_map <- ny_county_map
        coors <- coors_ny
      }
      
      
      
      if (dataset == "Washington") {
        selected_data <- selected_data_wa %>%
          select(year, county, SRS_TOTAL) %>%
          filter(year == 1990 | year == 1999 |
                   year == 2000 | year == 2009 |
                   year == 2010 | year == 2016)
        
        selected_data <- mutate(
          selected_data,
          percentage = 0
        )
        for(i in 1:47)
          selected_data$percentage[i] =  
          (selected_data$SRS_TOTAL[i + 1] -  selected_data$SRS_TOTAL[i]) / 
          selected_data$SRS_TOTAL[i] * 100
        
        selected_data <- selected_data %>%
          filter(year == input$year) 
        names(selected_data)[2] <- "subregion"
        selected_data <- left_join(selected_data, wa_county_map, by = "subregion")
        county_map <- wa_county_map
        coors <- coors_wa
      }
      
    
      ggplot(selected_data, aes(x = long, y = lat, group = group, fill = percentage)) +
        geom_polygon() +
        scale_fill_gradient(limits = range(selected_data$percentage), 
                            low = "pink", high = "red") +
        ggtitle("Here are the criminal rate change in the given time period:") +
        theme(axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.ticks.x = element_blank()) +
        theme(axis.title.y = element_blank(),
              axis.text.y = element_blank(),
              axis.ticks.y = element_blank()) +
        geom_polygon(data = county_map, aes(x=long, y = lat, group = group), fill = NA, color = "grey") +
        coord_fixed(1.3) +
        geom_point(data = coors, aes(x=longc, y=latc,group = NULL, fill = NULL), color="black", size=2) +
        geom_text(data = coors, aes(x = longc +0.35, y = latc+0.15, label = cities, group = NULL, fill = NULL), 
                  size = 3.9, col = "black", fontface = "italic") 
    
      
      
    })
    
    output$demo_cor <- renderPlot({
      if(input$year == 1990){
        start <- 1990
        end <- 1999
      }
      if(input$year == 2000){
        start <- 2000
        end <- 2009
      }
      if(input$year == 2010){
        start <- 2010
        end <- 2016
      }
      dataset <- input$dataset
      if (dataset == "Washington") {
        selected_data <- selected_data_wa %>%
          select(year, county, POP_TOTAL, SRS_TOTAL) %>%
          filter(year <= end & year >= start)
        names(selected_data)[4] <- "Crime_Count"
        names(selected_data)[3] <- "Population"
        ggplot(selected_data, aes(x=Population, y=Crime_Count, color = county, na.rm = TRUE)) + 
          geom_smooth(method = 'lm', se=FALSE) +
          geom_point() 
      }
      else if (dataset == "New York") {
        selected_data <- selected_data_ny  %>%
          select(Year, County, Population, Index.Count) %>%
          filter(Year <= end & Year >= start)
        names(selected_data)[4] <- "Crime_Count"
        ggplot(selected_data, aes(x=Population, y=Crime_Count, color = County, na.rm = TRUE)) + 
          geom_smooth(method = 'lm', se=FALSE) +
          geom_point() 
      }
      
    })
    
    
    output$demo_cor1 <- renderPlot({
      dataset <- input$dataset
      if (dataset == "Washington") {
        selected_data <- selected_data_wa %>%
          select(year, county, POP_TOTAL, SRS_TOTAL) %>%
          filter(county == "king")
        names(selected_data)[4] <- "Crime_Count"
        names(selected_data)[3] <- "Population"
        ggplot(selected_data, aes(x=Population, y=Crime_Count, na.rm = TRUE)) + 
          geom_smooth(method = 'lm', se=FALSE) +
          geom_point() 
      }
      else if (dataset == "New York") {
        selected_data <- selected_data_ny  %>%
          select(Year, County, Population, Index.Count) %>%
          filter(County == "new york")
        names(selected_data)[4] <- "Crime_Count"
        ggplot(selected_data, aes(x=Population, y=Crime_Count, na.rm = TRUE)) + 
          geom_smooth(method = 'lm', se=FALSE) +
          geom_point() 
      }
      
    })
      
    
    output$demo_cor2 <- renderPlot({
      dataset <- input$dataset
      if (dataset == "Washington") {
        selected_data <- selected_data_wa %>%
          select(year, county, POP_TOTAL, SRS_TOTAL) %>%
          filter(county == "benton")
        names(selected_data)[4] <- "Crime_Count"
        names(selected_data)[3] <- "Population"
        ggplot(selected_data, aes(x=Population, y=Crime_Count, na.rm = TRUE)) + 
          geom_point() 
      }
      else if (dataset == "New York") {
        selected_data <- selected_data_ny  %>%
          select(Year, County, Population, Index.Count) %>%
          filter(County == "hamilton")
        names(selected_data)[4] <- "Crime_Count"
        ggplot(selected_data, aes(x=Population, y=Crime_Count, na.rm = TRUE)) + 
          geom_point() 
      }
      
    })
    
    
    output$intro_main <- renderText({
    "In this section, we will study how the population reidstribution
     will relate to the local situation of criminal acts in east coast and west 
    coast for the last three decades. We will use New York state and Washington
    state as representives of these two coast areas. Something happened here,
    it might also happened in other places."
    })
    
    output$intro_spe <- renderText({
    "According to Claude Fischer's subcultural theory, the movements of human beings
    encourage social integration. People with same interests or backgrounds will be
    brought together which means this kind of demographic change could help
    creating the social network for criminals. However, crime rates change and 
    pooulation change will not be associated with a linear relationship since criminal
    behavior is a dynamic process including different factors. More socio-economic
    causes should be considered. We will start our study by looking at the situation
    of the areas with dramatic population changes in these two states."
    })
    
    output$population <- renderText({
      if(input$dataset == "Washington" & as.numeric(input$year) == 1990)
       text <- "In 1990s, the population of Washington state kept increased just 
            like in 1980s. Large numbers of immigrants moved in at this time 
            from other states. Not only the local residents but also these 
            immigrants started to move to urbanized areas and adjacent densely 
            settled areas."
      else
      if(input$dataset == "Washington" & as.numeric(input$year) == 2000)
      text <- "In 2000s, by enjoying the economic prosperity, Washington continued
      its population change. But this time, secondary cities such as Spokane
      have bigger population growth rates."
      else
      if(input$dataset == "Washington" & as.numeric(input$year) == 2010)
       text <- "In 2010s, The growth of populaiton started to decrease and 
      Washington is enjoying a healthy population growth rate which ranks 
      top in the country. Most immigrants at this time choose to move to
      the urbanized areas."
    })
    
    
}


