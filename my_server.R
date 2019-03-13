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

wa_crime_df <- wa_crime_report %>%
  filter(year == c(1990:2016)) %>%
  select(year, SRS_AG_ASSLT, SRS_ARSON, SRS_BURGLARY, SRS_MURDER, SRS_MVT, 
         SRS_RAPE, SRS_ROBBERY, SRS_THEFT, SRS_TOTAL) %>%
  group_by(year) %>%
  summarize(mean_violent_Washington = mean((SRS_MURDER + SRS_BURGLARY + SRS_AG_ASSLT + 
                                     SRS_RAPE) / SRS_TOTAL * 100),
            mean_property_Washington = mean((SRS_ROBBERY + SRS_THEFT + SRS_MVT + 
                                      SRS_ARSON) / SRS_TOTAL * 100))
ny_crime_df <- ny_crime_report %>%
  filter(Year == c(1990:2016)) %>%
  select(Year, Violent.Rate, Property.Rate) %>%
  group_by(Year) %>%
  summarize(mean_violent_New_York = mean(Violent.Rate / 10),
            mean_property_New_York = mean(Property.Rate / 100))
wa_ny_crime_df <- left_join(ny_crime_df, wa_crime_df, by = c("Year" = "year"))

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

pop_wa <- selected_data_wa %>%
  filter(year == 2016)
pop_wa_2016 <- sum(pop_wa$POP_TOTAL)
pop_waa <- selected_data_wa %>%
  filter(year == 1990)
pop_wa_1990 <- sum(pop_waa$POP_TOTAL)
pop_wa_change <- pop_wa_2016 / pop_wa_1990 * 100 - 100


pop_ny_r <- selected_data_ny %>%
  filter(Year == 2016) %>%
  filter(County == "chautauqua" | County == "erie" |
           County == "niagara" )
pop_ny_2016_r <- sum(pop_ny_r$Population)
pop_nyy_r <- selected_data_ny %>%
  filter(Year == 1990) %>%
  filter(County == "chautauqua" | County == "erie" |
           County == "niagara" )
pop_ny_1990_r <- sum(pop_nyy_r$Population)
pop_ny_change_r <- pop_ny_2016_r / pop_ny_1990_r * 100 - 100

pop_ny_u <- selected_data_ny %>%
  filter(Year == 2016) %>%
  filter(County == "new york" |
           County == "kings" | County == "queens" |
           County == "bronx")
pop_ny_2016_u <- sum(pop_ny_u$Population)
pop_nyy_u <- selected_data_ny %>%
  filter(Year == 1990) %>%
  filter(County == "new york" |
           County == "kings" | County == "queens" |
           County == "bronx")
pop_ny_1990_u <- sum(pop_nyy_u$Population)
pop_ny_change_u <- pop_ny_2016_u / pop_ny_1990_u * 100 - 100


data_wa <- selected_data_wa %>%
  select(year, county, POP_TOTAL, SRS_TOTAL) %>%
  filter(county == "king")
cor_wa <- cor(data_wa$POP_TOTAL, data_wa$SRS_TOTAL)

data_ny <- selected_data_ny  %>%
  select(Year, County, Population, Index.Count)%>%
  filter(County == "new york")
cor_ny <- cor(data_ny$Population, data_ny$Index.Count)

data_wa_c <- selected_data_wa %>%
  select(year, county, POP_TOTAL, SRS_TOTAL) %>%
  filter(county == "benton") %>%
  filter(year <= 2011)
cor_wa_c <- cor(data_wa_c$POP_TOTAL, data_wa_c$SRS_TOTAL)

data_ny_c <- selected_data_ny  %>%
  select(Year, County, Population, Index.Count)%>%
  filter(County == "hamilton")
cor_ny_c <- cor(data_ny_c$Population, data_ny_c$Index.Count)


my_server <- function(input, output) {
  
    output$vio_table <- renderDataTable({
      filtered_table <- wa_ny_crime_df
      if(input$dataset == "New York") {
        filtered_vio_ny <- filtered_table %>% 
          select(Year, mean_violent_New_York)
      } else if(input$dataset == "Washington") {
        filter_vio_wa <- filtered_table %>%
          select(Year, mean_violent_Washington)
      } else if(input$dataset == "Both") {
        filter_vio_both <- filtered_table %>%
          select(Year, mean_violent_Washington, mean_violent_New_York)
      }
      if(input$year == "2010's") {
        filtered_table <- filtered_table %>%
          filter(Year == c(input$year : 2016))
      } else{
        filtered_table <- filtered_table %>%
          filter(Year == c(as.numeric(input$year) : (as.numeric(input$year) + 9)))
      }
      filtered_table
    })
    
    output$vio_text <- renderText({
      text_front <- paste("The data table is showing the criminal rate about", 
                          input$dataset, "state(s) in United States within", 
                          input$year.)
      if(input$dataset == "New York") {
        if(input$year == 1990) {
          text_front <- paste(text_front, "In 1990's decade, United States were 
                              still developing and focusing on improving security
                              and technology. Therefore, this period of time 
                              raised low-weaponed terrorism and crimes related 
                              to drugs. However, more people were focusing on
                              building houses and fighting for their careers.
                              As report said, from 1950 to 2000, 800,000 homes 
                              and apartments were created—while the number of 
                              residents only increased by just fewer than 
                              120,000 people.")
        } else if(input$year == 2000) {
          text_front <- paste(text_front, "During 2000's, New York became the most 
                              popular city to find jobs. As population
                              increased, the security increased as well. Police 
                              stations started to dearmed terrorisms and added 
                              more safety ensures in New York States. The peace
                              didn't last long, until year of 2007, the Great 
                              Recession started and lasted for 2 years. This big 
                              movement of economics changed thousands of American 
                              people's lives. Between 2007 and 2009, there were a
                              large increase of amounts of homeless people and big
                              decrease of GDP of Unites States. Especially, there 
                              was a big increase of criminal rates in violent crimes.
                              Therefore, from The Great Recession, we can conclude
                              that economy directly affects violent crime rate.")
        } else if(input$year == 2010) {
          text_front <- paste(text_front, "Year 2010 was a really sad year. There
                              was a lot of terrorism going on in New York. In 2010, 
                              the Times Square had a car bombing which terrified 
                              a lot of people and stimulated the increase of violent
                              criminal rates to the maximum point in past eight years.
                              One year after, the violent crimes was slightly decreased,
                              but people in New York were still very aggressive because 
                              of the Weinergate Sexting Scandal Erupts event. New
                              Yorkers were mad at American congress. In year 2012, 
                              the criminal rate decreased because of a big hurricane
                              in New York. People was trying to survive and decrease
                              money loss instead of to do evils. Later on, New York 
                              put more attention on economy, so the criminal rate 
                              was flatten in a while.")
        } 
      } else if(input$dataset == "Washington") {
        if(input$year == 1990) {
          text_front <- paste(text_front, "Washington State in 1990's was relly peaceful.
                              It was the time period for Washington to develop farming
                              and increasement of crops. The economy was increased, so
                              the violent crime was decreased. And also, in 1990's, 
                              a lot of schoosls and museums opened. People were educated 
                              well. The criminal rate kept flat because Washington 
                              was in developing the whole time.")
        } else if(input$year == 2000) {
          text_front <- paste(text_front, "Washington State started to develop 
                              technology and economy accosiating with business.
                              However, the violent criminal rate slightly increased
                              because there were several severe killers which frightened
                              public. And also, in 2006, gay marriage was a hot topic
                              which caused a raise of criminal rates. However, one
                              year later, Boeing company revealed 787 jet to public
                              which attracted a lot of attentions and increased
                              economy as well. Therefore, the violent criminal rate in
                              Washington in 2000's was shaking up and down which 
                              both caused by social events and technology developments.")
        } else if(input$year == 2010) {
          text_front <- paste(text_front, "There were a few killing of terrorisms
                              happened in 2010's. And Washington was developing techonolgy
                              companies, like Boeing, Amazon, and so on. The violent criminal
                              rate was flat but still higher than 10 years ago.")
        } 
      } else {
        if(input$year == 1990) {
          text_front <- paste(text_front, "Washington State in 1990's was relly peaceful 
                              because they were working on crops and economy. However, 
                              in New York, there were a lot of terrorisms and gangsters
                              going on; people in New York lived in fear. Therefore,
                              comparing to Washington States, New York had a really
                              high average violent crime rate.")
        } else if(input$year == 2000) {
          text_front <- paste(text_front, "Washington State started to develop 
                              technologies, but had more terrorisms and activists 
                              on social events. Therefore the violent crime rate was
                              high. New York's crime rate decreased a lot bit but still 
                              higher than Washington State. New York became a big
                              city with great economy. But, the Great Recession 
                              happened to whole United States, during this period
                              of time, all States experienced high c=violent criminal
                              rates.")
        } else if(input$year == 2010) {
          text_front <- paste(text_front, "in 2010's, both of States had higher violent
                              crime rate than ten years ago. However, the whole Unite
                              States was in developing economy and technologies. 
                              More people were educated. So the crime rate was flat.")
        }
      }
   })
    
    
    output$pro_table <- renderDataTable({
      filtered_table <- wa_ny_crime_df
      if(input$dataset == "New York") {
        filtered_pro_ny <- filtered_table %>% 
          select(Year, mean_property_New_York)
      } else if(input$dataset == "Washington") {
        filter_pro_wa <- filtered_table %>%
          select(Year, wa_ave_proporty)
      } else if(input$dataset == "Both") {
        filter_pro_both <- filtered_table %>%
          select(Year, wa_ave_proporty, mean_property_New_York)
      }
      if(input$year == "2010's") {
        filtered_table <- filtered_table %>%
          filter(Year == c(input$year : 2016))
      } else{
        filtered_table <- filtered_table %>%
          filter(Year == c(as.numeric(input$year) : (as.numeric(input$year) + 9)))
      }
      filtered_table
    })
    
    output$pro_text <- renderText({
      text_explain <- paste("This table is showing the property crime rate of", 
                          input$dataset, "state(s) in United States within", 
                          input$year.)
      if(input$dataset == "New York") {
        if(input$year == 1990) {
          text_explain <- paste(text_explain, "during 1990's periods, several things happened iN
                            in New York. In 1990, although the property crime rate is not high yet
                            the Murder rate hits record-high. The most important events happens at
                            1994 as  Rudolph Giuliani was sworn in as the 107th Mayor of New York 
                            City. Among the things he do, he set out to reduce crime and reinvent 
                            the Times Square area as a family-friendly tourist destination, therefore,
                            the crime rate either property or violent rate reduce dramatically. As 
                            you can see in the table, in 1994, the crime rate decrease to the half 
                            of the rate in pervious years")
        } else if(input$year == 2000) {
          text_explain <- paste(text_explain, "At April 2000, U.S. unemployment rate 
                              drops to 3.8 percent, the lowest it has been since the 60s.
                              As the unemployment rate decrease, more people find jobs and 
                              can make money to live. Therefore, those  who need to survive 
                              by robbing or stealing stuff has largely decreased. Additionally,
                              durig 2002 to 2008, the stop-and-frisk in New York city increases
                              from 97,296 to 500,000 incidents, which means the police are more
                              strict on managing the city's crime. Therefore, during 2000's, the 
                              property crime rate is almost all below 30% in New York.")
        } else if(input$year == 2010) {
          text_explain <- paste(text_explain, "At March 2010, construction on the Barcays Center
                              begins in Downtown Brooklyn, marking a turning point in the 
                              development and gentrification sweeping across the borough. Therefore,
                              the property crime rate provides a decreasing trend after 2010. As we
                              can see in the table, after 2010, the property rate in New York city
                              reducs from 29% to 28% to 19% etc. One thing also worth to mention:
                              at November 6, 2012, for the first time in New York city??s recorded history, 
                              there are no reported shootings, stabbings, or murders on this day.")
        } 
        } else if(input$dataset == "Washington") {
          if(input$year == 1990) {
            text_explain <- paste(text_explain, "one thing need to first mention in the duration of
                                Washington state is that in 1990 census, Populations grow in urbanized 
                                areas and decline in rural areas; Washington state continues to draw 
                                population from outside its borders. Therefore, a increasing amount of 
                                people in Washington state, especially those foreigner, brings more 
                                uncertainties of criminal events. For example, the conflict between 
                                different people in different culture. Therefore, as we can see in the 
                                table, the property crime rate increases from around 70% to 74% in 1990's")
          } else if(input$year == 2000) {
            text_explain <- paste(text_explain, "during this periods in Washington State, its
                                technology starts to develop, however comes more strikes by the
                                workers. For example, In Mar 17, Boeing Co. agreed to settle a 
                                38-day strike by its engineers. It was the largest white-collar
                                walkout in US history. On Nov 21, Newspaper Guild members of the Seattle 
                                Times and Seattle Post-Intelligencer went on strike. All theses happened 
                                in 2000, so it foreshadows a high uncompatibility in the society, which can
                                be reflected in the table that since 2000, the property crime rate is highest 
                                and last for three years. Only at the end of the 2000's, we can see a slight 
                                decrease in the property crime rate.")
          } else if(input$year == 2010) {
            text_explain <- paste(text_explain, "The property crime rate changed a bit in 2010's and become better. 
                                This is Probably due to  Washington state was developing techonolgy
                                companies, like Boeing, Amazon, and so on. As we can see in the table,
                                between 2009 t0 2011, the average property crime rate is around 70%. Although
                                it stills a large number in Washington state, the situations will get better.")
          } 
          } else {
            if(input$year == 1990) {
              text_explain <- paste(text_explain, "Washington State's property crime rate in 1990's was
                                  larger than New York. This is probably due to Washington state not developing
                                  quickly as New York. Since there is still not very much policy in Washington
                                  than New York, it has more porperty crime rate.")
            } else if(input$year == 2000) {
              text_explain <- paste(text_explain, "as we all know that at the start of the 21 century, the eastesn 
                                  America was developed first. Therefore, since then, Washington state in the Western 
                                  of the United State still have less chance to devleop and grows slower than New York.
                                  Additionally, Since New York city is the center of legilation, it has more of the
                                  policies in regulating those crime rate than it than Washington state has.")
            } else if(input$year == 2010) {
              text_explain <- paste(text_explain, "in 2010's, As we can see that Washington state still has more 
                                  property crime rate than New York, this is probably due to the population in
                                  each state. As I mentioned before, Washington state is more open to the immigration,
                                  so the population increases in this state and therefore could bring more crimes 
                                  in it.")
            }
        }
    })
    
    
    output$plot <- renderPlot({
    dataset <- input$dataset
    year <- as.numeric(input$year)
    if (dataset == "New York") {
      selected_data <- joined_ny
      colnames(selected_data)[3] <- "Crime_rate"
    } else if (dataset == "Washington" | dataset == "Both") {
      selected_data <- joined_wa
      colnames(selected_data)[3] <- "Crime_rate"
    } 
    selected_data <- selected_data[selected_data$year > year & selected_data$year < (year + 10),]
    ggplot(data = selected_data) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = Crime_rate)) +
      coord_quickmap() +
      scale_fill_gradient(limits = range(selected_data[3]),
                          low = "pink", high = "red")
    })
    
    output$plot2 <- renderPlot({
      dataset <- input$dataset
      year <- as.numeric(input$year)
      if (dataset == "Both") {
        selected_data <- joined_wa
        joined_ny <- joined_ny[joined_ny$year > year & joined_ny$year < (year + 10), ]
        ggplot(data = joined_ny) +
          geom_polygon(aes(x = long, y = lat, group = group, fill = joined_ny[,3])) +
          coord_quickmap() +
          scale_fill_gradient(limits = range(joined_ny[3]),
                              low = "pink", high = "red")
      }
    })
    
    output$text1 <- renderText({
      dataset <- input$dataset
      year <- input$year
      if (dataset == "New York") {
        selected_data <- "New York"
        msg <- paste0("The map below shows the distribution of crime rate in ",
                      selected_data, " in the ", year, "'s. We can see that
                      crime rates are almost the same in New york. Based on the
                      legend on the right, crime is diminshing over the decades.")
      } else if (dataset == "Washington") {
        selected_data <- "Washington"
        msg <- paste0("The map below shows the distribution of crime rate in ",
                      selected_data, " in the ", year, "'s. From the graph, we
                       can see that King's county is where the crime rate is
                      highest. The distribution and frequency of crime does not
                      change dramatically over the year.")
      } else if (dataset == "Both") {
        msg <- paste0("The maps below represent the crime incidents distribution in 
        Washington and New York respectively in the ", year, "'s. Comparing this two garphs we can
        tell that generally crime distribution is more spreaded out in
        New York, and it's more concentrated in Washington.")
      }
      
    })
    
    
    
    output$demo_map <- renderPlot({
      dataset <- input$dataset
      if(dataset != "Both"){
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
      
      }
    })
    
    output$demo_map1 <- renderPlot({
      dataset <- input$dataset
      if(dataset != "Both"){
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
    
      }
      
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
          ggtitle("       +Here is the plot about the correlation between crime counts
          and population in every counties in the given time period:") +
          geom_point() 
      }
      else if (dataset == "New York") {
        selected_data <- selected_data_ny  %>%
          select(Year, County, Population, Index.Count) %>%
          filter(Year <= end & Year >= start)
        names(selected_data)[4] <- "Crime_Count"
        ggplot(selected_data, aes(x=Population, y=Crime_Count, color = County, na.rm = TRUE)) + 
          geom_smooth(method = 'lm', se=FALSE) +
          ggtitle("       +Here is the plot about the correlation between crime counts
          and population in every counties in the given time period:") +
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
          ggtitle("         +Here is the plot about the correlation between crime counts
          and population in the representative urbanized areas(king county) 
          in this state in the last three decades:") +
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
          ggtitle("         +Here is the plot about the correlation between crime counts
          and population in the representative urbanized areas(New York city) 
          in this state in the last three decades:") +
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
          ggtitle("         +Here is the plot about the correlation between crime counts
          and population in the representative country areas(Benton county) in 
          this state in the last three decades:") +
          geom_point() 
      }
      else if (dataset == "New York") {
        selected_data <- selected_data_ny  %>%
          select(Year, County, Population, Index.Count) %>%
          filter(County == "hamilton")
        names(selected_data)[4] <- "Crime_Count"
        ggplot(selected_data, aes(x=Population, y=Crime_Count, na.rm = TRUE)) + 
          ggtitle("         +Here is the plot about the correlation between crime counts
          and population in the representative country areas(Hamilton county) 
          in this state in the last three decades:") +
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
    "According to Claude Fischer's subcultural theory(check more information
    at the link in Reference)
    , the movements of human beings
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
          else
            if(input$dataset == "New York" & as.numeric(input$year) == 1990)
              text <- "In 1990s, there is a population decline happened in the areas
            that experienced the industrial decline starting 1980s(the Rust Belt).
            The population of New York city stayed stable. The population of the 
            suburb areas surrounding the New York city started to increase."
            else
              if(input$dataset == "New York" & as.numeric(input$year) == 2000)
                text <- "In 2000s, the Rust Belt areas were still suffering from the 
              huge decline of local population. However, more and more people started 
              to move into the urbanized area of New York City and adjacent suburb 
              areas(meanwhile same things happened in the west coast)."
              else
                if(input$dataset == "New York" & as.numeric(input$year) == 2010)
                  text <- "In 2010s, people started to moved back to the Rust Belt areas.
                The population of those areas started to increase. The population of 
                New York city was still increasing but in a slow rate."

    })
    
    output$crime <- renderText({
      if(input$dataset == "Washington" & as.numeric(input$year) == 1990)
        text <- "In 1990s, The crime rate of all areas in Washington states kept
      increasing. These places just experienced the big companies recession 
      (such as the Boeing Bust) and energy crisis."
      else
        if(input$dataset == "Washington")
          text <- "In this time period, most areas of Washington states continued
        its crime rate declines."
        else 
          if(input$dataset == "New York" & as.numeric(input$year) == 1990)
            text <- "The crime rate kept decreasing in most areas. However, the 
          situations in the Rust Belt areas were still not good."
        else 
          if(input$dataset == "New York" & as.numeric(input$year) == 2000)
            text <- "The crime rate kept decreasing in most areas. However, the 
          situations in the Rust Belt areas basically remained the same."
        else 
          if(input$dataset == "New York" & as.numeric(input$year) == 2010)
            text <- "The crime rate in the Rust Belt areas decreased in this
          time period. The crime rate in the some New York city areas 
          and country areas started to increase."
    })
    

    output$number1 <- renderText({
      if(input$dataset == "Washington")
      paste('<p>Things needed to be catched up:<p>',
      "The correlation rate
      of crime count and population in Great Seattle Area is ", 
      round(cor_wa, digits = 3), 
      " which means when the population increase, the crime count decrease.",
      "this number of country areas or small cities of Washington is ", 
      round(cor_wa_c, digits = 3), ". We can find out that population increase 
      has a bigger impact of decreasing crime in metropolitan urbanized 
      areas than country areas or small cities. Meanwhile, the population
      rate change for Washington state for last three decades is ", 
      round(pop_wa_change, digits = 3),"%. This shows that the populations of 
      most places in Washington increased in different rates.")
      else if(input$dataset == "New York")
        paste('<p>Things needed to be catched up:<p>'
              ,"The population rate change of New York city for the last
              three decades is ",round(pop_ny_change_u,digits = 3), "%. 
              This number of the Rust 
              Belt areas is ", round(pop_ny_change_r, digits = 3), "%. 
              We can find out that
              unlike somewhere else, the population living in the Rust Belt 
              area(such as Buffalo) decreased for the last three decades. But
              this trend has been stopped in recent years. According to the 
              correlation plot ,we can find out that population increase 
              has a bigger impact of decreasing crime in metropolitan urbanized 
              areas than country areas or small cities.")
    })

      output$conclusion1 <- renderText({
        if(input$dataset == "Both")
        "From the history of Washington state and New York state, we can find out 
        that there is a population redistribution pattern happened. From 1990s, 
        in east coast, the past blue-collar manufacturing regions such as
        West New York state and Pennsylvania started to lose 
        its energy and people started to leave. This trend seems to be ended 
        this years. In west coast, the population kept increasing for three 
        decades. If we want to discuss the relationship between crime rate and
        population change, we would find out that in urbanized areas, increasing
        population means declining crime rate. However, in country areas or scattered
        small cities, increased population means increased crime rate. Overall, it shows
        that those first tier cities like Seattle and New York have decreasing
        crime counts. In population-decreasing areas in east coast or non-urbanized 
        areas in west coast, the crime rates remain the same or have rises."
      })
      
      output$slogan1 <- renderText({
        if(input$dataset == "Both")
          paste('<p></p>', '<p></p>',
            '<p><B>Conclusion:<B></p>',
            '<p><B>Increasing population(in big cities) = less crime counts</B></p>',
            '<p><B>Increasing population(in other areas) or 
            decreasing population = more crime counts</B></p>',
            '<p><B>Big cities in both coasts are getting safer<B></p>',
            '<p><B>Scattered small cities and country areas used to be
             dangerous, but now it is getting safer<B></p>',
            '<p><B>The situation in the Rust Belt Area is getting better recently.<B></p>'
            )
        
        
      })
      
      output$conclusion2 <- renderText({
        if(input$dataset == "Both")
          "In fact, we can find out that economy situation is the real cause behind
        the crime count change. When people moving into big city, it means the city is
        getting energetic and flourishing. When the population desnity is getting bigger,
        the cost to provide law enforcement is getting lower. People can enjoy better
        social benefits if they live in densely populated regions which means they are
        the society is safer. From the view of the whole country, people lead to move
        to places with better economic potential. West coast becomes the target place
        for those kind of people. US was getting through energy crisis and economic
        recession before 1990s so that the crime rate was still unstable(relative high). With the 
        development of mature social system and economic situation, the crime rate change
        is getting lower(stable)."
      })
      output$slogan2 <- renderText({
        if(input$dataset == "Both")
          paste('<p><B>Dense population = less crime counts<B></p>',
                '<p><B>stable economic situation = less crime counts</B></p>'
                ) })
        output$subtitle <- renderText({
          if(input$dataset != "Both")
            paste('<p><B>More Specific Explorations:<B></p>'
            )
      })
    
    
    
}


