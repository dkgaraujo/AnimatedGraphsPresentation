
# Loading the packages ----------------------------------------------------

library(ggplot2)
library(gganimate)
library(dplyr)
library(data.table)


# Loading the data --------------------------------------------------------

GoogleMobilityData_County <- data.table(read.csv(
  "https://github.com/OpportunityInsights/EconomicTracker/raw/main/data/Google%20Mobility%20-%20County%20-%20Daily.csv",
  stringsAsFactors = FALSE,
  na.strings = "."))

CardExpenditureData_County <- data.table(read.csv(
  "https://github.com/OpportunityInsights/EconomicTracker/raw/main/data/Affinity%20-%20County%20-%20Daily.csv",
  stringsAsFactors = FALSE,
  na.strings = "."))

CountyInfo <- data.table(read.csv("https://github.com/OpportunityInsights/EconomicTracker/raw/main/data/GeoIDs%20-%20County.csv"))


# Re-format the dates into a single column --------------------------------

GoogleMobilityData_County[, 
                          Date := as.Date(paste(year, month, day, sep = "-"),
                                          format = "%Y-%m-%d")]
GoogleMobilityData_County[, ':='(year = NULL,
                                 month = NULL,
                                 day = NULL)]

CardExpenditureData_County[, 
                           Date := as.Date(paste(year, month, day, sep = "-"),
                                           format = "%Y-%m-%d")]

CardExpenditureData_County[, ':='(year = NULL,
                                  month = NULL,
                                  day = NULL)]


# Merging the datasets ----------------------------------------------------

CountyDataset <- merge(GoogleMobilityData_County,
                       CardExpenditureData_County[Date >= "2020-02-24",],
                       by = c("countyfips", "Date"),
                       all.x = TRUE,
                       all.y = TRUE)

CountyDataset <- merge(CountyDataset, CountyInfo,
                       by = "countyfips")

CountyDataset$Date <- as.Date(CountyDataset$Date, format = "%Y-%m-%d")
rm(GoogleMobilityData_County, CardExpenditureData_County, CountyInfo)


# Cleaning the dataset ----------------------------------------------------

counties_with_away_from_home_data <- unique(CountyDataset[!is.na(gps_away_from_home)]$countyfips)
CountyDataset <- CountyDataset[countyfips %in% counties_with_away_from_home_data,]


# Some statistics that will help us make the graph more informative -------

number_counties_sample <- length(unique(CountyDataset$countyfips))
population_counties_sample <- sum(CountyDataset[, mean(county_pop2019), by = countyfips]$V1)
end_2019_USA_pop <- 329131338 # source: https://www.census.gov/popclock/
perc_pop_counties_sample <- round(100 * population_counties_sample / end_2019_USA_pop, 1)


# Plotting the graph! -----------------------------------------------------

all_counties_base_graph <- CountyDataset %>% 
  ggplot(aes(x = 100 * gps_away_from_home, y = 100 * spend_all,
             color = log(county_pop2019))) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, color = "black", size = 0.8) +
  geom_vline(xintercept = 0, color = "black", size = 0.8) +
  scale_colour_gradientn(colours=rainbow(4)) +
  labs(title = 'Evolution of mobility and expenditure in US counties',
       x = '% change in time away from home',
       y = '% change in expenditure',
       color = 'County population, in log',
       caption = 'Source: TrackTheRecovery.org') +
  theme(text = element_text(size = 14),
        plot.title = element_text(size = 16, face = "bold"),
        legend.position = "bottom",
        legend.title = element_text(size = 12))

all_counties_anim_graph <- all_counties_base_graph +
  # the animated part
  transition_time(Date) +
  ease_aes('linear') +
  enter_fade() +
  labs(subtitle = '{number_counties_sample} counties, {perc_pop_counties_sample}% of US pop. - Date: {frame_time}')  

animate(all_counties_anim_graph,
        duration = 25,
        fps = 30,
        start_pause = 5,
        end_pause = 60,
        width = 500,
        height = 400)


# Saving the animated graph -----------------------------------------------

anim_save("UScounties_mobility_expenditure.gif", 
          animation = all_counties_anim_graph,
          duration = 25,
          fps = 30,
          start_pause = 5,
          end_pause = 60,
          width = 500,
          height = 400)
