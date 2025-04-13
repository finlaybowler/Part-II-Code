####Loading in data####

#Reading in data on species distributions
bird_ranges <- read.csv("All_bird_country_intersections.csv", na.strings = c(""))

#Removing codes that are not meaningful
bird_ranges <- bird_ranges %>% filter(PRESENCE %in% c(1, 2, 6), SEASONAL != 4) %>%
  select(species, country, ISO) %>% distinct()
colnames(bird_ranges) <- c("Species", "Country", "ISO")

#Loading in data of country polygons 
library(rnaturalearth)
countries <- ne_countries(type = "countries", scale = 110,  returnclass = "sf")


####Maps for forest-dependent birds####

#Joining threat data to species distribution data
data3$every <- 1
bird_ranges_data <- left_join(bird_ranges, data3, by = "Species")

#Namibia has country coda NA - this ensures it is read as a character variable
bird_ranges_data$ISO[is.na(bird_ranges_data$ISO)] <- "NA"

#For trade-threatened species
#Selecting only the data we need and viewing threat data as a number not a factor
bird_country_threat <- bird_ranges_data %>% select(Country, ISO, major_threat_binary)
bird_country_threat$major_threat_binary <- as.numeric(as.character(bird_country_threat$major_threat_binary))

#Removes any birds in maps data with no threat data associated
bird_country_threat <- bird_country_threat %>% filter(!is.na(bird_country_threat$major_threat_binary))

#Calculates the number of trade-threatened birds for each country
bird_country_threat <- bird_country_threat %>%
  group_by(Country, ISO) %>%
  summarise(Total = sum(major_threat_binary))

#Joining data to the country polygons
bird_country_threat_for_plot <- countries %>%
  left_join(bird_country_threat, by = c("iso_a2_eh" = "ISO"))

#Plotting and savinvg the map
plot <- ggplot() +
  geom_sf(data = bird_country_threat_for_plot, aes(fill = Total)) +
  scale_fill_gradient(low = "#FFFD74", high = "#DC143C", name = "Richness") +
  theme_classic() +
  theme(
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 16)
  )

plot

ggsave("map_traded_forest.jpeg", plot = plot, width = 25, height = 15, units = "cm")

#For all forest-dependent species
#Selecting only the data we need
bird_country_threat <- bird_ranges_data %>% select(Country, ISO, every)

#Removes any birds in maps data with no threat data associated
bird_country_threat <- bird_country_threat %>% filter(!is.na(bird_country_threat$every))

#Calculates the number of trade-threatened birds for each country
bird_country_threat <- bird_country_threat %>%
  group_by(Country, ISO) %>%
  summarise(Total = sum(every))

#Joining data to the country polygons
bird_country_threat_for_plot <- countries %>%
  left_join(bird_country_threat, by = c("iso_a2_eh" = "ISO"))

#Plotting and saving the map
plot <- ggplot() +
  geom_sf(data = bird_country_threat_for_plot, aes(fill = Total)) +
  scale_fill_gradient(low = "#FFFD74", high = "#DC143C", name = "Richness") +
  theme_classic() +
  theme(
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 16)
  )

plot

ggsave("map_traded_forest.jpeg", plot = plot, width = 25, height = 15, units = "cm")


####Maps for forest-dependent frugivores####

#Selecting only frugivorous species from the dataset
data3_Frugivore <- data3[data3$Trophic.Niche == "Frugivore",]

#Joining threat data to species distribution data
bird_ranges_data <- left_join(bird_ranges, data3_Frugivore, by = "Species")

#Namibia has country coda NA - this ensures it is read as a character variable
bird_ranges_data$ISO[is.na(bird_ranges_data$ISO)] <- "NA"

#For trade-threatened frugivore species
#Selecting only the data we need and viewing threat data as a number not a factor
bird_country_threat <- bird_ranges_data %>% select(Country, ISO, major_threat_binary)
bird_country_threat$major_threat_binary <- as.numeric(as.character(bird_country_threat$major_threat_binary))

#Removes any birds in maps data with no threat data associated
bird_country_threat <- bird_country_threat %>% filter(!is.na(bird_country_threat$major_threat_binary))

#Calculates the number of trade-threatened birds for each country
bird_country_threat <- bird_country_threat %>%
  group_by(Country, ISO) %>%
  summarise(Total = sum(major_threat_binary))

#Joining data to the country polygons
bird_country_threat_for_plot <- countries %>%
  left_join(bird_country_threat, by = c("iso_a2_eh" = "ISO"))

#Plotting and savinvg the map
plot <- ggplot() +
  geom_sf(data = bird_country_threat_for_plot, aes(fill = Total)) +
  scale_fill_gradient(low = "#FFFD74", high = "#DC143C", name = "Richness") +
  theme_classic() +
  theme(
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 16)
  )

plot

ggsave("map_traded_frugivore.jpeg", plot = plot, width = 25, height = 15, units = "cm")

#For all forest-dependent frugivore speciesw
#Selecting only the data we need
bird_country_threat <- bird_ranges_data %>% select(Country, ISO, every)

#Removes any birds in maps data with no threat data associated
bird_country_threat <- bird_country_threat %>% filter(!is.na(bird_country_threat$every))

#Calculates the number of trade-threatened birds for each country
bird_country_threat <- bird_country_threat %>%
  group_by(Country, ISO) %>%
  summarise(Total = sum(every))

#Joining data to the country polygons
bird_country_threat_for_plot <- countries %>%
  left_join(bird_country_threat, by = c("iso_a2_eh" = "ISO"))

#Plotting and saving the map
plot <- ggplot() +
  geom_sf(data = bird_country_threat_for_plot, aes(fill = Total)) +
  scale_fill_gradient(low = "#FFFD74", high = "#DC143C", name = "Richness") +
  theme_classic() +
  theme(
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 16)
  )

plot

ggsave("map_traded_forest.jpeg", plot = plot, width = 25, height = 15, units = "cm")

