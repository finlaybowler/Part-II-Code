####Combining functional trait data with data on trade threat####

setwd("C:\\Users\\finla\\OneDrive\\Documents\\Finlay\\University\\Year 3/Part II Project/")

library(dplyr)
library(ggplot2)
library(tidyr)

#Reading in data on IUCN threat scores and AVONET functional traits 
traits <- read.csv("BirdLife_AVONET_final.csv")
threats <- read.csv("IUCN_all_ThreatAssessments_Sept2024_3.csv")
threats$code <- as.factor(threats$code)
summary(threats$code)

#Filtering data on threats so that only 5.1.1 is included
intentional_use <- threats[threats$code == "5.1.1", ]
intentional_use <- intentional_use %>% drop_na(code)

#Categorising threat data as major threat (Overall score of 6 and above) or minor threat
intentional_use <- intentional_use %>%
  mutate(threat_level = ifelse(score_upd > 5, "major_threat", "minor_threat"))

#Removing duplicates in the intentional_use data set
intentional_use <- intentional_use %>%
  distinct(Species, .keep_all = TRUE)

#Joining data on threat 5.1.1 to functional trait data
data <- left_join(traits, intentional_use, by = "Species")

#Replacing the NAs in the threat_level column with "no_threat"
data <- data %>%
  mutate(threat_level = replace_na(threat_level, "no_threat"))

#Making a binary column: 1 = overall threat score of 6 and above, 0 = threat score of 5 and below, or 5.1.1 not listed as a threat
data <- data %>%
  mutate(major_threat_binary = ifelse(threat_level == "major_threat", 1, 0))


####Accounting for forest dependency####

#Reading in the subset of birds that are categorised as forest-dependent
forest_dependent <- read.csv("Verts_forest_dependent_aves.csv")

#Removing any extinct species from this datasect
forest_dependent <- forest_dependent %>% filter(Category != "EX")

#Taking the forest-dependent subset of data on threats and functional traits
data2 <- left_join(forest_dependent, data, by = "Species")


####Adding in data on life history traits####

#Reading in life history trait data
gen_length <- read.csv("Generation_length_data_with_added_species_names_FINAL.csv")

#Joining the life history trait data to the data set of threats and other functional traits
data3 <- left_join(data2, gen_length, by = "Species")

#This is the dataset used in all subsequent analyses
#It contains data on all forest-dependent species: their functional and life history traits, and also the threat posed by 5.1.1



