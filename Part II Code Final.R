setwd("C:\\Users\\finla\\OneDrive\\Documents\\Finlay\\University\\Year 3/Part II Project/")

library(dplyr)
library(ggplot2)
library(tidyr)

#Obtaining the dataset of all birds, traits and their use in trade

#reading the data in
traits <- read.csv("BirdLife_AVONET_final.csv")
threats <- read.csv("IUCN_all_ThreatAssessments_Sept2024_3.csv")
threats$code <- as.factor(threats$code)
summary(threats$code)

#filtering so threats are only 5.1.1
intentional_use <- threats[threats$code == "5.1.1", ]
intentional_use <- intentional_use %>% drop_na(code)

#categorising as either major (score 6 or greater) or minor threat 
intentional_use <- intentional_use %>%
  mutate(threat_level = ifelse(score_upd > 5, "major_threat", "minor_threat"))

#there are species duplicates in the intentional_use data idk why
intentional_use <- intentional_use %>%
  distinct(Species, .keep_all = TRUE)

#now need to join the threat data to the traits data
data <- left_join(traits, intentional_use, by = "Species")

#replacing the NAs in the threat_level column with "no_threat"
data <- data %>%
  mutate(threat_level = replace_na(threat_level, "no_threat"))

#making a binary column - either major threat or not threatened
data <- data %>%
  mutate(major_threat_binary = ifelse(threat_level == "major_threat", 1, 0))

#another binary column - either threatened by trade or not
data <- data %>%
  mutate(threat_binary = ifelse(threat_level %in% c("major_threat", "minor_threat"), 1, 0))


#Obtaining the dataset containing only forest dependent birds

forest_dependent <- read.csv("Verts_forest_dependent_aves.csv")
forest_dependent <- forest_dependent %>% filter(Category != "EX")
data2 <- left_join(forest_dependent, data, by = "Species")


#Plotting logistic regressions for the traits
#Looks at predicting whether a species is threatened by trade (5.1.1 according to IUCN) using a logistic model

glm_scaled_Mass <- glm(major_threat_binary ~ Hand.Wing.Index,
               family=binomial(link='logit'),
               data=data3)
summary(glm_scaled_Mass)

test_plot <- ggplot(data3, aes(scaled_Mass, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0, height = 0.1, color = "black") +
  #scale_x_continuous(limits = c(0, 10000)) +
  labs(x = "scaled_Mass", y = "P(Threatened by trade)")
test_plot

numbers <- seq(0.3, 4, 0.1)
predicted <- predict(glm_scaled_Mass,
                     newdata = data.frame(scaled_Mass = numbers),
                     type = "link", se = TRUE)
model_scaled_Mass <- data.frame(scaled_Mass = numbers,
                        estimate = boot::inv.logit(predicted$fit),
                        confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                        confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

plot_scaled_Mass <- ggplot(model_scaled_Mass, aes(scaled_Mass, estimate)) +
  geom_jitter(data = data3, aes(scaled_Mass, major_threat_binary), width = 0, height = 0.01, alpha = 0.1) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  theme_classic() +
  scale_x_continuous(limits = c(0.3, 4)) +
  labs(x = expression("log"[10]*" Mass (g)"), y = "P(Threatened by trade)") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19, margin = margin(r = 15))
  )
plot_scaled_Mass

ggsave("plot_scaled_Mass_final.jpeg", plot = plot_scaled_Mass, width = 15, height = 15, units = "cm")



#This is for testing correlations
cor.test(x=data3$Mass, y=data3$Hand.Wing.Index, method = "spearman")

#I'm gonna try removing the cassowaries
data2_no_cassowary <- data2 %>% filter(Mass <= 20000)
#Having looked at it, does not seem to change the regression much


#Plotting logistic regressions for discrete traits

data2$Order <- factor(data2$Order)
data3$Order <- relevel(data3$Order, ref = "Passeriformes")

data3_Passeriformes <- data3[data3$Order == "Passeriformes",]

glm_Order <- glm(major_threat_binary ~ Order,
               family = binomial(link='logit'),
               data=data3)
summary(glm_Order)

test_plot <- ggplot(data3, aes(Order, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0.4, height = 0.1, color = "black") +
  labs(x = "Order", y = "P(Threatened by trade)")
test_plot

categories_Order <- unique(data3$Order)

predicted <- predict(glm_Order,
                     newdata = data.frame(Order = categories_Order),
                     type = "link", se = TRUE)
model_Order <- data.frame(Order = categories_Order,
                        estimate = boot::inv.logit(predicted$fit),
                        confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                        confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

model_Order_new <- model_Order[Order %in% c("Accipitriformes", "Bucerotiformes", "Caprimulgiformes", "Columbiformes", 
                                            "Coraciiformes", "Cuculiformes", "Galliformes", 
                                            "Passeriformes", "Piciformes", "Psittaciformes", "Strigiformes"), ]

significance_scores <- data.frame(Order = c("Accipitriformes", "Bucerotiformes", "Caprimulgiformes", "Columbiformes", 
                                            "Coraciiformes", "Cuculiformes", "Galliformes", 
                                            "Passeriformes", "Piciformes", "Psittaciformes", "Strigiformes"),
                                  Significance = c("***", "***", "", "***", "", "**", "***", "", "", "***", ""))
model_Order_new <- left_join(model_Order_new, significance_scores, by = "Order")


plot_Order <- ggplot(model_Order_new, aes(Order, estimate)) +
  geom_errorbar(aes(ymin = confidence_low, ymax = confidence_high), width = 0.6) +
  geom_point(size = 2) +
  geom_text(aes(label = Significance, y = confidence_high + 0.05), size = 5) +
  theme_classic() +
  labs(x = "Order", y = "P(Threatened by trade)") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    axis.text.x = element_text(size = 14, angle = 45, vjust = 1.05, hjust = 1.04),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17, margin = margin(r = 15))
  )
plot_Order

ggsave("plot_Order_final.jpeg", plot = plot_Order, width = 20, height = 15, units = "cm")


#Trying a generalised linear mixed model using glmer

library(lme4)

data2$scaled_Mass <- log10(data2$Mass)

model <- glmer(major_threat_binary ~ scaled_Mass + (scaled_Mass|Order), family = binomial(), data = data2)
summary(model)

#Order <- unique(data2$Order)
#Scaled_mass <- seq(min(data2$scaled_Mass), max(data2$scaled_Mass), length.out = 100)

#newdata <- expand_grid(Order = Order, scaled_Mass = Scaled_mass)
#newdata$Order <- factor(newdata$Order)


newdata <- data2 %>% 
  group_by(Order) %>% 
  summarise(scaled_Mass = seq(from = min(scaled_Mass), to = max(scaled_Mass), length.out = 1000))

predicted <- predict(model,
                     newdata = newdata,
                     type = "link", se = TRUE)

model_Scaled_Mass <- data.frame(Order = newdata$Order,
                                Scaled_mass = newdata$scaled_Mass,
                        estimate = boot::inv.logit(predicted$fit),
                        confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                        confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

model_Scaled_Mass <- model_Scaled_Mass[model_Scaled_Mass$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]
data3_specific_order <- data3[data3$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]


glmer_plot <- 
  ggplot(model_Scaled_Mass, aes(Scaled_mass, estimate)) +
  geom_jitter(data = data3_specific_order, aes(scaled_Mass, major_threat_binary), width = 0, height = 0.01, alpha = 0.15) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  theme_minimal() +
  labs(x = expression("log"[10]*" Mass (g)"), y = "P(Threatened by trade)") +
  facet_wrap(Order ~ .,) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17, margin = margin(r = 15)),
    strip.text = element_text(size = 14)
  )

glmer_plot

ggsave("plot_glmer_mass_final.jpeg", plot = glmer_plot, width = 20, height = 15, units = "cm")


#looking at mass and trophic niche glmer

model <- glmer(major_threat_binary ~ scaled_Mass + (scaled_Mass|Trophic.Niche), family = binomial(), data = data3)
summary(model)

newdata <- data3 %>%
  filter(!is.na(Trophic.Niche)) %>%
  filter(scaled_Mass <= 4)
newdata <- newdata %>% 
  group_by(Trophic.Niche) %>% 
  summarise(scaled_Mass = seq(from = min(scaled_Mass), to = max(scaled_Mass), length.out = 1000))

predicted <- predict(model,
                     newdata = newdata,
                     type = "link", se = TRUE)

model_Scaled_Mass <- data.frame(Trophic.Niche = newdata$Trophic.Niche,
                                Scaled_mass = newdata$scaled_Mass,
                                estimate = boot::inv.logit(predicted$fit),
                                confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                                confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

model_Scaled_Mass <- model_Scaled_Mass[model_Scaled_Mass$Trophic.Niche %in% c("Frugivore", "Invertivore", "Nectarivore", "Omnivore"), ]
data3_specific_Trophic.Niche <- data3[data3$Trophic.Niche %in% c("Frugivore", "Invertivore", "Nectarivore", "Omnivore"), ]
data3_specific_Trophic.Niche <- data3_specific_Trophic.Niche %>% filter(scaled_Mass <= 4)


glmer_plot <- 
  ggplot(model_Scaled_Mass, aes(Scaled_mass, estimate)) +
  geom_jitter(data = data3_specific_Trophic.Niche, aes(scaled_Mass, major_threat_binary), width = 0, height = 0.01, alpha = 0.15) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  theme_minimal() +
  labs(x = expression("log"[10]*" Mass (g)"), y = "P(Threatened by trade)") +
  facet_wrap(Trophic.Niche ~ .,) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19, margin = margin(r = 15)),
    strip.text = element_text(size = 16)
  )

glmer_plot

ggsave("plot_glmer_mass_trophicniche_final.jpeg", plot = glmer_plot, width = 20, height = 15, units = "cm")

#Doing some linear regressions in specific orders

data2_Psittaciformes <- data2[data2$Order == "Psittaciformes",]
data2_Bucerotiformes <- data2[data2$Order == "Bucerotiformes",]
data2_Frugivore <- data2[data2$Trophic.Niche == "Frugivore",]
data2_Frugivore <- data2_Frugivore %>% filter(Mass <= 4000)

glm_Mass <- glm(major_threat_binary ~ Mass,
                family=binomial(link='logit'),
                data=data2_Frugivore)
summary(glm_Mass)

test_plot <- ggplot(data2_Frugivore, aes(Mass, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0, height = 0.1, color = "black") +
  labs(x = "Mass", y = "P(Trade threat)")
test_plot

numbers <- seq(min(data2_Frugivore$Mass), max(data2_Frugivore$Mass), length.out = 1000)
predicted <- predict(glm_Mass,
                     newdata = data.frame(Mass = numbers),
                     type = "link", se = TRUE)
model_Mass <- data.frame(Mass = numbers,
                         estimate = boot::inv.logit(predicted$fit),
                         confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                         confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

plot_Mass <- ggplot(model_Mass, aes(Mass, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  theme_classic() +
  #scale_x_continuous(limits = c(0, 10000)) +
  labs(x = "Mass", y = "P(Listed under Intentional Use (5.1.1))") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )
plot_Mass


###########

#Looking at generation length data

gen_length <- read.csv("Generation_length_data_with_added_species_names_FINAL.csv")
data3 <- left_join(data2, gen_length, by = "Species")

#Quick look at some correlation tests
cor.test(x=data3$Mass, y=data3$Age.at.first.breeding, method = "spearman")
plot(data3$Mass ~ data3$Age.at.first.breeding,
     ylim = c(0, 5000))

data3$major_threat_binary <- factor(data3$major_threat_binary)
data3_Psittaciformes <- data3[data3$Order == "Psittaciformes",]
data3_Bucerotiformes <- data3[data3$Order == "Bucerotiformes",]
data3_Frugivore <- data3[data3$Trophic.Niche == "Frugivore",]
data3_Columbiformes <- data3[data3$Order == "Columbiformes",]
data3_Bucerotiformes <- data3[data3$Order == "Bucerotiformes", ]
data3_Galliformes <- data3[data3$Order == "Galliformes",]
data3_Struthioniformes <- data3[data3$Order == "Struthioniformes",]

data3_Invertivore <- data3[data3$Trophic.Niche == "Invertivore",]
data3_Nectarivore <- data3[data3$Trophic.Niche == "Nectarivore",]


glm_Range.Size <- glm(major_threat_binary ~ Range.Size,
                family=binomial(link='logit'),
                data=data3)
summary(glm_Range.Size)

test_plot <- ggplot(data3, aes(Range.Size, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0, height = 0.1, color = "black") +
  labs(x = "Range.Size", y = "P(Trade threat)")

test_plot

numbers <- seq(1000, 50000000, 10000)
predicted <- predict(glm_Range.Size,
                     newdata = data.frame(Range.Size = numbers),
                     type = "link", se = TRUE)
model_Range.Size <- data.frame(Range.Size = numbers,
                         estimate = boot::inv.logit(predicted$fit),
                         confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                         confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

plot_Range.Size <- ggplot(model_Range.Size, aes(Range.Size, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  theme_classic() +
  #scale_x_continuous(limits = c(0, 10000)) +
  scale_x_log10() +
  labs(x = "Range Size", y = "P(Trade threat)") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17)
  )
plot_Range.Size

glm_Trophic.Niche <- glm(major_threat_binary ~ Trophic.Niche,
                         family=binomial(link='logit'),
                         data=data3_Bucerotiformes)
summary(glm_Trophic.Niche)

test_plot <- ggplot(data3_Bucerotiformes, aes(Trophic.Niche, threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0.4, height = 0.1, color = "black") +
  labs(x = "Trophic.Niche", y = "P(Listed under Intentional Use (5.1.1))")
test_plot

categories_Trophic.Niche <- unique(data3_Bucerotiformes$Trophic.Niche)

predicted <- predict(glm_Trophic.Niche,
                     newdata = data.frame(Trophic.Niche = categories_Trophic.Niche),
                     type = "link", se = TRUE)
model_Trophic.Niche <- data.frame(Trophic.Niche = categories_Trophic.Niche,
                                  estimate = boot::inv.logit(predicted$fit),
                                  confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                                  confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

plot_Trophic.Niche <- ggplot(model_Trophic.Niche, aes(Trophic.Niche, estimate)) +
  geom_errorbar(aes(ymin = confidence_low, ymax = confidence_high), width = 0.6) +
  geom_point(size = 2) +
  theme_classic() +
  labs(x = "Trophic.Niche", y = "P(Trade threat)") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 8),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10)
  )
plot_Trophic.Niche


############

#Maps

bird_ranges <- read.csv("All_bird_country_intersections.csv", na.strings = c(""))

#Remove codes that are not meaningful
bird_ranges <- bird_ranges %>% filter(PRESENCE %in% c(1, 2, 6), SEASONAL != 4) %>%
  select(species, country, ISO) %>% distinct()
colnames(bird_ranges) <- c("Species", "Country", "ISO")

bird_ranges_data <- left_join(bird_ranges, data3_Frugivore, by = "Species")
bird_ranges_data$major_threat_binary <- replace(bird_ranges_data$major_threat_binary, is.na(bird_ranges_data$major_threat_binary), 0)
bird_ranges_data$ISO[is.na(bird_ranges_data$ISO)] <- "NA"

bird_country_threat <- bird_ranges_data %>% select(Country, ISO, major_threat_binary)

bird_country_threat$major_threat_binary <- replace(bird_country_threat$major_threat_binary, is.na(bird_country_threat$major_threat_binary), 0)

bird_country_threat <- bird_country_threat %>%
  group_by(Country, ISO) %>%
  summarise(Total = sum(major_threat_binary))

library(rnaturalearth)

countries <- ne_countries(type = "countries", scale = 110,  returnclass = "sf")

bird_country_threat_for_plot <- countries %>%
  left_join(bird_country_threat, by = c("iso_a2_eh" = "ISO"))

plot <- ggplot() +
  geom_sf(data = bird_country_threat_for_plot, aes(fill = Total)) +
  scale_fill_gradient(low = "#FFFD74", high = "#DC143C", name = "Richness") +
  theme_classic() +
  theme(
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 16)
  )

plot

ggsave("map_traded_forest_prop.jpeg", plot = plot, width = 25, height = 15, units = "cm")

#######
data3$every <- 1
data3_Frugivore$every <- 1

bird_ranges_data <- left_join(bird_ranges, data3_Frugivore, by = "Species")
bird_ranges_data$major_threat_binary <- replace(bird_ranges_data$major_threat_binary, is.na(bird_ranges_data$major_threat_binary), 0)
bird_ranges_data$every <- replace(bird_ranges_data$every, is.na(bird_ranges_data$every), 0)
bird_ranges_data$ISO[is.na(bird_ranges_data$ISO)] <- "NA"

bird_country_threat <- bird_ranges_data %>% select(Country, ISO, major_threat_binary, every)

#bird_country_threat$major_threat_binary <- replace(bird_country_threat$major_threat_binary, is.na(bird_country_threat$major_threat_binary), 0)

bird_country_threat <- bird_country_threat %>%
  group_by(Country, ISO) %>%
  summarise(Total = sum(major_threat_binary) / sum(every))

library(rnaturalearth)

countries <- ne_countries(type = "countries", scale = 110,  returnclass = "sf")

bird_country_threat_for_plot <- countries %>%
  left_join(bird_country_threat, by = c("iso_a2_eh" = "ISO"))

plot <- ggplot() +
  geom_sf(data = bird_country_threat_for_plot, aes(fill = Total)) +
  scale_fill_gradient(low = "white", high = "black", name = "Richness") +
  theme_classic() +
  theme(
    legend.text = element_text(size = 13),
    legend.title = element_text(size = 16)
  )

plot

ggsave("map_all_frugivore.jpeg", plot = plot, width = 25, height = 15, units = "cm")


########

glm_Hand.Wing.Index <- glm(major_threat_binary ~ Hand.Wing.Index,
                family=binomial(link='logit'),
                data=data3)
summary(glm_Hand.Wing.Index)

test_plot <- ggplot(data3, aes(Hand.Wing.Index, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0, height = 0.01, color = "black") +
  labs(x = "Hand.Wing.Index", y = "P(Trade threat)")
test_plot

numbers <- seq(0, 70, 0.1)
predicted <- predict(glm_Hand.Wing.Index,
                     newdata = data.frame(Hand.Wing.Index = numbers),
                     type = "link", se = TRUE)
model_Hand.Wing.Index <- data.frame(Hand.Wing.Index = numbers,
                         estimate = boot::inv.logit(predicted$fit),
                         confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                         confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

plot_Hand.Wing.Index <- ggplot(model_Hand.Wing.Index, aes(Hand.Wing.Index, estimate)) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  #geom_jitter(data = data3, aes(Hand.Wing.Index, major_threat_binary), width = 0, height = 0.01) +
  theme_classic() +
  scale_x_continuous(limits = c(0, 70)) +
  labs(x = "Hand Wing Index", y = "P(Trade threat)") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 17),
    axis.title.y = element_text(size = 17)
  )
plot_Hand.Wing.Index

ggsave("map_final.jpeg", plot = plot, width = 25, height = 15, units = "cm")



########

#Going to try looking at trait space

library(tidyverse)
install.packages("funspace")
library(funspace)

# Log transformation
data3_log <- data3 %>% 
  mutate(log_Beak.Width = log10(Beak.Width), 
         log_Hand.Wing.Index = log10(Hand.Wing.Index),
         log_Mass = log10(Mass),
         log_Maximum.longevity = log10(Maximum.longevity))

#z standardization

data3_log.z <- data3_log %>% 
  mutate(Beak.Width_z = (log_Beak.Width - mean(log_Beak.Width)) / sd(log_Beak.Width),
         Hand.Wing.Index_z = (log_Hand.Wing.Index - mean(log_Hand.Wing.Index)) / sd(log_Hand.Wing.Index),
         Mass_z = (log_Mass - mean(log_Mass)) / sd(log_Mass),
         Maximum.longevity_z = (log_Maximum.longevity - mean(log_Maximum.longevity)) / sd(log_Maximum.longevity))

data3_traits <- data3_log.z %>% 
  select(Species, Beak.Width_z, Hand.Wing.Index_z, Mass_z, Maximum.longevity_z)

Species_trait_matrix <- data.frame(data3_traits, row.names = 1)



#making a matrix thingy for plotting groups

data3_log.z$Category <- factor(data3_log.z$Category)
data3_log.z <- data3_log.z[data3_log.z$Trophic.Niche == "Frugivore", ]
data3_log.z <- data3_log.z %>%
  filter(!is.na(Trophic.Niche))

Species_trait_matrix_threat <- data3_log.z %>%
  select(Species, major_threat_binary, Category)
Species_trait_matrix_threat <- data.frame(Species_trait_matrix_threat, row.names = 1)
Species_trait_matrix_threat$major_threat_binary <- factor(Species_trait_matrix_threat$major_threat_binary)

Species_trait_matrix_threat <- Species_trait_matrix_threat %>%
  mutate(major_threat_binary_VU = ifelse(major_threat_binary == 1 & Category %in% c("VU", "EN", "CR"), 1, 0))
Species_trait_matrix_threat <- Species_trait_matrix_threat %>%
  mutate(major_threat_binary_EN = ifelse(major_threat_binary == 1 & Category %in% c("EN", "CR"), 1, 0))
Species_trait_matrix_threat <- Species_trait_matrix_threat %>%
  mutate(major_threat_binary_CR = ifelse(major_threat_binary == 1 & Category %in% c("CR"), 1, 0))

Species_trait_matrix_threat$major_threat_binary_VU <- factor(Species_trait_matrix_threat$major_threat_binary_VU)
Species_trait_matrix_threat$major_threat_binary_EN <- factor(Species_trait_matrix_threat$major_threat_binary_EN)
Species_trait_matrix_threat$major_threat_binary_CR <- factor(Species_trait_matrix_threat$major_threat_binary_CR)

#This allows us to visualise the proportion of variation explained by each PCA axis
pca_result <- prcomp(Species_trait_matrix, scale = TRUE)
screeplot(pca_result, type = "lines")  # Scree plot
summary(pca_result)  # Shows variance explained

library(mFD)

#okay need to make a threat matrix whatever that means
Species_threat_matrix_long <- data.frame(Species = data3$Species, Threat = data3$major_threat_binary)
Species_threat_matrix <- Species_threat_matrix_long %>%
  pivot_wider(names_from = Species, values_from = Threat, values_fill = 0)

rownames(Species_threat_matrix)[rownames(Species_threat_matrix) == "1"] <- "Trade_threatened"

Species_threat_matrix <- as.matrix(Species_threat_matrix)

fspace <- mFD::tr.cont.fspace(sp_tr = Species_trait_matrix, pca = TRUE, 
                              nb_dim = 4, scaling = "no_scale", compute_corr = "pearson")

trade_threat_fd_indices <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = fspace$"sp_faxes_coord"[ , c("PC1", "PC2", "PC3")], 
  asb_sp_w         = Species_threat_matrix,                                
  ind_vect         = c("fric"),
  scaling          = FALSE,
  check_input      = TRUE,
  details_returned = TRUE)

PCA_Traded <- princomp(Species_trait_matrix) # this should be the overall species dataset

trait_space_traded_1_2 <- funspace(x = PCA_Traded, PCs = c(1,2), n_divisions = 300)

plot(x = trait_space_traded_1_2,
     type = "global",
     quant.plot = TRUE,
     arrows = TRUE,
     arrows.length = 0.4,
     xaxs = "r",  # Adjust x-axis style to reduce gap
     yaxs = "r"   # Adjust y-axis style to reduce gap
)

summary(trait_space_traded_1_2)

funtest <- funspace(x = PCA_Traded, PCs = c(1,2), group.vec = Species_trait_matrix_threat$major_threat_binary_CR, n_divisions = 300)
summary(funtest)
plot(funtest, type = "global")

jpeg("functional_space_frugivore_EN.jpg", width = 2000, height = 2000, res = 300)

plot(x = funtest,
     type = "groups",
     which.group = "1",
     quant.plot = TRUE,
     quant.col = "black",
     quant.lwd = 2,
     quant.labels = FALSE,
     colors = c("#FFFD74", "#DC143C"),
     globalContour = TRUE,
     globalContour.lwd = 2,
     globalContour.lty = 1,
     globalContour.col = "grey50",
     arrows = TRUE,
     arrows.length = 1,
     xlim = c(-5, 9),
     ylim = c(-7, 7), 
     axis.title.cex = 1
     )

dev.off()

######

#Try some pie charts

pie_chart_data <- data.frame(Order = data3$Order, major_threat_binary = data3$major_threat_binary)
pie_chart_data <- pie_chart_data[pie_chart_data$Order == "Psittaciformes",]

overall_pie <- pie_chart_data %>%
  count(major_threat_binary) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(major_threat_binary = factor(major_threat_binary, levels = major_threat_binary))


pie_chart <- ggplot(overall_pie, aes(x = "major_threat_binary", y = n, fill = major_threat_binary)) +
  geom_bar(stat = "identity", width = 1, size = 1) + 
  coord_polar(theta = "y") + 
  scale_fill_manual(values = c("1" = "#DC143C", "0" = "gray")) +
  theme_void() + 
  theme(legend.title = element_blank(),
        legend.position = "none")

pie_chart

ggsave("pie_chart.jpeg", plot = pie_chart, width = 20, height = 20, units = "cm")





###### 

#more logistic regressions

glm_Beak.Width <- glm(major_threat_binary ~ scaled_GenLength,
                family=binomial(link='logit'),
                data=data3)
summary(glm_Beak.Width)

test_plot <- ggplot(data3, aes(scaled_Beak.Width, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0, height = 0.1, color = "black") +
  #scale_x_continuous(limits = c(0, 10000)) +
  labs(x = "scaled_Beak.Width", y = "P(Threatened by trade)")
test_plot

numbers <- seq(0, 1.7, 0.005)
predicted <- predict(glm_Beak.Width,
                     newdata = data.frame(scaled_Beak.Width = numbers),
                     type = "link", se = TRUE)
model_Beak.Width <- data.frame(scaled_Beak.Width = numbers,
                         estimate = boot::inv.logit(predicted$fit),
                         confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                         confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

plot_Beak.Width <- ggplot(model_Beak.Width, aes(scaled_Beak.Width, estimate)) +
  geom_jitter(data = data3, aes(scaled_Beak.Width, major_threat_binary), width = 0, height = 0.01, alpha = 0.1) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  theme_classic() +
  scale_x_continuous(limits = c(0, 1.7)) +
  labs(x = expression("log"[10]*" Beak Width (mm)"), y = "P(Threatened by trade)") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19, margin = margin(r = 15))
  )
plot_Beak.Width

ggsave("plot_scaled_Beak.Width_final.jpeg", plot = plot_Beak.Width, width = 15, height = 15, units = "cm")


data3$scaled_Beak.Width <- log10(data3$Beak.Width)

model <- glmer(major_threat_binary ~ scaled_Beak.Width + (scaled_Beak.Width|Order), family = binomial(), data = data3)
summary(model)

#Order <- unique(data2$Order)
#Scaled_Beak.Width <- seq(min(data2$scaled_Beak.Width), max(data2$scaled_Beak.Width), length.out = 100)

#newdata <- expand_grid(Order = Order, scaled_Beak.Width = Scaled_Beak.Width)
#newdata$Order <- factor(newdata$Order)


newdata <- data3 %>% 
  group_by(Order) %>% 
  summarise(scaled_Beak.Width = seq(from = min(scaled_Beak.Width), to = max(scaled_Beak.Width), length.out = 1000))

predicted <- predict(model,
                     newdata = newdata,
                     type = "link", se = TRUE)

model_Scaled_Beak.Width <- data.frame(Order = newdata$Order,
                                Scaled_Beak.Width = newdata$scaled_Beak.Width,
                                estimate = boot::inv.logit(predicted$fit),
                                confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                                confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

model_Scaled_Beak.Width <- model_Scaled_Beak.Width[model_Scaled_Beak.Width$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]
data3_specific_Order <- data3[data3$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]

glmer_plot <- 
  ggplot(model_Scaled_Beak.Width, aes(Scaled_Beak.Width, estimate)) +
  geom_jitter(data = data3_specific_Order, aes(scaled_Beak.Width, major_threat_binary), width = 0, height = 0.01, alpha = 0.15) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  theme_minimal() +
  labs(x = expression("log"[10]*" Beak Width (mm)"), y = "P(Threatened by trade)") +
  facet_wrap(Order ~ .,) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19, margin = margin(r = 15)),
    strip.text = element_text(size = 16)
  )

glmer_plot

ggsave("plot_glmer_Beak.Width_final.jpeg", plot = glmer_plot, width = 20, height = 15, units = "cm")

summary(data3$Order)
summary(data3_Frugivore$Order)


data3$scaled_Hand.Wing.Index <- log10(data3$Hand.Wing.Index)

glm_Hand.Wing.Index <- glm(major_threat_binary ~ Hand.Wing.Index,
                      family=binomial(link='logit'),
                      data=data3)
summary(glm_Hand.Wing.Index)

test_plot <- ggplot(data3, aes(Hand.Wing.Index, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0, height = 0.1, color = "black") +
  #scale_x_continuous(limits = c(0, 10000)) +
  labs(x = "Hand.Wing.Index", y = "P(Threatened by trade)")
test_plot

numbers <- seq(0, 75, 0.1)
predicted <- predict(glm_Hand.Wing.Index,
                     newdata = data.frame(Hand.Wing.Index = numbers),
                     type = "link", se = TRUE)
model_Hand.Wing.Index <- data.frame(Hand.Wing.Index = numbers,
                               estimate = boot::inv.logit(predicted$fit),
                               confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                               confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

plot_Hand.Wing.Index <- ggplot(model_Hand.Wing.Index, aes(Hand.Wing.Index, estimate)) +
  geom_jitter(data = data3, aes(Hand.Wing.Index, major_threat_binary), width = 0, height = 0.01, alpha = 0.1) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  theme_classic() +
  scale_x_continuous(limits = c(0, 75)) +
  labs(x = "Hand Wing Index", y = "P(Threatened by trade)") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19, margin = margin(r = 15))
  )
plot_Hand.Wing.Index

ggsave("plot_scaled_Hand.Wing.Index_final.jpeg", plot = plot_Hand.Wing.Index, width = 15, height = 15, units = "cm")


model <- glmer(major_threat_binary ~ Hand.Wing.Index + (Hand.Wing.Index|Order), family = binomial(), data = data3)
summary(model)

#Order <- unique(data2$Order)
#Scaled_Hand.Wing.Index <- seq(min(data2$scaled_Hand.Wing.Index), max(data2$scaled_Hand.Wing.Index), length.out = 100)

#newdata <- expand_grid(Order = Order, scaled_Hand.Wing.Index = Scaled_Hand.Wing.Index)
#newdata$Order <- factor(newdata$Order)


newdata <- data3 %>% 
  group_by(Order) %>% 
  summarise(Hand.Wing.Index = seq(from = min(Hand.Wing.Index), to = max(Hand.Wing.Index), length.out = 1000))

predicted <- predict(model,
                     newdata = newdata,
                     type = "link", se = TRUE)

model_Hand.Wing.Index <- data.frame(Order = newdata$Order,
                                      Hand.Wing.Index = newdata$Hand.Wing.Index,
                                      estimate = boot::inv.logit(predicted$fit),
                                      confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                                      confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

model_Hand.Wing.Index <- model_Hand.Wing.Index[model_Hand.Wing.Index$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]
data3_specific_Order <- data3[data3$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]

glmer_plot <- 
  ggplot(model_Hand.Wing.Index, aes(Hand.Wing.Index, estimate)) +
  geom_jitter(data = data3_specific_Order, aes(Hand.Wing.Index, major_threat_binary), width = 0, height = 0.01, alpha = 0.15) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  theme_minimal() +
  labs(x = "Hand Wing Index", y = "P(Threatened by trade)") +
  facet_wrap(Order ~ .,) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19, margin = margin(r = 15)),
    strip.text = element_text(size = 16)
  )

glmer_plot

ggsave("plot_glmer_Hand.Wing.Index_final.jpeg", plot = glmer_plot, width = 20, height = 15, units = "cm")





##### gen length and age at first breeding

glm_GenLength <- glm(major_threat_binary ~ scaled_GenLength,
                      family=binomial(link='logit'),
                      data=data3)
summary(glm_GenLength)

test_plot <- ggplot(data3, aes(scaled_GenLength, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0, height = 0.1, color = "black") +
  #scale_x_continuous(limits = c(0, 10000)) +
  labs(x = "scaled_GenLength", y = "P(Threatened by trade)")
test_plot

numbers <- seq(0.2, 1.25, 0.005)
predicted <- predict(glm_GenLength,
                     newdata = data.frame(scaled_GenLength = numbers),
                     type = "link", se = TRUE)
model_GenLength <- data.frame(scaled_GenLength = numbers,
                               estimate = boot::inv.logit(predicted$fit),
                               confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                               confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

plot_GenLength <- ggplot(model_GenLength, aes(scaled_GenLength, estimate)) +
  geom_jitter(data = data3, aes(scaled_GenLength, major_threat_binary), width = 0, height = 0.01, alpha = 0.1) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  theme_classic() +
  scale_x_continuous(limits = c(0.2, 1.25)) +
  labs(x = expression("log"[10]*" Generation length (years)"), y = "P(Threatened by trade)") +
  scale_y_continuous(limits = c(0, 1)) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19, margin = margin(r = 15))
  )
plot_GenLength

ggsave("plot_scaled_GenLength_final.jpeg", plot = plot_GenLength, width = 15, height = 15, units = "cm")


data3$scaled_GenLength <- log10(data3$GenLength)

model <- glmer(major_threat_binary ~ scaled_GenLength + (scaled_GenLength|Order), family = binomial(), data = data3)
summary(model)

#Order <- unique(data2$Order)
#Scaled_GenLength <- seq(min(data2$scaled_GenLength), max(data2$scaled_GenLength), length.out = 100)

#newdata <- expand_grid(Order = Order, scaled_GenLength = Scaled_GenLength)
#newdata$Order <- factor(newdata$Order)


newdata <- data3 %>% 
  group_by(Order) %>% 
  summarise(scaled_GenLength = seq(from = min(scaled_GenLength), to = max(scaled_GenLength), length.out = 1000))

predicted <- predict(model,
                     newdata = newdata,
                     type = "link", se = TRUE)

model_Scaled_GenLength <- data.frame(Order = newdata$Order,
                                      Scaled_GenLength = newdata$scaled_GenLength,
                                      estimate = boot::inv.logit(predicted$fit),
                                      confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                                      confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

model_Scaled_GenLength <- model_Scaled_GenLength[model_Scaled_GenLength$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]
data3_specific_Order <- data3[data3$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]

glmer_plot <- 
  ggplot(model_Scaled_GenLength, aes(Scaled_GenLength, estimate)) +
  geom_jitter(data = data3_specific_Order, aes(scaled_GenLength, major_threat_binary), width = 0, height = 0.01, alpha = 0.15) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  theme_minimal() +
  labs(x = expression("log"[10]*" Generation length (years)"), y = "P(Threatened by trade)") +
  facet_wrap(Order ~ .,) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19, margin = margin(r = 15)),
    strip.text = element_text(size = 16)
  )

glmer_plot

ggsave("plot_glmer_GenLength_final.jpeg", plot = glmer_plot, width = 20, height = 15, units = "cm")


cor.test(x=data3$Mass, y=data3$Maximum.longevity, method = "spearman")
