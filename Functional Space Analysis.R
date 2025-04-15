####Loading in necessary packages and making Indonesia and Brazil datasets####

library(tidyverse)
library(funspace)
library(mFD)
library(funspace)

#Loading in data on species distributions by country
bird_ranges <- 
  read.csv("All_bird_country_intersections.csv")
bird_ranges <- bird_ranges %>% filter(PRESENCE %in% c(1, 2, 6), SEASONAL != 4) %>%
  select(species, country, ISO) %>% distinct()
colnames(bird_ranges) <- c("Species", "Country", "ISO")

#Selecting for just Indonesian species
birds_indonesia <- bird_ranges %>%
  filter(Country == "Indonesia") %>%
  select(Species)

#Selecting for just Brazilian species
birds_brazil <- bird_ranges %>%
  filter(Country == "Brazil") %>%
  select(Species)

####Calculating FRic for Indonesian frugivores####

#Log transformation of data
data3_log <- data3 %>% 
  mutate(log_Beak.Width = log10(Beak.Width), 
         log_Hand.Wing.Index = log10(Hand.Wing.Index),
         log_Mass = log10(Mass),
         log_Maximum.longevity = log10(Maximum.longevity))

#Z-transforming the data to standardise it 
data3_log.z <- data3_log %>% 
  mutate(Beak.Width_z = (log_Beak.Width - mean(log_Beak.Width)) / sd(log_Beak.Width),
         Hand.Wing.Index_z = (log_Hand.Wing.Index - mean(log_Hand.Wing.Index)) / sd(log_Hand.Wing.Index),
         Mass_z = (log_Mass - mean(log_Mass)) / sd(log_Mass),
         Maximum.longevity_z = (log_Maximum.longevity - mean(log_Maximum.longevity)) / sd(log_Maximum.longevity))

#Filtering the data so it only includes Indonesian frugivores
data3_log.z$Category <- factor(data3_log.z$Category)
data3_log.z <- data3_log.z[data3_log.z$Trophic.Niche == "Frugivore", ]
data3_log.z <- data3_log.z %>%
  filter(!is.na(Trophic.Niche))
data3_log.z <- left_join(birds_indonesia, data3_log.z, by = "Species")
data3_log.z <- data3_log.z %>%
  filter(!is.na(Trophic.Niche))

Species_trait_matrix_threat <- data3_log.z %>%
  select(Species, major_threat_binary, Category)

#Making a traits matrix
data3_traits <- data3_log.z %>% 
  select(Species, Beak.Width_z, Hand.Wing.Index_z, Mass_z, Maximum.longevity_z)

Species_trait_matrix <- data.frame(data3_traits, row.names = 1)

#Adding various levels to the extinction threat matrix
Species_trait_matrix_threat <- Species_trait_matrix_threat %>%
  mutate(major_threat_binary_VU = ifelse(major_threat_binary == 1 & Category %in% c("VU", "EN", "CR"), 1, 0))
Species_trait_matrix_threat <- Species_trait_matrix_threat %>%
  mutate(major_threat_binary_EN = ifelse(major_threat_binary == 1 & Category %in% c("EN", "CR"), 1, 0))
Species_trait_matrix_threat <- Species_trait_matrix_threat %>%
  mutate(major_threat_binary_CR = ifelse(major_threat_binary == 1 & Category %in% c("CR"), 1, 0))
Species_trait_matrix_threat <- Species_trait_matrix_threat %>%
  mutate(all_forest_dependent = 1)

Species_threat_matrix_long <- data.frame(Species = Species_trait_matrix_threat$Species, Threat = Species_trait_matrix_threat$all_forest_dependent)
Species_threat_matrix <- Species_threat_matrix_long %>%
  pivot_wider(names_from = Species, values_from = Threat)

rownames(Species_threat_matrix)[rownames(Species_threat_matrix) == "1"] <- "Trade_threatened"

Species_threat_matrix <- as.matrix(Species_threat_matrix)

#Calculating FRic in the mFD package
fspace <- mFD::tr.cont.fspace(sp_tr = Species_trait_matrix, pca = TRUE, 
                              nb_dim = 4, scaling = "no_scale", compute_corr = "pearson")

trade_threat_fd_indices <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = fspace$"sp_faxes_coord"[ , c("PC1", "PC2", "PC3")], 
  asb_sp_w         = Species_threat_matrix,                                
  ind_vect         = c("fric", "fspe"),
  scaling          = FALSE,
  check_input      = TRUE,
  details_returned = TRUE)

fd_ind_values <- trade_threat_fd_indices$"functional_diversity_indices"
fd_ind_values


####Calculating FRic for trade-threatened and endangered Indonesian frugivores####

Species_threat_matrix_long <- data.frame(Species = Species_trait_matrix_threat$Species, Threat = Species_trait_matrix_threat$major_threat_binary_VU)
Species_threat_matrix_long$Threat <- 1 - (Species_threat_matrix_long$Threat)
Species_threat_matrix <- Species_threat_matrix_long %>%
  pivot_wider(names_from = Species, values_from = Threat)

rownames(Species_threat_matrix)[rownames(Species_threat_matrix) == "1"] <- "Trade_threatened"

Species_threat_matrix <- as.matrix(Species_threat_matrix)

#Calculating FRic in the mFD package
fspace <- mFD::tr.cont.fspace(sp_tr = Species_trait_matrix, pca = TRUE, 
                              nb_dim = 4, scaling = "no_scale", compute_corr = "pearson")

trade_threat_fd_indices <- mFD::alpha.fd.multidim(
  sp_faxes_coord   = fspace$"sp_faxes_coord"[ , c("PC1", "PC2", "PC3")], 
  asb_sp_w         = Species_threat_matrix,                                
  ind_vect         = c("fric", "fspe"),
  scaling          = FALSE,
  check_input      = TRUE,
  details_returned = TRUE)

fd_ind_values <- trade_threat_fd_indices$"functional_diversity_indices"
fd_ind_values


####Plotting functional space for trade-threatened and endangered Indonesian species####

#Filtering to just include Indonesian frugivorous species
data3_log.z$Category <- factor(data3_log.z$Category)
data3_log.z <- data3_log.z[data3_log.z$Trophic.Niche == "Frugivore", ]
data3_log.z <- data3_log.z %>%
  filter(!is.na(Trophic.Niche))
data3_log.z <- left_join(birds_indonesia, data3_log.z, by = "Species")
data3_log.z <- data3_log.z %>%
  filter(!is.na(Trophic.Niche))

#Creating the threat matrix for different levels
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
Species_trait_matrix_threat <- Species_trait_matrix_threat %>%
  mutate(all_forest_dependent = 1)

Species_trait_matrix_threat$major_threat_binary_VU <- factor(Species_trait_matrix_threat$major_threat_binary_VU)
Species_trait_matrix_threat$major_threat_binary_EN <- factor(Species_trait_matrix_threat$major_threat_binary_EN)
Species_trait_matrix_threat$major_threat_binary_CR <- factor(Species_trait_matrix_threat$major_threat_binary_CR)
Species_trait_matrix_threat$all_forest_dependent <- factor(Species_trait_matrix_threat$all_forest_dependent)

#Creating necessary data for plotting
PCA_Traded <- princomp(Species_trait_matrix)

trait_space_traded_1_2 <- funspace(x = PCA_Traded, PCs = c(1,2), n_divisions = 300)

funtest <- funspace(x = PCA_Traded, PCs = c(1,2), group.vec = Species_trait_matrix_threat$major_threat_binary_VU, n_divisions = 300)

#Plotting and saving the functional space
jpeg("fspace_indonesia_no_threatened.jpg", width = 2000, height = 2000, res = 300)

plot(x = funtest,
     type = "groups",
     which.group = "0",
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
     xlim = c(-6, 8),
     ylim = c(-5, 10), 
     axis.title.cex = 1.2
)

dev.off()

####Plotting functional space for all Indonesian frugivores####

funtest <- funspace(x = PCA_Traded, PCs = c(1,2), group.vec = Species_trait_matrix_threat$all_forest_dependent, n_divisions = 300)

#Plotting and saving the functional space
jpeg("fspace_indonesia_all.jpg", width = 2000, height = 2000, res = 300)

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
     xlim = c(-6, 8),
     ylim = c(-5, 10), 
     axis.title.cex = 1.2
)

dev.off()

#########

#The FRic scores and plots can be made for the Brazilian frugivores and for all frugivores
#by using the same code but with a different subset of species.