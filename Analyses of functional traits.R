####Binary logistic regression of body mass####

#Transforming body mass by log10
data3$scaled_Mass <- log10(data3$Mass)

#Fitting the glm for log10 body mass 
glm_scaled_Mass <- glm(major_threat_binary ~ scaled_Mass,
                       family=binomial(link='logit'),
                       data=data3)
summary(glm_scaled_Mass)

#Check of how the data looks
test_plot <- ggplot(data3, aes(scaled_Mass, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0, height = 0.1, color = "black") +
  labs(x = "scaled_Mass", y = "P(Threatened by trade)")
test_plot

#Estimating probabilities and 95% confidence intervals
numbers <- seq(0.1, 4, 0.1)
predicted <- predict(glm_scaled_Mass,
                     newdata = data.frame(scaled_Mass = numbers),
                     type = "link", se = TRUE)
model_scaled_Mass <- data.frame(scaled_Mass = numbers,
                                estimate = boot::inv.logit(predicted$fit),
                                confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                                confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

#Plotting and saving the logistic regression for mass
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


####Mixed effects binary logistic regression for mass and taxonomic order####

library(lme4)

#Fitting the glmer for mass with order as a random effect
model <- glmer(major_threat_binary ~ scaled_Mass + (scaled_Mass|Order), family = binomial(), data = data3)
coef(model)

#Estimating the probabilities and 95% confidence intervals for each order
newdata <- data3 %>% 
  group_by(Order) %>% 
  summarise(scaled_Mass = seq(from = min(scaled_Mass), to = max(scaled_Mass), length.out = 1000))

predicted <- predict(model,
                     newdata = newdata,
                     type = "link", se = TRUE)

model_Scaled_Mass <- data.frame(Order = newdata$Order,
                                      Scaled_Mass = newdata$scaled_Mass,
                                      estimate = boot::inv.logit(predicted$fit),
                                      confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                                      confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

#Selecting data and model estimations for only the orders with the highest trade-threatened species richness
model_Scaled_Mass <- model_Scaled_Mass[model_Scaled_Mass$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]
data3_specific_Order <- data3[data3$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]

#Plotting and saving the the mixed effects linear regression for mass and order
glmer_plot <- 
  ggplot(model_Scaled_Mass, aes(Scaled_Mass, estimate)) +
  geom_jitter(data = data3_specific_Order, aes(scaled_Mass, major_threat_binary), width = 0, height = 0.01, alpha = 0.15) +
  geom_ribbon(aes(ymin = confidence_low, ymax = confidence_high), fill = "#A8D5BA") +
  geom_line() +
  theme_minimal() +
  labs(x = expression("log"[10]*" Mass (g)"), y = "P(Threatened by trade)") +
  facet_wrap(Order ~ .,) +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.x = element_text(size = 19),
    axis.title.y = element_text(size = 19, margin = margin(r = 15)),
    strip.text = element_text(size = 16)
  )

glmer_plot

ggsave("plot_glmer_Mass_final.jpeg", plot = glmer_plot, width = 20, height = 15, units = "cm")


####Mixed effects binary linear regression for mass and trophic niche####

#Fitting the glmer for mass with trophic niche as a random effect
model <- glmer(major_threat_binary ~ scaled_Mass + (scaled_Mass|Trophic.Niche), family = binomial(), data = data3)
coef(model)

#Estimating the probabilities and 95% confidence intervals for each trophic niche
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

#Selecting data and model estimations for only the most speciose trophic niches 
model_Scaled_Mass <- model_Scaled_Mass[model_Scaled_Mass$Trophic.Niche %in% c("Frugivore", "Invertivore", "Nectarivore", "Omnivore"), ]
data3_specific_Trophic.Niche <- data3[data3$Trophic.Niche %in% c("Frugivore", "Invertivore", "Nectarivore", "Omnivore"), ]

#This removes the cassowaries from the plots since only these three speices are larger than 10,000 grams
data3_specific_Trophic.Niche <- data3_specific_Trophic.Niche %>% filter(scaled_Mass <= 4)

#Plotting and saving the mixed effects linear regression for mass and trophic niche
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


####Binary logistic regressions for beak width####

#Correlation test between beak width and body mass
cor.test(x=data3$Mass, y=data3$Beak.Width, method = "spearman")

#Transforming beak width by log10
data3$scaled_Beak.Width <- log10(data3$Beak.Width)

#Fitting the glm for log10 beak width
glm_Beak.Width <- glm(major_threat_binary ~ scaled_Beak.Width,
                      family=binomial(link='logit'),
                      data=data3)
summary(glm_Beak.Width)

#Quick check to visualise the data
test_plot <- ggplot(data3, aes(scaled_Beak.Width, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0, height = 0.1, color = "black") +
  labs(x = "scaled_Beak.Width", y = "P(Threatened by trade)")
test_plot

#Estimating probabilities and 95% confidence intervals
numbers <- seq(0, 1.7, 0.005)
predicted <- predict(glm_Beak.Width,
                     newdata = data.frame(scaled_Beak.Width = numbers),
                     type = "link", se = TRUE)
model_Beak.Width <- data.frame(scaled_Beak.Width = numbers,
                               estimate = boot::inv.logit(predicted$fit),
                               confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                               confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

#Plotting and saving the binary logistic regression for log10 beak width
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


#Fitting the glmer for beak width with order as a random effect
model <- glmer(major_threat_binary ~ scaled_Beak.Width + (scaled_Beak.Width|Order), family = binomial(), data = data3)
coef(model)

#Estimating probabilities and 95% confidence intervals for each order
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

#Selecting data and model estimations for only the orders with the highest trade-threatened species richness
model_Scaled_Beak.Width <- model_Scaled_Beak.Width[model_Scaled_Beak.Width$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]
data3_specific_Order <- data3[data3$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]

#Plotting and saving the mixed effects logistic regression for beak width and order
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


####Binary logistic regressions for hand-wing index####

#Correlation test between hand-wing index and body mass
cor.test(x=data3$Mass, y=data3$Hand.Wing.Index, method = "spearman")

#Fitting a binary logistic regression for hand-wing index 
glm_Hand.Wing.Index <- glm(major_threat_binary ~ Hand.Wing.Index,
                           family=binomial(link='logit'),
                           data=data3)
summary(glm_Hand.Wing.Index)

#Check of what the data looks like 
test_plot <- ggplot(data3, aes(Hand.Wing.Index, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0, height = 0.1, color = "black") +
  labs(x = "Hand.Wing.Index", y = "P(Threatened by trade)")
test_plot

#Estimating probabilities and 95% confidence intervals from hand-wing index model
numbers <- seq(0, 75, 0.1)
predicted <- predict(glm_Hand.Wing.Index,
                     newdata = data.frame(Hand.Wing.Index = numbers),
                     type = "link", se = TRUE)
model_Hand.Wing.Index <- data.frame(Hand.Wing.Index = numbers,
                                    estimate = boot::inv.logit(predicted$fit),
                                    confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                                    confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

#Plotting and saving the binary logistic regression of hand-wing index 
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

ggsave("plot_Hand.Wing.Index_final.jpeg", plot = plot_Hand.Wing.Index, width = 15, height = 15, units = "cm")


#Fitting the glmer for hand-wing index with order as a random effect
model <- glmer(major_threat_binary ~ Hand.Wing.Index + (Hand.Wing.Index|Order), family = binomial(), data = data3)
coef(model)

#Estimating probabilities and 95% confidence intervals for each order
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

#Selecting data and model estimations for only the orders with the highest trade-threatened species richness
model_Hand.Wing.Index <- model_Hand.Wing.Index[model_Hand.Wing.Index$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]
data3_specific_Order <- data3[data3$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]

#Plotting and saving the mixed effects linear regression for hand-wing index and order
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


####Binary logistic regressions for generation length####

#Correlation test between generation length and body mass
cor.test(x=data3$Mass, y=data3$GenLength, method = "spearman")

#Transforming generation length by log10
data3$scaled_GenLength <- log10(data3$GenLength)

#Fitting the glm for log10 generation length
glm_GenLength <- glm(major_threat_binary ~ scaled_GenLength,
                     family=binomial(link='logit'),
                     data=data3)
summary(glm_GenLength)

#Visualising the data as a check
test_plot <- ggplot(data3, aes(scaled_GenLength, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0, height = 0.1, color = "black") +
  labs(x = "scaled_GenLength", y = "P(Threatened by trade)")
test_plot

#Estimating the probabilities and 95% confidence intervals
numbers <- seq(0.2, 1.25, 0.005)
predicted <- predict(glm_GenLength,
                     newdata = data.frame(scaled_GenLength = numbers),
                     type = "link", se = TRUE)
model_GenLength <- data.frame(scaled_GenLength = numbers,
                              estimate = boot::inv.logit(predicted$fit),
                              confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                              confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

#Plotting and saving the binary logistic regression for generation length
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


#Fitting the glmer for log10 generation length with order as a random effect
model <- glmer(major_threat_binary ~ scaled_GenLength + (scaled_GenLength|Order), family = binomial(), data = data3)
coef(model)

#Estimating the probabilities and 95% confidence intervals for each order
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

#Selecting data and model estimations for only the orders with the highest trade-threatened species richness
model_Scaled_GenLength <- model_Scaled_GenLength[model_Scaled_GenLength$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]
data3_specific_Order <- data3[data3$Order %in% c("Bucerotiformes", "Columbiformes", "Galliformes", "Psittaciformes"), ]

#Plotting and saving the mixed effects binary logistic regression for generation length and order
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

