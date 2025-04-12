####Binary logistic regression on taxonomic order####

#Preparing data for glm analysis
data3$Order <- factor(data3$Order)
data3$Order <- relevel(data3$Order, ref = "Passeriformes")

#Fitting the glm and identifying significantly different orders 
glm_Order <- glm(major_threat_binary ~ Order,
                 family = binomial(link='logit'),
                 data=data3)
summary(glm_Order)

#Check of what the data looks like
test_plot <- ggplot(data3, aes(Order, major_threat_binary)) +
  theme_classic() +
  geom_jitter(width = 0.4, height = 0.1, color = "black") +
  labs(x = "Order", y = "P(Threatened by trade)")
test_plot

#Estimating probabilities and 95% confidence intervals
categories_Order <- unique(data3$Order)

predicted <- predict(glm_Order,
                     newdata = data.frame(Order = categories_Order),
                     type = "link", se = TRUE)
model_Order <- data.frame(Order = categories_Order,
                          estimate = boot::inv.logit(predicted$fit),
                          confidence_high = boot::inv.logit(predicted$fit + 1.96 * predicted$se.fit),
                          confidence_low = boot::inv.logit(predicted$fit - 1.96 * predicted$se.fit))

#Taking the subset of orders with over 50 forest-dependent species
model_Order_new <- model_Order[model_Order$Order %in% c("Accipitriformes", "Bucerotiformes", "Caprimulgiformes", "Columbiformes", 
                                            "Coraciiformes", "Cuculiformes", "Galliformes", 
                                            "Passeriformes", "Piciformes", "Psittaciformes", "Strigiformes"), ]

#Creating a dataset of the significance scores for the plot
significance_scores <- data.frame(Order = c("Accipitriformes", "Bucerotiformes", "Caprimulgiformes", "Columbiformes", 
                                            "Coraciiformes", "Cuculiformes", "Galliformes", 
                                            "Passeriformes", "Piciformes", "Psittaciformes", "Strigiformes"),
                                  Significance = c("***", "***", "", "***", "", "**", "***", "", "", "***", ""))
model_Order_new <- left_join(model_Order_new, significance_scores, by = "Order")

#Plotting and saving the binary logistic regression
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


####Pie charts of trade-threatened species proportion across taxonomic order####

#Selecting data needed for the pie chart
pie_chart_data <- data.frame(Order = data3$Order, major_threat_binary = data3$major_threat_binary)

#Selecting which taxonomic order to be looked at
#Here, Psittaciformes are looked at - this can be replaced with any other order
pie_chart_data <- pie_chart_data[pie_chart_data$Order == "Bucerotiformes",]

#Calculating percentages and preparing data for plot
overall_pie <- pie_chart_data %>%
  count(major_threat_binary) %>%
  mutate(Percentage = n / sum(n) * 100) %>%
  mutate(major_threat_binary = factor(major_threat_binary, levels = major_threat_binary))

#Plotting and saving the pie chart
pie_chart <- ggplot(overall_pie, aes(x = "major_threat_binary", y = n, fill = major_threat_binary)) +
  geom_bar(stat = "identity", width = 1) + 
  coord_polar(theta = "y") + 
  scale_fill_manual(values = c("1" = "#DC143C", "0" = "gray")) +
  theme_void() + 
  theme(legend.title = element_blank(),
        legend.position = "none")

pie_chart

ggsave("pie_chart.jpeg", plot = pie_chart, width = 20, height = 20, units = "cm")

