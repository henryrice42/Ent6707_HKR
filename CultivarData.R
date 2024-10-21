library(ggplot2)
library(readxl)
library(dplyr)

grass_data = read_excel("C:/Users/henry/Documents/ABWData/GreenhouseTrial.xlsx")

summary(grass_data$Larvae_count)

average_data = grass_data %>% group_by(Cultivar) %>% summarise(
  average_larva_count = mean(Larvae_count),
  se = sd(Larvae_count)/sqrt(n())
)

ggplot(average_data, aes(x = Cultivar, y = average_larva_count, fill = Cultivar))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_errorbar(aes(ymin = average_larva_count - se, ymax = average_larva_count+se), width = 0.3)+
  labs(title = "Average Number of Larva by Cultivar", x = "Cultivar", y = "Average Larva count")+
  theme_classic()+
    scale_fill_brewer(palette = "Greens")


anova_model = aov(Larvae_count ~ Cultivar, data = grass_data)
summary(anova_model)
