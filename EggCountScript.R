
library(dplyr)
library(purrr)
library(broom)
library(ggplot2)
library(readxl)

egg_data = read_excel("C:/Users/henry/Documents/ABWData/ABWEggAnalysis.xlsx")
egg_data$location = egg_data$Site
egg_data$date = egg_data$Date
egg_data$egg_count = egg_data$Eggs
egg_data$date = as.Date(egg_data$date)

egg_data$location <- as.factor(egg_data$location)

eggsOSU = egg_data %>% filter(Site %in% c("OSU")) 
eggsHW = egg_data %>% filter(Site %in% c("HW")) 
eggsMF = egg_data %>% filter(Site %in% c("MF")) 
eggsBA = egg_data %>% filter(Site %in% c("BA")) 

ggplot(eggsMF, aes(x = Date, y = Eggs))+
  stat_summary(fun = mean, geom = "bar")+
  theme_classic()


models_by_location <- egg_data %>%
  group_by(location) %>%
  summarize(model = list(glm(egg_count ~ date, data = cur_data(), family = poisson)))


new_data <- expand.grid(
  date = unique(egg_data$date),
  location = levels(egg_data$location)
)

predictions <- new_data %>%
  rowwise() %>%
  mutate(predicted_counts = predict(models_by_location$model[[which(levels(egg_data$location) == location)]], newdata = data.frame(date = date)))


egg_data_with_predictions <- egg_data %>%
  left_join(predictions, by = c("date", "location"))

nb_model <- glm.nb(egg_count ~ date + location, data = egg_data)
summary(nb_model)

ggplot(egg_data_with_predictions, aes(x = date)) +
  
  geom_point(aes(y = egg_count, color = location)) +
  geom_jitter(aes(y = egg_count, color = location), width = 0.4, height = 0.3, size = 1) +  
  geom_line(aes(y = predicted_counts, color = location), size = 1) +
  labs(title = "Poisson Regression: Egg Counts vs. Date by Location", 
       x = "Date", 
       y = "Egg Counts",
       color = "Site Location") +
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size = 24))+
  scale_color_manual(values = c("blue", "red", "green", "orange"), labels = c("Blue Ash", "Heatherwoode", "Muirfield Village", "OSU GC"))

ggplot(egg_data_with_predictions, aes(x = date)) +
  
  geom_point(aes(y = egg_count, color = location)) +
  geom_jitter(aes(y = egg_count, color = location), width = 0.4, height = 0.3, size = 1) +  
  labs(title = "ABW Egg Counts vs. Date by Location", 
       x = "Date", 
       y = "Egg Counts",
       color = "Site Location") +
  theme_classic() +
  theme (plot.title = element_text(hjust = 0.5, size = 24))+
  scale_color_manual(values = c("blue", "red", "green", "orange"), labels = c("Blue Ash", "Heatherwoode", "Muirfield Village", "OSU GC"))


