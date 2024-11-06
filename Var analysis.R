library(tidyverse)

data <- listings

avg_price <- data %>%
  filter(!is.na(price)) %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(mean_price = mean(price, na.rm = TRUE))

ggplot(avg_price, aes(x = neighbourhood_group, y = mean_price, fill = room_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Price by Neighborhood and Room Type",
       x = "Neighborhood Group", y = "Average Price") +
  theme_minimal()

avg_availability <- data %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(mean_availability = mean(availability_365, na.rm = TRUE))

ggplot(avg_availability, aes(x = neighbourhood_group, y = mean_availability, fill = room_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Availability by Neighborhood and Room Type",
       x = "Neighborhood Group", y = "Average Availability (Days per Year)") +
  theme_minimal()

data_reviews <- data %>% filter(!is.na(reviews_per_month))

avg_reviews <- data_reviews %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(mean_reviews_per_month = mean(reviews_per_month, na.rm = TRUE))

ggplot(avg_reviews, aes(x = neighbourhood_group, y = mean_reviews_per_month, fill = room_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Reviews per Month by Neighborhood and Room Type",
       x = "Neighborhood Group", y = "Average Reviews per Month") +
  theme_minimal()

median_min_nights <- data %>%
  group_by(neighbourhood_group, room_type) %>%
  summarise(median_minimum_nights = median(minimum_nights, na.rm = TRUE))

ggplot(median_min_nights, aes(x = neighbourhood_group, y = median_minimum_nights, fill = room_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Median Minimum Nights Requirement by Neighborhood and Room Type",
       x = "Neighborhood Group", y = "Median Minimum Nights") +
  theme_minimal()

