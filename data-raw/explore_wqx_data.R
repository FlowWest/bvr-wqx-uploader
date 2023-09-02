# install.packages("tidyverse")
library(tidyverse)

# read data from csv into environment
raw_data <- read_csv('data-raw/resultfromweb081321.csv')

# opens tab to show table
View(raw_data)

# previews dataset in console providing column details
glimpse(raw_data)

# https://r4ds.had.co.nz/transform.html#dplyr-basics
# https://r4ds.had.co.nz/pipes.html#pipes
table(raw_data$ResultDetectionConditionText)
# group by characteristic and calculate stats about the values
var_stats <- raw_data %>% 
  select(CharacteristicName, ResultMeasureValue) %>% 
  mutate(ResultMeasureValue = parse_double(ResultMeasureValue)) %>% 
  filter(!is.na(ResultMeasureValue)) %>% 
  group_by(CharacteristicName) %>% 
  summarise(
    n = n(),
    min = min(ResultMeasureValue, na.rm = TRUE),
    mean = mean(ResultMeasureValue, na.rm = TRUE),
    max = max(ResultMeasureValue, na.rm = TRUE),
    sd = sd(ResultMeasureValue, na.rm = TRUE),
    q25 = quantile(ResultMeasureValue, .25),
    q75 = quantile(ResultMeasureValue, .75),
    IQR = q75 - q25,
    outlier = 1.5 * IQR)

View(var_stats)

# https://r4ds.had.co.nz/data-visualisation.html
# box plots of characteristics with more than 10 records
raw_data %>% 
  mutate(ResultMeasureValue = parse_double(ResultMeasureValue)) %>% 
  filter(!is.na(ResultMeasureValue)) %>% 
  group_by(CharacteristicName) %>% 
  mutate(n = n()) %>% 
  filter(n >= 10) %>% 
  ggplot(aes(y = ResultMeasureValue)) +
  geom_boxplot() +
  facet_wrap(~CharacteristicName, scales = "free_y")

# join outlier table to original data
raw_data %>% 
  mutate(ResultMeasureValue = parse_double(ResultMeasureValue)) %>% 
  left_join(var_stats) %>% 
  filter(ResultMeasureValue >= outlier)

unique(raw_data$CharacteristicName)
raw_data %>% 
  select(CharacteristicName, `ResultMeasure/MeasureUnitCode`) %>% 
  unique() %>% 
  group_by(CharacteristicName) %>% 
  summarise(n = n()) %>% View
