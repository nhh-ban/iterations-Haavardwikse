library(httr)
library(jsonlite)
library(ggplot2)
library(DescTools)
library(tidyverse)
library(magrittr)
library(rlang)
library(lubridate)
library(anytime)
library(readr)
library(yaml)


#### 1: Beginning of script

# Load function for posting GQL-queries and retrieving data: 
source("functions/GQL_function.r")

# The URL we will use is stored below: 

configs <- 
  read_yaml("vegvesen_configs.yml")


gql_metadata_qry <- read_file("gql-queries/station_metadata.gql")

# Let's try submitting the query: 

stations_metadata <-
  GQL(
    query=gql_metadata_qry,
    .url = configs$vegvesen_url
    ) 


#### 2: Transforming metadata

source("functions/data_transformations.r")

stations_metadata_df <- 
  stations_metadata %>% 
  transform_metadata_to_df(.)


#### 3: Testing metadata
source("functions/data_tests.r")
test_stations_metadata(stations_metadata_df)


### 5: Final volume query: 

source("gql-queries/vol_qry.r")

stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) %$% 
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  ggplot(aes(x=from, y=volume)) + 
  geom_line() + 
  theme_classic()

#improving plot
stations_metadata_df %>% 
  filter(latestData > Sys.Date() - days(7)) %>% 
  sample_n(1) -> sampled_station

station_name <- sampled_station$name

sampled_station %$% 
  vol_qry(
    id = id,
    from = to_iso8601(latestData, -4),
    to = to_iso8601(latestData, 0)
  ) %>% 
  GQL(., .url = configs$vegvesen_url) %>%
  transform_volumes() %>% 
  mutate(from = as.POSIXct(from, format="%Y-%m-%dT%H:%M:%S")) %>%
  ggplot(aes(x=from, y=volume, group = 1)) + 
  geom_line(aes(color = station_name)) + 
  geom_point(aes(color = station_name), size = 2, alpha = 0.7) +  # Adjust point size and transparency
  scale_color_brewer(palette = "Set2") +  # Using a Brewer palette for pleasant colors
  theme_minimal() +  # Use a minimal theme for a clean look
  labs(color = 'Traffic Station') + 
  scale_x_datetime(labels = scales::date_format("%Y-%m-%d %H:%M")) 


