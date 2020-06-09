
library(sf)
library(tidyverse)
library(openxlsx)
library(lwgeom)

# 1. Clean street dictionary

dict <- read.xlsx('data/street_width_2014.xlsx')
dict <- dict %>% 
  mutate(
    name = str_replace_all(string = name, pattern = '^ул\\.', replacement = ' '),
    name = str_replace_all(string = name, pattern = '^пер\\.', replacement = ' '),
    name = str_replace_all(string = name, pattern = '^пл\\.', replacement = ' '),
    name = str_replace_all(string = name, pattern = '[:space:][:space:]', replacement = ' '),
    name = str_replace_all(string = name, pattern = '^[:space:]|[:space:]$', replacement = ''),
    name = tolower(name)
  )

# street_types <- dict %>% 
#   mutate(type = str_replace_all(name, pattern = '\\. .*', replacement = '')) %>% 
#   group_by(type) %>% 
#   summarise(n = n())

street_stats <- dict %>%
  group_by(name) %>% 
  summarise(n = n(), std = sd(width)) %>% 
  arrange(desc(n))

# Проблема - есть очень длинные улицы, которые имеют разную ширину на разных участках
# - Вариант 1: там, где разброс в ширине небольшой, то просто усреднять
# - Вариант 2: работать точечно с оставшимися участками

all_streets <- street_stats %>% pull(name)
clean_streets <- street_stats %>% filter(n == 1) %>% pull(name)
alt1_streets <- street_stats %>% filter(std < 2) %>% pull(name)
alt2_streets <- street_stats %>% filter(std >= 2) %>% pull(name)

dict_1 <- dict %>% filter(name %in% clean_streets)
dict_2 <- dict %>% filter(name %in% alt1_streets) %>% group_by(name) %>% summarise(length = sum(length), width = mean(width))

clean_dict <- rbind(dict_1, dict_2)

# 2. Try to join data

roads <- readRDS('data/astra_roads.rds')
roads$length <- st_length(roads)
roads <- roads %>% 
  mutate(
    name = str_replace_all(string = name, pattern = 'улица|переулок|площадь', replacement = ''),
    name = str_replace_all(string = name, pattern = '[:space:][:space:]', replacement = ' '),
    name = str_replace_all(string = name, pattern = '^[:space:]|[:space:]$', replacement = ''),
    name = tolower(name)
  )

roads2 <- roads %>% filter(name %in% all_streets)

roads_wo_data <- roads %>% 
  filter(!(name %in% all_streets)) %>% 
  group_by(name) %>% 
  summarise(len = sum(length) %>% as.numeric()) %>% 
  arrange(desc(len))

ggplot(roads_wo_data, aes(x = len)) + geom_histogram()
ggplot() + geom_sf(data = roads2, inherit.aes = F)



# Сколько осталось распознать улица длиной более 300 метров
roads_wo_data %>% filter(len > 300) %>% nrow()

# 3. Try to split streets in groups by width and map em

# Classic hierarchy:
# 1. pedestians
# 2. bikes
# 3. public transit
# 4. taxis
# 5. carpool
# 6. single occupancy vehicles

# biclycles: 2 m. / 4 m.
# + bus/tram: 3.6 m. / 7.2 m. // 11.2 m.
# + car: 3 m. / 6 m. // 17.2 m.

# Варианты:
# Где можно сделать велодорожки (везде)
# Где можно сделать автобус (1) + велодорожки (2)
# Где можно сделать автобус (2) + велодорожки (2)
# Где можно сделать автобус (1) + обычные полосы (1)
# Где можно сделать автобус (2) + обычные полосы (2)
# Где можно сделать автобус (2) + обычные полосы (2) + велодорожки (2)

roads3 <- roads2 %>% 
  left_join(clean_dict, by = 'name')

ggplot(roads3, aes(x = width)) + geom_histogram()

roads3 <- roads3 %>% 
  mutate(
    type = case_when(
      width <= 6.2 ~ 'bicycle',
      width > 6.2 & width <= 14.2 ~ 'bus/tram + bicycle',
      TRUE ~ 'cars + bus/tram + bicycle'
    )
  )

table(roads3$type)



ggplot() +
  geom_sf(data = roads3, inherit.aes = F, col = 'grey70') +
  geom_sf(data = roads3 %>% filter(type == 'bus/tram + bicycle'), inherit.aes = F, col = 'red')

ggplot() +
  geom_sf(data = roads3, inherit.aes = F, col = 'grey70') +
  geom_sf(data = roads3 %>% filter(type == 'bicycle'), inherit.aes = F, col = 'red')












