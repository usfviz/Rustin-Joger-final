# This script preprocesses the data needed for the visualizations.
# Zipcode to county code data
# https://www.census.gov/geo/maps-data/data/zcta_rel_download.html
# County code to county name data
# https://www.census.gov/geo/reference/codes/cou.html

rm(list = ls())
cat('\014')

library(tm)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggvis)

setwd('~/Github/data_viz/Rustin-Joger-/project-prototype/')

# Helper function to capitalize first letter of each word in a string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2), collapse=" ")
}

# Lending Club data
dat <- read.csv('loan.csv', stringsAsFactors = F)

# Census data
zip_to_county <- read.csv('zcta_county_rel_10.csv', stringsAsFactors = F)
county_code <- read.csv('census_county_codes.csv', header = F, stringsAsFactors = F)
colnames(county_code) <- c('state', 'state_code', 'county_code', 'county', 'class_code')
county_code$county <- trimws(removeWords(county_code$county, c("County", "Parish")))
state_code <- read.table('census_state_codes.txt', header = T, sep = "|", stringsAsFactors = F)

# Clean the zip codes and counties
zip_to_county <- zip_to_county %>%
  select(ZCTA5, STATE, COUNTY, POPPT) %>%
  mutate(zip_code = substring(as.character(ZCTA5), 1, 3)) %>%
  left_join(county_code, by = c('COUNTY' = 'county_code', 'STATE' = 'state_code')) %>%
  left_join(state_code, by = 'STATE') %>%
  mutate(county = tolower(county)) %>%
  group_by(STUSAB, STATE_NAME, county, zip_code) %>%
  summarise(pop = sum(as.numeric(POPPT) / 1000))

# Aggregate data to county level
dat_summ_county <- dat %>%
  mutate(zip_code = substring(zip_code, 1, 3),
         row_num = 1:nrow(dat)) %>%
  left_join(zip_to_county, by = 'zip_code') %>%
  group_by(row_num) %>%
  slice(1) %>%
  group_by(county) %>%
  summarise(cnt = n(),
            pop = max(pop))

# County data that will be used for plotting
counties <- map_data("county") %>%
  left_join(dat_summ_county, by = c('subregion' = 'county'))
counties$Region <- apply(matrix(counties$region, ncol = 1),
                         1, simpleCap)
counties$Subregion <- apply(matrix(counties$subregion, ncol = 1),
                            1, simpleCap)
# write.csv(counties, 'map_county_data.csv', row.names = F)


# Preprocessing state level data
state_pop <- zip_to_county %>%
  group_by(state) %>%
  summarise(pop = sum(as.numeric(POPPT) / 1000))
dat_summ_state <- dat %>%
  group_by(addr_state) %>%
  summarise(cnt = n()) %>%
  left_join(state_pop, by = c('addr_state' = 'state'))

state_abb <- data.frame(abb = state.abb, full = state.name, stringsAsFactors = F)
all_states <- map_data("state")
all_states$Region <- apply(matrix(data = all_states$region, ncol = 1),
                           1, simpleCap)
all_states <- all_states %>%
  left_join(state_abb, by = c('Region' = 'full')) %>%
  left_join(dat_summ_state, by = c('abb' = 'addr_state'))
# write.csv(all_states, 'map_state_data.csv', row.names = F)



# Sample plots
counties %>%
  filter(Region == 'Texas') %>%
  mutate(loan_per_pop = cnt / pop) %>%
  group_by(group, Subregion) %>%
  ggvis(~long, ~lat) %>%
  layer_paths(fill = ~loan_per_pop) %>%
  scale_numeric(property = 'fill', domain = c(0, 5), clamp = T,
                range = c("#fee8c8", "#e34a33")) %>%
  hide_axis("x") %>% 
  hide_axis("y")

all_states %>%
  mutate(loan_per_pop = cnt / pop) %>%
  group_by(group, Region) %>%
  ggvis(~long, ~lat) %>%
  layer_paths(fill = ~loan_per_pop) %>%
  scale_numeric(property = 'fill', domain = c(0, 5), clamp = T,
                range = c("#fee8c8", "#e34a33")) %>%
  hide_axis("x") %>% 
  hide_axis("y")
