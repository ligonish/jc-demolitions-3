library(tidyverse)
library(tidycensus)
library(janitor)
library(sf)
library(ggmap)
options(tigris_use_cache = TRUE)

# see https://walker-data.com/tidycensus/articles/spatial-data.html

# Get 2018 5-yr estimate variables

v18 <- load_variables(2018, "acs5")

# Get mortgage data for Hudson County
mortgages_tract_18 <- get_acs(
  state = "NJ",
  county = "Hudson County",
  geography = "tract",
  table = "B25096",
  summary_var = "B25096_001",
  #variables = c("val_by_mortgage" = "B25096"),
  geometry = TRUE,
  year = 2018,
  #cb = FALSE,
  #output = "wide",
  cache_table = T) %>% 
  mutate(pct_tot = round(estimate/summary_est, 2), .after = estimate) 

# % Mortgaged
mortgages_tract_18 %>%
  filter(variable == "B25096_002") %>% 
  mutate(percent = 100 * (estimate / summary_est)) %>%
  ggplot(aes(fill = percent)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c() 

# % Not Mortgaged
mortgages_tract_18 %>%
  filter(variable == "B25096_012") %>% 
  mutate(percent = 100 * (estimate / summary_est)) %>%
  ggplot(aes(fill = percent)) + 
  geom_sf(color = NA) + 
  scale_fill_viridis_c()

# Faceted by whatever
mortgages_tract_18 %>%
  mutate(percent = 100 * (estimate / summary_est)) %>%
  ggplot(aes(fill = percent)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "% of all owner-occupied units\n(2018 Census)")

# Tenure, by race: B25003 ----------------------------------
tenure_tract_18 <- get_acs(
  state = "NJ",
  county = "Hudson County",
  geography = "tract",
  table = "B25003",
  summary_var = "B25003_001",
  #variables = c("val_by_mortgage" = "B25096"),
  geometry = TRUE,
  year = 2018,
  #cb = FALSE,
  #output = "wide",
  cache_table = T) %>% 
  mutate(percent = round(100 * (estimate / summary_est)), .after = estimate)
  

# Faceted by whatever
tenure_tract_18 %>%
  mutate(
    variable = case_when(variable == "B25003_001" ~ "All",
                         variable == "B25003_002" ~ "Owner-Occupied",
                         variable == "B25003_003" ~ "Renter-Occupied")
  ) %>% 
  filter(variable != "All") %>%
  ggplot(aes(fill = percent)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "% tenure (2018 ACS 5-yr)")

# White alone, not HL = B25003H
wh_tenure_tract_18 <- get_acs(
  state = "NJ",
  county = "Hudson County",
  geography = "tract",
  table = "B25003H",
  summary_var = "B25003H_001",
  geometry = TRUE,
  year = 2018,
  cb = FALSE,
  #output = "wide",
  cache_table = T) %>% 
  mutate(percent = round(100 * (estimate / summary_est)), .after = estimate)

wh_tenure_tract_18 %>%
  mutate(
    variable = case_when(variable == "B25003H_001" ~ "All",
                         variable == "B25003H_002" ~ "Owner-Occupied",
                         variable == "B25003H_003" ~ "Renter-Occupied")
  ) %>% 
  filter(variable != "All") %>%
  ggplot(aes(fill = percent)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "% tenure (2018 ACS 5-yr)")

# Trying for general tenure w/ race variables

race_tenure_18 <- get_acs(
  state = "NJ",
  county = "Hudson County",
  geography = "tract",
  variables = c("B25003_001", 
                "B25003_002", 
                "B25003_003", 
                "B25003H_001", 
                "B25003H_002", 
                "B25003H_003",
                "B25003B_001",
                "B25003B_002",
                "B25003B_003",
                "B25003I_001",
                "B25003I_002",
                "B25003I_003"
                ),
  geometry = TRUE,
  year = 2018,
  cb = FALSE,
  output = "wide",
  cache_table = T) %>%
  mutate(pct_w_a = 100 * (B25003H_001E / B25003_001E)) %>%
  mutate(pct_n_w_a = 100 * ((B25003_001E - B25003H_001E) / B25003_001E))


# Saving for GIS map
st_write(race_tenure_18, "tidycensus_shapefiles/tenure_18.shp", layer_options = "GEOMETRY=AS_XY")

  
  

# Wow: tracts primarily nonwhite -----------------------------------------------
race_tenure_18 %>% 
  mutate(percent_non_wh = 100 * ((B25003_001E - B25003H_001E) / B25003_001E)) %>%
  select(GEOID, percent_non_wh, geometry) %>% 
  ggplot(aes(fill = percent_non_wh)) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "% non white hh (2018 ACS 5-yr)")

race_tenure_18 %>% 
  mutate(percent_h = 100 * (B25003I_001E / B25003_001E)) %>%
  select(GEOID, percent_h, geometry) %>% 
  ggplot(aes(fill = percent_h)) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "% h hh (2018 ACS 5-yr)")

# Rent burden: B25071_001
rentburden_tract_18 <- get_acs(
  state = "NJ",
  county = "Hudson County",
  geography = "tract",
  variable = "B25071_001",
  geometry = TRUE,
  year = 2018,
  cb = FALSE,
  cache_table = T) %>% 
  mutate(over30 = (estimate > 30),
         over50 = (estimate > 50))

# also boom --------------------------------------------------
rentburden_tract_18 %>% 
  ggplot(aes(fill = estimate)) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "median hh rent-burdened (2018 ACS 5-yr)")

# Occupancy Status: B25002_001, 2, 3 -------------------------------------------------

vacant_tract_18 <- get_acs(
  state = "NJ",
  county = "Hudson County",
  geography = "zcta",
  zcta = c("07302", "07304", "07305", "07306", "07307", "07310"),
  table = "B25002",
  summary_var = "B25002_001",
  geometry = TRUE,
  year = 2018,
  cb = FALSE,
  cache_table = T) %>% 
  mutate(pct_tot = round(((estimate/summary_est)), 2), .after = estimate) 

  #mutate(pct_tot = round(((estimate/summary_est)* 100), 2), .after = estimate) 

vacant_tract_18 %>% 
  filter(variable == "B25002_003") %>% 
  ggplot(aes(fill = pct_tot)) +
  geom_sf(color = NA, alpha = .9) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "% units vacant (2018 ACS 5-yr)")

vacant_tract_18 %>% 
  filter(variable == "B25002_003") %>%  
  ggplot() +
  geom_sf(
    aes(fill = pct_tot), 
    lwd = 0,
    colour = "white") +
  scale_fill_viridis_c(
    direction = -1,
    labels = scales::percent,
    name = "Percent vacant housing units") +
  theme_void()+
  theme(
    text = element_text(family = "Roboto Condensed"),
    legend.title = element_text(family = "Roboto Condensed", size = 10),
    legend.text = element_text(family = "Roboto Condensed", size = 10)
  )

jc <- c(lon = -74.0431, lat = 40.7178)
jc_map <- get_map(location = jc,
                  source = "stamen",
                  maptype = "toner",
                  zoom = 12,
                  scale = 1)
zips <- c("07302", "07304", "07305", )

#Quickie map
ggmap(jc_map) +
  geom_sf(data = filter(vacant_tract_18, variable == "B25002_003"),
          color = NA,
          aes(fill = pct_tot), 
          inherit.aes = FALSE) +
  scale_fill_viridis_c(alpha = .9,
                       direction = -1,
                       oob = scales::squish,
                       labels = scales::percent
) + 
  theme_void() + 
  coord_sf(crs = st_crs(4269))+
  theme(
    text = element_text(family = "Roboto Condensed"),
    legend.title = element_text(family = "Roboto Condensed", size = 10),
    legend.text = element_text(family = "Roboto Condensed", size = 10)
  )+
  labs (title = "Five Years Ago, the Heights & JSQ Had the Highest Vacancy Rate",
        fill = "% units vacant (2018 ACS 5-yr)",
        caption = "Source: U.S. Census Bureau 2014-18 occupancy status estimates, table B25002.")

# Barplot
vacant_tract_18 %>% 
  filter(variable == "B25002_003") %>% 
  separate(NAME, sep = " ", into = c("zcta", "zip")) %>% 
  group_by(zip) %>% 
  summarise(pct_vac =mean(pct_tot, na.rm = T)) %>%
  mutate(zip = as_factor(zip),
         zip = fct_reorder(zip, pct_vac)) %>% 
  ggplot(aes(fill = fct_reorder(zip, pct_vac, .desc = T), 
             y = pct_vac, 
             x = fct_reorder(zip, pct_vac, .desc = T))) + 
  geom_bar(position = "dodge", 
           stat = "identity",
           show.legend = T)+
  labs (x = NULL, y = "Percent of All Housing Units Vacant",
        title = "Five Years Ago, the Heights and JSQ \nHad the Highest Vacancy Rates",
        subtitle = "... but they've had the most demolitions since.",
        caption = "Source: U.S. Census Bureau 2014-18 occupancy status estimates, Table B25002.",
        fill = NULL)+
  scale_fill_viridis_d()+
  scale_x_discrete(limits = rev)+
  coord_flip()+
  theme_ipsum_rc(grid = FALSE)+
  theme(axis.text.x=element_blank(), legend.position="none")+
  geom_text(
    aes(label = scales::percent(pct_vac)),
    color = "white",
    size = 4,
    position = position_dodge(.9),
    hjust = 1.25,
    family = "Roboto Condensed"
  )


# Public assistance: B19058

assistance <- get_acs(
  state = "NJ",
  county = "Hudson County",
  geography = "tract",
  table = "B19058",
  summary_var = "B19058_001",
  geometry = TRUE,
  year = 2018,
  cb = FALSE,
  cache_table = T) %>% 
  mutate(pct_tot = round(estimate/summary_est, 2), .after = estimate)


# YUP
assistance %>% 
  filter(variable == "B19058_002") %>% 
  ggplot(aes(fill = pct_tot)) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "% hh receiving pub asst (2018 ACS 5-yr)")

# Tenure duration: B25039
duration <- get_acs(
  state = "NJ",
  county = "Hudson County",
  geography = "tract",
  table = "B25039",
  geometry = TRUE,
  year = 2018,
  cb = FALSE,
  #output = "wide",
  cache_table = T) 

duration %>% 
  ggplot(aes(fill = estimate)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "med yr moved in (2018 ACS 5-yr)")

# Med Income: B19013_001 -------------------------------------
race_medinc_18 <- get_acs(
  state = "NJ",
  county = "Hudson County",
  geography = "tract",
  variables = c("All" = "B19013_001", 
                "Black" = "B19013B_001", 
                "White Alone" = "B19013H_001", 
                "Hispanic or Latinx" = "B19013I_001"),
  geometry = TRUE,
  year = 2018,
  #cb = FALSE,
  #output = "wide",
  cache_table = T)

race_medinc_18 %>% 
  ggplot(aes(fill = estimate)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "med hh inc (2018 ACS 5-yr)")

# MGR B25064, agg rent asked B25062_001; med value B25077, agg. price asked B25086_001
#B25064_001

housing_prices <- get_acs(
  state = "NJ",
  #county = "Hudson County",
  geography = "zcta",
  zcta = c("07302", "07304", "07305", "07306", "07307", "07310"),
  table = "B25085",
  geometry = TRUE,
  year = 2018,
  cb = FALSE,
  #output = "wide",
  cache_table = T) 

housing_prices %>% 
  ggplot(aes(fill = estimate)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "med yr moved in (2018 ACS 5-yr)")

rent_asked <- get_acs(
  state = "NJ",
  geography = "zcta",
  zcta = c("07302", "07304", "07305", "07306", "07307", "07310"),
  table = "B25061",
  geometry = TRUE,
  year = 2018,
  cb = FALSE,
  #output = "wide",
  cache_table = T)

rent_asked %>% 
  ggplot(aes(fill = estimate)) +
  facet_wrap(~variable) +
  geom_sf(color = NA) +
  theme_void() + 
  scale_fill_viridis_c() + 
  labs(fill = "med yr moved in (2018 ACS 5-yr)")



# price test B25085


