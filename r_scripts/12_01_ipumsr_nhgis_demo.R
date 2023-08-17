library(tidyverse)     # tidyness
library(janitor)     # normalizing variable names
library(ipumsr)     # see https://cran.r-project.org/web/packages/ipumsr/vignettes/ipums.html
library(sf)     # trying this out instead of GIS for initial run through ipumsr's nhgis vignette
library(viridis)

# Here's the NHGIS & API vignettes
vignette("ipums-nhgis", package = "ipumsr")
vignette("ipums-api", package = "ipumsr")

# Here's how to request NHGIS extracts from the API in Base R:
# https://developer.ipums.org/docs/workflows/create_extracts/nhgis_data/

# IPUMSR lets your read nhgis files without even unzipping them

nhgis_csv <- "data_raw/nhgis0004_csv.zip"
nhgis_shp <- "data_raw/nhgis0004_shape.zip"

nhgis_ddi <- read_ipums_codebook(nhgis_csv)     # Contains metadata, nice to have as separate object

cat(ipums_file_info(nhgis_ddi, "conditions"))     # Citation info

nhgis_varlist <- tibble(nhgis_ddi$var_info) 

nhgis <- read_nhgis_sf(
  data_file = nhgis_csv,
  shape_file = nhgis_shp     # "Projected CRS: USA_Contiguous_Albers_Equal_Area_Conic"
) %>% 
  clean_names() %>% 
  filter(statenam == "New Jersey") %>%     # 584 people in Bergen County were listed on the 1830 Census as "Nonwhite, Slave"; 1,894 as "Nonwhite, Free"
  remove_constant()

# add a couple of variables for proportions

nhgis <- nhgis %>% 
  mutate(pop_wh = abo001 + abo002,
         pop_nonwh = abo003 + abo004 + abo005 + ABO006,
         pop_ensl = abo003 + abo004,
         pop_tot = abo001 + abo002 + abo003 + abo004 + abo005 + ABO006,
         pct_ensl = pop_ensl / pop_tot
         )

nhgis %>% 
  select(nhgisnam, pop_ensl, pct_ensl) %>% 
  arrange(desc(pop_ensl))     # Bergen County had the highest population of enslaved people as both raw count & % of total
                              # I wonder what NYC's was?

# they use a ggplot2 map - oooo this works for NJ counties, just was slow!
ggplot(data = nhgis, 
       aes(fill = pct_ensl)) +
  geom_sf() + 
  scale_fill_continuous("", labels = scales::percent) + 
  labs(
    title = "Percent of Population that was Enslaved, by County",
    subtitle = "1830 Census",
    caption = paste0("Source: ", ipums_file_info(nhgis_ddi, "ipums_project"))
  )


