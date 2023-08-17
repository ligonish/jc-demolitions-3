library(tidyverse)   # data cleaning & manipulation
library(janitor)     # data cleaning & easy table percentages
library(lubridate)   # date variable normalization
library(campfin)     # street address normalization
library(rvest)       # slurping USPS street addresses
library(sf)          # mapping inside R
library(ggmap)       # mapping inside R


# Read in 10/13/2022 data (still not updated as of 12/02/22)
jc_raw <- read_csv("data_raw/jc_raw_2022_11_06.csv") %>% 
  clean_names

# Massive cleaning chunk lifted out of Assignment 2 Notebook
jc_demos <- jc_raw %>% 
  select(recordid,
         block, 
         lot,
         permitstatusdesc,
         permitdate,
         certdate,
         permittypedesc,
         certtypedesc,
         certcount,
         buildfee,
         plumbfee,
         electfee,
         firefee,
         dcafee,
         certfee,
         elevfee,
         otherfee,
         totalfee,
         constcost,
         salegained,
         rentgained,
         usegroup,
         usegroupdesc,
         processdate) %>% 
  mutate(
    lot_ext = str_extract(lot, "(?<=\\.)[:digit:]+"),
    lot = case_when(
      is.na(lot_ext) ~ lot,
      lot_ext > 0 ~ str_extract(lot, "[:digit:]+(?=\\.?)")),
    block_lot = paste0(block, "_", lot),
    ibcnj_use_desc = case_when(
      usegroup == "R-1" ~ "Hotels, motels, boarding houses etc",
      usegroup == "R-2" ~ "3 or more units",
      usegroup == "R-3" ~ "1-2 units, >3 stories",
      usegroup == "R-5" ~ "1-2 units, <= 3 stories"
    ),
    permitdate = date(permitdate),
    certdate = date(certdate),
    last_pc_year = case_when(
      certdate > permitdate ~ year(certdate),  # signoff yr
      permitdate >= certdate | is.na(certdate) ~ year(permitdate)
    )) %>% 
  relocate(lot_ext, block_lot, .after = lot)

# MOD-IV 2016
mod_iv_2016 <- read_csv("data_raw/mod_iv_data_2016_jc.csv") %>%     #61,595 obs 
  clean_names() 

mod_iv_2016 <- mod_iv_2016 %>% 
  select(
    property_id_blk,
    property_id_lot,
    property_id_qualifier,
    qualification_code_name,
    property_class,
    property_class_code_name,
    property_location,
    building_description,
    land_description,
    zoning,
    owner_name,
    number_of_owners,
    deed_date_mmddyy,
    sale_price,
    sale_assessment,
    sale_sr1a_non_usable_code,
    number_of_dwellings,
    year_constructed,
    land_value,
    property_use_code,
    senior_citizen_count,
    veteran_count,
    widows_of_veterans_count,
    surviving_spouse_count,
    disable_person_count,
    old_block,
    old_lot
  ) %>% 
  mutate(property_id_lot = as.character(property_id_lot),
         lot_ext = str_extract(property_id_lot, "(?<=\\.)[:digit:]+"),
         property_id_lot = case_when(
           is.na(lot_ext) ~ property_id_lot,
           lot_ext > 0 ~ str_extract(property_id_lot, "[:digit:]+(?=\\.?)")),
         block_lot = paste0(property_id_blk, "_", property_id_lot),
         number_of_dwellings = as.integer(number_of_dwellings)
  ) %>% 
  relocate(lot_ext, block_lot, .after = property_id_lot)

# Sat 12/3: NB some (but not all!) of the MOD-IV and JC parcels aren't matching up bc they do or do not use lot extensions

merge_test <- anti_join(jc_demos, mod_iv_2016, by = "block_lot")

# Merge MOD-IV & construction permit data; check for dupes
tax_demo_merge <- jc_demos %>% 
  left_join(mod_iv_2016, by = "block_lot") %>%
  arrange(recordid) %>% 
  filter(recordid!=lag(recordid) | is.na(lag(recordid)))

# Isolate residential (& probable residential; see Part 2) obs
jc_res_demos <- tax_demo_merge %>% 
  filter(str_starts(usegroup, 'R'))

poss_res_demos <- tax_demo_merge %>%
  filter(!str_starts(usegroup, 'R')) %>%
  filter(!is.na(number_of_dwellings)) %>% 
  filter(property_class_code_name == "Residential")

# Definite & possible residential merge; unit counts
jc_res_demos <- jc_res_demos %>% 
  rows_append(poss_res_demos) %>% 
  mutate(described_units = as.integer(str_extract(building_description, pattern = "[:digit:]+(?=U)"))) %>% 
  mutate(salegained = abs(salegained),
         rentgained = abs(rentgained)) %>%
  mutate(declared_loss = case_when(
    salegained != rentgained ~ salegained + rentgained,
    salegained == rentgained & salegained != 0 ~ salegained,
    salegained + rentgained == 0 ~ 0)) %>%  
  mutate(unit_estimate = ifelse(declared_loss == 0, described_units, declared_loss)) %>% 
  mutate(unit_estimate = ifelse(is.na(unit_estimate), number_of_dwellings, unit_estimate))
  
empties <- jc_res_demos %>% 
  filter(is.na(unit_estimate))
  
unit_summary <- jc_res_demos %>% 
  summarize(min_u = min(number_of_dwellings, na.rm = T),
            max_u = max(number_of_dwellings, na.rm = T),
            med_u = median(number_of_dwellings, na.rm = T),
            mean_u = mean(number_of_dwellings, na.rm = T),
            min_est = min(unit_estimate, na.rm = T),
            max_est = max(unit_estimate, na.rm = T),
            med_est = median(unit_estimate, na.rm = T),
            mean_est = mean(unit_estimate, na.rm = T))
  
  
#  mutate(unit_estimate = case_when(
#    is.na(described_units) & declared_loss == 0 ~ number_of_dwellings,
#    !is.na(described_units) & declared_loss == 0 ~ described_units,
#    declared_loss != 0 ~ declared_loss)


# Trim down for sanity

jc_res_demos <- jc_res_demos %>% 
  select(-c(permittypedesc,
            certtypedesc,
            usegroupdesc,
            processdate,
            property_id_blk,
            property_id_lot,
            property_id_qualifier,
            lot_ext.y,
            qualification_code_name,
            property_use_code,
            described_units)
         )

rm(merge_test)
rm(poss_res_demos)
rm(tax_demo_merge)
rm(jc_raw)
rm(jc_demos) # this is just for speed atm; may need to bump the code below up to apply to all demos, not just res

# Save the res_demos base set for future fiddling
write_csv(jc_res_demos, "data_working/jc_res_demos_12_09_2022.csv")


# Note these only reflect the 2016 owners
deductible_counts <- jc_res_demos %>% 
  summarize(seniors = sum(senior_citizen_count, na.rm = T),
            veterans = sum(veteran_count, na.rm = T),
            widows_of_vets = sum(widows_of_veterans_count, na.rm = T),
            disabled = sum(disable_person_count, na.rm = T)
  )

deductible_owner_details <- jc_res_demos %>% 
  filter(
    senior_citizen_count > 0 | 
      veteran_count > 0 | 
      widows_of_veterans_count > 0 | 
      disable_person_count > 0
  )  # 51 obs

sr1a_counts <- jc_res_demos %>% 
  mutate(code = factor(sale_sr1a_non_usable_code)) %>% 
  group_by(code) %>% 
  summarize(code_n = n_distinct(recordid)) %>% 
  arrange(desc(code_n))
  # This is really useful, potentially - eg code "15" (10 obs) is "Sales from ... any political subdivision of the State of New Jersey" 

# Thinking about who owned multiple properties, but need post-2016 data for this realistically
owners <- jc_res_demos %>% 
  group_by(owner_name) %>% 
  summarize(n_demos = n_distinct(recordid)) %>%
  arrange(n_demos)
  
# OK! Now we're up to speed, sort of, after Assignment 2.
# Let's now bring in the street address lat/long/ZIP data I downloaded from NJ.gov's API
# See 12_02_NJOGIS_API_experiment

usps <- read_html("https://pe.usps.com/text/pub28/28apc_002.htm") %>% 
  html_node(xpath = '//*[@id="ep533076"]') %>% 
  html_table() %>% 
  row_to_names(row_number = 1) %>% 
  clean_names() %>% 
  select(3,1) %>% 
  distinct()

# url: https://pe.usps.com/text/pub28/28apc_002.htm
# xpath: //*[@id="ep533076"]

# Normalize addresses (ranged/hyphenated shortened; note this is uppercase)
addr_normed <- jc_res_demos %>%
  mutate(
    short_address = str_replace(
      property_location, "[-][:digit:]+",""
    )
  ) %>%  # quick 'n dirty cut of ranged, hyphenated addresses (e.g. 119-121 Monticello Ave.) to just their first number
  mutate(
    short_address = str_replace(
      short_address, "\\.5", " 1/2"
    )
  ) %>% 
  mutate(
    norm_str_add = normal_address(
      short_address, abbs = usps
      ),
    fulladdr = str_to_title(norm_str_add)
  ) %>% 
  relocate(short_address, norm_str_add, fulladdr, .after = property_location)

rm(usps)

# Cool. OK. *Now* let's import the geolocation data I pulled from NJ.gov (it's their 911 call address pinpoints).
# See 12_02_NJOGIS_API_experiment.R

njogis_jc_addr <- read_csv("data_raw/jc_parcel_addresses_coords_12_02_2022.csv") %>% 
  clean_names

# I want post_code, long, lat; could also do geometry but eh?
     # Sat 12/3 1:55 pm: 77 obs left out
     # ah! forgot that several house numbers end in .5, eg "170.5 MONTICELLO AVENUE" became "1705" via campfin's normal_address function.
          # NJOGIS stores ".5" as "1/2"
     # also, a bunch of them used "AV" or "AV." instead of "Ave", so that never got elongated by campfin
     # fixed the ".5" > "1/2" issue and JFK > "John F Kennedy"; now to fix "Av
    # test case: campfin made "225 FREEMAN AV." into "225 Freeman Av" 

          # thanks, Stack Overflow https://stackoverflow.com/questions/63585270/r-stringr-mutate-i-think-multiple-partial-string-replacements-in-multiple

addr_normed <- addr_normed %>% 
  mutate(
    fulladdr = str_replace_all(
      fulladdr, pattern = c("Kennedy" = "John F Kennedy", 
                            "Av(?!enue)" = "Avenue",
                            "Ml King" = "Martin Luther King Junior",
                            "Mc Adoo" = "Mcadoo",
                            "De Kalb" = "Dekalb",
                            "St Pauls" = "Saint Pauls",
                            "Tonnele" = "Tonnelle",
                            "First" = "1st",
                            "Second" = "2nd",
                            "Third" = "3rd",
                            "Fourth" = "4th",
                            "Fifth" = "5th",
                            "Sixth" = "6th",
                            "Seventh" = "7th",
                            "Eighth" = "8th",
                            "Ninth" = "9th")
    )
  ) %>% 
  mutate(  
    fulladdr = case_when(  # hand-imputing adjacent buildings for geolocation
      property_location == "119-121 MONTICELLO AVE." ~ "117 Monticello Avenue",  
      property_location == "319-21 JOHNSTON AVE." ~ "317 Johnston Avenue",
      property_location == "414-16 FIRST ST." ~ "418 1st Street",
      property_location == "270-2 NEWARK AVE." ~ "333 3rd Street",
      property_location == "38 WAVERLY ST." ~ "40 Waverly Street",
      property_location == "27-25 JEFFERSON AVE." ~ "29 Jefferson Avenue",
      property_location == "330-330.5 FIFTH ST." ~ "326 5th Street",
      property_location == "9 LIENAU PL." ~ "97 Waverly Street",
      property_location == "177-175 ACADEMY ST." ~ "173 Academy Street",
      property_location == "192-94 MC ADOO AVE." ~ "188 Mcadoo Avenue",
      property_location == "43-35 CHARLES AVE." ~ "41 Charles Street",
      property_location == "543-7 M.L. KING DRIVE" ~ "541 Martin Luther King Junior Drive",
      recordid == "00053705" ~ "160 Morgan Street",
      recordid == "00055440" ~ "20 Corbin Avenue",
      TRUE ~ as.character(fulladdr)
    )
  )

# Join up the NJ coordinate data

merge_test <- anti_join(addr_normed, njogis_jc_addr, by = "fulladdr")
rm(merge_test)

located_res_demos <- addr_normed %>% 
  left_join(njogis_jc_addr, by = "fulladdr") %>%
  arrange(recordid) %>% 
  filter(recordid!=lag(recordid) | is.na(lag(recordid)))

write_csv(located_res_demos, "data_working/geolocated_demos_12_03_2022.csv")

# Simpler version for QGIS

located_res_demos %>% 
  select(recordid,
         block,
         lot,
         permitdate,
         certdate,
         last_pc_year,
         constcost,
         usegroup,
         property_location,
         post_code,
         building_description,
         unit_estimate,
         year_constructed,
         long,
         lat) %>% 
write_csv("data_working/for_gis_geolocated_demos_12_08_2022.csv")

# Can I do it in here, actually?
# Note EPSG:3424

mappy = located_res_demos %>%
  st_as_sf(coords = c("long", "lat"), crs = 3857)

# st_transform(mappy, 4326)

# add basemap
# see ancient stuff in my old projects folder, esp. when I did this w/ tenure duration from -tidycensus-
# NB ggmaps() currently I think only supports crs 4326?

# maybe try more like long = -74.067014, lat = 40.722530
jc <- c(lon = -74.067014, lat = 40.72253)
jc_map <- get_map(location = jc,
                  source = "stamen",
                  maptype = "toner-background",
                  zoom = 13,
                  scale = 1)
ggmap(jc_map)

plot(mappy)
plot(mappy["constcost"])

ggmap(jc_map) + 
  coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
  geom_sf(data = mappy, aes(color = last_pc_year, alpha = 0.3), inherit.aes = FALSE)+
  theme_minimal()

plot(st_transform(mappy, crs = 3857)[1], bgMap = jc_map)

rc_maybes <- located_res_demos %>% 
  filter(unit_estimate > 4) %>% 
  view() # 16 parcels, 6 of which alleged 0 unit loss

# next up: the reason I put this in R was to think abt some of the 
# variable gen that's a pain directly in GIS. 
# By tract/ZIP: land value?
# might be worth just geocoding these via census site honestly, to get tracts


