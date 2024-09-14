library(shiny)
library(dplyr)
library(tigris)
library(socviz)
library(sp)
library(googlesheets4)


## GET SHAPE FILES #########################

# options(tigris_use_cache=TRUE)
# 
# states_data <- counties()
# states_data <- rmapshaper::ms_simplify(states_data, keep=0.001)
# states_data <- states_data %>% sf::st_transform(4326)
# 
# saveRDS(states_data, "data/new_001.rds")


# counties_data <- sf::st_read("data/counties/cb_2018_us_county_500k.shp",
#                              quiet = TRUE)
# 
# counties_data_simpl <- rmapshaper::ms_simplify(counties_data,
#                                                keep=0.02,
#                                                keep_shapes=TRUE) %>%
#   sf::st_transform(4326)
# 
# saveRDS(counties_data_simpl, "data/nowater_02.rds")


## LOAD SAVED SHAPE FILES #########################

temp0 <- readRDS("data/new_001.rds") %>%
  select("GEOID", "NAMELSAD")  %>%
  sf::st_drop_geometry() %>%
  rbind(c("24510", "Baltimore city"))

counties <- readRDS("data/nowater_02.rds") %>%
  select("GEOID", "geometry") %>%
  filter(GEOID %in% temp0$GEOID)

# sf::sf_use_s2(FALSE)
# truecentroids <- sf::st_centroid(counties)
# 
# counties2 <- truecentroids %>%
#   dplyr::mutate(lat = sf::st_coordinates(.)[,2],
#                 lon = sf::st_coordinates(.)[,1]) %>%
#   sf::st_drop_geometry() %>%
#   rename(INTPTLAT = lat, INTPTLON = lon)
# write.csv(counties2, "new_lat_lon.csv", row.names=FALSE)

lat_lon <- read.csv("data/new_lat_lon.csv",
                    colClasses=c("GEOID"="character"))
counties <- left_join(counties, lat_lon,
                      by = "GEOID")

temp <- left_join(counties, temp0,
                  by = "GEOID")

temp[temp$NAMELSAD == "Oglala Lakota County",]$GEOID = 46113
goodlon <- temp[temp$NAMELSAD == "San Mateo County",]$INTPTLON
temp[temp$NAMELSAD == "San Francisco County",]$INTPTLON = goodlon

## LOAD AND PREPARE MISC DATA #####################

# COUNTY DATA
county_data <- socviz::county_data

educ_data <- read.csv("data/county_education.csv",
                      colClasses=c("county_fips"="character"))

county_data <- left_join(county_data, educ_data,
                         by = c("id"="county_fips"))

voting_2020_data <- read.csv("data/county_pres_2020.csv",
                             colClasses=c("county_fips"="character")) %>%
  rbind(c("11001", 0.9215, 0.0540)) %>%
  mutate(per_dem_2020 = as.numeric(per_dem_2020),
         per_gop_2020 = as.numeric(per_gop_2020))

county_data <- left_join(county_data, voting_2020_data,
                         by = c("id"="county_fips"))

county_data$per_degree[is.na(county_data$per_degree)] <- mean(county_data$per_degree,
                                                              na.rm=TRUE)

urban_codes <- read.csv("data/NCHSURCodes2013_small.csv",
                        colClasses=c("FIPS"="character",
                                     "code"="character")) %>%
  select(FIPS, code) %>%
  rename(code_raw = code) %>%
  mutate(FIPS = stringr::str_pad(FIPS, 5, pad = "0"))
urban_codes$code_num <- case_when(
  urban_codes$code_raw == 1 ~ 4,
  urban_codes$code_raw == 2 ~ 3,
  urban_codes$code_raw %in% c(3, 4) ~ 2,
  TRUE ~ 1
)
urban_codes$code <- case_when(
  urban_codes$code_raw == 1 ~ "Urban",
  urban_codes$code_raw == 2 ~ "Suburban",
  urban_codes$code_raw %in% c(3, 4) ~ "Small metro",
  TRUE ~ "Rural"
)

county_data <- left_join(county_data, urban_codes,
                         by = c("id"="FIPS"))

county_data$code_raw <- factor(county_data$code_raw, ordered = TRUE, 
                           levels = c("1", "2", "3", "4", "5", "6"))
county_data$code <- factor(county_data$code, ordered = TRUE, 
                           levels = c("Urban", "Suburban", "Small metro", "Rural"))

county_data <- county_data %>%
  select(-c("name", "census_region", "pop_dens4", "pop_dens6", "female", 
            "white", "black", "travel_time", "land_area",
            "su_gun4", "su_gun6", "fips",
            "votes_dem_2016", "votes_gop_2016", "total_votes_2016", "diff_2016",
            "per_dem_2012", "per_gop_2012", "diff_2012",
            "winner", "partywinner16", "winner12", "partywinner12", "flipped"))

# FINAL JOINED COUNTY DATA WITH SHAPEFILES FOR SERVER #######

countydata0 <- left_join(temp, county_data,
                         by = c("GEOID"="id"),
                         keep = FALSE) %>%
  filter(!is.na(state),
         state != "AK")


# MIGRATION DATA
migration_data_raw <- read.csv("data/migration_2010_to_2019.csv",
                               colClasses=c(ori_county="character",
                                            des_county="character"))

# migration_data_raw <- read.csv("data/migration_2010_to_2015.csv",
#                                colClasses=c(ori_county="character",
#                                             des_county="character"))

# migration_data_raw <- read.csv("data/migflow2015_county_nodeid_dist.csv",
#                                colClasses=c(ori_county="character",
#                                             des_county="character")) %>%
#   select("ori_county", "des_county", "exemptions")

# Add populations, for normalizing migration later
county_data_pop <- county_data %>%
  dplyr::select(id, pop)

migration_data_inter <- left_join(migration_data_raw, county_data_pop,
                                  by=c("ori_county" = "id")) %>%
  rename("ori_county_pop" = "pop")

migration_data <- left_join(migration_data_inter, county_data_pop,
                            by=c("des_county" = "id")) %>%
  rename("des_county_pop" = "pop")

# Compute net migration
migration_data_dup <- data_frame(migration_data)
colnames(migration_data_dup) <- paste0(colnames(migration_data_dup), "_dup")

double <- left_join(migration_data, migration_data_dup, 
                    by=c("ori_county"="des_county_dup",
                         "des_county"="ori_county_dup"))

double$exemptions_net <- double$exemptions - double$exemptions_dup
double$exemptions_net <- coalesce(double$exemptions_net,
                                  double$exemptions)

double$total <- coalesce(double$exemptions + double$exemptions_dup,
                         double$exemptions,
                         double$exemptions_dup)
double$efficiency <- double$exemptions_net / double$total * 100

migration_data_with_net <- double %>%
  select(c(colnames(migration_data), "exemptions_net", "total", "efficiency"))

migration_data <- data_frame(migration_data_with_net) %>%
  select("ori_county", "des_county", "ori_county_pop", "des_county_pop",
         "exemptions", "exemptions_net", "total", "efficiency")

## FOR OVERVIEW COLORING ######

# Compute overall migration in/out/net
migration_data_in <- migration_data %>%
  group_by(des_county) %>%
  summarize(total_in = sum(exemptions),
            pop = median(des_county_pop)) %>%
  mutate(total_in_norm = total_in / pop * 1000) %>%
  select(-c(pop))

migration_data_out <- migration_data %>%
  group_by(ori_county) %>%
  summarize(total_out = sum(exemptions),
            pop = median(ori_county_pop)) %>%
  mutate(total_out_norm = total_out / pop * 1000) %>%
  select(-c(pop))

migration_data_net <- migration_data %>%
  group_by(des_county) %>%
  summarize(total_net = sum(exemptions_net),
            pop = median(des_county_pop)) %>%
  mutate(total_net_norm = total_net / pop * 1000,
         total_net_norm_abs = abs(total_net_norm)) %>%
  select(-c(pop))

# Construct final overview dataframe
# all_GEOIDs <- union(migration_data$ori_county,
#                     migration_data$des_county)

all_GEOIDs <- countydata0$GEOID

migration_data_overview <- data.frame(matrix(nrow = length(all_GEOIDs),
                                             ncol = 1))
colnames(migration_data_overview) <- "GEOID"
migration_data_overview$GEOID <- all_GEOIDs

migration_data_overview <- left_join(migration_data_overview,
                                     migration_data_in,
                                     by = c("GEOID" = "des_county"))

migration_data_overview <- left_join(migration_data_overview,
                                     migration_data_out,
                                     by = c("GEOID" = "ori_county"))

migration_data_overview <- left_join(migration_data_overview,
                                     migration_data_net,
                                     by = c("GEOID" = "des_county"))

# migration_data_overview <- migration_data_overview %>%
#   tidyr::replace_na(0)
migration_data_overview[is.na(migration_data_overview)] <- 0


## REALLY THE FINAL DATA FOR REAL THIS TIME

## Data for overview
countydata0 <- left_join(countydata0, migration_data_overview,
                         by = "GEOID")


## COUNTY ADJACENCY DATA FOR EDGE BUNDLING ####

adjacency <- read.csv("data/county_adjacency2010.csv",
                      colClasses=c("fipscounty"="character",
                                   "fipsneighbor"="character")) %>%
  select(fipscounty, fipsneighbor)


## LOAD TEXT FOR DEMO AND EXAMPLES ############

help_text_raw <- readr::read_file("data/help_text_expert.txt")
help_text_clean <- stringr::str_trim(help_text_raw)
help_text <- unlist(stringr::str_split(help_text_clean, "\n"))

example_text_raw <- readr::read_file("data/example_text.txt")
example_text_clean <- stringr::str_trim(example_text_raw)
example_text <- unlist(stringr::str_split(example_text_clean, "\n"))

demo_data <- readxl::read_excel("data/demo_apr13.xlsx")

## MISC. GLOBAL VARIABLES TO BE ACCESSED IN SERVER ############

flow_vars_agg <- c(
  "Migrants into counties w/ attribute" = "incoming", 
  "Migrants from counties w/ attribute" = "outgoing",
  "Net migration into counties w/ attribute" = "net_incoming",
  "Net migration from counties w/ attribute" = "net_outgoing"
)

flow_vars_one <- c(
  "Migrants into clicked county" = "incoming", 
  "Migrants from clicked county" = "outgoing",
  "Net migration into clicked county" = "net_incoming",
  "Net migration from clicked county" = "net_outgoing"
)

color_label_map <- c(
  "hh_income" = "Median Household Income",
  "pop" = "Population",
  "white" = "% White",
  "pct_black"  = "% Black",
  "pop_dens" = "Pop. per Square Mile",
  "per_degree" = "% With Degree",
  "migrants_bin" = "Number of Migrants",
  "total_in" = "Number of Migrants In",
  "total_out" = "Number of Migrants Out",
  "total_net" = "Net Migration"
)

color_vars <- c(
  "Presidential Vote '20" = "per_dem_2020",
  "Presidential Vote '16" = "per_dem_2016",
  "Median Household Income" = "hh_income",
  "Percent With College Degree" = "per_degree",
  # "Population Density" = "pop_dens",
  # "Percent Black" = "pct_black",
  "Urban Classification" = "code",
  "Total Migrants In" = "total_in",
  "Total Migrants Out" = "total_out",
  "Net Migration" = "total_net"
)

color_vars_selection <- c(
  "Presidential Vote '20" = "per_dem_2020",
  "Presidential Vote '16" = "per_dem_2016",
  "Median Household Income" = "hh_income",
  "Percent With College Degree" = "per_degree",
  # "Population Density" = "pop_dens",
  # "Percent Black" = "pct_black",
  "Urban Classification" = "code",
  "Number of Migrants" = "migrants_bin"
)

select_by_vars_labels <- c("gop_pct_20" = "% GOP in 2020",
                           "gop_pct_16" = "% GOP in 2016",
                           "hh_income" = "Median HH Income",
                           "per_degree" = "% With Degree",
                           "code" = "Urban Class.")

color_label_map_scatter <- c(
  "per_dem_2020" = "% Republican in 2020",
  "per_dem_2016" = "% Republican in 2016",
  "hh_income" = "Median Household Income",
  "per_degree" = "% With Degree",
  "migrants_bin" = ""
)


## LOAD SHAPE FILES FOR COUNTRY REGIONS ################

regions <- sf::st_read("data/regions/cb_2018_us_region_20m.shx",
                       quiet = TRUE)
# cb_2018_us_division_20m.shx
# regions <- sf::st_transform(regions, '+proj=longlat +datum=WGS84')


## AUTHENTICATE ################
# Authenticate using token. If no browser opens, the authentication works.
gs4_auth(cache = ".secrets", email = "alexanderbendeck@gmail.com")

sheet_url_nonexperts <- "https://docs.google.com/spreadsheets/d/1bqV8ey5e32ynEa8DCgJib6KtezvWChvQrHyHL6IB698/edit#gid=0"
sheet_url_experts <- "https://docs.google.com/spreadsheets/d/1wiJBI3fA3PD2EzX03KZVCX_Cpg6oPr3WgK-q26oerWw/edit#gid=0"
sheet_url <- sheet_url_experts
