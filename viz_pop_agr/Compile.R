library(sf)
library(tidyverse)
library(countrycode)

agr_africa <- read_csv("data/FAOSTAT_data_en_1-30-2026_Africa.csv") %>%
  select('Area', 'Year', 'Value') %>%
  rename(Country = Area,
         Agr_area = Value)

agr_america <- read_csv("data/FAOSTAT_data_en_1-30-2026_America.csv") %>%
  select("Area", "Year", "Value") %>%
  rename(Country = Area,
         Agr_area = Value)
  
agr_asia <- read_csv("data/FAOSTAT_data_en_1-30-2026_Asia.csv") %>%
  select("Area", "Year", "Value") %>%
  rename(Country = Area,
         Agr_area = Value)
  
agr_europe <- read_csv("data/FAOSTAT_data_en_1-30-2026_Europe.csv") %>%
  select("Area", "Year", "Value") %>%
  rename(Country = Area,
         Agr_area = Value)
  
agr_oceania <- read_csv("data/FAOSTAT_data_en_1-30-2026_Oceania.csv") %>%
  select("Area", "Year", "Value") %>%
  rename(Country = Area,
         Agr_area = Value)

# agricultural land ############################################################

agr_world <- bind_rows(agr_africa,
                   agr_america,
                   agr_asia,
                   agr_europe,
                   agr_oceania)%>%
  pivot_wider(names_from  = Year,
              values_from = Agr_area)

agr_world <- agr_world %>%
  mutate(iso3 = countrycode(Country, "country.name", "iso3c")) %>% drop_na()

nas <- agr_world %>% filter(if_any(everything(), is.na))

# population ###################################################################

pop_world <- read_csv("data/API_SP.POP.TOTL_DS2_en_csv_v2_7.csv", skip = 4) %>%
  
  select("Country Name", "Country Code",
         "1990","1991","1992","1993","1994",
         "1995","1996","1997","1998","1999",
         "2000","2001","2002","2003","2004",
         "2005","2006","2007","2008","2009",
         "2010","2011","2012","2013","2014",
         "2015","2016","2017","2018","2019",
         "2020","2021","2022","2023") %>%
  
  rename(Country = "Country Name") %>%
  
  filter(Country != "Not classified",
         nchar(`Country Code`) == 3, !`Country Code` %in% c(
             "WLD","AFE","AFW","ARB","CEB","CSS",
             "EAP","EAS","ECA","ECS","EMU","EUU",
             "FCS","HIC","LIC","LMC","UMC","LMY",
             "MIC","IBD","IBT","IDA","IDX","IDB",
             "LAC","LCN","LDC","LTE","MEA","MNA",
             "NAC","OED","OSS","PRE","PSS","PST",
             "SAS","SSA","SSF","SST","TEA","TEC",
             "TLA","TMN","TSA","TSS")) 

# geometries ###################################################################

countries_world <- st_read("data/World Bank Official Boundaries - Admin 0_all_layers/WB_GAD_ADM0_complete.shp") %>%
  filter(WB_STATUS == "Member State",!is.na(ISO_A3)) %>%
  select("ISO_A3","NAM_0","geometry") %>%
  rename(Country_Code = ISO_A3,
         Country_Name = NAM_0)

#-------------------------------------------------------------------------------

countries_world <- countries_world %>%
  mutate(Country_Name = case_when(
    Country_Code == "CIV" ~ "Côte d’Ivoire",
    Country_Code == "STP" ~ "São Tomé and Príncipe",
    Country_Code == "REU" ~ "eéunion",
    Country_Code == "MYT" ~ "Mayotte",
    Country_Code == "GLP" ~ "Guadeloupe",
    Country_Code == "MTQ" ~ "Martinique",
    Country_Code == "GUF" ~ "French Guiana",
    TRUE ~ Country_Name))

# combine ######################################################################

world_pop <- countries_world %>%
  left_join(pop_world,
            by = c("Country_Code" = "Country Code")) %>% 
  drop_na() %>%
  pivot_longer(cols = matches("^[0-9]{4}$"),
               names_to = "year",
               values_to = "Pop_value") %>%
  mutate(year = as.integer(year)) %>%
  st_drop_geometry() %>%
  select(Country_Code, Country_Name, everything(), -Country)

#-------------------------------------------------------------------------------

year_cols <- names(agr_world)[grepl("^[0-9]{4}$", names(agr_world))]

world_agr_1 <- countries_world %>%
  left_join(agr_world,
            by = c("Country_Code" = "iso3")) %>%
  st_transform(6933) %>%
  st_make_valid() %>%
  group_by(Country_Code, Country_Name) %>%
  summarise(
    across(all_of(year_cols), first),
    geometry = st_union(geometry),
    .groups = "drop")

world_agr_2 <- world_agr_1 %>% 
  mutate(country_area_m2 = as.numeric(st_area(geometry))) %>%
  mutate(across(all_of(year_cols),
                ~ (.x * 1e7 / country_area_m2) * 100)) %>%
  distinct(Country_Code, .keep_all = TRUE)

world_agr_3 <- world_agr_2 %>%
  st_drop_geometry()

world_agr <- world_agr_3 %>%
  pivot_longer(
    cols = matches("^[0-9]{4}$"),
    names_to = "year",
    values_to = "Crop_per") %>%
  mutate(year = as.integer(year))%>%
  select(Country_Code, Country_Name, everything(), -country_area_m2)

# save #########################################################################

st_write(world_pop,
         "data/world_pop.csv")

st_write(world_agr,
         "data/world_agr.csv")

st_write(countries_world,
         "data/World_boundaries/world_boundaries.shp")

#lokk for NULL values
#check countries names
#nas <- world_agr %>% filter(if_any(everything(), is.na))