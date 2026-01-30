library(sf)
library(tidyverse)
library(countrycode)

# https://www.fao.org/faostat/en/#data/RL
# https://data.worldbank.org/indicator/SP.POP.TOTL
# https://datacatalog.worldbank.org/search/dataset/0038272/world-bank-official-boundaries

agr_africa <- read_csv("data/FAOSTAT_data_en_1-29-2026_Africa.csv") %>%
  select('Area', 'Year', 'Value') %>%
  rename(Country = Area,
         Agr_area = Value)

agr_america <- read_csv("data/FAOSTAT_data_en_1-29-2026_America.csv") %>%
  select("Area", "Year", "Value") %>%
  rename(Country = Area,
         Agr_area = Value)
  
agr_asia <- read_csv("data/FAOSTAT_data_en_1-29-2026_Asia.csv") %>%
  select("Area", "Year", "Value") %>%
  rename(Country = Area,
         Agr_area = Value)
  
agr_europe <- read_csv("data/FAOSTAT_data_en_1-29-2026_Europe.csv") %>%
  select("Area", "Year", "Value") %>%
  rename(Country = Area,
         Agr_area = Value)
  
agr_oceania <- read_csv("data/FAOSTAT_data_en_1-29-2026_Oceania.csv") %>%
  select("Area", "Year", "Value") %>%
  rename(Country = Area,
         Agr_area = Value)

# agricultural land ############################################################

agr_world <- bind_rows(agr_africa,
                   agr_america,
                   agr_asia,
                   agr_europe,
                   agr_oceania)%>%
  filter(Year != 2024) %>%
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
             "TLA","TMN","TSA","TSS")) #%>%

 # pivot_longer(cols = -Country,
  #             names_to = "Year",
   #            values_to = "Population") %>%
#  mutate(Year = as.integer(Year))

# geometries ###################################################################

countries_world <- st_read("data/World Bank Official Boundaries - Admin 0_all_layers/WB_GAD_ADM0_complete.shp") %>%
  filter(WB_STATUS == "Member State",!is.na(ISO_A3)) %>%
  select("ISO_A3","NAM_0","geometry") %>%
  rename(Country_Code = ISO_A3,
         Country_Name = NAM_0)

# combine ######################################################################

world_pop <- countries_world %>%
  left_join(pop_world,
    by = c("Country_Code" = "Country Code")) %>% drop_na()

world_agr <- countries_world %>%
  left_join(agr_world,
            by = c("Country_Code" = "iso3"))

# save #########################################################################

st_write(world_pop,
  "data/world_pop.shp")

st_write(world_agr,
  "data/world_agr.shp")
