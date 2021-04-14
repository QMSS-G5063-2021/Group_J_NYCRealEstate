
# ------ load data


acs1 <- read_csv("acs_demographics/acs_1yr_2009_2019.csv") %>% rename(puma_code = PUMA)
acs5 <- read_csv("acs_demographics/acs_5yr_2014_and_2019.csv") %>% filter(borough == "Manhattan") %>% rename(puma_code = PUMA)

construction <- read_csv("construction_data/HousingDB_post2010.csv") %>% 
  rename(puma_code = PUMA2010)

puma <- read_csv("construction_data/HousingDB_by_PUMA.csv") %>% 
  filter(boro == "Manhattan") %>%
  select(puma_code = puma2010, puma_name = pumaname10)

pre_puma <- read.csv('construction_data/HousingDB_by_PUMA.csv') # by puma
pre_post2010 <- read.csv('construction_data/HousingDB_post2010.csv') # detailed

# ------ edit variables

puma$puma_name <- sub("CD.* -", "-", puma$puma_name) # shorten puma name -- can this be shortened further????
puma$puma_name <- gsub("Manhattan - ", "", puma$puma_name)

acs1 <- acs1 %>% left_join(puma, by = "puma_code") # add puma names
acs1_manhattan <- acs1 %>% filter(borough == "Manhattan")

acs5 <- acs5 %>% left_join(puma, by = "puma_code")
acs5_manhattan <- acs5 %>% filter(borough == "Manhattan")
