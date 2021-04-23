library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(readr)
library(scales)
library(RColorBrewer)
library(leaflet)
library(gghighlight)
library(lubridate)
library(tm)
library(tidytext)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(plotly)
library(quanteda)
library(treemapify)
library(igraph)
library(network)
library(ggnetwork)
library(tidygraph)
library(stringr)
library(tidyr)
library(ggraph)
library(magrittr)
library(visNetwork)
#library(rsconnect)
library(DT)
#rsconnect::deployApp('app.R')


#setwd('/Users/Melissa/Desktop/Data Visualization SP21/Group_J_NYCRealEstate/dashboard/')
#setwd("C:/Users/natal/Desktop/QMSS/Spring 2021/Data_Visualization/project/Group_J_NYCRealEstate/dashboard/")
setwd("G:/My Drive/0 Data Viz/project/Group_J_NYCRealEstate/dashboard")
#setwd("~/Documents/GitHub/Group_J_NYCRealEstate/")

## ---------------------------------------------------- DATA -----------------------------------------

# ------ load data

acs1 <- read_csv("acs_1yr_2009_2019.csv") %>% rename(puma_code = PUMA)
acs5 <- read_csv("acs_5yr_2014_and_2019.csv") %>% filter(borough == "Manhattan") %>% rename(puma_code = PUMA)

construction <- read_csv("HousingDB_post2010.csv") %>% 
  rename(puma_code = PUMA2010)

puma <- read_csv("HousingDB_by_PUMA.csv") %>% 
  filter(boro == "Manhattan") %>%
  select(puma_code = puma2010, puma_name = pumaname10)

pre_puma <- read.csv('HousingDB_by_PUMA.csv') # by puma
pre_post2010 <- read.csv('HousingDB_post2010.csv') # detailed

timemachine <- read_csv("TimeMachine.csv")
df_sentiment <- read_csv("neighborhood_sentiment_scores.csv") %>% filter(year >=2009)
df_wordcount <- read_csv("wordcount_by_year.csv") %>% filter(year >=2009)

# ------ edit variables

puma$puma_name <- sub("CD.* -", "-", puma$puma_name) # shorten puma name -- can this be shortened further????
puma$puma_name <- gsub("Manhattan - ", "", puma$puma_name)

acs1 <- acs1 %>% left_join(puma, by = "puma_code") # add puma names
acs1_manhattan <- acs1 %>% filter(borough == "Manhattan")

acs5 <- acs5 %>% left_join(puma, by = "puma_code")
acs5_manhattan <- acs5 %>% filter(borough == "Manhattan")

construction <- construction %>%
  mutate(borough = ifelse(puma_code %in% c(3701:3710), "Bronx",
                          ifelse(puma_code %in% c(4001:4018), "Brooklyn",
                                 ifelse(puma_code %in% c(3801:3810), "Manhattan",
                                        ifelse(puma_code %in% c(4101:4114), "Queens",
                                               ifelse(puma_code %in% c(3901:3903), "Staten Island", NA)))))) %>%
  left_join(puma, by = "puma_code") %>%
  filter(Job_Type != "Demolition")

construction$prop_use_cat <- sub(":.*)", "", construction$Occ_Prop)
construction$prop_use_cat <- ifelse(grepl("Unknown", construction$prop_use_cat), "Unknown", 
                                    ifelse(grepl("Educational", construction$prop_use_cat), "Educational", construction$prop_use_cat))
construction_manhattan <- construction %>% filter(borough == "Manhattan")

input_boro <- unique(acs1$borough)
input_puma <- puma$puma_name[order(unique(puma$puma_name), puma$puma_name)]



# ------ wrangle data
med_age <- acs1 %>%
  filter(variable == "med_age") %>%
  pivot_longer(names_to = "year", values_to = "estimate", cols = c(est_2009:est_2019)) %>%
  mutate(year = as.numeric(gsub("est_", "", year))) %>%
  select(-GEOID)

res_permit_count <- construction %>%
  filter(prop_use_cat == "Residential") %>%
  group_by(PermitYear, puma_code, puma_name, Job_Type, borough, Occ_Prop) %>%
  tally() %>%
  ungroup() %>%
  rename(permit_count = n, year = PermitYear, permit_type = Job_Type)
res_permit_count$prop_use <- gsub("\\s*\\([^\\)]+\\)\\s*$", "", as.character(res_permit_count$Occ_Prop)) # J and R codes used interchangeably
res_permit_count <- res_permit_count %>% filter(prop_use != "Residential: Hotels, Dormitories") # remove hotels/ dorms

#table(res_permit_count$prop_use)

housing_acs <- acs1 %>% 
  filter(variable %in% c("population", "occupied_total", "occupied_owner", "occupied_renter", 
                         "med_value", "med_grossrent"))  %>%
  pivot_longer(names_to = "year", values_to = "estimate", cols = c(est_2009:est_2019)) %>%
  mutate(year = as.numeric(gsub("est_", "", year))) 

permit_rental_price <- housing_acs %>% 
  filter(variable == "med_grossrent",
         borough == "Manhattan") %>%
  select(puma_code, year, variable, estimate) %>%
  left_join(res_permit_count, by = c("puma_code", "year"))

permit_home_value <- housing_acs %>% 
  filter(variable == "med_value",
         borough == "Manhattan") %>%
  select(puma_code, year, variable, estimate) %>%
  left_join(res_permit_count, by = c("puma_code", "year"))

permit_homevalue_and_rent <- rbind(permit_home_value,
                                   permit_rental_price)

renter_pct <- acs1_manhattan %>%
  filter(variable %in% c("occupied_total", "occupied_renter", "occupied_owner")) %>%
  pivot_longer(names_to = "year", values_to = "estimate", cols = c(est_2009:est_2019)) %>%
  mutate(year = as.numeric(gsub("est_", "", year))) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(renter_pct = occupied_renter/ occupied_total,
         owner_pct = occupied_owner/ occupied_total) %>%
  select(-occupied_owner, -occupied_renter) %>%
  pivot_longer(names_to = "variable", values_to = "estimate", cols = c(renter_pct, owner_pct))

## ---------------------------------------------------- melissa -----------------------------------------
############################# Leaflet Map Data Wrangling
puma <- pre_puma %>% select(boro, puma2010, pumaname10)

# Remove Demolition projects from dataset
post2010 <- pre_post2010 %>% 
  select(Job_Number, Job_Type, ResidFlag, NonresFlag, Job_Status, PermitYear, Boro, 
         AddressNum, AddressSt, Occ_Init, Occ_Prop, Job_Desc, 
         Ownership, NTAName10, PUMA2010, Latitude, Longitude) %>%
  filter(!is.na(PermitYear)) %>%
  filter(!is.na(Occ_Init)) %>%
  filter(!is.na(Occ_Prop)) %>%
  filter(Job_Type != 'Demolition') %>%
  left_join(puma, by = c('PUMA2010' = 'puma2010'))

# Create complete address field
post2010$Complete_Address <- paste(post2010$AddressNum, post2010$AddressSt)

# Create a column for transformation type
post2010$from <- sapply(strsplit(post2010$Occ_Init, ':'), head, 1)
post2010$to <-  sapply(strsplit(post2010$Occ_Prop, ':'), head, 1)

# Collapse those categories with (###) into single types
post2010$from <- gsub("\\s*\\([^\\)]+\\)","", as.character(post2010$from))
post2010$to <- gsub("\\s*\\([^\\)]+\\)","", as.character(post2010$to))

# Remove the collapsed Unknown and Miscellaneous types
post2010 <- post2010 %>%
  filter(from != 'Unknown' & from != 'Miscellaneous') %>%
  filter(to != 'Unknown' & to != 'Miscellaneous') 

# Remove one occurrence by index 
post2010 <- post2010[-9133,]

# Create transformation type column, note if there is a change in occupancy type, otherwise enter "no change"
for (i in 1:nrow(post2010)){
  if (post2010$from[i] != post2010$to[i]){
    post2010$transformation_type[i] <- paste(post2010$from[i], 'to', post2010$to[i])
  } else {
    post2010$transformation_type[i] <- 'No Occupancy Change'
  }
}

# Limit to just Manhattan
manhattan <- post2010 %>% filter(boro == 'Manhattan') 
manhattan$PermitYear <- as.numeric(manhattan$PermitYear)

# Create a column that groups by permit year
for (i in 1:nrow(manhattan)){
  if (manhattan$PermitYear[i] >= 2000 && manhattan$PermitYear[i] <= 2004){
    manhattan$permit_yr_group[i] <- '2000 - 2004'} 
  else if (manhattan$PermitYear[i] >= 2005 && manhattan$PermitYear[i] <= 2009){
    manhattan$permit_yr_group[i] <- '2005 - 2009'} 
  else if (manhattan$PermitYear[i] >= 2010 && manhattan$PermitYear[i] <= 2014){
    manhattan$permit_yr_group[i] <- '2010 - 2014'} 
  else if (manhattan$PermitYear[i] >= 2015 && manhattan$PermitYear[i] <= 2019){
    manhattan$permit_yr_group[i] <- '2015 - 2019'} 
  else {
    manhattan$permit_yr_group[i] <- '2020 - Present'}
}

# Manhattan New Buildings
manhattan_nb <- manhattan %>% filter(Job_Type == 'New Building')

# Manhattan Alterations
manhattan_a <- manhattan %>% filter(Job_Type == 'Alteration')

############################# Treemap Data Wrangling
font_styling <- list(size = 12, family = 'Arial')
colors <- 10
mycolors <- colorRampPalette(brewer.pal(8, 'Dark2'))(colors)

# Treemap Manhattan New Buildings
library(treemapify)
tree_map_newb <- manhattan_nb %>% 
  select(pumaname10, Job_Type) %>%
  group_by(pumaname10) %>%
  count(Job_Type) 

colnames(tree_map_newb)[3] <- 'project_count'  
tree_map_newb$Job_Type <- stringr::str_replace(tree_map_newb$Job_Type, 'New Building', 'New Buildings')
tree_map_newb$shortened_puma <- sapply(strsplit(tree_map_newb$pumaname10, '- '), tail, 1)

tree_map_newb$wrapped_puma <- sapply(tree_map_newb$shortened_puma, 
                                     FUN = function(x) {paste(strwrap(x, width = 20), collapse = '<br>')})

# Treemap Manhattan Alterations
tree_map_alt <- manhattan_a %>% 
  select(pumaname10, Job_Type) %>%
  group_by(pumaname10) %>%
  count(Job_Type)

colnames(tree_map_alt)[3] <- 'project_count' 
tree_map_alt$shortened_puma <- sapply(strsplit(tree_map_alt$pumaname10, '- '), tail, 1)
tree_map_alt$Job_Type <- stringr::str_replace(tree_map_alt$Job_Type , 'Alteration', 'Alterations')

tree_map_alt$wrapped_puma <- sapply(tree_map_alt$shortened_puma, 
                                    FUN = function(x) {paste(strwrap(x, width = 20), collapse = '<br>')})

############################# Interactive Network Data Wrangling
trans_types <- manhattan %>% 
  select(transformation_type, PermitYear) %>%
  group_by(transformation_type) %>% count(PermitYear) %>%
  filter(transformation_type != 'No Occupancy Change')

colnames(trans_types)[3] <- 'count_type'
trans_types$from <- sapply(strsplit(trans_types$transformation_type, ' to '), head, 1)
trans_types$to <- sapply(strsplit(trans_types$transformation_type, ' to '), tail, 1)

# Create edge dataframe
trans_types <- trans_types %>% select(from, to, count_type, PermitYear, transformation_type)

# Create vertex dataframe
trans_types_vertices <- unique(trans_types[,1]) %>% as.data.frame()
colnames(trans_types_vertices)[1] <- 'occupancy_type'  


## ---------------------------------------------------- melissa end -------------------------------------

# - clean TimeMachine.csv data for text analysis
#timemachine = timemachine %>%
#  mutate(date = substr(date, 1, 8)) %>%
#  mutate(date = ymd(date)) %>%
#  mutate(year = year(date)) %>%
#  mutate(clean_text = tolower(text)) %>%
#  mutate(clean_text = removeNumbers(clean_text)) %>%
#  mutate(clean_text = stripWhitespace(clean_text)) %>%
#  mutate(clean_text = removeWords(clean_text, stopwords("en")))

#write.csv("TimeMachine.csv")

# ------- labels
year_lab <- "Year"
age_lab <- "Median Age"
permit_lab <- "Number of Permits"
input_neighborhood <- unique(timemachine$neighborhood)

unique(housing_acs$variable)


## Data manipulation for demographics



# ------ wrangle data
med_age <- acs1_manhattan %>%
  filter(variable == "med_age") %>%
  pivot_longer(names_to = "year", values_to = "estimate", cols = c(est_2009:est_2019)) %>%
  mutate(year = as.numeric(gsub("est_", "", year))) %>%
  select(-GEOID)

#View(med_age)
mean_group <- aggregate(x = med_age$estimate,
                        by=list(puma_name = med_age$puma_name, year = med_age$year),
                        FUN=mean)
#View(mean_group)
names(mean_group)[3] <- "median_age"
## 

# population 

population <- acs1_manhattan %>%
  filter(variable == "population") %>%
  pivot_longer(names_to = "year", values_to = "estimate", cols = c(est_2009:est_2019)) %>%
  mutate(year = as.numeric(gsub("est_", "", year))) %>%
  select(-GEOID)

#View(med_age)
pop_group <- aggregate(x = population$estimate,
                       by=list(puma_name = population$puma_name, year = population$year),
                       FUN=mean)

names(pop_group)[3] <- "median_population"


# Income level 

income <- acs1_manhattan %>%
  filter(variable == "hhincome") %>%
  pivot_longer(names_to = "year", values_to = "estimate", cols = c(est_2009:est_2019)) %>%
  mutate(year = as.numeric(gsub("est_", "", year))) %>%
  select(-GEOID)


income_group <- aggregate(x = income$estimate,
                          by=list(puma_name = income$puma_name, year = income$year),
                          FUN=mean)

names(income_group)[3] <- "median_income"

# gender 

#View(acs1_manhattan)
gender_female <- acs1_manhattan %>%
  filter(variable == "female") %>%
  pivot_longer(names_to = "year", values_to = "estimate", cols = c(est_2009:est_2019)) %>%
  mutate(year = as.numeric(gsub("est_", "", year))) %>%
  select(-GEOID)

gender_male <- acs1_manhattan %>%
  filter(variable == "male") %>%
  pivot_longer(names_to = "year", values_to = "estimate", cols = c(est_2009:est_2019)) %>%
  mutate(year = as.numeric(gsub("est_", "", year))) %>%
  select(-GEOID)

gender = rbind(gender_female,gender_male)

gender_2019 <- gender %>%
  filter (year == 2019)

library(tidyverse)
gender_clean <- gender_2019 %>% 
  group_by(puma_code) %>% 
  mutate(fraction = estimate/sum(estimate)) %>% 
  ungroup 

gender_clean$ymax <- gender_clean$fraction
# Compute the bottom of each rectangle
gender_clean$ymin <- c(0, head(gender_clean$ymax, n=-1))
# Compute label position
gender_clean$labelPosition <- (gender_clean$ymax + gender_clean$ymin) / 2

# Compute a good label
gender_clean$percentage <- label_percent()(gender_clean$fraction)
gender_clean$label <- paste0(gender_clean$variable, "\n value: ", gender_clean$percentage)


names(gender_clean)[1] <- "gender"

# Move situation 
# View(acs5_manhattan)

move <-  acs5_manhattan %>%
  filter(variable == "B07001_017" | variable == "B07001_033" |variable == "B07001_049"|variable == "B07001_065"|variable == "B07001_081")

# segmenting data 

move_clean <- move %>% 
  group_by(puma_code) %>% 
  mutate(fraction = est_2019/sum(est_2019)) %>% 
  ungroup 

library(dplyr)
library(car)
move_clean <- mutate(move_clean, variable= recode(variable, "'B07001_017'='No move'"))
move_clean <- mutate(move_clean, variable= recode(variable, "'B07001_033'='Same county'"))
move_clean <- mutate(move_clean, variable= recode(variable, "'B07001_049'='Different county, same state'"))
move_clean <- mutate(move_clean, variable= recode(variable, "'B07001_065'='Different state'"))
move_clean <- mutate(move_clean, variable= recode(variable, "'B07001_081'='Abroad'"))

names(move_clean)[2] <- "move_from"


# ------ color 
dark2 <- colorRampPalette(brewer.pal(8, "Dark2"))(10)

ggplot(permit_homevalue_and_rent, aes())


## ---------------------------------------------------- DASHBOARD -----------------------------------------
## ---------------------------------------------------- UI -----------------------------------------
ui <- navbarPage("Manhattan Construction",
                 
                 ## ---------------------------------------------------- main -----------------------------------------
                 tabPanel("Main",
                          mainPanel(
                            
                            img(src = "main.png", align = "right", height = "500px"),
                            
                            h2("Manhattan Construction and Neighborhood Changes Over Time"),
                            p("By Melissa S Feeney, Catherine Chen, Natalie Weng, Michelle A. Zee"),
                            br(), br(),
                            "This project examines the Manhattan residential and commercial building construction permits 
                               and associated changes in residents and neighborhoods from 2009 to 2019. Data used include 
                               construction permit data from the NYC Department of Buildings, demographic and home price 
                               data from the American 
                               Community Survey, and neighborhood descriptions scraped from archived Wikipedia pages.",
                            br(), br(),
                            
                            
                            
                            "The data visualizations are broken up into 5 tabs:",
                            br(), br(),
                            
                            "1) ", strong("Construction "), "explores the quality and type of projects in the last 10 years 
                            as well as the relationship between land use",
                            br(), br(),
                            
                            "2) ", strong("Home Value "), "looks at home values and rents and its association with permits and home ownership",
                            br(), br(),
                            
                            "3) ", strong("Demographics "), "examines the people in each neighborhood through indicators like age, population, and income",
                            br(), br(),
                            
                            "4) ", strong("Neighborhood in Words "), "dives into how Wikipedia's users have added to descriptions of the neighborhoods over time",
                            br(), br(),
                            
                            "5)", strong("Construction Data Reference "), "provides the construction data used in the analysis",
                            
                            
                            
                            
                            
                            # fluidRow(
                            #   box(plotOutput("homeprice_vs_permit", height = "45vh"), height = "45vh"),
                            #   box(plotOutput("rent_vs_permit", height = "45vh"), height = "45vh")
                            )
                          ),
                          
                 
                 
                 ## ---------------------------------------------------- melissa -----------------------------------------
                 
                 tabPanel("Construction",
                          mainPanel(
                            fluidRow(
                              h2('Construction across Manhattan'),
                              br(), br(),
                              h4('Overview of Manhattan Construction'),
                              h5('Within the past 20 years, there have been many types of construction projects within Manhattan. Judging by the maps, some of the highest 
                                  concentrations of new building projects have occurred in areas other than Midtown. This is not surprising since Midtown is already congested 
                                  with lots of buildings, without many empty sites on which to build. However, Midtown has seen a fair amount of building alteration projects, 
                                  implying that changes in building occupancy types may be a large motivation in these project decisions.'),
                              br(),
                              h5('The interactive maps below show the precise locations of permits for new building projects and building alteration projects within 
                                  the past 20 years. Permit year, as opposed to completion year, is used in an effort to better illustrate project intention. For ease of use
                                  the permit years are separated into 5 groups- one for permits dated 2000 to 2004, one for those dated 2005 to 2009, one for those dated 
                                  2010 to 2014, one for those dated 2015 to 2019, and finally one for those dated 2020 to present. The map uses clustering to make it easier to
                                  see high concentrations of projects in certain areas when zoomed out. This ability to zoom in and out allows for precise location tracking.'),
                              
                              leafletOutput('const_new_map'),
                              br(), br(), br(),
                              
                              leafletOutput('const_alt_map'),
                              br(), br(), br(),
                            ),
                            
                            fluidRow(
                              h4('Construction Project Composition by Neighborhood'),
                              h5('Over the past 20 years, neighborhood groups (dictated by PUMA) on the West Side of Manhattan have seen some of the highest volume of new 
                                 building projects. With the exception of Harlem neighborhoods, the neighborhood groups in the Upper parts of Manhattan have seen the 
                                 lowest volume of new building projects. It is harder to detect a pattern within new building alterations, as there are a wide variety
                                 of these projects in many neighborhoods. Given these observations, it could be interesting to investigate the changes in intended occupancies
                                 among both types of projects.'),
                              br(),
                              h5('The interactive tree maps below allow for comparison of construction projects among neighborhood groups, as determined by PUMA. 
                                   The sizes of the boxes represent the volume of new building projects or building alteration projects within each neighborhood group.
                                   Hovering over a box will display the number of projects specifically within that neighborhood group.'),
                              br(),   
                              box(plotlyOutput('treemap_newb_int')),
                              box(plotlyOutput('treemap_alt_int')),
                              br(), br(), br(), 
                            ),
                            
                            fluidRow(
                              h4('Building Occupancy Transformations'),
                              h5('As a result of the new building and building alteration projects mentioned above,it is not uncommon for buildings in Manhattan to change
                                 ccupancy types. Each node represents one of the occupancy types, with the size determined by the in-degree of each node, as a way to
                                 represent the transformations into said occupancy type. It is worth noting that since all new building construction projects start from
                                 empty sites, the in-degree of the empity site node is 0. Across both types of construction projects, transformations into residential 
                                 and commercial buildings have been the most frequent in the past 20 years. On the contrary, transformations into educational occupancy 
                                 buildings have been the least frequent in this time range. Given the changes that COVID-19 has inflicted upon office buildings, it could
                                 be interesting to see how the building alteration trend evolves.'),
                              br(), 
                              h5('In the interactive directed network graph below, each node represents a building occupancy type and is labeled as such. Hovering over a node will
                                  display a pop-up which indicates the number of projects in which a building was transformed into the occupancy type of that node. This is
                                  determined by the in-degree of each node, with the direction being dictated by the edge arrows. Clicking on a node will better highlight its 
                                  connections with other nodes, and dragging and/or rearranging the nodes can reveal additional node relationships.'),
                              br(), br(),
                              visNetworkOutput('constr_network')
                            )
                          )
                 ),
                 
                 
                 ## ---------------------------------------------------- michelle -----------------------------------------
                 
                 tabPanel("Home Value",
                          mainPanel(
                            fluidRow(
                                h2("Explore Construction Data with Neighborhood Attributes"),
                                p("This tab shows the relationship between housing prices (home value and rent), construction 
                                  permits, and home ownership. There is a clear relationship between greater construction activity 
                                  in areas of higher home prices and also higher home ownership in the same areas"),
                              br(), br(),

                                selectInput("puma_homevalue",
                                            label = "Choose Neighborhood:",
                                            choices = input_puma,
                                            selected = "Upper West Side & West Side",
                                            width = "50%")),
                            br(), br(),
                            
                            fluidRow(
                              box(plotlyOutput("res_new_permit")),
                              box(plotlyOutput("res_alt_permit"))),
                            br(), br(),
                            
                            fluidRow(
                              box(plotlyOutput("rental_price")),
                              box(plotlyOutput("home_value"))),
                            
                            fluidRow(
                              box(plotlyOutput("renter_pct"))
                              )
                          )
                 ),
                 ## ---------------------------------------------------- catherine -----------------------------------------
                 
                 tabPanel("Demographics",
                          mainPanel(
                            fluidRow(
                              h2("Explore Neighborhood Demographics"),
                              p("The below graphs show the representative demographics 
                                of 10 the neighborhood in Manhattan. These include: median
                                age, population, median income, gender and where they moved from.
                                The demographics painted a vivid picture of what type of people 
                                live in and consist of each neighborhood, and how their migration
                                pattern affects the alteration and progression of city's real estate.
                                
                                It is worth noting that the higher the income and the population, and the younger
                                the population in an neighborhood gets, there is an increased activity 
                                in new building projects.
                                "),
                              
                              
                              selectInput("puma",
                                          label = "Choose Neighborhood:",
                                          choices = input_puma,
                                          selected = "Upper West Side & West Side",
                                          width = "50%")),
                            fluidRow(
                              box(plotOutput("median_age")),
                              box(plotOutput("population"))),
                            br(), br(),
                            
                            fluidRow(
                              plotOutput("income")),
                              br(), br(),
                              
                            fluidRow(
                                box(plotOutput("gender")),
                                box(plotOutput("move"))
                              )
                            )),
                 
                 ## ---------------------------------------------------- natalie -----------------------------------------
                 
                 tabPanel("Neighborhoods in Words",
                          mainPanel(
                            fluidRow(
                              box(
                                h2("How are neighborhoods described through time?"),
                                p("The four graphs below show how a Manhattan neighborhood is described from 2010 to 2021. The word cloud comapres the words that are used to describe a neighborhood in 2010 vs. 2021. The data was collected using the way-back-machine Python API that queries historical versions of a certain webpage stored in Internet Archives."),
                                p("Select the year to compare against 2010 below and the number of words to use in the comparison.")),
                              box(
                                selectInput("nlp_neighborhood",
                                            label = "Choose Neighborhood:",
                                            choices = input_neighborhood,
                                            selected= "Chinatown",
                                            width = "50%"),
                                sliderInput( inputId = "nlp_year",
                                             label="Choose a Year",
                                             value=2019, min=2010, max=2021, sep = "", ticks = FALSE),
                                sliderInput("max",
                                            "Maximum Number of Words:",
                                            min = 50,  max = 300,  value = 100, ticks = FALSE))),
                            br(), br(),
                            
                            fluidRow(
                              column( width=6,
                                      fluidRow("Chosen Year vs. 2010", style = "height:600px; background-color: white;",
                                               plotOutput("wordcloud", width = "600px", height="600px"))),
                              column(width=6,
                                     fluidRow("Most Used Words to Describe a Neighborhood", 
                                              plotOutput("word_freq_graph"), style = "height:600px; background-color:white;",
                                              width="600px", height="600px"))),
                            br(), br(),
                            fluidRow(
                              p(""),
                              p("The below plots show the number of revisions to a neighborhood's 
                                wikipedia page through time as well as the sentiment score using a 
                                positive/negative dictionary."),
                              box(plotlyOutput("wiki_edits_through_time")),
                              box(plotlyOutput("sentiment_score")),
                              p("")),
                            br(), br(),
                            fluidRow(
                              p(""),
                              p("The below shows the number of words used to describe a neighbood's 
                                wikipedia page through time."),
                              plotlyOutput("word_count"),
                              p(""))
                          )
                 ),
                 
                 ## ---------------------------------------------------- melissa construction dataset tab -----------------------------------------
                 
                 tabPanel("Construction Data Reference",
                          mainPanel(
                            fluidRow(
                              h2('Manhattan Construction Data'),
                              br(), br(),
                              p('This tab serves as a reference of the construction data used in this analysis. It contains various points of data
                                for construction projects with completion dates ranging from 2010 through present. However, for this analysis permit date, 
                                as opposed to completion date, was used to better illustrate project intention. Initially, the dataset included detail
                                on new building projects, building alteration projects, and building demolition projects, in every borough of New York City. 
                                For this analysis, building demolitions were excluded.'),
                              h3('New Buildings Data'),
                              DT::dataTableOutput('newbuild_dt'),
                              br(), br(), br(), br(),
                              h3('Building Alterations Data'),
                              DT::dataTableOutput('buildalt_dt')
                            )
                            
                          )
                          
                        )
  )


## ---------------------------------------------------- SERVER -----------------------------------------
server <- function(input, output) {
  
  ## ---------------------------------------------------- michelle -----------------------------------------
  output$homeprice_vs_permit <- renderPlot({
    ggplot(permit_home_value, aes(log(permit_count), estimate, color = borough)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", alpha = 0.3)  +
      scale_y_continuous(labels = scales::dollar) +
      scale_color_brewer(palette = "Dark2", name = "Borough") +
      theme_minimal() +
      labs(x = "Number of Permits (Log Transformed)", y = "Median Home Value",
           title = "NYC Median Home Value vs Residential Construction Permits") +
      theme(legend.position = "none")
  })
  
  output$rent_vs_permit <- renderPlot({
    ggplot(permit_rental_price, aes(log(permit_count), estimate, color = borough)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", alpha = 0.3)  +
      scale_y_continuous(labels = scales::dollar) +
      scale_color_brewer(palette = "Dark2", name = "Borough") +
      theme_minimal() +
      labs(x = "NYC Number of Permits (Log Transformed)", y = "Median Gross Rent (per Month)",
           title = "Median Rent vs Residential Construction Permits") +
      theme(legend.position = "none")
  })
  
  output$res_new_permit <- renderPlotly({
    data <- res_permit_count %>% 
      filter(permit_type == "New Building",
             borough == "Manhattan",
             year >= 2009 & year < 2020) %>%
      group_by(puma_name, year) %>%
      summarise(permit_count = sum(permit_count))
    
    p <- ggplot(data, aes(year, permit_count)) + 
      geom_point(data = subset(data, puma_name != input$puma_homevalue),
                 color = "gray",
                 size = 2, 
                 alpha = 0.5) +
      geom_point(data = subset(data, puma_name == input$puma_homevalue), 
                 color = "#1B9E77",
                 size = 3) +
      scale_x_continuous(breaks= c(2009:2019)) +
      theme_minimal() +
      labs(x = year_lab, y = permit_lab,
           title = "Residential New Building Permits")
    
    ggplotly(p)
    style(p, text = paste(data$puma_name,"\n",
                          'Value: ',data$permit_count))
  })
  
  output$res_alt_permit <- renderPlotly({
    data <- res_permit_count %>%
      filter(permit_type == "Alteration",
             borough == "Manhattan",
             year >= 2009 & year < 2020) %>%
      group_by(puma_name, year) %>%
      summarise(permit_count = sum(permit_count))
    
    p <- ggplot(data, aes(year, permit_count)) +
      geom_point(color = "gray", 
                 size = 2, 
                 alpha = 0.5) +
      geom_point(data = subset(x = data, puma_name == input$puma_homevalue), 
                 color = "#1B9E77",
                 size = 3) +
      scale_x_continuous(breaks= c(2009:2019)) +
      theme_minimal() +
      labs(x = year_lab, y = permit_lab,
           title = "Residential Alteration Permits")
    
    ggplotly(p)
    
    style(p, text = paste(data$puma_name,"\n",
                          'Value: ',data$permit_count))
  })
  
  output$rental_price <- renderPlotly({
    data <- permit_rental_price %>%
      filter(permit_type == "New Building",
             borough == "Manhattan",
             year >= 2009)
    
    p <- ggplot(data, aes(x = year, y = estimate, color = puma_name)) +
      geom_line(size = 1.5) +
      gghighlight(puma_name == input$puma_homevalue) +
      scale_color_manual(values = dark2) +
      scale_x_continuous(breaks= c(2009:2019)) +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal() +
      labs(x = year_lab, y = "Median Gross Rent (Month)",
           title = "Median Rental Prices") +
      theme(legend.position = "none")
    
    ggplotly(p)
    
    style(p, text = paste(data$puma_name,"\n",
                          'Value: ',data$permit_count))
  })
  
  output$home_value <- renderPlotly({
    data <- permit_home_value %>%
      filter(permit_type == "New Building",
             borough == "Manhattan",
             year >= 2009)
    
    p <- ggplot(data, aes(x = year, y = estimate, color = puma_name)) +
      geom_line(size = 1.5) +
      gghighlight(puma_name == input$puma_homevalue) +
      scale_color_manual(values = dark2) +     
      scale_x_continuous(breaks= c(2009:2019)) +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal() +
      labs(x = year_lab, y = "Median Home Value",
           title = "Median Home Value") +
      theme(legend.position = "none")
    
    ggplotly(p)
    
    style(p, text = paste(data$puma_name,"\n",
                          'Value: ',data$permit_count))
  })
  
  output$renter_pct <- renderPlotly({
    data <- renter_pct %>%
      filter(puma_name == input$puma_homevalue)
    
    p <- ggplot(data, aes(year, estimate, fill = variable)) +
      geom_bar(stat = "identity", position = "fill") + 
      facet_wrap(~puma_name) +
      theme_minimal() +
      scale_fill_manual(values = c("owner_pct" = "gray", "renter_pct" = "#1B9E77"), 
                        name = "Occupant", 
                        labels = c("owner_pct" = "Owner", "renter_pct" = "Renter")) +
      scale_x_continuous(breaks= c(2009:2019)) +
      labs(x = year_lab, y = "Proportion",
           title = "Occupancy by Renters vs Owners") +
      theme(legend.position = "bottom")
    
    ggplotly(p)
    
    style(p, text = paste("Owner Proportion: ",round(data[data$variable == "owner_pct", ]$estimate, digits = 3),"\n",
                          "Renter Proportion: ",round(data[data$variable == "renter_pct", ]$estimate, digits = 3)))
  })
  
  
  ## ---------------------------------------------------- melissa -----------------------------------------
  
  output$const_new_map <- renderLeaflet({
    # New Construction Map
    # Prep Pop-up details
    popup_content1 <- paste('Project Address:', manhattan_nb$Complete_Address, '<br/>',
                            'Job Status:',manhattan_nb$Job_Status,'<br/>',
                            'Transformation Type:',manhattan_nb$transformation_type, '<br>',
                            'Building Ownership:',manhattan_nb$Ownership,'<br/>',
                            'PUMA:',manhattan_nb$PUMA2010,'<br/>')
    # Map Title
    map_title1 <- tags$p(tags$style('p {color: black; font-size: 20px}'),
                         tags$b('Construction in Manhattan: \n New Buildings'))
    
    # Color Palette 1 for the Map: Job Permit Year Group
    pal1 = colorFactor('Dark2', domain = manhattan_nb$permit_yr_group) 
    color_permit_yr_group = pal1(manhattan_nb$permit_yr_group)
    
    # Add ability to check the permit year and job type
    manhattan_nb_map <- leaflet(manhattan_nb) %>%
      addTiles() %>%
      setView(lng = -73.98928, lat = 40.75042, zoom = 12) %>%
      addProviderTiles(providers$Wikimedia) %>% 
      
      # Add Permit Year Data
      addCircleMarkers(color = color_permit_yr_group, 
                       popup = popup_content1,
                       group = 'Toggle: Project Permit Year Group',
                       clusterOptions = markerClusterOptions()) %>%
      addLegend(pal = pal1, values = ~manhattan_nb$permit_yr_group, title = 'Project Permit Year Group', position = 'bottomright') %>%
      
      # Layers to add toggle ability
      addLayersControl(#baseGroups = c('Toggle: Project Permit Year Group'),
        options = layersControlOptions(collapsed = FALSE), position = 'bottomright') %>%
      
      # Add map title
      addControl(map_title1, position = 'topright')
    
    manhattan_nb_map
  })
  
  output$const_alt_map <- renderLeaflet({
    # Alteration Map
    # Prep Pop-up details
    popup_content2 <- paste('Project Address:', manhattan_a$Complete_Address, '<br/>',
                            'Job Status:',manhattan_a$Job_Status,'<br/>',
                            'Transformation Type:',manhattan_a$transformation_type, '<br>',
                            'Building Ownership:',manhattan_a$Ownership,'<br/>',
                            'Permit Year:',manhattan_a$PermitYear, '<br>',
                            'PUMA:',manhattan_a$PUMA2010,'<br/>')
    
    # Map Title
    map_title2 <- tags$p(tags$style('p {color: black; font-size: 20px}'),
                         tags$b('Construction in Manhattan: \n Building Alterations'))
    
    # Color Palette 2 for the Map: Job Permit Year
    pal2 = colorFactor('Dark2', domain = manhattan_a$permit_yr_group) 
    color_permit_yr_group = pal2(manhattan_a$permit_yr_group)
    
    
    # Add ability to check the permit year and job type
    manhattan_alt_map <- leaflet(manhattan_a) %>%
      addTiles() %>%
      setView(lng = -73.98928, lat = 40.75042, zoom = 12) %>%
      addProviderTiles(providers$Wikimedia) %>% 
      
      # Add Permit Year Data
      addCircleMarkers(color = color_permit_yr_group, 
                       popup = popup_content2,
                       group = 'Toggle: Project Permit Year Group',
                       clusterOptions = markerClusterOptions()) %>%
      addLegend(pal = pal2, values = ~manhattan_a$permit_yr_group, title = 'Project Permit Year Group', position = 'bottomright') %>%
      
      # Layers to add toggle ability
      addLayersControl(#baseGroups = c('Toggle: Project Permit Year'),
        options = layersControlOptions(collapsed = FALSE), position = 'bottomright') %>%
      
      # Add map title
      addControl(map_title2, position = 'topright')
    
    manhattan_alt_map
  })
  
  ########## Interactive Treemaps   
  # New Buildings
  output$treemap_newb_int <- renderPlotly({
    tree_map_nb_int <- plot_ly(
      tree_map_newb,
      labels = ~ wrapped_puma,
      parents = NA,
      values = ~ project_count,
      type = 'treemap',
      textposition = 'middle center',
      hovertemplate = "PUMA Neighborhood: %{label}<br>Project Count: %{value}<extra></extra>") %>%
      config(
        displaylogo = FALSE) %>%
      layout(title = 'Volume of New Buildings', 
             colorway = mycolors, 
             font = font_styling)
    
    
    tree_map_nb_int
  })  
  
  # Alterations
  output$treemap_alt_int <- renderPlotly({ 
    tree_map_alt_int <- plot_ly(
      tree_map_alt,
      labels = ~ wrapped_puma,
      parents = NA,
      values = ~ project_count,
      type = 'treemap',
      textposition = 'middle center',
      hovertemplate = "PUMA Neighborhood: %{label}<br>Project Count: %{value}<extra></extra>") %>%
      config(
        displaylogo = FALSE) %>%
      layout(title = 'Volume of Building Alterations', 
             colorway = mycolors, 
             font = font_styling)
    
    tree_map_alt_int
  })  
  
  ########## Interactive Network  
  output$constr_network <- renderVisNetwork({
    
    nodes <- data.frame(id = trans_types_vertices$occupancy_type,
                        label = trans_types_vertices$occupancy_type,
                        color = c("#1B9E77", "#666666","#A16864", "#9B58A5", "#D8367D", "#749829", "#BBA90B", "#97722D"))
    
    edges <- data.frame(from = trans_types$from, 
                        to = trans_types$to, 
                        arrows = c("to"),
                        label.family = 'Arial')
    
    graph <- graph.data.frame(edges, directed = T)
    degree_value <- degree(graph, mode = "in")
    nodes$value <- degree_value[match(nodes$id, names(degree_value))]
    nodes$title <- paste0(nodes$value, ' building(s)<br>were transformed into<br>', nodes$label, ' Occupancy')
    
    network_g <- visNetwork(nodes, edges, main = list(text = 'Building Occupancy Type Transformations in Manhattan Construction',
                                                      style = 'font-family:Arial; color: black; font-size:20px; text-align:center;'),
                            submain = list(text = '2000 to Present',
                                           style = 'font-family:Arial; color: black; font-size:20px; text-align:center;')) %>%
      visIgraphLayout(layout = 'layout_in_circle') %>% 
      visLayout(randomSeed = 13)
    network_g
  })
  
  ####
  ################# Data table wrangling

  
  ########## Data Tables
  output$newbuild_dt <- DT::renderDataTable({
    # New Buildings
    manhattan_nb_dt <- manhattan_nb %>% select(Job_Number, Job_Type, pumaname10, Complete_Address, Latitude, Longitude, 
                                               Job_Status, PermitYear, permit_yr_group, Occ_Init, Occ_Prop, Job_Desc, 
                                               transformation_type)
    
    names1 <- c('Job Number', 'Job Type', 'PUMA Name', 'Complete Address', 'Project Latitude', 'Project Longitude', 'Job Status', 
                'Permit Year', 'Permit Year Grouping', 'Initial Occupancy Type', 'Proposed Occupancy Type', 'Occupancy Transformation Type', 
                'Job Description')
    
    # New Buildings DT
    newbuild_dt <- manhattan_nb_dt %>% datatable(rownames = FALSE, colnames = names1, filter = list(position = "top"), 
                                                 options = list(language = list(sSearch = "Filter:")))
    newbuild_dt
  })
  

  output$buildalt_dt <- DT::renderDataTable({
    # Alterations
    manhattan_alt_dt <- manhattan_a %>% select(Job_Number, Job_Type, pumaname10, Complete_Address, Latitude, Longitude, 
                                               Job_Status, PermitYear, permit_yr_group, Occ_Init, Occ_Prop, Job_Desc, 
                                               transformation_type)
    
    names2 <- c('Job Number', 'Job Type', 'PUMA Name', 'Complete Address', 'Project Latitude', 'Project Longitude', 'Job Status', 
                'Permit Year', 'Permit Year Grouping', 'Initial Occupancy Type', 'Proposed Occupancy Type', 'Occupancy Transformation Type', 
                'Job Description')
    
    # Alterations DT
    altbuild_dt <- manhattan_alt_dt %>% datatable(rownames = FALSE, colnames = names2, filter = list(position = "top"), 
                                                 options = list(language = list(sSearch = "Filter:")))
    altbuild_dt
  })
  
  ## ---------------------------------------------------- natalie -----------------------------------------
  
  output$wordcloud <- renderPlot({
    cloud.year = timemachine %>%
      filter(neighborhood==input$nlp_neighborhood) %>%
      filter(year == input$nlp_year) %>%
      head(1) %>%
      select(year, clean_text)
    
    cloud.hist = timemachine %>%
      filter(neighborhood==input$nlp_neighborhood) %>%
      arrange(desc(-1*year)) %>%
      head(1) %>%
      select(year, clean_text)
    
    combined = c(cloud.year$clean_text, cloud.hist$clean_text)
    corpus = Corpus(VectorSource(combined))
    tdm = TermDocumentMatrix(corpus)
    tdm = as.matrix(tdm)
    colnames(tdm) = c(cloud.year$year, cloud.hist$year)
    
    # comparison cloud
    comparison.cloud(tdm, random.order=FALSE,
                     colors = c(dark2[[1]], dark2[[2]]), title.size=1, max.words=input$max)
  })
  
  output$word_freq_graph <- renderPlot({
    dict = tidytext::sentiments
    pos = dict %>% filter(sentiment == "positive") %>% select(word)
    neg = dict %>% filter(sentiment == "negative") %>% select(word)
    
    top_10_words = timemachine %>%
      filter(neighborhood==input$nlp_neighborhood) %>%
      filter(year == input$nlp_year) %>%
      select(year, clean_text) %>%
      rename(text=clean_text, doc_id=year)
    
    top_10 = DataframeSource(top_10_words)
    corpus = VCorpus(top_10)
    tdm <- DocumentTermMatrix(corpus)
    tdm = tidy(tdm) %>%
      filter(term %in% dict[[1]]) %>%
      mutate(pos.neg = ifelse(term %in% pos[[1]], "positive", "negative"))%>%
      dplyr::group_by(document, term, pos.neg) %>%
      summarise(count=median(count)) %>%
      arrange(desc(count)) %>%
      head(10)
    
    ggplot(tdm, aes(x = reorder(term, count),
                    y = count, fill = pos.neg)) +
      geom_bar(stat = "identity") + coord_flip() + theme_minimal() +
      geom_text(aes(label=count), colour = "white", size = 5, position = position_stack(vjust= 0.75)) +
      scale_fill_brewer(palette = "Dark2", direction=-1) + 
      labs(y = "Median Word Usage Frequency", x="Positive/Negative Words",
           title="Top 10 Positive/Negative Words Used")
    
  })
  
  output$wiki_edits_through_time <- renderPlotly({
    df = timemachine %>%
      filter(date >=2009) %>%
      group_by(year, neighborhood) %>%
      summarise(total = n())
    
    g_wiki = 
      ggplot(df, aes(year, total, color=neighborhood)) +
      geom_line(size=1)+
      gghighlight(neighborhood == input$nlp_neighborhood)+
      scale_x_continuous(breaks= c(2009:2021)) +
      scale_color_manual(values=dark2) + 
      labs(x = "Year", y="Total Number of Wikipedia Page Revisions",
           title="Number of Wikipedia Page Revisions by Year") +
      theme_minimal()
    
    ggplotly(g_wiki)
    style(g_wiki, text = paste(df$neighborhood,
                               "\n",'Value: ', df$total,
                               "\n", 'Year: ', df$year))
    
  })
  
  output$sentiment_score <- renderPlotly({
    
    g_sentiment = 
      ggplot(df_sentiment, aes(year, score, color=neighborhood)) +
      geom_line(size=1) + theme_minimal() +
      gghighlight(neighborhood == input$nlp_neighborhood)+
      scale_x_continuous(breaks= c(2009:2021)) +
      scale_color_manual(values=dark2) + 
      labs(x = "Year", y="Positive/Negative Sentiment Score",
           title="Neighborhood Sentiment Through Time")
    
    ggplotly(g_sentiment)
    style(g_sentiment, text = paste(df_sentiment$neighborhood,
                                    "\n",'Value: ', df_sentiment$score,
                                    "\n", 'Year: ', df_sentiment$year))
    
  })
  
  output$word_count <- renderPlotly({
    
    g_wordcount =
      ggplot(df_wordcount, aes(year, count, color=neighborhood)) +
      geom_line(size=1) + theme_minimal() +
      gghighlight(neighborhood == input$nlp_neighborhood)+
      scale_x_continuous(breaks= c(2009:2021)) +
      scale_color_manual(values=dark2) + 
      labs(x = "Year", y="Median Word Count",
           title="Wikipedia Page Length by Year")
    
    ggplotly(g_wordcount)
    style(g_wordcount, text = df_wordcount$neighborhood,
          "\n", 'Value: ', df_wordcount$count,
          "\n", 'Year: ', df_wordcount$year)
    
  })
  
  ## ---------------------------------------------------- catherine -----------------------------------------
  
  output$median_age <- renderPlot({
    
    ggplot(mean_group, aes(year, median_age, colour=puma_name)) + 
      geom_line(aes(group = puma_name)) + 
      scale_color_manual(values = dark2) +
      gghighlight(puma_name == input$puma) +
      theme_minimal() +
      scale_x_continuous(breaks= c(2009:2019)) +
      
      ggtitle("Neighborhood Median Age Through Years") +
      xlab("Year") + ylab("Median Age")
  })

  
  output$population <- renderPlot({
    
    ggplot(pop_group, aes(year, median_population, colour=puma_name)) + 
      geom_line(aes(group = puma_name)) + 
      gghighlight(puma_name == input$puma) +
      theme_minimal() +
      scale_x_continuous(breaks= c(2009:2019)) +
      
      scale_color_manual(values = dark2) +
      scale_x_continuous(breaks= c(2009:2019)) +
      theme_minimal() +
      ggtitle("Neighborhood Median Population Through Years") +
      xlab("Year") + ylab("Median Population")
  })

  
  
  output$income <- renderPlot({
    
    ggplot(income_group, aes(x = year, y = median_income, fill = puma_name)) +
      geom_area(color = "white", alpha = 0.4) +
      scale_fill_manual(values = dark2) +
      scale_x_continuous(breaks= c(2009:2019)) +
      theme_minimal() +
      # gghighlight(puma_name == input$puma) +
      theme_minimal() +
      scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
      labs(title = "",
           subtitle = "Median Household Income by Neighborhood, 2009-2019",
           caption = "Source: ACS",
           x = NULL,
           y = "Median Household Income ($)",
           fill = NULL) +
      theme(panel.grid.major.x = element_blank(),
            legend.position = "right")
         #   height = 400, width = 800)
  
  })
  
  
  output$gender <- renderPlot({
    
    ggplot(data = subset(x = gender_clean, puma_name == input$puma), aes(x="", y=fraction, fill=gender))+
      geom_bar(width = 1, stat = "identity") +
      scale_fill_manual(values = dark2)+
      theme_minimal()+
      theme(legend.key.size = unit(0.3, "cm"),
            legend.margin = margin(0.5, 10, 0.5, 10))+
      coord_polar("y", start=0)+
      labs(fill="gender",
           x=NULL,
           y=NULL, 
           title="Gender Distribution",
           caption="Source: ACS")
    }) 
  
  output$move <- renderPlot({
    
    ggplot(data = subset(x = move_clean, puma_name == input$puma), aes(x="", y=fraction, fill= move_from))+
      geom_bar(width = 1, stat = "identity") +
      theme(axis.line = element_blank() )+
      scale_fill_manual(values = dark2) +
      theme_minimal()+
      coord_polar("y", start=0) +
      
      theme(legend.key.size = unit(0.3, "cm"),
            legend.margin = margin(0.5, 10, 0.5, 10))+
      labs(fill="move_from",
           x=NULL,
           y=NULL, 
           title="Last Move From Distribution",
           caption="Source: ACS")
    }) 
  
  
}

shinyApp(ui, server)


