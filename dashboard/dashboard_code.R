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

setwd("G:/My Drive/0 Data Viz/project/Group_J_NYCRealEstate/")

# ------ load data

acs1 <- read_csv("acs_demographics/acs_1yr_2009_2019.csv") %>% filter(borough == "Manhattan") %>% rename(puma_code = PUMA)
acs5 <- read_csv("acs_demographics/acs_5yr_2014_and_2019.csv") %>% filter(borough == "Manhattan") %>% rename(puma_code = PUMA)
construction <- read_csv("construction_data/HousingDB_post2010.csv") %>% rename(puma_code = PUMA2010)
puma <- read_csv("construction_data/HousingDB_by_PUMA.csv") %>% 
  filter(boro == "Manhattan") %>%
  select(puma_code = puma2010, puma_name = pumaname10)

pre_puma <- read.csv('construction_data/HousingDB_by_PUMA.csv') # by puma
pre_post2010 <- read.csv('construction_data/HousingDB_post2010.csv') # detailed

# ------ edit variables

puma$puma_name <- sub("CD.* -", "-", puma$puma_name) # shorten puma name -- can this be shortened further????
puma$puma_name <- gsub("Manhattan - ", "", puma$puma_name)

acs1 <- acs1 %>% left_join(puma, by = "puma_code") # add puma names
acs5 <- acs5 %>% left_join(puma, by = "puma_code")

construction <- construction %>%
  mutate(borough = ifelse(puma_code %in% c(3701:3710), "Bronx",
                          ifelse(puma_code %in% c(4001:4018), "Brooklyn",
                                 ifelse(puma_code %in% c(3801:3810), "Manhattan",
                                        ifelse(puma_code %in% c(4101:4114), "Queens",
                                               ifelse(puma_code %in% c(3901:3903), "Staten Island", NA)))))) %>%
  left_join(puma, by = "puma_code") %>%
  filter(Job_Type != "Demolition",
         borough == "Manhattan")

construction$prop_use_cat <- sub(":.*)", "", construction$Occ_Prop)
construction$prop_use_cat <- ifelse(grepl("Unknown", construction$prop_use_cat), "Unknown", 
                                        ifelse(grepl("Educational", construction$prop_use_cat), "Educational", construction$prop_use_cat))

input_boro <- unique(acs1$borough)
input_puma <- puma$puma_name[order(unique(puma$puma_name), puma$puma_name)]

# ------ wrangle data
med_age <- acs1 %>%
  filter(variable == "med_age") %>%
  pivot_longer(names_to = "year", values_to = "estimate", cols = c(est_2009:est_2019)) %>%
  mutate(year = as.numeric(gsub("est_", "", year))) %>%
  select(-GEOID)

res_permit_count <- construction %>%
  filter(proposed_use_cat == "Residential") %>%
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
  filter(variable == "med_grossrent") %>%
  select(puma_code, year, variable, estimate) %>%
  left_join(res_permit_count, by = c("puma_code", "year"))

permit_home_value <- housing_acs %>% 
  filter(variable == "med_value") %>%
  select(puma_code, year, variable, estimate) %>%
  left_join(res_permit_count, by = c("puma_code", "year"))

puma <- pre_puma %>% select(boro, puma2010, pumaname10)
post2010 <- pre_post2010 %>%
  select(Job_Number, Job_Type, ResidFlag, NonresFlag, Job_Status, CompltYear, Boro, 
         AddressSt, Occ_Init, Occ_Prop, Job_Desc, DateComplt, Landmark, Ownership, 
         NTAName10, PUMA2010, Latitude, Longitude) %>% 
  left_join(puma, by = c('PUMA2010' = 'puma2010'))

# ------- labels
year_lab <- "Year"
age_lab <- "Median Age"
permit_lab <- "Number of Permits"

unique(housing_acs$variable)

# ------ color 
dark2 <- colorRampPalette(brewer.pal(8, "Dark2"))(10)

# ------ wip plots
res_permit_count %>% # breaks by proposed use
  filter(year %in% c(2009:2019)) %>%
  ggplot(aes(year, permit_count, fill = prop_use)) +
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Dark2") +
  scale_x_continuous(breaks= c(2009:2019)) +
  theme_minimal() +
  facet_wrap(~puma_name)




# --------------------- dashboard w/ sidebar panel ----------------------

## -------------------- UI
ui <- navbarPage("Manhattan Construction",
                 
                 tabPanel("Main",
                   mainPanel(
                     h2("Manhattan Construction and Neighborhood Changes Over Time"),
                     h5("By Melissa S Feeney, Catherine Chen, Natalie Weng, Michelle A. Zee"),
                     p("This project examines the Manhattan residential and commercial building construction permits 
                     and associated changes in neighborhoods from 2009 to 2019. Data used include construction permit 
                     data from the NYC Department of Buildings, demographic and home price data from the American 
                     Community Survey, and neighborhood descriptions from Wikipedia.
                       ")
                   )
                 ),
                 
                 tabPanel("Construction",
                          mainPanel(
                            fluidRow(
                              leafletOutput("p_const_new_map"),
                              
                              br(),
                              br(),
                              
                              leafletOutput("p_const_alt_map")
                              )
                            )
                          ),

                 tabPanel("Neighborhood Demographics",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("puma",
                                          label = "Choose NYC Neighborhood:",
                                          choices = input_puma,
                                          selected = "Manhattan"
                                          ),
                              # sliderInput("year",
                              #             label = "Select Year:",
                              #             value = 2019, min = 2009, max = 2019,
                              #             step = 1,
                              #             ticks = FALSE,
                              #             sep = "")
                              ),
                            
                            mainPanel(
                              fluidRow(
                                plotOutput("res_permit"),
                                
                                plotOutput("rental_price"),
                                plotOutput("home_value"),
                                br(), br(),
                                plotOutput("med_age"),
                                
                                        )
                                      )
                                    )
                          ),
                 
                 tabPanel("Neighborhoods in Words")
)

## -------------------- server
server <- function(input, output) {
  output$res_permit <- renderPlot({
    data <- res_permit_count %>% 
      filter(permit_type == "New Building",
             year >= 2009 & year < 2020) %>%
      group_by(puma_name, year) %>%
      summarise(permit_count = sum(permit_count))
    
    ggplot(data, aes(year, permit_count, fill = puma_name)) +
      geom_bar(stat = "identity") +
      gghighlight(puma_name == input$puma) +
      scale_fill_manual(values = dark2) +
      scale_x_continuous(breaks= c(2009:2019)) +
      theme_minimal() +
      labs(x = year_lab, y = permit_lab,
           title = "Number of Residential Permits")
  })
  
  output$rental_price <- renderPlot({
    data <-permit_rental_price %>%
      filter(permit_type == "New Building",
             year >= 2009)

    ggplot(data, aes(x = year, y = estimate, color = puma_name)) +
      geom_line(size = 1) +
      gghighlight(puma_name == input$puma) +
      scale_x_continuous(breaks= c(2009:2019)) +
      scale_y_continuous(labels = scales::dollar) +
      scale_color_manual(values = dark2) +
      theme_minimal() +
      labs(x = year_lab, y = "Median Gross Rent (Month)",
           title = "Median Rental Prices") 
  })
    
  output$home_value <- renderPlot({
    data <- permit_home_value %>%
      filter(permit_type == "New Building", 
             year >= 2009)

    ggplot(data, aes(x = year, y = estimate, color = puma_name)) +
      geom_line(size = 1) +
      gghighlight(puma_name == input$puma) +
      scale_x_continuous(breaks= c(2009:2019)) +
      scale_y_continuous(labels = scales::dollar) +
      scale_color_manual(values = dark2) +
      theme_minimal() +
      labs(x = year_lab, y = "Median Home Value",
           title = "Median Home Value") 
  })
  
  
  output$med_age <- renderPlot({
    ggplot(med_age, aes(year, estimate, color = puma_name)) +
      geom_line(size = 1) +
      gghighlight(puma_name == input$puma) +
      scale_color_manual(values = dark2) +
      scale_x_continuous(breaks= c(2009:2019)) +
      theme_minimal() +
      labs(x = year_lab, y = age_lab,
           title = "Median Age")
  })
  
  output$p_const_new_map <- renderLeaflet({
    # Total Map
    # Prep Pop-up details
    popup_content <- paste('Job Number:',post2010$Job_Number,'<br/>',
                           'Job Status:',post2010$Job_Status,'<br/>',
                           'Initial Occupier:',post2010$Occ_Init,'<br/>',
                           'Proposed Occupier:',post2010$Occ_Prop,'<br/>',
                           'Landmark Status:',post2010$Landmark,'<br/>',
                           'Building Ownership:',post2010$Ownership,'<br/>',
                           'PUMA:',post2010$PUMA2010,'<br/>')
    
    # Map Title
    map_title <- tags$p(tags$style('p {color: black; font-size: 20px}'),
                        tags$b('New Building'))
    
    post2010 <- post2010 %>% filter(CompltYear == 2020,
                                    Job_Type == "New Building")
    
    post2010_map <- leaflet(post2010) %>%
      addTiles() %>%
      addProviderTiles(providers$Wikimedia) %>% 
      
      # Add Job Type Data
      addCircleMarkers(popup = popup_content,
                       clusterOptions = markerClusterOptions()) %>%
      
      # Add map title
      addControl(map_title, position = 'topright')
    
    post2010_map
    
  })
  
  output$p_const_alt_map <- renderLeaflet({
    # Total Map
    # Prep Pop-up details
    popup_content <- paste('Job Number:',post2010$Job_Number,'<br/>',
                           'Job Status:',post2010$Job_Status,'<br/>',
                           'Initial Occupier:',post2010$Occ_Init,'<br/>',
                           'Proposed Occupier:',post2010$Occ_Prop,'<br/>',
                           'Landmark Status:',post2010$Landmark,'<br/>',
                           'Building Ownership:',post2010$Ownership,'<br/>',
                           'PUMA:',post2010$PUMA2010,'<br/>')
    
    # Map Title
    map_title <- tags$p(tags$style('p {color: black; font-size: 20px}'),
                        tags$b('Alteration'))
    
    post2010 <- post2010 %>% filter(CompltYear == 2020,
                                    Job_Type == "Alteration")
    
    post2010_map <- leaflet(post2010) %>%
      addTiles() %>%
      addProviderTiles(providers$Wikimedia) %>% 
      
      addCircleMarkers(popup = popup_content,
                       clusterOptions = markerClusterOptions()) %>%
      
      # Add map title
      addControl(map_title, position = 'topright')
    
    post2010_map
    
  })
}

shinyApp(ui, server)


