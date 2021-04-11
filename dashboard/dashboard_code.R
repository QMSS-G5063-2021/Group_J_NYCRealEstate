library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(readr)
library(scales)
library(RColorBrewer)
library(leaflet)

setwd("G:/My Drive/0 Data Viz/project/Group_J_NYCRealEstate/")

# Load data
demographics <- read_csv("acs_demographics/acs_1yr_2009_2019.csv")
construction <- read_csv("construction_data/HousingDB_post2010.csv")
puma <- read_csv("construction_data/HousingDB_by_PUMA.csv") %>% select(puma2010, pumaname10)

pre_puma <- read.csv('construction_data/HousingDB_by_PUMA.csv') # by puma
pre_post2010 <- read.csv('construction_data/HousingDB_post2010.csv') # detailed

# edit variables
puma$pumaname10 <- sub("CD.* -", "-", puma$pumaname10) # shorten puma name -- can this be shortened further????

demographics_new <- demographics %>% left_join(puma, by = c("PUMA" = "puma2010")) # add puma names


input_boro <- unique(demographics$borough)
input_puma <- puma$pumaname10[order(unique(puma$pumaname10), puma$pumaname10)]

# wrangle data
med_age <- demographics_new %>%
  filter(variable == "med_age") %>%
  pivot_longer(names_to = "year", values_to = "estimate", cols = c(est_2009:est_2019)) %>%
  mutate(year = as.numeric(gsub("est_", "", year))) %>%
  select(-GEOID)

puma <- pre_puma %>% select(boro, puma2010, pumaname10)
post2010 <- pre_post2010 %>%
  select(Job_Number, Job_Type, ResidFlag, NonresFlag, Job_Status, CompltYear, Boro, 
         AddressSt, Occ_Init, Occ_Prop, Job_Desc, DateComplt, Landmark, Ownership, 
         NTAName10, PUMA2010, Latitude, Longitude) %>% 
  left_join(puma, by = c('PUMA2010' = 'puma2010'))

# labels
year_lab <- "Year"
age_lab <- "Median Age"


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
                              box(width = 12, 
                                  
                                  leafletOutput("p_const_new_map")
                              ),
                              
                              br(),
                              br(),
                              
                              box(width = 12, 
                                  
                                  leafletOutput("p_const_alt_map")
                              )
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
                              sliderInput("year",
                                          label = "Select Year:",
                                          value = 2012, min = 2009, max = 2019,
                                          step = 1,
                                          ticks = FALSE,
                                          sep = "")
                            ),
                            
                            mainPanel(
                              fluidRow(
                                plotOutput("p_med_age")
                                        )
                                      )
                                    )
                          ),
                 
                 tabPanel("Neighborhoods in Words")
)

## -------------------- server
server <- function(input, output) {
  output$p_med_age <- renderPlot({
    age <- subset(med_age,
                  year <= input$year &
                    #  borough == input$borough
                    pumaname10 == input$puma)
    ggplot(age, aes(year, estimate, color = as.factor(pumaname10))) +
      geom_line() +
      scale_x_continuous(breaks= c(2009:2019)) +
      labs(x = year_lab, y = age_lab,
           title = "Median Age by PUMA")
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


