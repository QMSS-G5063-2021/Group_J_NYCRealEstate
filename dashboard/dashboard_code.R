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
library(plm)
library(lubridate)
library(tm)
library(tidytext)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(plotly)
library(quanteda)


##### Change to my working directory

#setwd('/Users/Melissa/Desktop/Data Visualization SP21/Group_J_NYCRealEstate/')
#setwd("C:/Users/natal/Desktop/QMSS/Spring 2021/Data_Visualization/project/Group_J_NYCRealEstate/")
setwd("G:/My Drive/0 Data Viz/project/Group_J_NYCRealEstate/")

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

timemachine <- read_csv("time-machine-NLP/TimeMachine.csv")
df_sentiment <- read_csv("time-machine-NLP/neighborhood_sentiment_scores.csv")

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
  filter(variable == "med_grossrent") %>%
  select(puma_code, year, variable, estimate) %>%
  left_join(res_permit_count, by = c("puma_code", "year"))

permit_home_value <- housing_acs %>% 
  filter(variable == "med_value") %>%
  select(puma_code, year, variable, estimate) %>%
  left_join(res_permit_count, by = c("puma_code", "year"))

renter_pct <- acs1_manhattan %>%
  filter(variable %in% c("occupied_total", "occupied_renter", "occupied_owner")) %>%
  pivot_longer(names_to = "year", values_to = "estimate", cols = c(est_2009:est_2019)) %>%
  mutate(year = as.numeric(gsub("est_", "", year))) %>%
  pivot_wider(names_from = variable, values_from = estimate) %>%
  mutate(renter_pct = occupied_renter/ occupied_total,
         owner_pct = occupied_owner/ occupied_total) %>%
  select(-occupied_owner, -occupied_renter) %>%
  pivot_longer(names_to = "variable", values_to = "estimate", cols = c(renter_pct, owner_pct))

puma <- pre_puma %>% select(boro, puma2010, pumaname10)
post2010 <- pre_post2010 %>%
  select(Job_Number, Job_Type, ResidFlag, NonresFlag, Job_Status, CompltYear, Boro, 
         AddressSt, Occ_Init, Occ_Prop, Job_Desc, DateComplt, Landmark, Ownership, 
         NTAName10, PUMA2010, Latitude, Longitude) %>% 
  left_join(puma, by = c('PUMA2010' = 'puma2010'))

# - clean TimeMachine.csv data for text analysis
timemachine = timemachine %>%
  mutate(date = substr(date, 1, 8)) %>%
  mutate(date = ymd(date)) %>%
  mutate(year = year(date)) %>%
  mutate(clean_text = tolower(text)) %>%
  mutate(clean_text = removeNumbers(clean_text)) %>%
  mutate(clean_text = stripWhitespace(clean_text)) %>%
  mutate(clean_text = removeWords(clean_text, stopwords("en")))

# ------- labels
year_lab <- "Year"
age_lab <- "Median Age"
permit_lab <- "Number of Permits"
input_neighborhood <- unique(timemachine$neighborhood)

unique(housing_acs$variable)

# ------ color 
dark2 <- colorRampPalette(brewer.pal(8, "Dark2"))(10)

# ------ wip plots

# run some regression models to see if any relationship
#plm(permit_count ~ estimate, index = c("puma_name", "year"), model = "fd", data = permit_rental_price[permit_rental_price$permit_type == "New Building",])


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
                       "),
                     p("Explain background on project....why we chose to focus on Manhattan..."),
                     
                     fluidRow(
                       box(plotOutput("homeprice_vs_permit", height = "45vh"), height = "45vh"),
                       box(plotOutput("rent_vs_permit", height = "45vh"), height = "45vh")
                     )
                   )
                 ),
                 
                 tabPanel("Construction",
                          mainPanel(
                            fluidRow(
                              leafletOutput("const_new_map"),
                              
                              br(),
                              br(),
                              
                              leafletOutput("const_alt_map")
                              )
                            )
                          ),

                 tabPanel("Neighborhood Attributes",
                            mainPanel(
                              fluidRow(
                                  h2("Explore Construction Data with Neighborhood Attributes"),
                                  p("Some text explaining graphs"),
                                  
                                  selectInput("puma",
                                              label = "Choose Neighborhood:",
                                              choices = input_puma,
                                              selected = "Upper West Side & West Side",
                                              width = "50%")),

                              fluidRow(
                                box(plotlyOutput("res_new_permit")),
                                box(plotlyOutput("res_alt_permit"))),
                              br(), br(),
                              
                              fluidRow(
                                box(plotlyOutput("rental_price")),
                                box(plotlyOutput("home_value"))),
                              
                              fluidRow(
                                box(plotOutput("renter_pct")),
                                box())
                              )
                          ),
                 
                 tabPanel("Neighborhoods in Words",
                          mainPanel(
                            fluidRow(
                              h2("How are neighborhoods described through time?"),
                              p("The below compares and contrasts words used to describe a Manhattan neighborhood through two points in time."),
                              
                              selectInput("nlp_neighborhood",
                                          label = "Choose Neighborhood:",
                                          choices = input_neighborhood,
                                          selected= "Chinatown",
                                          width = "50%")),
                            fluidRow(sliderInput( inputId = "nlp_year",
                                                  label="Choose a Year",
                                                  value=2019, min=2010, max=2021),
                                     sliderInput("max",
                                                 "Maximum Number of Words:",
                                                 min = 50,  max = 300,  value = 100)),
                            fluidRow(
                              column(3,p("Chosen Year"),offset=4),
                              plotOutput("wordcloud"),
                              column(3, p("2010"), offset=4)),
                            fluidRow(
                              plotlyOutput("wiki_edits_through_time")),
                            fluidRow(
                              column(3),
                              plotlyOutput("sentiment_score"))
                            )
                          )
)

## -------------------- server
server <- function(input, output) {
  
  output$homeprice_vs_permit <- renderPlot({
    ggplot(permit_home_value, aes(log(permit_count), estimate, color = borough)) +
      geom_point(alpha = 0.5) +
      geom_smooth(method = "lm", alpha = 0.3)  +
      scale_y_continuous(labels = scales::dollar) +
      scale_color_brewer(palette = "Dark2", name = "Borough") +
      theme_minimal() +
      labs(x = "Number of Permits (Log Transformed)", y = "Median Home Value",
           title = "NYC Median Home Value vs Residential Construction Permits") +
      theme(legend.position = "bottom")
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
      theme(legend.position = "bottom")
  })
  
  output$res_new_permit <- renderPlotly({
    data <- res_permit_count %>% 
      filter(permit_type == "New Building",
             borough == "Manhattan",
             year >= 2009 & year < 2020) %>%
      group_by(puma_name, year) %>%
      summarise(permit_count = sum(permit_count))
    
    p <- ggplot(data, aes(year, permit_count)) + 
      geom_point(color = "gray",
                 size = 2, 
                 alpha = 0.5) +
      geom_point(data = subset(x = data, puma_name == input$puma), 
                 color = "#1B9E77",
                 size = 3) +
      scale_x_continuous(breaks= c(2009:2019)) +
      theme_minimal() +
      labs(x = year_lab, y = permit_lab,
           title = "Residential New Building Permits")
    
    ggplotly(p)
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
      geom_point(data = subset(x = data, puma_name == input$puma), 
                 color = "#1B9E77",
                 size = 3) +
      scale_x_continuous(breaks= c(2009:2019)) +
      theme_minimal() +
      labs(x = year_lab, y = permit_lab,
           title = "Residential Alteration Permits")
    
    ggplotly(p)
  })
  
  output$rental_price <- renderPlotly({
    data <- permit_rental_price %>%
      filter(permit_type == "New Building",
             borough == "Manhattan",
             year >= 2009)

    p <- ggplot(data, aes(x = year, y = estimate, color = puma_name)) + 
      geom_line(aes(x = year, y = estimate, color = puma_name), alpha = 0.3) +
      geom_line(data = subset(x = data, puma_name == input$puma), 
                 color = "#1B9E77",
                 size = 1.5) +      
      scale_x_continuous(breaks= c(2009:2019)) +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal() +
      labs(x = year_lab, y = "Median Gross Rent (Month)",
           title = "Median Rental Prices") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$home_value <- renderPlotly({
    data <- permit_home_value %>%
      filter(permit_type == "New Building",
             borough == "Manhattan",
             year >= 2009)

    p <- ggplot(data, aes(label = puma_name, label2 = estimate)) + # ----------------------------------- figure out textbox info
      geom_line(aes(x = year, y = estimate, color = puma_name), alpha = 0.3) +
      geom_line(data = subset(x = data, puma_name == input$puma),
                aes(x = year, y = estimate),
                color = "#1B9E77",
                size = 1.5) +     
      scale_x_continuous(breaks= c(2009:2019)) +
      scale_y_continuous(labels = scales::dollar) +
      theme_minimal() +
      labs(x = year_lab, y = "Median Home Value",
           title = "Median Home Value") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  output$renter_pct <- renderPlot({
    data <- renter_pct %>%
      filter(puma_name == input$puma)
    
    ggplot(data, aes(year, estimate, fill = variable)) +
      geom_bar(stat = "identity", position = "fill") + 
      facet_wrap(~puma_name) +
      theme_minimal() +
      scale_fill_manual(values = c("owner_pct" = "gray", "renter_pct" = "#1B9E77"), 
                        name = "Occupant", 
                        labels = c("owner_pct" = "Owner", "renter_pct" = "Renter")) +
      scale_x_continuous(breaks= c(2009:2019)) +
      labs(x = year_lab, y = "Proportion",
           title = "Occupancy by Renters vs Owners")
  })
  
  
  # output$med_age <- renderPlot({
  #   ggplot(med_age, aes(year, estimate, color = puma_name)) +
  #     geom_line(size = 1.5) +
  #     gghighlight(puma_name == input$puma) +
  #     scale_color_manual(values = dark2) +
  #     scale_x_continuous(breaks= c(2009:2019)) +
  #     theme_minimal() +
  #     labs(x = year_lab, y = age_lab,
  #          title = "Median Age")
  # })
  
  output$const_new_map <- renderLeaflet({
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

  output$const_alt_map <- renderLeaflet({
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
  
  output$wiki_edits_through_time <- renderPlotly({
    df = timemachine %>%
      group_by(year, neighborhood) %>%
      summarise(total = n())
    
    ggplotly(
      ggplot(df, aes(year, total, color=neighborhood)) +
      geom_line(size=1)+
      gghighlight(neighborhood == input$nlp_neighborhood)+
      scale_color_manual(values=dark2) + 
      labs(x = "Year", y="Total Number of Wikipedia Page Revisions",
           title="Number of Wikipedia Page Revisions by Year") +
      theme_minimal()
    )
  })
  
  output$sentiment_score <- renderPlotly({
  
    ggplotly(
        ggplot(df_sentiment, aes(year, score, color=neighborhood)) +
          geom_line(size=1) + theme_minimal() +
          gghighlight(neighborhood == input$nlp_neighborhood)+
          scale_color_manual(values=dark2) + 
          labs(x = "Year", y="Positive/Negative Sentiment Score",
               title="Neighborhood Sentiment Through Time")
    )
  })
}

shinyApp(ui, server)


