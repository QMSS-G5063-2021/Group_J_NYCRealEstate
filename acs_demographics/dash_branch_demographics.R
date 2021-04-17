
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

setwd("~/OneDrive/Documents/MAC/CU/3.Data Visualization/Project/Group_J_NYCRealEstate-main 2")


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

#View(acs1)


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

# View(acs1_manhattan)



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

 
 # Move situation 
# View(acs5_manhattan)
 
 move <-  acs5_manhattan %>%
   filter(variable == "B07001_017" | variable == "B07001_033" |variable == "B07001_049"|variable == "B07001_065"|variable == "B07001_081")
 
 # segmenting data 
 
move_clean <- move %>% 
   group_by(puma_code) %>% 
   mutate(fraction = est_2019/sum(est_2019)) %>% 
   ungroup 
 

ui <- navbarPage("Manhattan Construction",
                
                 tabPanel("Demographics",
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
                              box(plotOutput("median_age")),
                              box(plotOutput("population")),
                              box(plotOutput("income")),
                              box(plotOutput("gender")),
                              box(plotOutput("move"))
                              )
                            )
                          )
                 )
                          



server <- function(input, output) {
  
  output$median_age <- renderPlot({
    
      ggplot(mean_group, aes(as.factor(year), median_age, colour=puma_name)) + 
        geom_line(aes(group = puma_name)) + 
        gghighlight(puma_name == input$puma) +
        ggtitle("Neighborhood Median Age Through Years") +
        xlab("Year") + ylab("Median Age")+
       geom_point()

  }) 
  
  
  output$population <- renderPlot({
    
    ggplot(pop_group, aes(as.factor(year), median_population, colour=puma_name)) + 
      geom_line(aes(group = puma_name)) + 
      gghighlight(puma_name == input$puma) +
      ggtitle("Neighborhood Median Population Through Years") +
      xlab("Year") + ylab("Median Population")+
      geom_point()
  
  }) 
  
  
  output$income <- renderPlot({

    ggplot(income_group, aes(x = year, y = median_income, fill = puma_name)) +
      geom_area(color = "white", alpha = 0.4) +
      scale_fill_brewer(palette = "Paired") +
      scale_x_continuous(breaks= c(2009:2019)) +
     # gghighlight(puma_name == input$puma) +
      scale_y_continuous(expand = c(0, 0), labels = scales::dollar) +
      labs(title = "",
           subtitle = "Median Household Income by Neighborhood, 2009-2019",
           caption = "Source: ACS",
           x = NULL,
           y = "Median Household Income ($)",
           fill = NULL) +
      theme(panel.grid.major.x = element_blank(),
            legend.position = "bottom")
    
    
  }) 
  
  
  output$gender <- renderPlot({
    
    ggplot(data = subset(x = gender_clean, puma_name == input$puma), aes(x="", y=fraction, fill=variable))+
      geom_bar(width = 1, stat = "identity") +
      scale_fill_brewer(palette="Blues")+
      theme_minimal()+
      coord_polar("y", start=0)

    
  }) 
  
  
  output$move <- renderPlot({
    
    ggplot(data = subset(x = move_clean, puma_name == input$puma), aes(x="", y=fraction, fill=variable))+
      geom_bar(width = 1, stat = "identity") +
      scale_fill_brewer(palette="Blues")+
      theme_minimal()+
      coord_polar("y", start=0)
    
    
  }) 
  
  }

shinyApp(ui, server)

























