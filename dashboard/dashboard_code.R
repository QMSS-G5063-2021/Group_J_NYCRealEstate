library(shiny)
library(shinythemes)
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(readr)
library(scales)


setwd("G:/My Drive/0 Data Viz/project/Group_J_NYCRealEstate/")

# Load data
demographics <- read_csv("acs_demographics/acs_1yr_2009_2019.csv")
construction <- read_csv("construction_data/HousingDB_post2010.csv")

med_age <- demographics %>%
  filter(variable == "med_age") %>%
  pivot_longer(names_to = "year", values_to = "estimate", cols = c(est_2009:est_2019)) %>%
  mutate(year = as.numeric(gsub("est_", "", year))) %>%
  select(-GEOID)

boroughs <- unique(demographics$borough)

## -------------------- UI
ui <- dashboardPage(
  skin = "green",
  
  dashboardHeader(title = "NYC Construction"),
  
  dashboardSidebar(
      selectInput("borough", 
                  label = "Choose NYC Borough:",
                  choices = boroughs,
                  selected = "Manhattan"),
      
      sliderInput("year",
                  label = "Select Year:",
                  value = 2010, min = 2009, max = 2019, ## how to change label text color??
                  sep = "")
  ),
  
  dashboardBody(
    fluidRow(
      
      plotOutput("plot1")
      )
    )
  )

## -------------------- server
server <- function(input, output) {
  output$plot1 <- renderPlot({
    age <- subset(med_age,
                  year <= input$year &
                  borough == input$borough)
  
    ggplot(age, aes(year, estimate, color = as.factor(PUMA))) +
      geom_line() +
      scale_x_continuous(breaks= c(2009:2019))
  })
}

shinyApp(ui, server)


