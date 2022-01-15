# Installing Packages

r_packages <- c(

"tidyverse",    # tidyverse contains dplyr, ggplot2, tidyr, stringr, forcats, tibble, purrr, readr
"tidyquant",    # Financial Analysis R Packages
"qcc",
"ggplot2",      # Visualizations  
"plotly",       # Visualizations
"gganimate",    # Animations 
"knitr",        # Exporting 
"gridExtra",    # 
"data.table",
"rsconnect",
"contrib.url",
"shiny",          # Integrate UI, Elements & Reactivity
"shinydashboard", # Creating dashboards
"flexdashboard",  # RMarkdown format for creating web applications
"shinyWidgets",   # Some more widgets
"shinyjs",        # Integrate JS functionality into Web Apps.
"parsnip",        # Modeling API
"rsample",        # Sampling API
"xgboost"         # Extreme Gradient Boosted Trees
)

install.packages(r_packages)

# Loading libraries
library(shiny)
library(shinydashboard)
library(flexdashboard)
library(qcc)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(plotly)
library(gganimate)
library(purrr)
library(data.table)
library(knitr)
library(gridExtra)
library(png)
library(gifski)
library(rsconnect)
library(leaflet)

# Reading CSV File

athletes = read.csv("athlete_events.csv", stringsAsFactors = F)
col_types = cols(
  ID = col_character(),
  Name = col_character(),
  Sex = col_factor(levels = c("M","F")),
  Age =  col_integer(),
  Height = col_double(),
  Weight = col_double(),
  Team = col_character(),
  NOC = col_character(),
  Games = col_character(),
  Year = col_number(),
  Season = col_factor(levels = c("Summer","Winter")),
  City = col_character(),
  Sport = col_character(),
  Event = col_character(),
  Medal = col_factor(levels = c("Gold","Silver","Bronze"))
)

# LOAD DATA MATCHING NOCs (NATIONAL OLYMPIC COMMITTEE) WITH COUNTRIES
NOCs <- read_csv("noc_regions.csv", col_types = cols(
  NOC = col_character(),
  region = col_character()
))


## Clearing Data by removing NAs from "Medal" Column
cleared_data = athletes %>%
  filter(Medal != "<NA>") ## retain all data which is not NA
cleared_data$Year =as.factor(cleared_data$Year)
head(cleared_data)


## Medals won by Pakistan
medalsbyPak = subset(cleared_data, Team == "Pakistan")
head(medalsbyPak)

## Medals won by each country
countrywiseMEDALS = cleared_data %>% group_by(Team) %>% 
  summarise(Medals = length(Medal))%>% 
  arrange(desc(Medals))
head(countrywiseMEDALS)

## top ten Medals wining countries
countrywiseMEDALS %>% top_n(10) %>% ggplot(aes(x =Medals, y= Team))+
  geom_bar(stat = 'identity', colour="white", fill="red")+theme_dark()

## Medals by countries per year
peryearMedals = cleared_data %>% 
  group_by(Team, Year) %>%
  summarise(Total = n())
head(peryearMedals)

## Medals by selected countries per year
medalsbyselectedCOUNTRIES = peryearMedals %>% 
  filter(Team %in% c("USA", "Russia", "China", "Germany", "France"))
head(medalsbyselectedCOUNTRIES)


ui <- dashboardPage(
  dashboardHeader(title = "My R Project Dashboard"),
  dashboardSidebar(),
  dashboardBody(
    
    # Reading CSV File

    
    
    box(plotOutput("correlation_plot"), width = 10)
  )
)

server <- function(input, output){
output$correlation_plot<-renderPlot({
  plot(iris$Sepal.Length, iris$Sepal.Length)
}
  
  
)

}

shinyApp(ui, server)