

library(shiny)
library(tidyverse)
library(plotly)
#library(shinydashboard)
library(glue)
library(extrafont)
library(scales)
library(janitor)

# load ccc theme functions
source("ccc_ggplot.R")


# global variables
key_countries <- c("United Kingdom", "United States", "China")

# read in datasets
df_world_gdp_pc <- read_csv("input/01/gdp-per-capita-worldbank.csv")
df_world_co2_pc <- read_csv("input/01/global_per_capita_co2_1750.csv")
df_world_pop    <- read_csv("input/01/population-by-country.csv")


# plotly prep for historical emissions --------------

# retrieve recent GDP data (pre covid)
df_gdp_pc_2018 <- df_world_gdp_pc %>% 
  filter(Year == "2018") %>% 
  select(-Year)

# rename population column name
df_world_pop <- df_world_pop %>% 
  rename(population = 4)

# define which years to pick out for animation
years  <- seq(1850,2015,5)

##plotly setup, preparing a slightly different dataset

df_co2_gdp18_plotly <- df_world_co2_pc %>% 
  full_join(df_gdp_pc_2018, by = c("Entity", "Code")) %>% 
  left_join(df_world_pop, by= c(c("Entity", "Code", "Year"))) %>% 
  rename(tCO2_per_capita = 4, GDP_per_capita_2018 = 5 ) %>% 
  filter(!is.na(Year), !is.na(Code), !Entity=="World", Year>=1850) %>% 
  mutate(Year = as.integer(Year)) %>% 
  mutate(pcolour = case_when(Entity=="United States" ~ ccc_theme[1],
                             Entity=="United Kingdom" ~ ccc_theme[2],
                             Entity=="China" ~ ccc_theme[7],
                             TRUE ~ "grey")) %>% 
  identity()



##simple ggplotly method - this is really working nicely - don't delete it!

plot_font <- list(family = "Century Gothic", size = 12, colour = ccc_purple)

plt_ghg_gdp4 <- df_co2_gdp18_plotly %>% 
  rename(`Emissions per person (tCO2e)` = "tCO2_per_capita",
         Population = population) %>% 
  ggplot(aes(x=GDP_per_capita_2018, y=`Emissions per person (tCO2e)`, frame=Year, ids=Entity)) +
  geom_point(aes(size = Population),
             alpha = ifelse(df_co2_gdp18_plotly$Entity %in% key_countries, 1,0.5),
             colour = df_co2_gdp18_plotly$pcolour)+
  geom_text(aes(label=ifelse(Entity %in% key_countries, Entity, "")), 
            family = base_family,hjust=0,nudge_x = 10000) +
  ylim(c(0,23)) +
  xlim(c(0,1e+5))+
  scale_size_continuous(range = c(1, 10))+
  theme_custom()+
  theme(legend.position = "none")+
  labs(subtitle = "The United Kingdom has a large historical contribution to climate change",
       y        = "Emissions per person (tCO2e)",
       x        = "GDP per person in 2018 ($)")


fig <- plt_ghg_gdp4 %>% 
  ggplotly(tooltip = c("Emissions per person (tCO2e)", "Population")) %>% 
  style(hoverlabel = list(bgcolor = "white", bordercolor = "black", bordercolorsrc = "purple", font = plot_font)) %>%
  layout(plot_bgcolor  = "transparent",
         paper_bgcolor = "transparent") %>% 
  config(displayModeBar = FALSE) %>% 
  animation_slider(tickcolor = "white")




# Function for module UI ------------------------------------------------------------------------

m01_historicalUI <- function(id) {

  fluidPage(
    # panel title 
    titlePanel("Context: The UK has a large historical contribution to climate change"),
    
    
    # load in the plotly figure
    fluidRow(
      column(8,
      shinycssloaders::withSpinner(type = 6, plotlyOutput(NS(id, "fig1"))),
      downloadButton(NS(id, "downloadData"), 'Download data')
      ),
      column(3,  br(), br(),  br(), br(), 
             "Some example text describing the chart. gb gf gbt sf ljihab librnaej nsdk nail na", 
             br(), br(), "New paragraph, more text... gbw rg nihaublraj fd ksdukneiludfl iul",
             shinydashboardPlus::box("test", title = "title", collapsible = TRUE)))
  )
  
}

# Function for module server logic ------------------------------------------------------------

m01_historicalServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    output$fig1 <- renderPlotly({fig})
    
    output$downloadData <- downloadHandler(
      filename = function() { 
        paste("dataset-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(df_co2_gdp18_plotly, file)
      })
  })
}

