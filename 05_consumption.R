




library(ggalluvial)
library(ggplot2)
library(tidyverse)
library(readxl)
library(networkD3)
library(glue)
library(htmlwidgets)
library(htmltools)
library(scales)

source("ccc_ggplot.R")


df_scon <- "input/05/scotlands-carbon-footprint-1998-2017.xlsx" %>%
  read_xlsx(sheet = "2017 GHG", range = "D126:H134") %>% 
  rename(source = 1) %>% 
  pivot_longer(UK:`Rest of world`, names_to = "region", values_to = "emissions") 


df_sanky <- df_scon %>% 
  group_by(source) %>%
  mutate(source_total = sum(emissions, na.rm = TRUE)) %>%
  ungroup() %>% 
  mutate(source_rank = dense_rank(-source_total)) %>%
  group_by(region) %>% 
  mutate(region_total = sum(emissions, na.rm = TRUE)) %>% 
  ungroup() %>%  
  mutate(source = if_else(source_rank >=6, "Other", source)) %>% 
  group_by(source, region) %>% 
  summarise(emissions = sum(emissions, na.rm = TRUE), .groups = "drop") %>% 
  group_by(source) %>%
  mutate(source_total = sum(emissions, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(region) %>% 
  mutate(region_total = sum(emissions, na.rm = TRUE)) %>% 
  ungroup() %>%  
  arrange(-source_total, -region_total) %>% 
  mutate(source = glue("{source} ({signif(source_total, 3)} MtCO2e)"),
         region = glue("{region} ({signif(region_total, 3)} MtCO2e)")) 




# From these flows we need to create a node data frame: it lists every entities involved in the flow
nodes <- data.frame(name=c(as.character(df_sanky$source), as.character(df_sanky$region)) %>% unique())

df_sanky$IDsource=match(df_sanky$source, nodes$name)-1 
df_sanky$IDtarget=match(df_sanky$region, nodes$name)-1

# # if you want to remove node labels:
# nodes$name <- " "

# prepare colour scale
ColourScal =glue('d3.scaleOrdinal() .range([\"{ccc_theme[1]}\",\"{ccc_theme[2]}\","{ccc_theme[3]}\",\"{ccc_theme[4]}\",\"{ccc_theme[5]}\",\"{ccc_theme[6]}\"])')
ColourScal =glue('d3.scaleOrdinal() .range([\"{ccc_theme[2]}\",\"{ccc_theme[2]}\","{ccc_theme[3]}\",\"{ccc_theme[4]}\",\"{ccc_theme[5]}\",\"{ccc_theme[6]}\"])')
ColourScal

# # prepare color scale: give one specific color for each node.
# ColourScal <- glue('d3.scaleOrdinal() .domain([{nodes$name[1]}, {nodes$name[2]}, {nodes$name[3]}, {nodes$name[4]}, {nodes$name[5]}, {nodes$name[6]}, {nodes$name[7]}, {nodes$name[8]}, {nodes$name[9]}, {nodes$name[10]}]) 
#                    .range(["blue", "blue" , "blue", "red", "red", "yellow", "purple", "purple"])'


# Make the Network
sankey<- sankeyNetwork(Links = df_sanky, Nodes = nodes,
                       Source = "IDsource", Target = "IDtarget",
                       Value = "emissions", NodeID = "name", 
                       sinksRight=FALSE, colourScale=ColourScal, nodeWidth=40, fontSize=13, 
                       nodePadding=20, width = 1200, height = 1000, fontFamily = base_family)








# Function for module UI ------------------------------------------------------------------------

m05_consumptionUI <- function(id) {
  
  fluidPage(
    # Application title
    titlePanel("Where do our consumption emissions come from?"),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      shinycssloaders::withSpinner(type = 6, sankeyNetworkOutput(NS(id, "sankey"))
      )
    )
  )
}

# Function for module server logic ------------------------------------------------------------

m05_consumptionServer <- function(id) {
  
  m3_plotly1 <- make_abatement_plot()
  
  moduleServer(id, function(input, output, session) {
    output$sankey <- renderSankeyNetwork(sankey)
  })
  
}






