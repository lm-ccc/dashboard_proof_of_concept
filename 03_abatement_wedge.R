


library(tidyverse)
library(readxl)
library(extrafont)
library(plotly)
library(stringr)


make_abatement_plot <- function() {
  # load ccc theme functions
  source("ccc_ggplot.R")
  
  
  #read areas
  df_abatement <- read_xlsx("input/03/CB6_abatement_types.xlsx", range="A11:BM19")  %>% 
    pivot_longer(`1990`:`2050`, names_to = "year", values_to = "value") %>% 
    mutate(year = as.integer(year))
  
  
  #read lines
  df_paths <- read_xlsx("input/03/CB6_abatement_types.xlsx", range="C3:BM6")  %>% 
    pivot_longer(`1990`:`2050`, names_to = "year", values_to = "value") %>% 
    mutate(year = as.integer(year))
  
  
  fill_list <- c(`Reduce demand`= ccc_theme[5],
                 `Improve efficiency`=ccc_theme[6],
                 `electrification`=ccc_theme[7],
                 `hydrogen and other low-carbon technology`=ccc_theme[8],
                 `Carbon capture from fossil fuels and industry`=ccc_theme[9],
                 `Offset emissions using land and greenhouse gas removals`=ccc_theme[10],
                 `Produce low-carbon energy`=ccc_theme[11],
                 `Residual emissions`="light grey",
                 `Historical`="black",
                 `Baseline`=ccc_theme[1],
                 `Balanced Net Zero Pathway`=ccc_purple)
  
  
  line_list <- fill_list
  
  
  
  plot1 <- df_abatement %>% 
    mutate(Variable = fct_reorder(Variable, plot_order)) %>% 
    mutate(plot_alpha = as.factor(if_else(Variable == "Residual emissions", 0.5, 1))) %>% 
    ggplot(aes(x=year, y=value, text = str_wrap(string = Description,width = 100,indent = 3, exdent = 1))) +
    geom_area(aes(fill=Variable, alpha=plot_alpha)) +
    theme_custom() +
    geom_line(data=df_paths, aes(x=year, y=value, colour = Variable), size =1) +
    scale_fill_manual(values=fill_list) +
    scale_color_manual(values= line_list) +
    scale_alpha_manual(values = c("0.5"=0.5, "1"=1), guide='none') +
    scale_x_continuous(breaks=seq(1990,2050,10))+
    labs(y= "GHG Emissions (tCO2e)", x = "")
  
  plotly1 <- ggplotly(plot1, tooltip = c("Variable", "text")) %>%
    layout(autosize = T, width = 1500, height = 500,
           title = list(xanchor = "left",x = 0.01,
                        text = paste0("Sources of emissions reduction in CCC's Balanced Net Zero Pathway")))
  
  
  for (i in 1:length(plotly1$x$data)){
    if (!is.null(plotly1$x$data[[i]]$name)){
      plotly1$x$data[[i]]$name =  gsub("\\(","",str_split(plotly1$x$data[[i]]$name,",")[[1]][1])
    }
  }
  
  return(plotly1)

}



# Function for module UI ------------------------------------------------------------------------

m03_abatementUI <- function(id) {
  
  fluidPage(
    # Application title
    titlePanel("How could the UK achieve Net Zero?"),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      shinycssloaders::withSpinner(type = 6, plotlyOutput(NS(id, "fig3"))
      )
    )
  )
}

# Function for module server logic ------------------------------------------------------------

m03_abatementServer <- function(id) {
  
  m3_plotly1 <- make_abatement_plot()
  
  moduleServer(id, function(input, output, session) {
    output$fig3 <- renderPlotly({m3_plotly1})
  })
  
}




























