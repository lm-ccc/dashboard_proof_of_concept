


library(tidyverse)
library(readxl)
library(extrafont)
library(plotly)
library(stringr)

# load ccc theme functions
source("ccc_ggplot.R")


#read areas
df_abatement <- read_xlsx("input/04/policy_wedge_data.xlsx", range="A7:S12")  %>% 
  pivot_longer(`2020`:`2035`, names_to = "year", values_to = "value") %>% 
  mutate(year = as.integer(year))


#read lines
df_paths <- read_xlsx("input/04/policy_wedge_data.xlsx", range="B2:S4")  %>% 
  pivot_longer(`2020`:`2035`, names_to = "year", values_to = "value") %>% 
  mutate(year = as.integer(year))


fill_list <- c(`Fully on track`= ccc_theme[9],
               `Potentially on track`=ccc_theme[8],
               `Some action but risk of falling behind`=ccc_theme[6],
               `Falling behind`=ccc_theme[7],
               `Residual emissions`="light grey",
               `Baseline emissions`=ccc_theme[1],
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
  scale_x_continuous(breaks=seq(2020,2035,5))+
  labs(y= "Emissions (MtCO2e)", x = "")

plotly1 <- ggplotly(plot1, tooltip = c("Variable", "text")) %>%
  layout(width = 1000)


for (i in 1:length(plotly1$x$data)){
  if (!is.null(plotly1$x$data[[i]]$name)){
    plotly1$x$data[[i]]$name =  gsub("\\(","",str_split(plotly1$x$data[[i]]$name,",")[[1]][1])
  }
}





# Function for module UI ------------------------------------------------------------------------

m04_policy_wedgeUI <- function(id) {
  
  fluidPage(
    # Application title
    titlePanel("Is Government policy on track?"),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      shinycssloaders::withSpinner(type = 6, plotlyOutput(NS(id, "fig3"))
      )
    )
  )
}

# Function for module server logic ------------------------------------------------------------

m04_policy_wedgeServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    output$fig3 <- renderPlotly({plotly1})
  })
  
}




























