
library(plotly)
library(shiny)
library(readxl)

df_historical_raw  <- read_xlsx("input/02/historical_emissions_DAs.xlsx")

df_cb6_metrics <- read_xlsx("input/02/The-Sixth-Carbon-Budget-Dataset.xlsx", 
                            sheet = "Scenario key metrics", 
                            range = "C4:AJ73")


df_historical_uk  <- read_xlsx("input/02/historical_emissions.xlsx")


# clean key metrics

metrics <- c("Existing homes: Heat pumps (ASHP and GSHP, individual and communal)",
             "Low carbon hydrogen demand","BEVs", "Trees planted")

df_cb6_metrics_clean <- df_cb6_metrics %>% 
  filter(Element %in% metrics) %>% 
  select(-Category) %>% 
  pivot_longer(`2020`:`2050`, names_to = "Year", values_to = "value") %>% 
  mutate(value = ifelse(Element=="BEVs", round(value,2), value)) %>%  
  mutate(value = round(value,2)) %>% 
  mutate(Element = ifelse(Element==metrics[1], "Heat Pumps", Element)) %>% 
  mutate(Units = case_when(Units == "Deployment (millions)" ~ "million heat pumps", 
                           Units == "Kha/year" ~ "trees planted (kha/Year)",
                           Units == "TWh" ~ "TWh of low carbon hydrogen",
                           Units == "# (millions)" ~ "million EVs sold",
                           TRUE ~ Units)) %>% 
  mutate(Text = glue("{value}\n {Units}")) %>% 
  filter(Year != 2020)



df_BNZP <- df_cb6_metrics %>% 
  filter(Element == "Total emissions") %>% 
  pivot_longer(`2020`:`2050`, names_to = "Year", values_to = "Emissions") %>% 
  mutate(Year = as.integer(Year), type = "Balanced Net Zero Pathway") %>% 
  select(type, Year, Emissions) %>% 
  filter(Year != 2020)

df_historical_clean <- df_historical_uk %>% 
  pivot_longer(`1990`:`2020`, names_to = "Year", values_to = "Emissions") %>% 
  mutate(Year = as.integer(Year), type = "Historical") %>% 
  filter(Sector == "Total") %>% 
  select(type, Year, Emissions) 

df_emissions <- bind_rows(df_historical_clean, df_BNZP) %>% 
  mutate(percentage_reduction = Emissions/first(Emissions)-1) %>%
  adorn_pct_formatting(,digits = 0,,percentage_reduction)


# plotly prep for pathway ------------

plot_font <- list(family = "Century Gothic", size = 12, colour = ccc_purple)


fig_a <- plot_ly(data = df_historical_clean, x=~Year, y=~Emissions) %>%
  add_lines(name = "Historical", line = list(color = ccc_theme[1])) %>%
  add_lines(name = "CCC Pathway", data = df_BNZP, x=~Year, y=~Emissions, line = list(color = ccc_purple)) %>%
  add_trace(name = "Current value", data = df_BNZP, frame = ~Year, mode="markers", marker = list(color = ccc_theme[1], size = 10)) %>%
  layout(font=plot_font, yaxis = list(title = "Emissions"), xaxis = list(showgrid = F, title = "", range=c(1990,2050))) 


# fig2 - the key stats plot
fig_b <- df_cb6_metrics_clean %>%
  plot_ly(x = 0, y=~Element, type = "scatter", mode = "markers",
          frame = ~Year, opacity = 0) %>%
  add_text(text = ~Text, textfont = list(size=15), opacity = 1) %>%
  layout(
    xaxis = list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE),
    yaxis = list(
      title = "",
      zeroline = FALSE,
      showline = FALSE,
      showticklabels = FALSE,
      showgrid = FALSE)
  )

myfont <- list(family = "Century Gothic",color = ccc_purple)


fig2 <- subplot(fig_a, fig_b) %>% 
  hide_legend() %>% 
  layout(plot_bgcolor  = "transparent",
         paper_bgcolor = "transparent", 
         font=myfont) %>% 
  config(displayModeBar = FALSE) %>% 
  animation_slider(tickcolor = "white")
  




# Function for module UI ------------------------------------------------------------------------

m02_pathwayUI <- function(id) {

  fluidPage(
    # Application title
    titlePanel("CCC's balanced Net Zero pathway"),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      shinycssloaders::withSpinner(type = 6, plotlyOutput(NS(id, "fig2"))
      )
    )
  )
}

# Function for module server logic ------------------------------------------------------------

m02_pathwayServer <- function(id) {
  
  moduleServer(id, function(input, output, session) {
    output$fig2 <- renderPlotly({fig2})
  })
  
}



