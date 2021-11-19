



library(shiny)
source("01_historical.R")
source("02_netzero_pathway.R")
source("03_abatement_wedge.R")
source("04_policy_wedge.R")
source("05_consumption.R")


grey <- "#EBEBEB"
light_purple <- "#E3D9FF"
dark_purple <- "#280049"


# ui function ------------------------------------------------------------------
ui <- fluidPage(
  
  shiny::tags$head(tags$style("
      .row1{height:50px;}")
    ),
    
    tags$head(tags$style(HTML(".nav.nav-pills.nav-stacked > .active > a, .nav.nav-pills.nav-stacked > .active > a:hover {
    background-color: #280049;
    }

    .well {
        min-height: 20px;
        max-width: 200px;
        padding: 19px;
        margin-bottom: 20px;
        border: 1px solid #e3e3e3;
        border-radius: 4px;


    }
    "))),
    
    shinyWidgets::useShinydashboardPlus(),

    shiny::titlePanel("Mitigation Progress Datashboard (proof of concept)"),
    
    shiny::fluidRow("[Headline statement on overall UK progress]", class = "row1"),
    
    shiny::tabsetPanel(
        
      shiny::tabPanel("Context",
                 navlistPanel(
                   shiny::tabPanel("UK's historical emissions",  shiny::fluidRow( id = "mod2", m01_historicalUI(id = "mod1"))),
                   shiny::tabPanel("Consumption emissions",      shiny::fluidRow( id = "mod5", m05_consumptionUI(id = "mod5"))),
                   shiny::tabPanel("The path to Net Zero",       shiny::fluidRow( id = "mod2", m02_pathwayUI(id = "mod2"))),
                   shiny::tabPanel("How can we cut emissions?",  shiny::fluidRow( id = "mod3", m03_abatementUI(id = "mod3")))
                 )      
        ),
      shiny::tabPanel("Policy",
                fluidRow( id = "mod4", m04_policy_wedgeUI(id = "mod4"))
        )
        ,
      shiny::tabPanel("Delivery",
                 
                      shiny::navlistPanel(well=TRUE, "Select a sector:",
                    
                                          shiny::tabPanel("Buildings"),
                                          shiny::tabPanel("Transport"),
                                          shiny::tabPanel("Power"),
                                          shiny::tabPanel("Agriculture"),
                                          shiny::tabPanel("Industry")
            )
        )
    ),
    
    theme = bslib::bs_theme(base_font = "Century Gothic", bg = "white", fg = "#280049")

)


  

# server function --------------------------------------------------------------
server <- function(input, output) {
    
    # Call server function for each module
    
    m01_historicalServer("mod1")
      
    m02_pathwayServer("mod2")
    
    m03_abatementServer("mod3")
  
    m04_policy_wedgeServer("mod4")
    
    m05_consumptionServer("mod5")
}




# Run the application 
shinyApp(ui = ui, server = server)




# 
# 
# tabsetPanel(
#   
#   tabPanel("Context",
#            
#            navlistPanel(well = TRUE,
#                         
#                         tabPanel("UK's historical emissions",
#                                  fluidRow( id = "mod2", m01_historicalUI(id = "mod1")),
#                         )
#            )
#            fluidRow( id = "mod2", m01_historicalUI(id = "mod1")),
#            br(),
#            fluidRow( id = "mod2", m02_pathwayUI(id = "mod2")),
#            br(),
#            fluidRow( id = "mod3", m03_abatementUI(id = "mod3"))
#   ),
#   tabPanel("Policy",
#            fluidRow( id = "mod4", m04_policy_wedgeUI(id = "mod4"))
#   )
#   ,
#   tabPanel("Delivery",
#            
#            navlistPanel(well=TRUE, "Select a sector:",
#                         
#                         tabPanel("Buildings"),
#                         tabPanel("Transport"),
#                         tabPanel("Power"),
#                         tabPanel("Agriculture"),
#                         tabPanel("Industry")
#            )
#   )


# 
# tabsetPanel(
#   
#   tabPanel("Context",
#            fluidRow( id = "mod2", m01_historicalUI(id = "mod1")),
#            br(),
#            fluidRow( id = "mod2", m02_pathwayUI(id = "mod2")),
#            br(),
#            fluidRow( id = "mod3", m03_abatementUI(id = "mod3"))
#   ),
#   tabPanel("Policy",
#            fluidRow( id = "mod4", m04_policy_wedgeUI(id = "mod4"))
#   )
#   ,
#   tabPanel("Delivery",
#            
#            navlistPanel(well=TRUE, "Select a sector:",
#                         
#                         tabPanel("Buildings"),
#                         tabPanel("Transport"),
#                         tabPanel("Power"),
#                         tabPanel("Agriculture"),
#                         tabPanel("Industry")
#            )
#   )