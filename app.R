library(shiny)

# Preparing Data to be displayed in dashboard



# Shiny App code starts from here

ui <- fluidPage(
    
	titlePanel("Nepal Import Export Dashboard"),
	
	tabsetPanel(
	    
	    # Overview Panel for info about dashboard
	    tabPanel('Overview',
	             fluidRow(
	                 column(
	                     12,
	                     br(),
	                     wellPanel(
	                         h4(
	                             "This dashboard app visualizes the trade flow of Nepal from different countries from fiscal year 2073/074 to 2077/078 "
	                         ),
	                         h5(
	                             "1. Per-commodity tab shows the flow of trade based on the selected commodities.
                                Specifically, it shows the trend of imports/exports and the top countries for the selected commodities."
	                         ),
	                         h5(
	                             "2. Per-country shows the trend of imports/exports with the selected country.
                                Specifically, it shows the trade balance with the country and also shows commodity-level information."
	                         )
	                     ),
	                     br(),
	                     wellPanel(
	                         HTML(
	                             "<h4>Created by <a href = \"https://github.com/aryalsohan0/\" target = \"_blank\">Sohan Aryal.</a></h4>"
	                         ),
	                         h5(
	                             "All values are in NPR. Data for this project are from "
	                         ),
	                         HTML(
	                             "<h5><a href = \"https://nepaltradeportal.gov.np/report\" target = \"_blank\">Nepal Trade Information Portal.</a></h5>"
	                         )
	                     )
	                 )
	             )),
	    
	    # Tab for per-commodity info on trade
	    tabPanel("Per-commodity",
	             "Work to be done"),
	    
	    # Tab for per-country info on trade
	    tabPanel("Per-country",
	             "Work to be done")
	)
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)