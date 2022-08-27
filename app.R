library(shiny)
library(tidyverse)
library(plotly)


# Preparing Data to be displayed in dashboard


export <- read.csv("export.csv")
import <- read.csv("import.csv")


# List of all countries with which trade was conducted
export_countries <- export |> 
    select(CountryName) |> 
    unique()

import_countries <- import |> 
    select(CountryName) |> 
    unique()

countries_list <- export_countries |> 
    bind_rows(import_countries) |> 
    unique()


countries_list <- as.vector(countries_list[,1])

# List of all commodities traded in 5 years
export_commodities <- export |> 
    select(MainCommodityName) |> 
    unique()

import_commodities <- import |> 
    select(MainCommodityName) |> 
    unique()

commodities_list <- export_commodities |> 
    bind_rows(import_commodities) |> 
    select((MainCommodityName)) |> 
    unique()

commodities_list <- as.vector(commodities_list$MainCommodityName)



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
	                             "This dashboard app visualizes the trade flow of Nepal from/to different countries from fiscal year 2073/074 to 2077/078."
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
	                     
	                     
	                     helpText("Data for this projecty are from", a("Nepal Trade Information Portal", href = "https://nepaltradeportal.gov.np/report", target = "_blank")),
	                     br(),
	                     
	                         
	                   helpText("Created by", a("Sohan Aryal", href = "https://github.com/aryalsohan0/", target = "_blank"))
	                 )
	             )),
	    
	    # Tab for per-commodity info on trade
	    tabPanel("Per-commodity",
	             fluidRow(column(
	                 12,
	                 selectizeInput(
	                     "selected_commodity",
	                     "Commodity",
	                     choices = NULL,
	                     width = '100%',
	                     multiple = T,
	                     options = list(maxItems = 4, placeholder = 'Select commodities (max. 4)')
	                 )
	             )),
	             
	             fluidRow(column(
	                 6, plotlyOutput('export_commodity_total')
	             ),
	             column(
	                 6, plotlyOutput("import_commodity_total")
	             )
	                 
	             ),
	             
	             fluidRow(column(6, plotlyOutput(
	                 'top_export_countries'
	             )),
	             column(6, plotlyOutput(
	                 'top_import_countries'
	             )))
	             
	             ),
	    
	    # Tab for per-country info on trade
	    tabPanel("Per-country",
	             fluidRow(column(4,
	                 selectizeInput("selected_country", "Country",
	                                choices = NULL,
	                                width = "100%",
	                                multiple = FALSE,
	                                options = list(placeholder = "Choose a Country"))
	             ),
	             
	             column(8,
	                    selectizeInput("selected_country_commodity",
	                                   "Commodity",
	                                   choices = NULL,
	                                   width = "100%",
	                                   multiple = TRUE,
	                                   options = list(maxItems = 4,
	                                                  placeholder = "Choose Commodities (max 4) ")))
	                      ),
	             
	             fluidRow(
	                 
	                 column(12, plotlyOutput("plot_balance"))
	             ),
	             
	             fluidRow(
	                 
	                 column(6, plotlyOutput("plot_export_country_commodity")),
	                 column(6, plotlyOutput("plot_import_country_commodity"))
	             )
	             
	             
	             
	             
	             )
	)
)

server <- function(input, output, session){
    
    updateSelectizeInput(
        session,
        "selected_commodity",
        choices = commodities_list,
        server = TRUE,
        selected = c(commodities_list[1], commodities_list[2]))
    
    updateSelectizeInput(
        session,
        "selected_country",
        choices = countries_list,
        server = TRUE,
        selected = c(countries_list[1])
    )
    
    updateSelectizeInput(
        session,
        'selected_country_commodity',
        choices = commodities_list,
        server = TRUE,
        selected = c(commodities_list[1])
    )
    
    # Per-commodity Tab

    # First row output
   
    output$export_commodity_total <- renderPlotly({
        
        req(input$selected_commodity)
        
        commo_export <- export  |> 
            group_by(MainCommodityName, Year) |> 
            dplyr::summarise(R_value = sum(R_value)) |> 
            filter(MainCommodityName %in% input$selected_commodity) |> 
            mutate(R_value = round(R_value/1e6,2))
        
        g <- ggplot(commo_export, aes(x = Year, y = R_value, group = MainCommodityName,
                                color = MainCommodityName)) +
            geom_line() + theme(axis.text = element_text(size = 8)) +
            theme(text = element_text(size = 10))+
            theme(plot.title = element_text(face = "bold")) +
            ylab("Total Export in Million (NPR)") + 
            xlab("") +
            scale_y_continuous(expand = expansion(c(0, 0.1))) +
            ggtitle("Exports of Commodity")
        
        ggplotly(g, tooltip = c("y")) |> 
            config(displayModeBar = F) |> 
            layout(legend = list(orientation = 'h', title = ""))

    })
    
    
    output$import_commodity_total <- renderPlotly({
        
        req(input$selected_commodity)
        
        commo_import <- import  |> 
            group_by(MainCommodityName, Year) |> 
            dplyr::summarise(R_value = sum(R_value)) |> 
            filter(MainCommodityName %in% input$selected_commodity) |> 
            mutate(R_value = round(R_value/1e6,2))
        
        g <- ggplot(commo_import, aes(x = Year, y = R_value, group = MainCommodityName,
                                     color = MainCommodityName)) +
            geom_line() + theme(axis.text = element_text(size = 8)) +
            theme(text = element_text(size = 10))+
            theme(plot.title = element_text(face = "bold")) +
            ylab("Total Import in Million (NPR)") + 
            xlab("") +
            scale_y_continuous(expand = expansion(c(0, 0.1))) +
            ggtitle("Imports of Commodity")
        
        ggplotly(g, tooltip = c("y"))  |>
            config(displayModeBar = F) |> 
            layout(legend = list(orientation = 'h', title = ""))
        
    })
  
    
    # Second Row Output
    
    output$top_export_countries <- renderPlotly({
        
        req(input$selected_commodity)
        
        top_export_country <- export |> 
            filter(MainCommodityName %in% input$selected_commodity) |> 
            group_by(Year, MainCommodityName) |> 
            slice_max(R_value, n = 1, with_ties = FALSE) |> 
            mutate(R_value = round(R_value/1e6, 2))
        
        
        g2 <- ggplot(top_export_country, aes(x = Year, 
                                            y = R_value, fill = CountryName)) +
            geom_col() +
            facet_wrap(~MainCommodityName, scales = "free", dir = "v", 
                       labeller = label_wrap_gen(50)) +
            geom_line() + theme(axis.text.y = element_text(size = 8)) +
            theme(axis.text.x = element_blank()) +
            theme(text = element_text(size = 10))+
            theme(plot.title = element_text(face = "bold")) +
            theme(strip.text.x = element_text(size = 7)) +
            ylab("Export in Million (NPR)") + 
            xlab("") +
            scale_y_continuous(expand = expansion(c(0, 0.1))) +
            ggtitle("Top Countries Exported to")
            
        
        ggplotly(g2, tooltip = c("x","y"))  |>
            config(displayModeBar = F) |> 
            layout(legend = list(orientation = 'h', title = ""))
        
    })
    
    output$top_import_countries <- renderPlotly({
        
        req(input$selected_commodity)
        
        top_import_country <- import |> 
            filter(MainCommodityName %in% input$selected_commodity) |> 
            group_by(Year, MainCommodityName) |> 
            slice_max(R_value, n = 1, with_ties = FALSE) |> 
            mutate(R_value = round(R_value/1e6, 2))
        
        
        g3 <- ggplot(top_import_country, aes(x = Year, 
                                             y = R_value, fill = CountryName)) +
            geom_col() +
            facet_wrap(~MainCommodityName, scales = "free", dir = "v", 
                       labeller = label_wrap_gen(50)) +
            geom_line() + theme(axis.text.y = element_text(size = 8)) +
            theme(axis.text.x = element_blank()) +
            theme(text = element_text(size = 10))+
            theme(plot.title = element_text(face = "bold")) +
            theme(strip.text.x = element_text(size = 7)) +
            ylab("Import in Million (NPR)") + 
            xlab("") +
            scale_y_continuous(expand = expansion(c(0, 0.1))) +
            ggtitle("Top Countries Imported from")
        
        
        ggplotly(g3, tooltip = c("x","y"))  |>
            config(displayModeBar = F) |> 
            layout(legend = list(orientation = 'h', title = ""))
        
    })
    
    
    
    # Per-Country Tab
    
    output$plot_balance <- renderPlotly({
        
        export_country_user <- export |> filter(CountryName == input$selected_country)
        import_country_user <- import |> filter(CountryName == input$selected_country)
        
        
        balance_country_user <- export_country_user |> 
            group_by(Year) |> 
            summarize(R_value =  sum(R_value)) |> 
            inner_join((
                import_country_user |> 
                    group_by(Year) |> 
                    summarize(R_value = sum(R_value))
            ), by = "Year") |> 
            rename(Exports = R_value.x, Imports = R_value.y) |> 
            mutate(Exports = round(Exports/1e6,digits = 2),
                   Imports = round(Imports/1e6,digits = 2)) |> 
            mutate(TradeBalance = Exports - Imports) |> 
            pivot_longer(cols = c("Exports": "TradeBalance") ,names_to = "flow", values_to = 'value' )
        
        
        g4 <- ggplot(balance_country_user, aes(x = Year, y = value, fill = flow))+
            geom_col(position = position_dodge())  +
            theme(axis.text.y = element_text(size = 8)) +
            theme(text = element_text(size = 10))+
            theme(plot.title = element_text(face = "bold"))+
            labs(
                y = "Value  (in million NPR)",
                x = "",
                title = paste0("<b>Trade flow of all Commodity with ", input$selected_country, "</b>"),
                fill = ""
            )
        
        ggplotly(g4, tooltip = c("y"))  |>
            config(displayModeBar = F) |> 
            layout(legend = list(orientation = 'h', title = ""))
        
    })
    
    
    output$plot_export_country_commodity <- renderPlotly({
        
        req(input$selected_country)
        req(input$selected_country_commodity)
        
        export_country_commodity <- export |> 
            mutate(R_value = round(R_value/1e6, digits = 2)) |> 
            filter(CountryName == input$selected_country) |> 
            filter(MainCommodityName %in% input$selected_country_commodity) |> 
            group_by(MainCommodityName, Year) |> 
            summarize(Value = sum(R_value)) 
        
        g5 <- ggplot(export_country_commodity, aes(x = Year,
                                                   y = Value,
                                                  group = MainCommodityName,
                                                  color = MainCommodityName)) +
            geom_line() +
            theme(axis.text.y = element_text(size = 8)) +
            theme(text = element_text(size = 10))+
            theme(plot.title = element_text(face = "bold"))+
            labs(
                y = "Value  (in million NPR)",
                x = "",
                title = paste0("<b>Export of selected commodity with ", "<br>", input$selected_country, "</b>"),
                fill = ""
            )
        
        ggplotly(g5, tooltip = c("y"))  |>
            config(displayModeBar = F) |> 
            layout(legend = list(orientation = 'h', title = ""))
        
        
    })
    
    
    output$plot_import_country_commodity <- renderPlotly({
        
        req(input$selected_country)
        req(input$selected_country_commodity)
        
        import_country_commodity <- import |> 
            mutate(R_value = round(R_value/1e6, digits = 2)) |> 
            filter(CountryName == input$selected_country) |> 
            filter(MainCommodityName %in% input$selected_country_commodity) |> 
            group_by(MainCommodityName, Year) |> 
            summarize(Value = sum(R_value)) 
        
        g6 <- ggplot(import_country_commodity, aes(x = Year,
                                                   y = Value,
                                                   group = MainCommodityName,
                                                   color = MainCommodityName)) +
            geom_line() +
            theme(axis.text.y = element_text(size = 8)) +
            theme(text = element_text(size = 10))+
            theme(plot.title = element_text(face = "bold"))+
            labs(
                y = "Value  (in million NPR)",
                x = "",
                title = paste0("<b>Import of selected commodity with ", "<br>", input$selected_country, "</b>"),
                fill = ""
            )
        
        ggplotly(g6, tooltip = c("y"))  |>
            config(displayModeBar = F) |> 
            layout(legend = list(orientation = 'h', title = ""))
        
        
    })
    
    
}

shinyApp(ui, server)