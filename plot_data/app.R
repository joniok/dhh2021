library(shiny)
library(tidyverse)


app_data <- readRDS("districts.rds")

# Define UI for application
ui <- fluidPage(
    
    # Application title
    # titlePanel("Old Faithful Geyser Data"),
    
    sidebarLayout(
        sidebarPanel(
            
            selectInput(inputId = "time_period", label = h3("Select time period"), 
                        choices = list("Depression in Finland (1986-1995)" = "1986-1995", 
                                       "Global Financial Crisis (2004-2013)" = "2004-2013"), 
                        selected = 1),
            
            sliderInput(inputId = "year", label = "Select year",
                        min = 1986, max = 1995, value = 1990, sep = ""),
            
            selectInput(inputId = "district", 
                        label = "Select electoral districst", 
                        choices = levels(app_data$speaker_electoral_district),
                        selected = levels(app_data$speaker_electoral_district)[1])
            
            
        )
        ,
        
        # Show a plot of the generated maps
        mainPanel(
            plotOutput("map_plot") 
        )
    )
)


# Define server logic
server <- function(input, output) {
    
    
    observeEvent(input$time_period, {
        
        if(input$time_period == "2004-2013"){
            default_year <- 2008
            updateSliderInput(inputId = "year", min = 2004, max = 2013, value = default_year)
            
        } else {
            default_year <- 1990
            updateSliderInput(inputId = "year", min = 1986, max = 1995, value = default_year)
        }
    })
    
    plot_data <- reactive({
        app_data %>%
            filter(period == input$time_period) %>%
            filter(year == input$year) %>%
            filter(speaker_electoral_district == input$district) %>%
            group_by_at(.vars = c("electoral_district", "speaker_electoral_district")) %>%
            summarize_at(.vars = "speaker_district_count", .funs = max)
    })
    
    output$map_plot <- renderPlot({
        ggplot() +
            geom_sf(data  = plot_data() ,
                    aes_string(fill = "speaker_district_count" ,
                               geometry = "geom"),
                    colour = alpha("white", 1/3))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
