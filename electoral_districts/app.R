library(shiny)
library(tidyverse)


app_data <- readRDS("shiny_data/app_plot_data.rds")

global_limits <- c(0,  plyr::round_any(max(app_data$speaker_district_prop, na.rm = TRUE), 0.1))

# Define UI for application 
ui <- fluidPage(
    
    # Application title
    #titlePanel(paste(global_limits,collapse = ", ")),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput(inputId = "time_period", label = h3("Select time period"), 
                        choices = list("Depression in Finland (1986-1995)" = "1986-1995", 
                                       "Global Financial Crisis (2004-2013)" = "2004-2013"), 
                        selected = 1),
            
            sliderInput(inputId = "year", label = "Select year",
                        min = 1986, max = 1995, value = 1990, sep = ""),
            
            
            selectInput(inputId = "groups", label = "Select parlamentary group", choices = NULL)
        ),
        
        # Show a plot
        mainPanel(
            plotOutput("map_plot")
        )
    )
)

# Define server logic 
server <- function(input, output, session){
    
    observeEvent(input$time_period, {
        
        if(input$time_period == "2004-2013"){
            default_year <- 2008
            updateSliderInput(inputId = "year", min = 2004, max = 2013, value = default_year)
            
        } else {
            default_year <- 1990
            updateSliderInput(inputId = "year", min = 1986, max = 1995, value = default_year)
        }
        
        group_options <- app_data %>%
            filter(period == input$time_period) %>%
            filter(year == default_year) %>%
            mutate(party = droplevels(party)) %>%
            pull(party) %>%
            levels()
        
        updateSelectInput(session = session, inputId = "groups", choices = group_options, selected = group_options[1])
    })
    
    
    plot_data <- reactive({
        app_data %>%
            filter(period == input$time_period) %>%
            filter(year == input$year) %>%
            filter(party == input$groups)
    })
    
    
    
    output$map_plot <- renderPlot({
        ggplot() + 
            geom_sf(data = plot_data() %>% 
                        group_by(electoral_district) %>%
                        summarise(speaker_district_prop = median(speaker_district_prop)), 
                    aes_string(fill = "speaker_district_prop",
                               geometry = "geom"), 
                    colour = alpha("white", 1/3)) +
            viridis::scale_fill_viridis(limits = global_limits)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
