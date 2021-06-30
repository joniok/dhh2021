library(shiny)
library(tidyverse)


app_data <- readRDS("shiny_data/app_plot_data.rds")


# Define UI for application 
ui <- fluidPage(
    
    # Application title
    #titlePanel("Old Faithful Geyser Data"),
    
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
        
        group_options <- app_data %>%
            filter(period == input$time_period) %>%
            filter(year == input$year) %>%
            mutate(party = droplevels(party)) %>%
            pull(party) %>%
            levels()
        
        updateSelectInput(session = session, inputId = "groups", choices = group_options)
        
        if(input$time_period == "2004-2013"){
            updateSliderInput(inputId = "year", min = 2004, max = 2013, value = 2008)
        } else {
            updateSliderInput(inputId = "year", min = 1986, max = 1995, value = 1990)
        }
        
    })
    
    
    plot_data <- reactive({
        app_data %>%
            filter(period == input$time_period) %>%
            filter(year == input$year) %>%
            filter(party == input$groups)
    })
    
    # global_limits <- reactive({
    #     c(0, max(as.data.frame(app_data$speaker_district_count)))
    # 
    # })
    # 
    
    output$map_plot <- renderPlot({
        ggplot() + 
            geom_sf(data = plot_data(), 
                    aes_string(fill = "speaker_district_count",
                               geometry = "geom",), 
                    colour = alpha("white", 1/3)) #+
            #viridis::scale_fill_viridis(limits = global_limits())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
