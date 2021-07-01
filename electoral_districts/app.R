library(shiny)
library(tidyverse)


app_data <- readRDS("shiny_data/app_plot_data.rds")

# Define UI for application 
ui <- fluidPage(
    
    shinyjs::useShinyjs(),
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
            
            
            radioButtons("value", label = "Select values",
                         choices = list("By speaker's electoral district" = "speaker_district_prop", 
                                        "By speaker's party" = "speaker_party_prop"), 
                         selected = "speaker_district_prop"),
            
            uiOutput("groups")
        ),
        
        # Show a plot
        mainPanel(
            plotOutput("map_plot")
        )
    )
)

# Define server logic 
server <- function(input, output, session){
    
    observeEvent(input$value, {
        
        if(input$value == "speaker_district_prop"){
            group_choices <- app_data %>%
                filter(period == input$time_period) %>%
                filter(year == input$year) %>%
                mutate(party = droplevels(party)) %>%
                pull(party) %>%
                levels()
            
            output$groups <- renderUI({
                selectInput(inputId = "groups", label = "Select parlamentary group", choices = group_choices, selected = group_choices[1])
            })
        }
    })
    
    observe({
        shinyjs::hide("groups")
        
        if(input$value == "speaker_district_prop"){
            
            
            shinyjs::show("groups")
        }
    })
    
    
    observeEvent(input$time_period, {
        
        if(input$time_period == "2004-2013"){
            default_year <- 2008
            updateSliderInput(inputId = "year", min = 2004, max = 2013, value = default_year)
            
        } else {
            default_year <- 1990
            updateSliderInput(inputId = "year", min = 1986, max = 1995, value = default_year)
        }
        
        
        if(input$value == "speaker_district_prop"){
            group_choices <- app_data %>%
                filter(period == input$time_period) %>%
                filter(year == default_year) %>%
                mutate(party = droplevels(party)) %>%
                pull(party) %>%
                levels()
            
            updateSelectInput(session = session, inputId = "groups", choices = group_choices, selected = group_choices[1])
        }
    })
    
    
    plot_data <- reactive({
        app_data %>%
            filter(period == input$time_period) %>%
            filter(year == input$year) %>%
            filter(party == input$groups)
    })
    
    
    
    
    # output$map_plot <- renderPlot({
    #     ggplot() + 
    #         geom_sf(data = plot_data() %>% 
    #                     group_by(electoral_district) %>%
    #                     summarize_at(.vars = input$value, .funs = median), 
    #                 aes_string(fill = input$value,
    #                            geometry = "geom"), 
    #                 colour = alpha("white", 1/3)) +
    #         viridis::scale_fill_viridis(limits = c(0,plyr::round_any(max(app_data[[input$value]], na.rm = TRUE), 0.1)))
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
