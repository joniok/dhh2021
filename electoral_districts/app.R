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
            
            selectInput(inputId = "groups", label = "Select parlamentary group", choices = NULL)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("map_plot")
        )
    )
)

# Define server logic 
server <- function(input, output, session){
    
    
    selected_data <-  reactive({app_plot_data %>%
            filter(period == input$time_period)
    })
    
    observeEvent(input$time_period, {
        group_options <- selected_data() %>%
            mutate(party = droplevels(party)) %>%
            pull(party) %>%
            levels()
        
        updateSelectInput(session = session, inputId = "groups", choices = group_options)
        
    })
    
    
    
    
    output$map_plot <- renderPlot({
        ggplot(selected_data(), aes(fill = mention_count.x)) + 
            geom_sf(colour = alpha("white", 1/3), fill = )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
