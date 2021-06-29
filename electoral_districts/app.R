library(shiny)
library(tidyverse)


data_path <- "shiny_data/"
d1 <- readRDS(paste0(data_path, "map_data.rds"))

districts <- readRDS(paste0(data_path, "districts.rds"))
groups <- readRDS(paste0(data_path, "groups.rds"))

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
    
    
    selected_data <-  reactive({groups %>%
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
        ggplot(d1 %>% count(vaalipiiri_code), aes(fill = vaalipiiri_code)) + 
            geom_sf(colour = alpha("white", 1/3))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
