
library(plotly)
library(shiny)
library(RColorBrewer)
library(stringr)
library(ggplot2)
library(magrittr)
library(tidyverse)


imported_data <- read_rds("./export_for_shiny_17.rds")


ui <- fluidPage(
   
   # Application title
   titlePanel("GoT - Transcript appearances"),
   
   fluidRow(
     
     column(12,
            wellPanel(radioButtons(inputId = "method", 
                                label = "Select characters:",
                                choices = c("Top characters" = "top_n", 
                                            "Manually" = "manually"),
                                selected = "top_n"),
                   conditionalPanel(condition = "input.method == 'top_n'",
         sliderInput("top_number",
                     "Number of main characters plotted:",
                     min = 1,
                     max = 25,
                     value = 15) ),
         conditionalPanel(condition = "input.method == 'manually'",
         selectInput("manual_characters", "Selected characters",
                     choices = imported_data$plot_data$character %>% levels(),
                     multiple = T,
                     selected = imported_data$plot_data$character %>% 
                       levels() %>% head(15) 
                     ))
      ))),
   fluidRow( column(12,
                    wellPanel(width = 10,
        plotlyOutput("appearancesPlot", height = "600px")
      )
   )
)
)
server <- function(input, output, session) {
   
  selected_characters <- reactive({
    
    if(input$method == "top_n"){
      
      selected <- imported_data$plot_data %>%
        filter(row_ <= input$top_number) %$%
        character
      
    }else if(input$method == "manually"){
      
      selected <- input$manual_characters
      
    }
      
  })
  
observe({
    x <- selected_characters()
    
    updateSelectInput(session, "manual_characters",
                      choices = imported_data$plot_data$character %>% levels(),
                      selected = x
    )
   
  })


    output$appearancesPlot <- renderPlotly({
      
      ggplotly({
      
      p <- imported_data$plot_data %>%
        filter(character %in% selected_characters()) %>%
        arrange(row_) %>%
        ggplot(aes(x = episode, y = proportion, group = character, col = character)) +
        ggplot2::geom_line(size = 2) +
        scale_color_manual(values=imported_data$col_vector) +
        scale_x_discrete(labels = imported_data$labels_x_scale %>% names) +
        scale_y_continuous(limits= c(0,16)) +
        labs(title = "Figure 1: % of participations among alive characters",
             x = "Episode", y = "% appearances", col = "Character") +
        theme_dark()
      
   }) 
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

