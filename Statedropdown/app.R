
library(shiny)
library(xml2)
library(tidyverse)
library(stringr)
library(fiftystater)
library(mapproj)
library(ggplot2)

#load data
library(readr)
mapdata3 <- read_csv("./data/mapdata3.csv")
dat4 <- mapdata3
#need to figure out how to filter data based on dropdown selection...

dat4$senator <- paste(dat4$first_name,dat4$last_name,sep=" ")


# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Senator Bill Activity by State"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = "session",
                    label = "Select legislative session",
                    choices = c(
                                "113th Session" = "113",
                                "114th Session" = "114",
                                "115th Session" = "115")),
        selectInput(inputId = "state",
          label = "Select a state", choices = state.name,
                    selectize = FALSE)
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("map"),
        tableOutput("stats") 
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive({    dat4 %>% filter(session == input$session)   })
   output$map <- renderPlot({
     data() %>% filter(session == input$session) ##need to add code for filtering the map
        ggplot(fifty_states, aes(map_id = id)) + geom_map(map = fifty_states, fill="lightgrey", colour = alpha("grey", 3/4), size = 0.4)+ geom_map(aes(map_id = state_name_2, fill=total_state), colour=alpha("darkgrey", 3/4), data=data(), map=fifty_states) +
       expand_limits(x = fifty_states$long, y = fifty_states$lat) +
       coord_map() +
       scale_x_continuous(breaks = NULL) + 
       scale_y_continuous(breaks = NULL) +
       labs(x = "", y = "") +
       theme(legend.position = "bottom", panel.background = element_rect(), element_blank())+ fifty_states_inset_boxes() +
       ggtitle("Number of bills submitted, by state per session")
   })
   output$stats <- renderTable({ data() %>%
    group_by(state, senator) %>% filter(state_name==input$state) %>% summarize(Total = sum(n()))})
  
}

##need either renderDataTable - interactive
##renderPlot() 
#or renderTable() - table from data frame or matrix
##renderText() - character string

# Run the application 
shinyApp(ui = ui, server = server)

