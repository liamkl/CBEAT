
library(shiny)
library(xml2)
library(tidyverse)
library(stringr)
library(fiftystater)
library(mapproj)
library(ggplot2)
library(dplyr)
library(igraph)
library(grDevices)
library(readr)
library(networkD3)

#load data

mapdata3 <- read_csv("./data/mapdata3.csv")
dat4 <- mapdata3
nodes <- read_csv("./data/nodes_proc.csv")
simp_edges <- read_csv("./data/simplified_edges.csv")

nodes <- nodes %>% 
  mutate(label = paste0(last_name, ", ", first_name, " (", party, "-", state, ")"))



dat4$senator <- paste(dat4$first_name,dat4$last_name,sep=" ")
dat4$image <- paste("https://www.congress.gov/img/member/", tolower(dat4$bioguideId), ".jpg", sep="")

#create new vector, call is 'options' and list only the unique values. takes a named vector name(x) = x
# Define UI for application
ui <- fluidPage(
   
   # Application title
   titlePanel("Senator Bill Activity by State"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        radioButtons(inputId = "session",
                    label = "Select legislative session",
                    choices = c("All"="",
                                "113th Session" = "113",
                                "114th Session" = "114",
                                "115th Session" = "115"),
                    selected = NULL),
        selectInput(inputId = "policyarea",
                     label = "Select Bill Policy Area",
                     choices = c("All"= "*",
                       "Education" = "Education",
                       "Health" = "Health",
                       "Public Lands and Natural Resources" = "Public Lands and Natural Resources",
                       "Taxation" ="Taxation",
                         "Housing and Community Development" = "Housing and Community Development",
                       "Transportation and Public Works" = "Transportation and Public Works",
                       "Sports and Recreation" = "Sports and Recreation",
                       "Labor and Employment" = "Labor and Employment" ,
                       "Commerce" = "Commerce",
                       "Animals"  = "Animals",
                       "Water Resources Development" = "Water Resources Development",
                       "Energy" = "Energy",
                       "Economics and Public Finance" = "Economics and Public Finance",
                       "Government Operations and Politics" = "Government Operations and Politics" , 
                       "International Affairs"   = "International Affairs"  ,
                       "Armed Forces and National Security"  =   "Armed Forces and National Security", 
                       "Science, Technology, Communications" = "Science, Technology, Communications",
                       "Emergency Management"  = "Emergency Management" ,
                       "Crime and Law Enforcement" = "Crime and Law Enforcement",
                       "Finance and Financial Sector"  = "Finance and Financial Sector",
                       "Foreign Trade and International Finance" = "Foreign Trade and International Finance", 
                       "Agriculture and Food" = "Agriculture and Food",
                       "Social Welfare" = "Social Welfare", 
                       "Native Americans"  = "Native Americans",
                       "Environmental Protection" ="Environmental Protection",
                       "Arts, Culture, Religion"= "Arts, Culture, Religion",
                       "Congress" = "Congress",
                       "Immigration"  = "Immigration" ,
                       "Civil Rights and Liberties, Minority Issues" = "Civil Rights and Liberties, Minority Issues",
                       "Private Legislation" = "Private Legislation",
                       "None reported"  = "None reported", 
                       "Families" = "Families",
                       "Law"  = "Law",
                       "Social Sciences and History" = "Social Sciences and History",
                       "Animal and plant health" = "Animal and plant health")),
        selectInput(inputId = "state",
          label = "Select a state", choices = state.name,
                    selectize = FALSE)
        
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("map"),
        tableOutput("stats"),
        tableOutput("interests"),
        htmlOutput("photo"),
        forceNetworkOutput("nodes")
      )
   )
)
#set all = star and grepl=*

# Define server logic required to draw a histogram
server <- function(input, output) {
  data <- reactive({    dat4 %>% filter(grepl(input$session, session)) %>% group_by(state) %>% filter(grepl(input$policyarea, policy_area)) %>% mutate(total_submit = sum(n())) %>% ungroup()  })
   output$map <- renderPlot({
     data() 
     ##need to add code for filtering the map
        ggplot(fifty_states, aes(map_id = id)) + geom_map(map = fifty_states, fill="lightgrey", colour = alpha("grey", 3/4), size = 0.4)+ geom_map(aes(map_id = state_name_2, fill=total_submit), colour=alpha("darkgrey", 3/4), data=data(), map=fifty_states) +
       expand_limits(x = fifty_states$long, y = fifty_states$lat) +
       coord_map() +
       scale_x_continuous(breaks = NULL) + 
       scale_y_continuous(breaks = NULL) +
       labs(x = "", y = "") +
       theme(legend.position = "bottom", panel.background = element_rect(), element_blank())+ fifty_states_inset_boxes() +
       ggtitle("Number of bills submitted, by state per session")
   })
   output$stats <- renderTable({ dat4 %>% filter(session == input$session) %>%
    group_by(state_name, senator) %>% filter(state_name==input$state) %>% 
       summarise(sum(n()), 
                 sum(billoutcome == "Became Law"), 
                 Percent = mean(billoutcome == "Became Law")*100) %>%
       `colnames<-`(c("State", "Senator", "Bills Introduced", "Number Passed as Law", "% Passed as Law"))})
     output$interests <- renderTable({dat4 %>% filter(session == input$session) %>% 
                       group_by(state_name, senator, policy_area) %>% filter(state_name==input$state) %>% 
                       summarise(sum(n())) %>%
                       'colnames<-'(c("State", "Senator", "Policy Area", "Number Sponsored"))})
   output$photo <- renderText({ data() %>% group_by(state_name, senator) %>% filter(state_name == input$state) 
     c(
       '<img src="',
       "image",
       '">') })
    output$nodes <- renderForceNetwork({### Example bioguideId set here ### 
      test <- dat4 %>% filter(session == input$session)
      
      sen_ID <- test %>% filter(state_name == input$state) 
      sen_ID <- unique(as.character(sen_ID$bioguideId))
      
      sen_ID <- data() %>% filter(state_name == input$state)
      sen_ID <- unique(as.character(sen_ID$bioguideId))
      
        for(i in sen_ID){
          senator_edges <- simp_edges %>% 
            select(-X1) %>%
            filter((from == i | to == i))
          senator_edges <- senator_edges[order(senator_edges$weight,decreasing = TRUE)[1:20],]
          # Then subset nodes
          nodes_subset <- nodes %>%
            filter((bioguideId %in% senator_edges$to | bioguideId %in% senator_edges$from))
          
          # Set target and source as indices to node data
          # minus 1 because it converts to JS which uses 0-indexing
          senator_edges <- senator_edges %>%
            mutate(source = match(from, nodes_subset$bioguideId) - 1,
                   target = match(to, nodes_subset$bioguideId) - 1)
          
          p <- forceNetwork(Links = senator_edges, Nodes = nodes_subset,
                       Source = "source", Target = "target",
                       Value = "weight", NodeID = "label",
                       Group = "party", 
                       Nodesize = "total_in",
                       linkDistance = 150, 
                       fontSize = 15,
                       colourScale = JS('d3.scaleOrdinal().range(["green", "blue", "red"]).domain(function(d) { (d.party); });'),
                       opacity = 0.6, arrows = TRUE, charge = -60, zoom = TRUE,
                       opacityNoHover = TRUE)
          print(p)
          
}})

}

#data() %>% group_by(state_name, senator) %>%
 # filter(state_name == input$state) %>% image

# filter to policy area level
##need either renderDataTable - interactive
##renderPlot() 
#or renderTable() - table from data frame or matrix
##renderText() - character string

# Run the application 
shinyApp(ui = ui, server = server)

