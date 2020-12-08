# Homework #3 Shiny app
# Due November 9, 2020 by 11:59pm EST

library(shiny)
library(tidyverse)
library(forcats)
library(dslabs)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

library(usmap)
library(readr)
library(lubridate)
library(maps)
library(stringr)

data(gapminder)

us_states <- map_data("state") %>% mutate(state=region)

us_states %>% ggplot(aes(x = long, y = lat, fill = region, group = group)) + 
  geom_polygon(color = "white") + 
  coord_fixed(1.3) +
  guides(fill = FALSE) + # do this to leave off the color legend 
  theme(panel.grid.major = element_blank(), 
        panel.background = element_blank(),
        axis.title = element_blank(), 
        axis.text = element_blank(),
        axis.ticks = element_blank())

presidential_turnout <- presidential %>%
  group_by(year, state) %>%
  filter(row_number()==1) %>%
  dplyr::select(year, state, totalvotes) 

presidential_turnout_w_map <- presidential_turnout %>%
  mutate(state = tolower(state)) %>% left_join(us_states,by='state')

pres_winner <- presidential %>%
  group_by(year, state) %>%
  filter(row_number()==1) %>%
  dplyr::select(year,state,party)

pres_winner_w_map <- pres_winner %>%
  mutate(state = tolower(state)) %>% left_join(us_states,by='state')


voter_turnout<- presidential_turnout_w_map %>% filter(year=='2016')

margin_pres <- presidential %>%
  group_by(year,state) %>%
  arrange(desc(candidatevotes), .by_group = TRUE) %>%
  filter(row_number() %in% c(1,2)) %>%
  dplyr::select(year, state, candidate, candidatevotes, totalvotes, party) %>%
  summarize(margin = mean((candidatevotes[1]-candidatevotes[2])/totalvotes), party = party[1])
margin_pres_w_map <- margin_pres %>%
  mutate(state = tolower(state)) %>% left_join(us_states,by='state')


map_modes <- c('Race Result','Margins','Voter Turnout')
agencies <- c("DOE","DOJ")

ui <- fluidPage(
  theme = shinythemes::shinytheme("spacelab"),
  tabsetPanel(
    tabPanel("Election",
             # US map plots
             selectInput("mapmode","Mode",setNames(map_modes,map_modes)),
             sliderInput("year","Choose year",
                         value = 2000, min = 1976, 
                         max = 2016,step=4,sep="",width='85%'
             ),
             plotOutput(outputId = "election_map",height=700,width='100%'),
             plotOutput(outputId = "scatter",height=700,width='100%')
             
    ),
    tabPanel("Government Expidenture",
             # Treemap plot with year slider
             plotOutput(outputId = "treemp",height=500,width='100%'),
             # Line plot of budget over the years
             selectInput("agency","Gov Agency",setNames(agencies,agencies)),
             plotOutput(outputId = "line_expense",height=700,width='100%')
    ),
    tabPanel("Analysis",
             # Regression Output
             plotOutput(outputId = "analysis",height=700,width='100%')
    )
  )
)


server <- function(input, output) {

  mapdata <- reactive(
    if (input$mapmode =="Voter Turnout"){
      presidential_turnout_w_map %>% filter(year==input$year) %>% mutate(fillcolor=totalvotes)
    } else if (input$mapmode == "Margins"){
      margin_pres_w_map %>% filter(year==input$year) %>% mutate(fillcolor=margin)
    } else {
      pres_winner_w_map %>% filter(year==input$year) %>% mutate(fillcolor=party)
    }
    )
  
  scatterdata <- reactive(margin_pres %>% left_join(presidential_turnout,by='state') %>% filter(year=year))

  output$election_map <- renderPlot({ 
    mapdata() %>% ggplot(aes(x = long, y = lat, fill = fillcolor, group = group)) + 
      geom_polygon(color = "white") + 
      coord_fixed(1.3) +
      guides(fill = FALSE) + # do this to leave off the color legend 
      theme(panel.grid.major = element_blank(), 
            panel.background = element_blank(),
            axis.title = element_blank(), 
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
      ggtitle(input$mapmode)
  })
  
  output$scatter <- renderPlot({ 
    
    
  })
  
  output$treemp <- renderPlot({ 
    
  })
  
  output$line_expense <- renderPlot({ 
    
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
