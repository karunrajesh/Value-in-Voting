library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(RColorBrewer)
library(usmap)
library(maps)


us_states <- map_data("state") %>% mutate(state=region)
presidential <- read.csv("Data/1976-2016-president.csv")
context <- read.csv("Data/election-context-2018.csv")
budg_all <- read.csv("Data/budget_amount.csv")
electoral_votes <- read.csv('Data/Electoral_Votes_by_State.csv')

turnout <- read.csv("Data/Turnout.csv") %>%
  filter(X %in% seq(1976, 2016, by = 4)) %>%
  select(X, State, X.4) %>% 
  rename(year=X,state=State, turnout=X.4)
turnout_1976 <- read.csv("Data/1976_Turnout.csv")
turnout_2016 <- read.csv("Data/2016_Turnout.csv") %>% mutate(State=recode(State,!!!c("Dist. of Columbia"="District of Columbia")))
presidential_turnout <- turnout_1976 %>% 
  rename(year=Year,state=State,turnout=Turnout) %>% rbind(turnout) %>% 
  rbind(turnout_2016 %>% rename(year=Year,state=State,turnout=Turnout)) %>%
  mutate(year = as.numeric(as.character(year)),turnout=as.numeric(str_replace(turnout,"%",""))) %>%
  filter(state !='United States')


presidential_turnout_w_map <- presidential_turnout %>%
  mutate(state = tolower(state)) %>% left_join(us_states,by='state')

overall_turnout <- presidential_turnout %>%
  group_by(year) %>%
  summarize(total = mean(turnout))

parties_pres <- presidential %>%
  group_by(year,state) %>%
  arrange(desc(candidatevotes), .by_group = TRUE) %>%
  filter(row_number() == 1) %>%
  dplyr::select(year, state, candidate, party)

parties_pres_w_map <- parties_pres %>%
  mutate(state = tolower(state)) %>% left_join(us_states,by='state')


margin_pres <- presidential %>%
  group_by(year,state) %>%
  arrange(desc(candidatevotes), .by_group = TRUE) %>%
  filter(row_number() %in% c(1,2)) %>%
  dplyr::select(year, state, candidate, candidatevotes, totalvotes, party) %>%
  summarize(margin = mean((candidatevotes[1]-candidatevotes[2])/totalvotes), party = party[1])
margin_pres_w_map <- margin_pres %>%
  mutate(state = tolower(state)) %>% left_join(us_states,by='state')


pres_turnout_and_margin <- presidential_turnout %>% 
  left_join(margin_pres,by=c('state','year')) %>%
  left_join(electoral_votes,by=c('state','year'))

context_state<- context %>%
  group_by(state) %>%
  dplyr::select(-county) %>%
  summarise_each(funs(mean))
demo <- context_state[,-c(2:22)]



state_name_to_abb <- setNames(state.name,state.abb)
influence <- data.frame(state = c(state.abb), 
                       chance = c(1/7e9,1/100e6,1/40e6, 1/3e9,
                                  1/7e9,1/1e6, 1/40e6, 1/300e6,
                                  1/3e6, 1/60e6, 1/1e9, 1/10e9,
                                  1/1e9, 1/3e9, 1/30e6, 1/1e9,
                                  1/8e9, 1/5e9, 1/5e6, 1/10e9,
                                  1/4e9, 1/3e6, 1/10e6, 1/1e9,
                                  1/1e9, 1/2e9, 1/4e9, 1/2e6,
                                  1/1e6, 1/400e6, 1/3e6, 1/3e9,
                                  1/3e6, 1/4e9, 1/20e6, 1/30e9,
                                  1/40e6, 1/2e6, 1/30e6, 1/100e6,
                                  1/3e9, 1/3e9, 1/1e9, 1/2e9,
                                  1/10e9, 1/10e6, 1/200e6, 1/9e9,
                                  1/2e6, 1/30e9)) %>%
            mutate(state = recode(state,!!!state_name_to_abb))
influence$chance <- as.numeric(influence$chance)



budg_all <- data.frame(lapply(budg_all, as.character), stringsAsFactors=FALSE)
colnames(budg_all) = budg_all[2,]
rownames(budg_all) = budg_all[,1]
budg_all = budg_all[-c(1,2,33,34),-1]
cols <- colnames(budg_all)
rows <- rownames(budg_all)
budg = as.data.frame(lapply(budg_all, function(x) as.numeric(gsub(",","",x))))
colnames(budg) <- cols
rownames(budg) <- rows
budg <- budg[,-c(1,2)]
budg_admin <- as.data.frame(sapply(seq(1,dim(budg)[2]-1,by=4),function(i) rowSums(budg[,i:(i+3)], na.rm=TRUE)))
colnames(budg_admin) <- c(as.numeric(colnames(budg[,seq(1,dim(budg)[2]-5,by=4)])) - 1, 2020)





state_choices <- unique(presidential_turnout$state)
govagency_choices <- rownames(budg_admin)
map_modes <- c('Race Result','Margins','Voter Turnout')

color_scale_min <- c('Margins'= min(margin_pres$margin),'Voter Turnout'=min(presidential_turnout$turnout))
color_scale_max <- c('Margins'= max(margin_pres$margin),'Voter Turnout'=max(presidential_turnout$turnout))

agencies <- c("DOE","DOJ")

ui <- fluidPage(
  theme = shinythemes::shinytheme("spacelab"),
  titlePanel("Value of Voting"),
  tabsetPanel(
    tabPanel("EDA",
             # US map plots
             column(3,
                radioButtons("mapmode","Select variable to display:",
                             choices=map_modes
                )
             ),
             column(9,
                sliderInput("year","Choose year",
                            value = 2000, min = 1976, 
                            max = 2016,step=4,sep="",width='80%'
                ),
                plotOutput(outputId = "election_map",height=500,width='100%'),
                align='center'
             ),
             selectizeInput("states_turnout","Select up to 6 states:",
                            choices=state_choices,
                            selected=c('California', 'Alabama', 'Michigan', 'Florida','Virginia'),
                            multiple = T,
                            options = list(maxItems = 6),
                            width='100%'
                            ),
             column(6,
                   plotOutput(outputId = "line_turnout",height=500,width='100%')
                   ),
             column(6,
                    plotOutput(outputId = "bar_influence",height=500,width='100%')
                    ),
             h4("States Demographic"),
             tableOutput('table_demo'),
             column(6,
                  selectInput("states_margin","Select state:",setNames(state_choices,state_choices)),
                  plotOutput(outputId = "line_margin",height=500,width='100%')
             ,align='center'),
             column(6,
                    selectizeInput("gov_agencies","Select up to 6 agencies:",
                                   choices=govagency_choices,
                                   selected=c("Department of Education", "Department of Transportation"),
                                   multiple = T,
                                   options = list(maxItems = 6),
                                   width='100%'
                    ),
                    plotOutput(outputId = "line_votevalue",height=500,width='100%')
             ),
             sliderInput("year_scatter","Choose year",
                         value = 2000, min = 1976, 
                         max = 2016,step=4,sep="",width='85%'
             ),
             plotOutput(outputId = "scatter",height=700,width='100%')
             
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
      presidential_turnout_w_map %>% filter(year==input$year) %>% mutate(fillcolor=turnout)
    } else if (input$mapmode == "Margins"){
      margin_pres_w_map %>% filter(year==input$year) %>% mutate(fillcolor=margin)
    } else {
      parties_pres_w_map %>% filter(year==input$year) %>% mutate(fillcolor=str_to_title(party))
    }
    )

  
  output$election_map <- renderPlot({ 
    p<- mapdata() %>% ggplot(aes(x = long, y = lat, fill = fillcolor, group = group)) + 
      geom_polygon(color = "white") + 
      coord_fixed(1.3) +
      ggtitle(input$mapmode) +
      theme(panel.grid.major = element_blank(), 
            panel.background = element_blank(),
            axis.title = element_blank(), 
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            text = element_text(size=15),
            legend.position="bottom",
            plot.title = element_text(hjust = 0.5)
      ) 
     if (input$mapmode == 'Voter Turnout') {
         p <- p + scale_fill_distiller(name=input$mapmode,
                                  palette="Spectral",
                                  limits= c(color_scale_min[input$mapmode],color_scale_max[input$mapmode])
             ) + theme(legend.key.width = unit(6,"lines"))
     } else if (input$mapmode == 'Margins'){
       p <- p + scale_fill_distiller(name=input$mapmode,
                                     trans='log10',
                                     palette="Spectral",
                                     limits= c(color_scale_min[input$mapmode],color_scale_max[input$mapmode])
       ) + theme(legend.key.width = unit(6,"lines"))
     } else {
        p <- p + scale_fill_manual(values=c("Democrat"="blue","Republican"="red","Democratic-Farmer-Labor"="darkgreen"))
      }
     p
  })
  
  line_turnout_data <- reactive(presidential_turnout %>% filter(state %in% input$states_turnout)) 
  output$line_turnout <- renderPlot({ 
    line_turnout_data() %>%
      ggplot(aes(year,turnout,color=state)) + 
      geom_line() +
      geom_point() +
      ggtitle("Voter Turnout") + 
      theme(text = element_text(size=15),
            legend.position="bottom",
            # center title
            plot.title = element_text(hjust = 0.5)) +
      # increase size of the marker inside the legend label
      guides(colour = guide_legend(override.aes = list(size=5)))
  })
  
  bar_influence_data <- reactive(influence %>% filter(state %in% input$states_turnout))
  output$bar_influence <- renderPlot({ 
    bar_influence_data() %>%
      ggplot(aes(state, chance,fill=state)) +
      geom_bar(stat="identity") +
      ggtitle("Chance of Flipping an Election") +
      theme(text = element_text(size=15),
            legend.position="bottom",
            # center title
            plot.title = element_text(hjust = 0.5)) +
      # increase size of the marker inside the legend label
      guides(colour = guide_legend(override.aes = list(size=9)))
  })
  
  table_demo_data <- reactive(demo %>% filter(state %in% input$states_turnout))
  output$table_demo <- renderTable({
    table_demo_data()
  })
  
  line_margin_data <- reactive(margin_pres %>%  filter(state ==input$states_margin) %>% mutate(party=str_to_title(party))) 
  output$line_margin <- renderPlot({ 
    line_margin_data() %>%
      ggplot(aes(year,margin)) + 
      geom_line() +
      geom_point(aes(colour = party), size = 5) +
      scale_color_manual(values=c("Democrat"="blue","Republican"="red","Democratic-Farmer-Labor"="darkgreen")) +
      ggtitle('Vote Margin') +
      theme(text = element_text(size=15),
            legend.position="bottom",
            # center title
            plot.title = element_text(hjust = 0.5))
  })
  
  
  line_votevalue_data <- reactive((influence %>% filter(state == input$states_margin) %>% 
                                    .$chance * budg_admin *1e6) %>% 
                                    .[input$gov_agencies,] %>% 
                                    tibble::rownames_to_column("agency") %>%
                                    gather(year,budget,-agency)
                                  )
  output$line_votevalue <- renderPlot({ 
    line_votevalue_data() %>%
      ggplot(aes(x=as.numeric(year), y=budget, color=agency)) + 
      geom_line() +
      xlab("Year") + 
      ylab("Value of Vote (in dollars)") +
      ggtitle('Value of Vote') +
      theme(text = element_text(size=15),
            legend.position="bottom",
            # center title
            plot.title = element_text(hjust = 0.5)) +
      # increase size of the marker inside the legend label
      guides(colour = guide_legend(override.aes = list(size=9)))
  })
  
  scatter_data <- reactive(pres_turnout_and_margin %>% filter(year==input$year_scatter) %>% 
                           left_join(influence,by='state') %>% 
                           mutate(party=str_to_title(party))
                           )
  output$scatter <- renderPlot({
    scatter_data() %>%
      ggplot(aes(margin, turnout, color = party, size=nvotes)) +
      geom_point(alpha = 0.5) +
      xlab("Margin") +
      ylab("Turnout") +
      scale_x_log10(limits=c(color_scale_min['Margins'],color_scale_max['Margins'])) +
      ylim(c(color_scale_min['Voter Turnout'],color_scale_max['Voter Turnout'])) +
      # Set x and y axis limits to keep the same for each year
      # Make the legend titles nicer
      scale_color_manual(values=c("Democrat"="blue","Republican"="red","Democratic-Farmer-Labor"="darkgreen")) +
      scale_size_continuous(name = "# of Electoral Votes",range=c(5,30)) +
      
      # Change the title of the plot for each year
      # Returns a character vector containing a formatted combination 
      # of text and variable values
      ggtitle(sprintf("Turnout vs Margin in %d", input$year_scatter)) +
      theme_bw() + 
      theme(text = element_text(size=20))
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)
