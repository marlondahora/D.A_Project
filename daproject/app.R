library(shiny)
library(ggiraph)
library(tidyverse)
library(reshape2)
library(shinydashboard)
library(shinydashboardPlus)
library(RColorBrewer)
library(forecast)
library(scales)
library(shinycssloaders)
library(psych)

load("sdgs.RData")
load("sdgsMap.RData")
load("sdgsIndex.RData")
load("worldMap.RData")
load("cli.RData")

### Creating index polar chart ####
sdgsIndexMap <- as.data.frame(sdgsIndex)
names(sdgsIndexMap)[1] <- "region"
polarIndex <- as.data.frame(sdgsIndex[,c(9:25)])
polarIndex<- melt(data = polarIndex, measure.vars = c("Goal_1", "Goal_2","Goal_3", "Goal_4","Goal_5", "Goal_6","Goal_7", "Goal_8",
                                                      "Goal_9", "Goal_10","Goal_11", "Goal_12","Goal_13", "Goal_14","Goal_15",
                                                      "Goal_16","Goal_17"))
## aggregating the data 
polarIndex$value<-replace_na(polarIndex$value, 0)
polarIndex <- polarIndex%>%
  select(variable,value)%>%
  group_by(variable) %>%
  summarise(value = mean(value))

## creating the plot labels
polarIndex$value <- round(polarIndex$value, digits=2)
polarIndex <- as.data.frame(polarIndex)
polarIndex$variable <- factor(polarIndex$variable, labels = c( "Goal 1", "Goal 2","Goal 3", "Goal 4","Goal 5", "Goal 6","Goal 7", "Goal 8",
                                                               "Goal 9", "Goal 10","Goal 11", "Goal 12","Goal 13", "Goal 14","Goal 15",
                                                               "Goal 16","Goal 17" ))

## creating the colour object 
cores <- c("#FF3300","#FF9900","#00CC66","red3","red2","steelblue2",
           "gold2","red4","#FF9933","deeppink3","orange3","orange4",
           "green4","dodgerblue1","green2","dodgerblue4","midnightblue")

sdgsLabels <- c( "Goal 1", "Goal 2","Goal 3", "Goal 4","Goal 5", "Goal 6","Goal 7", "Goal 8",
                 "Goal 9", "Goal 10","Goal 11", "Goal 12","Goal 13", "Goal 14","Goal 15",
                 "Goal 16","Goal 17" )

### position chart index
#top 10
top10 <- tbl_df(sdgsIndex[order(sdgsIndex$Global_Rank),]) %>%
  group_by(Global_Rank) %>%
  top_n(10)
top10 <- top10[c(1:10),]
#bottom 10
bottom10 <- tbl_df(sdgsIndex[order(-sdgsIndex$Global_Rank),]) %>%
  group_by(Global_Rank) %>%
  top_n(10)
bottom10 <- bottom10[c(1:10),]
bottom10 <- bottom10[order(bottom10$Global_Rank),]
rankCores <- c(rep("blue",10), rep("red",10))
topBottom <- rbind(top10,bottom10)
topBottom <- topBottom[order(topBottom$Global_Rank),]

topBottom$Country <- factor(topBottom$Country, levels = c("Denmark","Sweden","Finland","France","Austria","Germany" ,"Czech Republic","Norway","Netherlands","Estonia","Afghanistan","Niger","Sierra Leone","Haiti","Liberia","Madagascar","Nigeria","Congo, (Kinshasa)" ,"Chad","Central African Republic"),
                            labels = c("Denmark","Sweden","Finland","France","Austria","Germany" ,"Czech Republic","Norway","Netherlands","Estonia","Afghanistan","Niger","Sierra Leone","Haiti","Liberia","Madagascar","Nigeria","Congo, (Kin.)" ,"Chad","C.African Rep."))

### countries polar chart
countriesPolar <- melt(sdgsIndexMap, id.vars = "region", measure.vars = c("Goal_1", "Goal_2","Goal_3", "Goal_4","Goal_5", "Goal_6","Goal_7", "Goal_8",
                                                                          "Goal_9", "Goal_10","Goal_11", "Goal_12","Goal_13", "Goal_14","Goal_15",
                                                                          "Goal_16","Goal_17"))                                                                         

#### Building the maps ####

sdgsIndexMapCo <- as.data.frame(sdgsIndexMap[,c(1,9:25)])
sdgsIndexMapCo <- merge(worldMap, sdgsIndexMapCo,  sort = FALSE, by = "region")
sdgsIndexMapCo <- sdgsIndexMapCo[order(sdgsIndexMapCo$order), ]

sdgsIndexMapCo1tb <- as.data.frame(sdgsIndexMap[,c(1,3)])
sdgsIndexMapCo1tb <- merge(worldMap, sdgsIndexMapCo1tb,  sort = FALSE, by = "region")
sdgsIndexMapCo1tb <- sdgsIndexMapCo1tb[order(sdgsIndexMapCo1tb$order), ]

###Regions maps
regions <- subset(sdgsMap, sdgsMap$GeoArea=="Central Asia"|sdgsMap$GeoArea=="Western Asia"|sdgsMap$GeoArea=="Northern Africa"|sdgsMap$GeoArea=="Sub-Saharan Africa"|sdgsMap$GeoArea=="South-Eastern Asia"
                  |sdgsMap$GeoArea=="Southern Asia"|sdgsMap$GeoArea=="Eastern Europe"|sdgsMap$GeoArea=="Western Europe"|sdgsMap$GeoArea=="Southern Europe"|sdgsMap$GeoArea=="Northern Europe"
                  |sdgsMap$GeoArea=="Northern Europe"|sdgsMap$GeoArea=="Polynesia"|sdgsMap$GeoArea=="Eastern Asia"|sdgsMap$GeoArea=="Northern America"|sdgsMap$GeoArea=="Micronesia"
                  |sdgsMap$GeoArea=="Latin America and the Caribbean"|sdgsMap$GeoArea=="Melanesia"|sdgsMap$GeoArea=="Australia and New Zealand")
names(regions)[3] <- "SDGsRegions"
regions$Indicator <- str_replace_all(regions$Indicator, c(", by sex and age"=" in"))
regions$Indicator <- str_replace_all(regions$Indicator, c(", by sex"=" in"))
regions$Value <- round(regions$Value, digits=2)
regions <- as.data.frame(regions [, -c(5,7)])


# my_theme functions
my_theme <- function () { 
  theme_bw() + theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(),
                     legend.title = element_blank(),
                     legend.position = "bottom",
                     legend.text = element_text(size =15 ),
                     legend.key.size = unit(1.0, "cm"),
                     legend.key.width = unit(2.5,"cm"),
                     panel.border = element_blank(), 
                     strip.background = element_rect(fill = 'white', colour = 'white'))
  
  
}

my_polar_theme <- function(){
  theme(
    text = element_text(size=15),
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(),
    panel.grid.major = element_line_interactive(size = 0.1, linetype = 'solid',
                                                colour = "grey"), 
    panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
    strip.background = element_rect(fill = 'transparent', colour = 'transparent'))
  
}

# Define the UI
ui = dashboardPage(
  
  # App title
  header <-  dashboardHeader(title = "SDGs Progress"),
  
  # Sidebar layout with input and output definitions
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Global Rank", tabName = "global_index", icon = icon("dashboard")),
      menuItem("SDGs Countries", tabName = "countries", icon = icon("dashboard")),
      menuItem("SDGs Indicators", tabName = "indicators", icon = icon("dashboard")),
      menuItem("Indicators Analysis", tabName = "analytics", icon = icon("table"))
    )
  ),
  # Sidebar panel for inputs 
  body <-  dashboardBody(
    
    tabItems(
      ######################################################## 1st tab ##########################################
      tabItem(tabName = "global_index",
              fluidRow(
                valueBoxOutput("populationBox"),
                valueBoxOutput("povertyBox"),
                valueBoxOutput("climateBox")
              ),
              fluidRow(
                #################### buttons##############
                tags$button(
                  id = "wgoal_1",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-01.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_2",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-02.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_3",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-03.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_4",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-04.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_5",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-05.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_6",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-06.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_7",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-07.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_8",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-08.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_9",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-09.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_10",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-10.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_11",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-11.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_12",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-12.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_13",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-13.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_14",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-14.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_15",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-15.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_16",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-16.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_17",
                  class = "btn action-button",
                  tags$img(src = "SDG_Icons_2019/E-WEB-Goal-17.png",
                           height = "40px"))
                
              ),
              #################### outputs ##############
              fluidRow(
                column(width = 3,
                       box(h2("SDGs Progress",align = "center"),
                           p("The Sustainable Development Goals (SDGs) interactive dashboard shows the progress across all of its objectives. To reveal the numerical values on the plots, please hover the mouse over them."),
                           p("The initial world map represents the overall SDGs index progress across the countries. To visualize each SDG, please click on the SDGs logos above to update the world map."),
                           p("The bottom left polar bar plot shows the overall SDGs index progress."),
                           p("The bottom right chart shows the position of the countries based on SDG’s index report 2019."),width = "100%"
                       )
                ),
                column(width = 9,
                       box(h2( textOutput('title'),align = "center"),
                           h3("Countries Index Score 2019",align = "center"),width = "100%",
                           withSpinner(girafeOutput("indexPlot",width = "100%"))
                       )
                )
              ),
              fluidPage(
                column(width = 6,
                       box(h2("SDGs Progress",align = "center"),
                           h3("Index 2019",align = "center"),width = "100%",
                           withSpinner(girafeOutput("indexPolarBar"))
                       )
                ),
                column(width = 6,
                       box(h2("Countries Position",align = "center"),width = "100%",
                           withSpinner(girafeOutput("indextPosition"))
                       )
                )
              )
              
      ), 
      ######################################################## 2nd tab ##########################################
      tabItem(
        tabName = "countries",
        fluidRow(
          column(width = 6,
                 box(title = "SDGs Progress",width = NULL,status = "primary",
                     selectInput(inputId = "country",
                                 label = "Please select a country:",
                                 choices = unique(sdgsIndex$Country)
                     )
                 )
          ),
          column(width = 6,
                 box(title = "SDGs",width = NULL,status = "primary",
                     tags$button(
                       id = "goal_1",
                       class = "btn action-button",
                       tags$img(src = "SDG_Icons_2019/E-WEB-Goal-01.png",
                                height = "40px")
                     ),
                     tags$button(
                       id = "goal_2",
                       class = "btn action-button",
                       tags$img(src = "SDG_Icons_2019/E-WEB-Goal-02.png",
                                height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_3",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-03.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_4",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-04.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_5",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-05.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_6",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-06.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_7",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-07.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_8",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-08.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_9",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-09.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_10",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-10.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_11",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-11.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_12",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-12.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_13",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-13.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_14",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-14.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_15",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-15.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_16",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-16.png",
                                          height = "40px")
                     ),
                     tags$button(style="border-radio: 10px",
                                 id = "goal_17",
                                 class = "btn action-button",
                                 tags$img(src = "SDG_Icons_2019/E-WEB-Goal-17.png",
                                          height = "40px")
                     )
                 )
          )
        ),
        fluidRow(
          column(width = 6,
                 box(title = "Country",width = NULL,status = "primary",
                     userOutput("countryProfile")
                 ),
                 box(h2("SDGs Progress",align = "center"),
                     h2( textOutput('countrytitle'),align = "center"),
                     width = NULL,status = "primary",
                     withSpinner(girafeOutput("countryPolarBar"))
                 )
          ),
          column(width = 6,
                 
                 box(h2("SDGs Global Index Scores",align = "center"),
                     h2( textOutput('title2'),align = "center"),
                     h3( textOutput('title2.1'),align = "center"),
                     width = NULL,status = "primary",
                     withSpinner(girafeOutput("countryPlot"))
                 )
          )
        )
      ),
      ######################################################## 3rd tab ##########################################
      tabItem(
        tabName = "indicators",
        fluidRow(
          column(width = 4,
                 box(h2("SDG Indicator Progress",align = "center"),width = "100%",
                     selectInput(inputId = "goal",
                                 label = "Please select a goal:",
                                 choices = unique(sdgsMap$Goal)
                                 
                     ),
                     uiOutput("secondSelection"),
                     uiOutput("thirdSelection")
                 )),
          column(width = 8,
                 box(h3("World Regions"),align = "center",
                     h4( textOutput('IndicatorTitle'),align = "center"),width = "100%",
                     withSpinner(girafeOutput("regionsPlot"))
                 )
          )
        ),
        fluidPage(
          column(width = 6,
                 box(h3("Box Plot of the Regions",align = "center"),
                     h5("The boxes shows the median levels of each region, and possible outlier in the data",align = "center"),width = "100%",
                     withSpinner(girafeOutput("regionBoxPlot"))
                 )
          ),
          column(width = 6,
                 box(h3("Indicator Progress",align = "center"),
                     h5("Yearly time line of the SDG indicator:",align = "center"),
                     h6( textOutput('IndicatorTitle2'),align = "center"),width = "100%",
                     withSpinner(girafeOutput("regionLinePlot"))
                 )
          ),
          # Hiden errors
          tags$style(type = "text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }")
          
        )
      ),
      ######################################################## 4th tab ##########################################
      tabItem(
        tabName = "analytics",
        fluidPage(
          column(width = 6,
                 box(h2("Analysis of the SDGs Indicators",align = "center"),
                     h3("Test of Normality & Time Series",align = "center"),width = "100%",
                     selectInput(inputId = "t2goal",
                                 label = "Please select a goal:",
                                 choices = unique(regions$Goal)
                     ),
                     uiOutput("t2secondSelection")
                 ),
                 box(title = "Dataset Table",width = "100%",
                     DT::dataTableOutput(outputId = "table", width = "auto")
                     #tableOutput("tablePlot")
                 )
          ),
          column(width = 6,
                 box(h3( "Indicator Descriptives Analysis",align = "center"),
                     h4( textOutput('IndicatorT'),align = "center"),width = "100%",
                     verbatimTextOutput("des"),
                     h3("Histogram",align = "center"),
                     h4("The histogram shows the distribution of the data across the Indicator values.",align = "center"),
                     withSpinner(plotOutput("hPlot")),
                     h3( "Shapiro Wilk Normality Tests",align = "center"),
                     h4("Hypothesis"),
                     h5(textOutput("h0")),
                     h5(textOutput("h1")),
                     h5("α = The alfa value is set to 0.05 with 95% confidence rate."),
                     verbatimTextOutput("Result"),
                     h5(textOutput('hypo'))
                 ),
                 box(h3("Time Series Analysis",align = "center"),
                     h4("ARIMA (Auto-Regressive Integrated Moving Averages)",align = "center"),
                     h5("This analysis is based on the ARIMA model which is a great tool for forecast using historical data"),width = "100%",
                     verbatimTextOutput("autoarima"),
                     h4("Model Residuals",align = "center"),
                     withSpinner(plotOutput("residualsP")),
                     
                     h3("Ljung-Box test",align = "center"),
                     h5("Ljung-Box test is used to determine if the data is stationary or not stationary ",align = "center"),
                     h4("Hypothesis"),
                     h5(textOutput("tsh0")),
                     h5(textOutput("tsh1")),
                     h5("α = The alfa value is set to 0.05 with 95% confidence rate."),
                     verbatimTextOutput("residuals"),
                     h5(textOutput('tshypo')),
                     h3("Indicator Prediction for 2030",align = "center"),
                     h4( textOutput('IndicatorT2'),align = "center"),
                     withSpinner(plotOutput("prediction"))
                 )
          ),
          # Hiden errors
          tags$style(type = "text/css",
                     ".shiny-output-error { visibility: hidden; }",
                     ".shiny-output-error:before { visibility: hidden; }")
        )
      )
    )
  )
)

dashboardPage(header,sidebar,body)
######################################################## Server Functions ##########################################
# Define the server
server = function(input, output) {
  subsetInputs <- reactive({
    subset(regions, regions$Goal == input$t2goal & regions$Indicator == input$t2indicator 
           & regions$SDGsRegions == input$t2geoarea)
    
  })
  ######################################################## 1st tab server side ##########################################
  #value box 1 population in 2019
  output$populationBox <- renderValueBox({
    pop<-sum(sdgsIndex$Population_in_2019)
    pop<-prettyNum(pop,big.mark=",",scientific=FALSE)
    valueBox(
      pop, "World population in 2019", icon = icon("users"),
      color = "purple"
    )
  })
  #value box 2 people in poverty
  output$povertyBox <- renderValueBox({
    pov <- filter(sdgsMap, Indicator %in% "Employed population below international poverty line, by sex and age (%)" &
                    GeoArea %in%  "World" & Period %in% "2018")
    pop<-sum(sdgsIndex$Population_in_2019)
    pov <- pov$Value
    pov1 <- pov/100
    pop1<- pop * pov1
    pop1 <- round(pop1,digits = 0)
    pop1 <-prettyNum(pop1,big.mark=",",scientific=FALSE)
    valueBox(
      value = pop1,
      subtitle = paste0("(",pov,"%",")"," Of the population remained poverty in 2019"), 
      color = "red"
    )
  })
  #value box 3 climate change
  output$climateBox <- renderValueBox({
    cliTotal <- sum(cli$Value)
    cliTotal<-prettyNum(cliTotal,big.mark=",",scientific=FALSE)
    valueBox(
      value = paste0(cliTotal), 
      subtitle = "Number of deaths due to climate changes in 2018",
      color = "green"
    )
  })
  
  #countries world map
  output$title <- renderText({"Global Score Progress Of All SDGs"})
  output$indexPlot <- renderGirafe({
    
    m<-ggplot(sdgsIndexMapCo1tb, aes(long, lat)) +
      geom_polygon_interactive(aes(group = group, fill = Global_Score,
                                   tooltip = sprintf("%s<br/>%s", region, Global_Score)),color = "white")+
      scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
      scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
      scale_x_continuous(breaks = c()) +
      my_theme()
    girafe(ggobj = m, width = 15,height=9)
  })
  #polar bar chart
  output$indexPolarBar <- renderGirafe({
    plotpolarIndex <- ggplot(polarIndex,
                             aes(
                               x = variable,
                               y = value,
                               fill = variable,
                               tooltip = sprintf("%s<br/>%s", variable, value)
                             )) +
      geom_col_interactive(width = 1, color = "white")+
      #scale_fill_manual_interactive(values=cores)+
      coord_polar()+
      my_polar_theme()
    girafe(ggobj = plotpolarIndex)
  })
  #country position chart
  output$indextPosition <- renderGirafe({
    p <- ggplot(topBottom, aes(x=reorder(Country,-Global_Rank), y=Global_Score, fill= reorder(Country,-Global_Rank),
                               tooltip = sprintf("%s<br/>%s", Country,Global_Rank))) +
      geom_bar_interactive(stat="identity", alpha=.5, width=.6) +
      geom_text(aes(label = Global_Score, y = Global_Score -2), size = 2)+
      scale_fill_manual_interactive(values=rankCores)+
      coord_flip() +
      xlab("") +
      ylab("")+
      theme(
        text = element_text(size=15),
        legend.position = "none",
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_text(),
        panel.grid.major = element_line_interactive(size = 0.1, linetype = 'solid',
                                                    colour = "grey"), 
        panel.background = element_rect(fill = 'transparent', colour = 'transparent'),
        strip.background = element_rect(fill = 'transparent', colour = 'transparent'))
    girafe(ggobj = p)
  })
  
  
  ######################### SDGs actions buttons ############
  observeEvent(input$wgoal_1,{
    output$title <- renderText({"Global Index SDG Progress in Goal 1: No Poverty"})
    
    output$indexPlot <- renderGirafe({
      g1 <- sdgsIndexMapCo[,c(1:5,10)]
      names(g1)[6]<-"goal1"
      m1<-ggplot(g1, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal1,
                                     tooltip = sprintf("%s<br/>%s", region, goal1)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(5, "Reds")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m1, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_2,{
    output$title <- renderText({"Global Index SDG Progress in Goal 2: Zero Hunger"})
    output$indexPlot <- renderGirafe({
      g2 <- sdgsIndexMapCo[,c(1:5,11)]
      names(g2)[6]<-"goal2"
      m2<-ggplot(g2, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal2,
                                     tooltip = sprintf("%s<br/>%s", region, goal2)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(4, "YlOrRd")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m2, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_3,{
    output$title <- renderText({"Global Index SDG Progress in Goal 3: Good Health and Well-being"})
    output$indexPlot <- renderGirafe({
      g3 <- sdgsIndexMapCo[,c(1:5,12)]
      names(g3)[6]<-"goal3"
      m3<-ggplot(g3, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal3,
                                     tooltip = sprintf("%s<br/>%s", region, goal3)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(5, "Greens")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m3, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_4,{
    output$title <- renderText({"Global Index SDG Progress in Goal 4: Quality Education"})
    output$indexPlot <- renderGirafe({
      g4 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g4)[6]<-"goal4"
      m4<-ggplot(g4, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal4,
                                     tooltip = sprintf("%s<br/>%s", region, goal4)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(5, "Reds")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m4, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_5,{
    output$title <- renderText({"Global Index SDG Progress in Goal 5: Gender Equality"})
    output$indexPlot <- renderGirafe({
      g5 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g5)[6]<-"goal5"
      m5<-ggplot(g5, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal5,
                                     tooltip = sprintf("%s<br/>%s", region, goal5)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(5, "PuOr")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m5, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_6,{
    output$title <- renderText({"Global Index SDG Progress in Goal 6: Clean Water and Sanitation"})
    output$indexPlot <- renderGirafe({
      g6 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g6)[6]<-"goal6"
      m6<-ggplot(g6, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal6,
                                     tooltip = sprintf("%s<br/>%s", region, goal6)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(5, "Blues")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m6, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_7,{
    output$title <- renderText({"Global Index SDG Progress in Goal 7: Affordable and Clean Energy"})
    output$indexPlot <- renderGirafe({
      g7 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g7)[6]<-"goal7"
      m7<-ggplot(g7, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal7,
                                     tooltip = sprintf("%s<br/>%s", region, goal7)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(3, "YlOrRd")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m7, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_8,{
    output$title <- renderText({"Global Index SDG Progress in Goal 8: Decent Work and Economic Growth"})
    output$indexPlot <- renderGirafe({
      g8 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g8)[6]<-"goal8"
      m8<-ggplot(g8, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal8,
                                     tooltip = sprintf("%s<br/>%s", region, goal8)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(3, "RdBu")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m8, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_9,{
    output$title <- renderText({"Global Index SDG Progress in Goal 9: Industry, Innovation and Infrastructure"})
    output$indexPlot <- renderGirafe({
      g9 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g9)[6]<-"goal9"
      m9<-ggplot(g9, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal9,
                                     tooltip = sprintf("%s<br/>%s", region, goal9)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(4, "PuOr"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m9, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_10,{
    output$title <- renderText({"Global Index SDG Progress in Goal 10: Reduced Inequality"})
    output$indexPlot <- renderGirafe({
      g10 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g10)[6]<-"goal10"
      m10<-ggplot(g10, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal10,
                                     tooltip = sprintf("%s<br/>%s", region, goal10)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(3, "PiYG")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m10, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_11,{
    output$title <- renderText({"Global Index SDG Progress in Goal 11: Sustainable Cities and Communities"})
    output$indexPlot <- renderGirafe({
      g11 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g11)[6]<-"goal11"
      m11<-ggplot(g11, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal11,
                                     tooltip = sprintf("%s<br/>%s", region, goal11)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(4, "Oranges")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m11, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_12,{
    output$title <- renderText({"Global Index SDG Progress in Goal 12:  Responsible Consumption and Production"})
    output$indexPlot <- renderGirafe({
      g12 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g12)[6]<-"goal12"
      m12<-ggplot(g12, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal12,
                                     tooltip = sprintf("%s<br/>%s", region, goal12)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(4, "YlOrBr")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m12, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_13,{
    output$title <- renderText({"Global Index SDG Progress in Goal 13: Climate Action"})
    output$indexPlot <- renderGirafe({
      g13 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g13)[6]<-"goal13"
      m13<-ggplot(g13, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal13,
                                     tooltip = sprintf("%s<br/>%s", region, goal13)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(5, "Greens")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m13, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_14,{
    output$title <- renderText({"Global Index SDG Progress in Goal 14: Life Below Water"})
    output$indexPlot <- renderGirafe({
      g14 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g14)[6]<-"goal14"
      m14<-ggplot(g14, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal14,
                                     tooltip = sprintf("%s<br/>%s", region, goal14)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(5, "Blues")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m14, width = 15,height=9)
      
    })
  })
  
  observeEvent(input$wgoal_15,{
    output$title <- renderText({"Global Index SDG Progress in Goal 15: Life on Land"})
    output$indexPlot <- renderGirafe({
      g15 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g15)[6]<-"goal15"
      m15<-ggplot(g15, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal15,
                                     tooltip = sprintf("%s<br/>%s", region, goal15)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(4, "Greens")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m15, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_16,{
    output$title <- renderText({"Global Index SDG Progress in Goal 16: Peace and Justice Strong Institutions"})
    output$indexPlot <- renderGirafe({
      g16 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g16)[6]<-"goal16"
      m16<-ggplot(g16, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal16,
                                     tooltip = sprintf("%s<br/>%s", region, goal16)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(6, "Blues")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m16, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_17,{
    output$title <- renderText({"Global Index SDG Progress in Goal 17: Partnerships to achieve the Goal"})
    output$indexPlot <- renderGirafe({
      g17 <- sdgsIndexMapCo[,c(1:5,13)]
      names(g17)[6]<-"goal17"
      m17<-ggplot(g17, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = goal17,
                                     tooltip = sprintf("%s<br/>%s", region, goal17)),color = "white")+
        scale_fill_gradientn(colours = rev(brewer.pal(7, "Blues")), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m17, width = 15,height=9)
      
    })
  })
  ######################################################## 2nd tab server side ##########################################
  #country profile
  output$countryProfile <- renderUser({
    lower <- tolower(input$country)
    lower<- str_replace_all(lower, " ","-")
    path <- "country_icons/"
    logo <- paste(path,lower,".svg",sep = "")
    pop<- sdgsIndex$Population_in_2019[ sdgsIndex$Country == input$country]
    pop<- prettyNum(pop,big.mark=",",scientific=FALSE)
    ggp<- sdgsIndex$GDP_per_capita_in_2017[ sdgsIndex$Country == input$country]
    ggp<- prettyNum(ggp,big.mark=",",scientific=FALSE)
    miss<- sdgsIndex$Percentage_missing_values[ sdgsIndex$Country == input$country]
    miss <- paste(miss,"%",sep = "")
    boxProfile(
      src = logo,
      title = input$country,
      subtitle = ,
      boxProfileItemList(
        bordered = TRUE,
        boxProfileItem(
          title = "Population in 2019",
          description = pop
        ),
        boxProfileItem(
          title = "GGP/Capita 2017",
          description = ggp
        ),
        boxProfileItem(
          title = "Percentage of missing values",
          description = miss
        )
      )
    )
  })
  #country plot
  output$title2 <- renderText({"Regional Index Score"})
  output$countryPlot <- renderGirafe({
    
    countryMap <- subset(worldMap, region == input$country)     
    countryMapInd <- subset(sdgsIndexMap, region == input$country)
    countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
    c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Regional_Score,
                                                                        tooltip = sprintf("%s<br/>%s", id, Regional_Score)))+
      scale_fill_gradientn(colours = "skyblue2", na.value = 'white') + 
      
      my_theme()+
      ggtitle(input$country)
    girafe(ggobj = c)
  })
  #Polar bar
  output$countrytitle <- renderText({input$country})
  output$countryPolarBar <- renderGirafe({
    
    countriesPolarChart <- subset(countriesPolar, region == input$country)
    countriesPolarChart$variable <- factor(countriesPolarChart$variable, labels = sdgsLabels)
    plotpolarCountries <- ggplot(countriesPolarChart,
                                 aes(
                                   x = variable,
                                   y = value,
                                   fill = variable,
                                   tooltip = sprintf("%s<br/>%s", variable, value)
                                 )) +
      scale_fill_manual_interactive(values=cores)+
      geom_col_interactive(width = 1, color = "white")+
      coord_polar()+
      my_polar_theme()
    girafe(ggobj = plotpolarCountries)
  })
  ######################### SDGs actions buttons ############
  observeEvent(input$goal_1,{
    output$title2 <- renderText({"Goal:1 No Poverty"})
    output$linePlot <- renderGirafe({
      #tmp<- sdgsMap$GeoArea == input$country
      tmp <- subset(g1EmpPovert, Country == input$country)
      wp<-ggplot(tmp) +
        aes(x = Period, y = Value, group=1,
            tooltip = sprintf("%s<br/>%s", tmp$Country, tmp$Value)) +
        geom_point_interactive(size = 2L, colour = "#ef562d") +
        geom_line_interactive(color="blue", size=1)+
        #geom_smooth_interactive(stat = "smooth", method = "loess") +
        #geom_text(aes(label = Value, y = Value +0.5), size = 2)+
        theme_minimal()
      
      girafe(ggobj = wp)
    })
    
    output$countryPlot <- renderGirafe({
      countryMap <- subset(worldMap, region == input$country)     
      countryMapInd <- subset(sdgsIndexMap, region == input$country)
      countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_1,
                                                                          tooltip = sprintf("%s<br/>%s", id, Goal_1)))+
        scale_fill_gradientn(colours = "red", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_2,{
    output$title2 <- renderText({"Goal:2 Zero Hunger"})
    output$countryPlot <- renderGirafe({
      countryMap <- subset(worldMap, region == input$country)     
      countryMapInd <- subset(sdgsIndexMap, region == input$country)
      countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_2,
                                                                          tooltip = sprintf("%s<br/>%s", id, Goal_2)))+
        scale_fill_gradientn(colours = "orange", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_3,{
    output$title2 <- renderText({"Goal:3 Good Health and Well-being"})
    output$countryPlot <- renderGirafe({
      countryMap <- subset(worldMap, region == input$country)     
      countryMapInd <- subset(sdgsIndexMap, region == input$country)
      countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_3,
                                                                          tooltip = sprintf("%s<br/>%s", id, Goal_3)))+
        scale_fill_gradientn(colours = "green3", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_4,{
    output$title2 <- renderText({"Goal:4 Quality Education"})
    output$countryPlot <- renderGirafe({
      countryMap <- subset(worldMap, region == input$country)     
      countryMapInd <- subset(sdgsIndexMap, region == input$country)
      countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_4,
                                                                          tooltip = sprintf("%s<br/>%s", id, Goal_4)))+
        scale_fill_gradientn(colours = "red3", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_5,{
    output$title2 <- renderText({"Goal:5 Gender Equality"})
    output$countryPlot <- renderGirafe({
      countryMap <- subset(worldMap, region == input$country)     
      countryMapInd <- subset(sdgsIndexMap, region == input$country)
      countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_5,
                                                                          tooltip = sprintf("%s<br/>%s", id, Goal_5)))+
        scale_fill_gradientn(colours = "red2", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_6,{
    output$title2 <- renderText({"Goal:6 Clean Water and Sanitation"})
    output$countryPlot <- renderGirafe({
      countryMap <- subset(worldMap, region == input$country)     
      countryMapInd <- subset(sdgsIndexMap, region == input$country)
      countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_6,
                                                                          tooltip = sprintf("%s<br/>%s", id, Goal_6)))+
        scale_fill_gradientn(colours = "steelblue2", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_7,{
    output$title2 <- renderText({"Goal:7 Affordable and Clean Energy"})
    output$countryPlot <- renderGirafe({
      countryMap <- subset(worldMap, region == input$country)     
      countryMapInd <- subset(sdgsIndexMap, region == input$country)
      countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_7,
                                                                          tooltip = sprintf("%s<br/>%s", id, Goal_7)))+
        scale_fill_gradientn(colours = "gold2", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_8,{
    output$countryPlot <- renderGirafe({
      output$title2 <- renderText({"Goal:8 Decent Work and Economic Growth"})
      output$title2.1 <- renderText({input$country})
      countryMap <- subset(worldMap, region == input$country)     
      countryMapInd <- subset(sdgsIndexMap, region == input$country)
      countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_8,
                                                                          tooltip = sprintf("%s<br/>%s", id, Goal_8)))+
        scale_fill_gradientn(colours = "red4", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
    observeEvent(input$goal_9,{
      output$countryPlot <- renderGirafe({
        output$title2 <- renderText({"Goal:9 Industry, Innovation and Infrastructure"})
        countryMap <- subset(worldMap, region == input$country)     
        countryMapInd <- subset(sdgsIndexMap, region == input$country)
        countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_9,
                                                                            tooltip = sprintf("%s<br/>%s", id, Goal_9)))+
          scale_fill_gradientn(colours = "orangered1", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_10,{
      output$title2 <- renderText({"Goal:10 Reduced Inequality"})
      output$countryPlot <- renderGirafe({
        countryMap <- subset(worldMap, region == input$country)     
        countryMapInd <- subset(sdgsIndexMap, region == input$country)
        countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_10,
                                                                            tooltip = sprintf("%s<br/>%s", id, Goal_10)))+
          scale_fill_gradientn(colours = "deeppink3", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_11,{
      output$title2 <- renderText({"Goal:11 Sustainable Cities and Communities"})
      output$countryPlot <- renderGirafe({
        countryMap <- subset(worldMap, region == input$country)     
        countryMapInd <- subset(sdgsIndexMap, region == input$country)
        countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_11,
                                                                            tooltip = sprintf("%s<br/>%s", id, Goal_11)))+
          scale_fill_gradientn(colours = "orange3", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_12,{
      output$title2 <- renderText({"Goal:12 Responsible Consumption and Production"})
      output$countryPlot <- renderGirafe({
        countryMap <- subset(worldMap, region == input$country)     
        countryMapInd <- subset(sdgsIndexMap, region == input$country)
        countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_12,
                                                                            tooltip = sprintf("%s<br/>%s", id, Goal_12)))+
          scale_fill_gradientn(colours = "orange4", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_13,{
      output$title2 <- renderText({"Goal:13 Climate Action"})
      output$countryPlot <- renderGirafe({
        countryMap <- subset(worldMap, region == input$country)     
        countryMapInd <- subset(sdgsIndexMap, region == input$country)
        countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_13,
                                                                            tooltip = sprintf("%s<br/>%s", id, Goal_13)))+
          scale_fill_gradientn(colours = "green4", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_14,{
      output$title2 <- renderText({"Goal:14 Life Below Water"})
      output$countryPlot <- renderGirafe({
        countryMap <- subset(worldMap, region == input$country)     
        countryMapInd <- subset(sdgsIndexMap, region == input$country)
        countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_14,
                                                                            tooltip = sprintf("%s<br/>%s", id, Goal_14)))+
          scale_fill_gradientn(colours = "dodgerblue1", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_15,{
      output$title2 <- renderText({"Goal:15 Life on Land"})
      output$countryPlot <- renderGirafe({
        countryMap <- subset(worldMap, region == input$country)     
        countryMapInd <- subset(sdgsIndexMap, region == input$country)
        countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_15,
                                                                            tooltip = sprintf("%s<br/>%s", id, Goal_15)))+
          scale_fill_gradientn(colours = "green2", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_16,{
      output$title2 <- renderText({"Goal:16 Peace and Justice Strong Institutions"})
      output$countryPlot <- renderGirafe({
        countryMap <- subset(worldMap, region == input$country)     
        countryMapInd <- subset(sdgsIndexMap, region == input$country)
        countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_16,
                                                                            tooltip = sprintf("%s<br/>%s", id, Goal_16)))+
          scale_fill_gradientn(colours = "dodgerblue4", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_17,{
      output$title2 <- renderText({"Goal:17 Partnerships to achieve the Goal"})
      output$countryPlot <- renderGirafe({
        countryMap <- subset(worldMap, region == input$country)     
        countryMapInd <- subset(sdgsIndexMap, region == input$country)
        countryMapIndex<- merge(countryMap,countryMapInd, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMapIndex, aes(x=long, y = lat, group = group,fill = Goal_17,
                                                                            tooltip = sprintf("%s<br/>%s", id, Goal_17)))+
          scale_fill_gradientn(colours = "midnightblue", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
  })
  
  ######################################################## 3rd tab server side ##########################################
  # Indicators world map
  output$IndicatorTitle <- renderText({input$indicator})
  output$IndicatorTitle2 <- renderText({input$indicator})
  output$regionsPlot <- renderGirafe({
    regionsMap1<- subset(regions, regions$Goal == input$goal & regions$Indicator == input$indicator & regions$Period == input$period )
    
    regionsMap1 <- merge(worldMap, regionsMap1,  sort = FALSE, by = "SDGsRegions")
    regionsMap1 <- regionsMap1[order(regionsMap1$order), ]
    m<-ggplot(regionsMap1, aes(long, lat)) +
      geom_polygon_interactive(aes(group = group, fill = Value,
                                   tooltip = sprintf("%s<br/>%s", SDGsRegions, Value)))+
      scale_fill_gradientn(colors = rev(brewer.pal(7, "RdBu")), na.value = 'white') + 
      scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
      scale_x_continuous(breaks = c()) +
      my_theme()
    girafe(ggobj = m, width = 15, height=9)
  })
  #indicartor box plot
  output$regionBoxPlot <-renderGirafe({
    boxregions <- data.frame(subset(regions, regions$Goal == input$goal & regions$Indicator == input$indicator))
    names(boxregions)[3] <- "GeoArea"
    b <- ggplot(boxregions) +
      aes(x = GeoArea, y = Value, fill = GeoArea, tooltip = GeoArea) +
      geom_boxplot_interactive(outlier.colour = "red" ) +
      guides(fill = "none") + 
      labs(y = "Levels in %", x = "Regions")+
      theme_minimal()+
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(),
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            text = element_text(family = "Times New Roman", size = 12 ))
    girafe(ggobj = b )
    
  })
  #indicator line plot
  output$regionLinePlot <- renderGirafe({
    lineregions <- data.frame(subset(regions, regions$Goal == input$goal & regions$Indicator == input$indicator))
    lineregions <- na.omit(lineregions)
    lineregions <- lineregions%>%
      select(Period,Value)%>%
      group_by(Period) %>%
      summarise(Value = mean(Value))
    lineregions$Value<- round(lineregions$Value, digits=2)
    
    lp <- ggplot(lineregions) +
      aes(x = Period, y = Value,group=1,
          tooltip = sprintf("%s<br/>%s", Period , Value) )+
      geom_point_interactive (size = 3L, colour = "#ef562d") +
      #geom_smooth (method = "loess") +
      geom_line(color="blue", size=1)+
      #geom_text_interactive (aes(label = Value, y = Value +1), size = 3)+
      labs(x = "Period of Time", y = "Levels in %") +
      
      theme_minimal()+
      theme(axis.text.x = element_text( angle = 45),
            text = element_text(family = "Times New Roman", size = 12 ))
    
    girafe(ggobj = lp)
  })
  # Change the choices for the second selection on the basis of the input to the first selection
  output$secondSelection <- renderUI({
    choice_second <- as.list(unique(regions$Indicator[which(regions$Goal == input$goal)]))
    selectInput(inputId = "indicator", choices = choice_second,
                label = "Choose an SDG Indicator:")
  })
  
  # Change the choices for the third selection on the basis of the input to the first and second selections
  output$thirdSelection <- renderUI({
    choice_third <- as.list(unique(regions$Period[regions$Goal == input$goal & regions$Indicator == input$indicator]))
    selectInput(inputId = "period", choices = choice_third,
                label = paste0("Choose the year to change the map values"))
  }) 
  
  ######################################################## 4th tab server side ##########################################
  t2subsetInputs <- reactive({
    subset(regions, regions$Goal == input$t2goal & regions$Indicator == input$t2indicator)
    
  })
  #normality test hypothesis
  output$IndicatorT <- renderText({input$t2indicator})
  output$IndicatorT2 <- renderText({input$t2indicator})
  output$h0 <- renderText({paste0("H₀: The null hypothesis states that data in ", input$t2indicator, " is normally distributed.")})
  output$h1 <- renderText({paste0("H₁: The alternative hypothesis states that data in ", input$t2indicator, " is not normally distributed.")})
  
  output$tsh0 <- renderText({paste0("H₀: The null hypothesis states that data in ", input$t2indicator, " is stationary.")})
  output$tsh1 <- renderText({paste0("H₁: The alternative hypothesis states that data in ", input$t2indicator, " is not stationary.")})
  
  #table plot
  output$table <- DT::renderDT({
    bx <- t2subsetInputs()
  }, options = list(pageLength = 20))
  
  #descriptives
  output$des <- renderPrint({
    des <- t2subsetInputs()
    describe(des$Value)
  })
  
  #hist plot
  output$hPlot <- renderPlot({
    bx <- t2subsetInputs()
    hist(bx$Value, col = "blue", border = "skyblue", main = "",xlab="Values")
    
  })
  
  #hypothesisnormality test
  shp <- reactive({
    bx <- t2subsetInputs()
    shapiro.test(bx$Value)
    
  })
  output$Result <- renderPrint({
    shp()
    
  })
  #hypothesis if statement
  output$hypo <- renderText({
    
    if (shp()$p.value <0.05) {
      "In this case the P-value < 0.05 reject H₀. Therefore, the data IS NOT normally distributed across the regions."
    } else {
      "In this case the P-value > 0.05 fail to reject H₀. Therefore, the data IS normally distributed across the regions."
    }
  })
  # time series
  output$prediction <- renderPlot({
    #preparing the data
    tmp<- t2subsetInputs()
    tmp <- na.omit(tmp)
    #aggregating the data
    ts <- tmp%>%
      select(Period,Value)%>%
      group_by(Period) %>%
      summarise(Value = mean(Value))
    ts$Value<- round(ts$Value, digits=2)
    ts$Period <- as.numeric (ts$Period)
    ts <- ts(ts$Value, freq=1,start =min(ts$Period), end = max (ts$Period))
    
    tsOPT <- auto.arima(ts)
    
    coef(tsOPT)
    predict(tsOPT, n.ahead = 12, se.fit = T)
    indicator.forcast <- forecast(object = tsOPT, h = 12)
    checkresiduals(indicator.forcast)
    par(mfrow=c(1,2))
    plot.ts(ts,xlab="Period of Time" , ylab="Levels in %" , main="Actual")
    plot(indicator.forcast, xlab="Period of Time" , ylab="Levels in %" , main="Forecast", col = "red", lwd=1 , pch=17 )
    abline(h=0, col="blue",lty=2)
    
    output$autoarima <- renderPrint({
      tsOPT
    })
    output$residualsP <- renderPlot ({
      checkresiduals(indicator.forcast)
    })
    output$residuals <- renderPrint({
      checkresiduals(indicator.forcast)
    })
    # if statement 
    output$tshypo <- renderText({
      res<-checkresiduals(indicator.forcast)
      if (res$p.value <0.05) {
        "In this case the P-value < 0.05 reject H₀. Therefore, the data IS NOT stationay."
      } else {
        "In this case the P-value > 0.05 fail to reject H₀. Therefore, the data IS stationay.."
      }
    })
    
  })
  
  # Change the choices for the second selection on the basis of the input to the first selection
  output$t2secondSelection <- renderUI({
    t2choice_second <- as.list(unique(regions$Indicator[which(regions$Goal == input$t2goal)]))
    selectInput(inputId = "t2indicator", choices = t2choice_second,
                label = "Choose the indicator for analysis:")
  })
  
}

shinyApp(ui = ui, server = server)

