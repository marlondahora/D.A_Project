##Ranking# https://sdgindex.org/reports/sustainable-development-report-2019/
### https://unstats.un.org/SDGAPI/v1/sdg/Goal/DataCSV
### downloading the dataset from year 2000 - present
sdgRaw <- read.csv("https://unstats.un.org/SDGAPI//staging/20200127152310260_x16117492@student.ncirl.ie_data.csv", 
                   stringsAsFactors = FALSE, 
                   sep = ',')

write.csv(sdgRaw, file = "sdgRaw.csv")

library(readr)

### the file sdgRaw was modified in excel
setwd("/home/marlon/R_projects")
sdg1_10 <- read.csv(file = "sdg1_10.csv", stringsAsFactors = FALSE, na = '0', sep = ',' )
sdg11_17 <- read.csv(file = "sdg11_17.csv",stringsAsFactors = FALSE, na = '0', sep = ',' )

sdgs <- rbind(sdg1_10, sdg11_17)
sdgs <- as.data.frame(sdgs [, c(-1,-2)])
sdgs <- as.data.frame(sdgs [, c(1,2,5,7,8,9,17,18,32)])

# rename columns 
names(sdgs)[1] <- "Goal"
names(sdgs)[3] <- "Indicator"
names(sdgs)[4] <- "GeoArea"
names(sdgs)[5] <- "Period"
names(sdgs)[6] <- "Value"
names(sdgs)[8] <- "Age"
names(sdgs)[9] <- "Sex"

#cleaning the dataset
sdgs$Value<- str_replace_all(sdgs$Value, "<","")
sdgs$Value<- str_replace_all(sdgs$Value, ">","")
sdgs$Value[sdgs$Value == "0" ] <- NA
sdgs$Value[sdgs$Value == "N" ] <- NA
sdgs$Value[sdgs$Value == "NV" ] <- NA
sdgs$Age[sdgs$Age == "" ] <- NA
sdgs$Sex[sdgs$Sex == "" ] <- NA
summary(sdgs)

write_csv(sdgs,"sdgs.csv")

###################
########################
##############################


sdgs <- read_csv("sdgs.csv",col_names = TRUE, col_types = 
                   cols(
                     Goal = col_double(),
                     Target = col_character(),
                     Indicator = col_character(),
                     GeoArea = col_character(),
                     Period = col_double(),
                     Value = col_double(),
                     Units = col_character(),
                     Age = col_character(),
                     Sex = col_character()
                   ))
str(sdgs)
summary(sdgs)
round(sdgs$Value, digits=3)
save(sdgs, file="sdgs.RData")
write_csv(sdgs,"sdgs.csv")

##
###Bulding the Map

library(magrittr)
library(rvest)
library(reshape2)
library(tidyverse)


url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="CountryCode"]') %>%
  html_table()
iso_codes <- iso_codes[[1]][, -1]
iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")
head(iso_codes)

#loading a checking the data
setwd("~/D.A_Project")


load("sdgs.RData")
head(sdgs)
summary(sdgs$Value)
sdgsMap <- sdgs
round(sdgsMap$Value, digits=3)
sdgsMap<- filter(sdgsMap, Units %in% "PERCENT") 
#aggregating data

sdgsMap <-  sdgsMap %>%
  select(Goal, Target, Indicator, GeoArea, Period, Value, Units)%>%
  group_by(Goal, Target, Indicator, GeoArea, Period, Units) %>%
  summarise(Value = mean(Value))
summary(sdgsMap$Value)

#tranforming the dataset to data.frame for performance enhancing
sdgsMap <- as.data.frame(sdgsMap)

#adding a new column with the un codes
sdgsMap['UN'] <- iso_codes$UN[match(sdgsMap$GeoArea, iso_codes$Country)]

# To map our data, we need to merge the sdgs and world map data, it is needed to chance some old names.

old_names <- c("Bolivia (Plurinational State of)", "Cabo Verde", "China, Hong Kong Special Administrative Region",
               "China, Macao Special Administrative Region", "Congo", "Democratic People's Republic of Korea",
               "Democratic Republic of the Congo", "Iran (Islamic Republic of)", "Lao People's Democratic Republic",
               "Micronesia (Federated States of)", "Republic of Korea", "Republic of Moldova", "Saint Vincent and the Grenadines",
               "State of Palestine", "Syrian Arab Republic", "The former Yugoslav Republic of Macedonia",
               "United Kingdom of Great Britain and Northern Ireland", "United Republic of Tanzania",
               "United States Virgin Islands", "Venezuela (Bolivarian Republic of)")
new_names <- c("Bolivia", "Cape Verde", "Hong Kong, SAR China", "Macao, SAR China", "Congo (Brazzaville)",
               "Korea (North)", "Congo, (Kinshasa)", "Iran, Islamic Republic of", "Lao PDR", "Micronesia, Federated States of",
               "Korea (South)", "Moldova", "Saint Vincent and Grenadines", "Palestinian Territory", "Syrian Arab Republic (Syria)",
               "Macedonia, Republic of", "United Kingdom", "Tanzania, United Republic of", "Virgin Islands, US", "Venezuela (Bolivarian Republic)")

for (i in 1:length(old_names)){
  sdgsMap$GeoArea[sdgsMap$GeoArea == old_names[i]] <- new_names[i]
}


#renaming the goals.
sdgsMap$Goal<-paste("Goal",sdgsMap$Goal)
sdgsMap <- sdgsMap[, c(-2)]
sdgsMap <- as.data.frame(sdgsMap)
sdgsMap[] <- lapply(sdgsMap, as.character)
sdgsMap$Value <- as.numeric(sdgsMap$Value)
#sdgsMap$Period <- as.numeric(sdgsMap$Period)

# load world map
library(maps)
library(ggplot2)
library(RColorBrewer)
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
head(world_data)

world_data["UN"] <- iso_codes$UN[match(world_data$region, iso_codes$Country)]
world_data["ISO3"] <- iso_codes$ISO3[match(world_data$region, iso_codes$Country)]

old_names <- c("French Southern and Antarctic Lands", "Antigua", "Barbuda", "Saint Barthelemy", "Brunei", "Ivory Coast",
               "Democratic Republic of the Congo", "Republic of Congo", "Falkland Islands", "Micronesia", "UK", 
               "Heard Island", "Cocos Islands", "Iran", "Nevis", "Saint Kitts", "South Korea", "Laos", "Saint Martin",
               "Macedonia", "Pitcairn Islands", "North Korea", "Palestine", "Russia", "South Sandwich Islands",
               "South Georgia", "Syria", "Trinidad", "Tobago", "Taiwan", "Tanzania", "USA", "Vatican", "Grenadines",
               "Saint Vincent", "Venezuela", "Vietnam", "Wallis and Fortuna")
new_names <- c("French Southern Territories", rep("Antigua and Barbuda", 2), "Saint-Barthélemy",
               "Brunei Darussalam", "Côte d'Ivoire", "Congo, (Kinshasa)", "Congo (Brazzaville)", 
               "Falkland Islands (Malvinas)", "Micronesia, Federated States of", "United Kingdom",
               "Heard and Mcdonald Islands", "Cocos (Keeling) Islands", "Iran, Islamic Republic of",
               rep("Saint Kitts and Nevis", 2), "Korea (South)", "Lao PDR", "Saint-Martin (French part)",
               "Macedonia, Republic of", "Pitcairn", "Korea (North)", "Palestinian Territory", "Russian Federation",
               rep("South Georgia and the South Sandwich Islands", 2), 
               "Syrian Arab Republic (Syria)", rep("Trinidad and Tobago", 2), "Taiwan, Republic of China",
               "Tanzania, United Republic of", "United States of America", "Holy See (Vatican City State)",
               rep("Saint Vincent and Grenadines", 2), "Venezuela (Bolivarian Republic)", "Viet Nam", "Wallis and Futuna Islands")

for (i in 1:length(old_names)){
  world_data$region[world_data$region == old_names[i]] <- new_names[i]
}


###
#### Building the map function
worldMaps <- function(sdgsMap, world_data, goal, period, indicator){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.title = element_blank(),
                       axis.text = element_blank(),
                       axis.ticks = element_blank(),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       legend.position = "bottom",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Select only the data that the user has selected to view
  plotsdgsMap <- sdgsMap[sdgsMap$Goal== goal & sdgsMap$Indicator == indicator & 
                           sdgsMap$Period == period,]
  plotsdgsMap <- plotsdgsMap[!is.na(plotsdgsMap$UN), ]
  
  # Add the data the user wants to see to the geographical world data
  world_data['Goal'] <- rep(goal, nrow(world_data))
  world_data['Period'] <- rep(period, nrow(world_data))
  world_data['Indicator'] <- rep(indicator, nrow(world_data))
  world_data['Value'] <- plotsdgsMap$Value[match(world_data$UN, plotsdgsMap$UN)]
  
  
  # Create caption with the data source to show underneath the map
  capt <- paste0("Source: ", ifelse(indicator == "SDGs", "United Nations" , "World Bank"))
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = subset(world_data, lat >= -60 & lat <= 90), color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s", ISO3, Value))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
    labs(fill = goal, color = goal, title = NULL, x = NULL, y = NULL, caption = capt) + 
    my_theme()
  
  return(g)
}

sdgsIndex <- read_csv("2019GlobalIndexResults.csv",col_names = TRUE, col_types =
                        cols(
                          .default = col_double(),
                          Country = col_character(),
                          id = col_character()
                        ))


my_theme <- function () { 
  theme_bw() + theme(axis.title = element_blank(),
                     axis.text = element_blank(),
                     axis.ticks = element_blank(),
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     legend.position = "bottom",
                     panel.border = element_blank(), 
                     strip.background = element_rect(fill = 'white', colour = 'white'))
  
  
}


sdgsIndex1<- sdgsIndex
names(sdgsIndex1)[1] <- "region"
world_map <- map_data("world")
sdgsIndexchoro <- merge(world_map, sdgsIndex1,  sort = FALSE, by = "region")
sdgsIndexchoro <- sdgsIndexchoro[order(sdgsIndexchoro$order), ]
str(sdgsIndexchoro)


library(shiny)
library(ggiraph)
library(tidyverse)
library(shinydashboard)
library(shinydashboardPlus)

# Define the UI

ui = dashboardPage(
  
  # App title
  header <-  dashboardHeader(title = "SDGs Progress"),
  
  # Sidebar layout with input and output definitions
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Global Rank", tabName = "global_index", icon = icon("dashboard")),
      menuItem("SDGs Countries", tabName = "countries", icon = icon("dashboard")),
      menuItem("SDGs Incators", tabName = "indicators", icon = icon("dashboard")),
      menuItem("Analytcs", tabName = "analytics", icon = icon("table"))
    )
  ),
  # Sidebar panel for inputs 
  body <-  dashboardBody(
    
    tabItems(
      ######################################################## 1st tab ##########################################
      tabItem(tabName = "global_index",
              valueBoxOutput("populationBox"),
              
              valueBoxOutput("povertyBox"),
              
              girafeOutput("indexPlot",width = "100%", height = "400px"),
      ),
      ######################################################## 2nd tab ##########################################
      tabItem(tabName = "countries",
              fluidRow(
                column(width = 5,
                       box(
                         title = "Sdgs Countries",width = NULL,status = "primary",
                         selectInput(inputId = "country",
                                     label = "Please select a country:",
                                     choices = unique(sdgsIndex$Country)
                         )
                         
                       ),
                       box(title = "SDGs",width = NULL,status = "primary",
                           tags$button(
                             id = "goal_1",
                             class = "btn action-button",
                             tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-01.png",
                                      height = "40px")
                           ),
                           tags$button(
                             id = "goal_2",
                             class = "btn action-button",
                             tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-02.png",
                                      height = "40px")
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_3",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-03.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_4",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-04.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_5",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-05.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_6",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-06.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_7",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-07.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_8",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-08.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_9",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-09.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_9",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-09.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_10",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-10.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_11",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-11.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_12",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-12.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_13",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-13.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_14",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-14.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_15",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-15.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_16",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-16.png",
                                                height = "40px",)
                           ),
                           tags$button(style="border-radio: 10px",
                                       id = "goal_17",
                                       class = "btn action-button",
                                       tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-17.png",
                                                height = "40px",)
                           )
                           
                           
                           
                       ),
                       
                       box(title = "Country",width = NULL,status = "primary",
                           userOutput("countryProfile")
                       )
                       
                ),
                column(width = 7,
                       box(title = "Country",width = NULL,status = "primary",
                           girafeOutput("countryPlot")
                       )
                )
              )
      ),
      ######################################################## 3rd tab ##########################################
      tabItem(
        tabName = "indicators",
        
        fluidRow(
          box(
            title = "SDGs Indicators Progress",
            selectInput(inputId = "goal",
                        label = "Please select a goal:",
                        choices = unique(sdgsMap$Goal)
                        
            ),
            uiOutput("secondSelection"),
            uiOutput("thirdSelection")
          ),
          box(
            title = "World Map",
            #girafeOutput("distPlot")
          )
        )
        
      ),
      ######################################################## 4th tab ##########################################
      tabItem(tabName = "analytics",
              fluidRow(
                box(
                  title = "Analytical Data",
                  selectInput(inputId = "t2goal",
                              label = "Please select a goal:",
                              choices = unique(sdgsMap$Goal)
                              
                  ),
                  uiOutput("t2secondSelection"),
                  uiOutput("t2thirdSelection")
                ),
                box(title = "Box Plot",
                    plotOutput("bPlot"),plotOutput("hPlot"),
                    verbatimTextOutput("Result") 
                ),
                box(title = "Dataset Table",
                    tableOutput("tablePlot")
                )
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
    subset(sdgsMap, sdgsMap$Goal == input$t2goal & sdgsMap$Indicator == input$t2indicator 
           & sdgsMap$GeoArea == input$t2geoarea)
    
  })
  ######################################################## 1st tab server side ##########################################
  #value boxes
  output$populationBox <- renderValueBox({
    pop<-sum(sdgsIndex$Population_in_2019)
    pop<-prettyNum(pop,big.mark=",",scientific=FALSE)
    valueBox(
      pop, "Population in 2019", icon = icon("users"),
      color = "purple"
    )
  })
  
  output$povertyBox <- renderValueBox({
    pov <- filter(sdgsMap, Indicator %in% "Employed population below international poverty line, by sex and age (%)" &
                    GeoArea %in%  "World" & Period %in% "2018")
    pov <- pov$Value
    valueBox(
      value = paste0(pov,"%"), 
      subtitle = "Population in Poverty", 
      #subtitle = tags$p("Population in Poverty",style = "font-size: 150%;"), 
      icon = icon("thumbs-up", lib = "glyphicon"),
      color = "red"
    )
  })
  
  #country maps
  output$indexPlot <- renderGirafe({
    m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
      geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                   tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Global_Score)),color = "white")+
      scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
      scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
      scale_x_continuous(breaks = c()) +
      my_theme()
    girafe(ggobj = m, width = 15,height=9)
  })
  
  ######################################################## 2nd tab server side ##########################################
  #country profile
  output$countryProfile <- renderUser({
    lower <- tolower(input$country)
    lower<- str_replace_all(lower, " ","-")
    path <- "http://109.255.170.99:8787/files/D.A_Project/country_icons/"
    logo <- paste(path,lower,".svg",sep = "")
    pop<- sdgsIndex$Population_in_2019[ sdgsIndex$Country == input$country]
    ggp<- sdgsIndex$GDP_per_capita_in_2017[ sdgsIndex$Country == input$country]
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
  output$countryPlot <- renderGirafe({
    
    countryMap <- map_data("world", region = input$country)
    countryMap<- merge(countryMap,sdgsIndex1, sort = FALSE, by = "region")
    c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Global_Score,
                                                                   tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Global_Score)))+
      scale_fill_gradientn(colours = "skyblue2", na.value = 'white') + 
      
      my_theme()+
      ggtitle(input$country)
    girafe(ggobj = c)
  })
  
  
  ######################################################## 3rd tab server side ##########################################
  # Indicators world map
  output$distPlot <- renderGirafe({
    ggiraph(code = print(worldMaps(sdgsMap, world_data, input$goal, input$period, input$indicator)))
    
  })
  
  # Change the choices for the second selection on the basis of the input to the first selection
  output$secondSelection <- renderUI({
    choice_second <- as.list(unique(sdgsMap$Indicator[which(sdgsMap$Goal == input$goal)]))
    selectInput(inputId = "indicator", choices = choice_second,
                label = "Choose the indicator for which you want to see the data:")
  })
  
  # Change the choices for the third selection on the basis of the input to the first and second selections
  output$thirdSelection <- renderUI({
    choice_third <- as.list(unique(sdgsMap$Period[sdgsMap$Goal == input$goal & sdgsMap$Indicator == input$indicator]))
    selectInput(inputId = "period", choices = choice_third,
                label = paste0("Choose the type of  you want to explore:"))
  })    
  
  ######################################################## 4th tab server side ##########################################
  #table plot
  output$tablePlot <- renderTable({
    subsetInputs()
  })
  
  #box Plot
  output$bPlot <- renderPlot({
    bx <- subsetInputs()
    bx$GeoArea <- as.factor(bx$GeoArea)
    ggplot(bx, aes(x=GeoArea, y=Value)) + 
      geom_boxplot()
    #hist(bx$Value)
    #boxplot(bx$Value)
    
    
  })
  
  #hist plot
  output$hPlot <- renderPlot({
    bx <- subsetInputs()
    hist(bx$Value)
    
  })
  
  #Shapiro-Wilk normality test
  output$Result <- renderPrint({
    bx <- subsetInputs()
    shapiro.test(bx$Value)
  })
  
  # Change the choices for the second selection on the basis of the input to the first selection
  output$t2secondSelection <- renderUI({
    t2choice_second <- as.list(unique(sdgsMap$Indicator[which(sdgsMap$Goal == input$t2goal)]))
    selectInput(inputId = "t2indicator", choices = t2choice_second,
                label = "Choose the indicator for which you want to see the data:")
  })
  
  # Change the choices for the third selection on the basis of the input to the first and second selections
  output$t2thirdSelection <- renderUI({
    t2choice_third <- as.list(unique(sdgsMap$GeoArea[sdgsMap$Goal == input$t2goal & sdgsMap$Indicator == input$t2indicator]))
    selectInput(inputId = "t2geoarea", choices = t2choice_third,
                label = paste0("Choose the type of  you want to explore:"))
  })
}

shinyApp(ui = ui, server = server)
