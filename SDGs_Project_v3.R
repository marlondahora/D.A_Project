# Libraries used

library(readr)
library(magrittr)
library(rvest)
library(reshape2)
library(tidyverse)
library(maps)
library(ggplot2)
library(RColorBrewer)

setwd("~/D.A_Project")

##Ranking# /
### https://unstats.un.org/SDGAPI/v1/sdg/Goal/DataCSV
### downloading the dataset from year 2000 - present
# Note to download that there are different ways to dowlonload the SDGs dataset. It was praticcal to donwload the entire collection of the SDGs and refine it.
# 
#sdgRaw <- read.csv("https://unstats.un.org/SDGAPI//staging/20200127152310260_x16117492@student.ncirl.ie_data.csv", 
#                   stringsAsFactors = FALSE, 
#                   sep = ',')
#
#write.csv(sdgRaw, file = "sdgRaw.csv")

url <- "https://www.nationsonline.org/oneworld/country_code_list.htm"
iso_codes <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[@id="CountryCode"]') %>%
  html_table()
iso_codes <- iso_codes[[1]][, -1]
iso_codes <- iso_codes[!apply(iso_codes, 1, function(x){all(x == x[1])}), ]
names(iso_codes) <- c("Country", "ISO2", "ISO3", "UN")
head(iso_codes)


### the file sdgRaw was split and modified in excel (the file execed the row supported in Excel)
sdg1_10 <- read.csv(file = "sdg1_10.csv", stringsAsFactors = FALSE, na = '0', sep = ',' )
sdg11_17 <- read.csv(file = "sdg11_17.csv",stringsAsFactors = FALSE, na = '0', sep = ',' )
sdgs <- rbind(sdg1_10, sdg11_17)
names(sdgs)[4] <- "GeoArea"

### load the World Map from the ggplot library
worldMap <- ggplot2::map_data('world')
worldMap <- fortify(worldMap)
head(worldMap)

### Index data was download from https://sdgindex.org/reports/sustainable-development-report-2019/ 
sdgsIndex <- read_csv("2019GlobalIndexResults.csv",col_names = TRUE, col_types =
                        cols(
                          .default = col_double(),
                          Country = col_character(),
                          id = col_character()
                        ))

###Now that all the main dataset are in the global environment it is necessary to mach and replace 
### the countries and regions names of all the main dataset using a for loop. This technique was adapted
### from https://www.r-bloggers.com/building-interactive-world-maps-in-shiny/ it was also learning tool for using the for loop
##### Changing countries and regions names ####

index_old_names <- c("Venezuela, RB","St. Vincent and the Grenadines","United States", "Slovak Republic", "St. Lucia","St. Kitts and Nevis","Kyrgyz Republic","Iran, Islamic Rep.", "Gambia, The", "Micronesia, Fed. Sts.", "Egypt, Arab Rep.", "Cote d'Ivoire","Bahamas, The", "Bolivia (Plurinational State of)", "Cabo Verde", "China, Hong Kong Special Administrative Region","China, Macao Special Administrative Region", "Congo, Rep.", "Korea, Dem. Rep.","Congo, Dem. Rep.", "Iran (Islamic Republic of)","Lao People's Democratic Republic","Micronesia (Federated States of)", "Korea, Rep.", "Republic of Moldova","Saint Vincent and the Grenadines","State of Palestine", "Syrian Arab Republic", "The former Yugoslav Republic of Macedonia","United Kingdom of Great Britain and Northern Ireland", "Tanzania", "United States Virgin Islands", "Venezuela (Bolivarian Republic of)")

index_new_names <- c("Venezuela (Bolivarian Republic of)", "Saint Vincent and Grenadines", "United States of America","Slovakia", "Saint Lucia","Saint Kitts and Nevis","Kyrgyzstan","Iran, Islamic Republic of","Gambia","Micronesia","Egypt","Côte d'Ivoire", "Bahamas","Bolivia", "Cape Verde", "Hong Kong, SAR China", "Macao, SAR China", "Congo (Brazzaville)", "Korea (North)", "Congo, (Kinshasa)","Iran, Islamic Republic of", "Lao PDR", "Micronesia, Federated States of", "Korea (South)", "Moldova", "Saint Vincent and Grenadines","Palestinian Territory", "Syrian Arab Republic (Syria)", "Macedonia, Republic of", "United Kingdom", "Tanzania, United Republic of","Virgin Islands, US", "Venezuela (Bolivarian Republic)")

for (i in 1:length(index_old_names)){
  sdgsIndex$Country[sdgsIndex$Country == index_old_names[i]] <- index_new_names[i]
}

map_old_names <- c("Wallis and Futuna",	"Reunion","French Southern and Antarctic Lands", "Antigua", "Barbuda", "Saint Barthelemy", "Brunei", "Ivory Coast", "Democratic Republic of the Congo", "Republic of Congo", "Falkland Islands", "Micronesia", "UK","Heard Island", "Cocos Islands", "Iran", "Nevis", "Saint Kitts", "South Korea", "Laos", "Saint Martin",  "Macedonia", "Pitcairn Islands", "North Korea", "Palestine", "Russia", "South Sandwich Islands", "South Georgia", "Syria", "Trinidad", "Tobago", "Taiwan", "Tanzania", "USA", "Vatican", "Grenadines",  "Saint Vincent", "Venezuela", "Vietnam", "Wallis and Fortuna")

map_new_names <- c("Wallis and Futuna Islands", "Réunion","French Southern Territories", rep("Antigua and Barbuda", 2), "Saint-Barthélemy", "Brunei Darussalam", "Côte d'Ivoire", "Congo, (Kinshasa)", "Congo (Brazzaville)", "Falkland Islands (Malvinas)", "Micronesia, Federated States of", "United Kingdom","Heard and Mcdonald Islands", "Cocos (Keeling) Islands", "Iran, Islamic Republic of", rep("Saint Kitts and Nevis", 2), "Korea (South)", "Lao PDR", "Saint-Martin (French part)",  "Macedonia, Republic of", "Pitcairn", "Korea (North)", "Palestinian Territory", "Russian Federation", rep("South Georgia and the South Sandwich Islands", 2), "Syrian Arab Republic (Syria)", rep("Trinidad and Tobago", 2), "Taiwan, Republic of China", "Tanzania, United Republic of", "United States of America", "Holy See (Vatican City State)", rep("Saint Vincent and Grenadines", 2), "Venezuela (Bolivarian Republic)", "Viet Nam", "Wallis and Futuna Islands")

for (i in 1:length(map_old_names)){
  worldMap$region[worldMap$region == map_old_names[i]] <- map_new_names[i]
}

sdgs_old_names <- c("Heard Island and McDonald Islands","Åland Islands","Lao People's Democratic Republic", "Republic of Moldova", "Czechia", "Bolivia (Plurinational State of)", "Cabo Verde", "China, Hong Kong Special Administrative Region",  "China, Macao Special Administrative Region", "Congo", "Democratic People's Republic of Korea", "Democratic Republic of the Congo", "Iran (Islamic Republic of)", "Lao People's Democratic Republic", "Micronesia (Federated States of)", "Republic of Korea", "Republic of Moldova", "Saint Vincent and the Grenadines", "State of Palestine", "Syrian Arab Republic", "The former Yugoslav Republic of Macedonia", "United Kingdom of Great Britain and Northern Ireland", "United Republic of Tanzania", "United States Virgin Islands", "Venezuela (Bolivarian Republic of)")

sdgs_new_names <- c("Heard and Mcdonald Islands","Aland Islands","Lao PDR", "Moldova","Czech Republic","Bolivia", "Cape Verde", "Hong Kong, SAR China", "Macao, SAR China", "Congo (Brazzaville)", "Korea (North)", "Congo, (Kinshasa)", "Iran, Islamic Republic of", "Lao PDR", "Micronesia, Federated States of", "Korea (South)", "Moldova", "Saint Vincent and Grenadines", "Palestinian Territory", "Syrian Arab Republic (Syria)", "Macedonia, Republic of", "United Kingdom", "Tanzania, United Republic of", "Virgin Islands, US", "Venezuela (Bolivarian Republic)")

for (i in 1:length(sdgs_old_names)){
  sdgs$GeoArea[sdgs$GeoArea == sdgs_old_names[i]] <- sdgs_new_names[i]
}

###cleaning transforming and saving the datasets ####
rm(sdg1_10,sdg11_17,index_new_names,index_old_names,map_new_names,map_old_names,sdgs_new_names,sdgs_old_names,url,i)

worldMap["UN"] <- iso_codes$UN[match(worldMap$region, iso_codes$Country)]
worldMap["ISO3"] <- iso_codes$ISO3[match(worldMap$region, iso_codes$Country)]

sdgs <- as.data.frame(sdgs [, c(-1,-2)])
sdgs <- as.data.frame(sdgs [, c(1,2,5,7,8,9,17,18,32)])

# rename columns 
names(sdgs)[1] <- "Goal"
names(sdgs)[3] <- "Indicator"
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

# reloading
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
#load("sdgs.RData")

##
###Bulding the Map objects ####
sdgsMap <- sdgs
round(sdgsMap$Value, digits=3)
sdgsMap<- filter(sdgsMap, Units %in% "PERCENT") 
#sdgsInd<- filter(sdgs, Units %in% "INDEX" & Period ==2019) 
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

#renaming the Goal rows.
sdgsMap$Goal<-paste("Goal",sdgsMap$Goal)
sdgsMap <- sdgsMap[, c(-2)]
sdgsMap <- as.data.frame(sdgsMap)
sdgsMap[] <- lapply(sdgsMap, as.character)
sdgsMap$Value <- as.numeric(sdgsMap$Value)
#sdgsMap$Period <- as.numeric(sdgsMap$Period)

############# adding a region column in the worldMap dataset based on the SDGS regions ######
# regions names if is needed 
#sdgsRegionsNames<- c("Central Asia","Western Asia","Northern Africa","Sub-Saharan Africa","South-Eastern Asia","Southern Asia","Eastern Europe","Western Europe",
#                     "Southern Europe","Northern Europe","Polynesia","Eastern Asia","Northern America","Micronesia","Latin America and the Caribbean","Melanesia")

worldMap["SDGsRegions"] <- c("")

####### Central Asia ###
Central_Asia <- c("Kazakhstan","Kyrgyzstan","Tajikistan","Turkmenistan","Uzbekistan")

for (i in 1:nrow(worldMap)){
  for(ix in (Central_Asia)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Central Asia")
      #print(paste(worldMap[i,5],ix)) 
    }
  }
}
####### Eastern Asia ###
Eastern_Asia <- c("Armenia","Azerbaijan","Bahrain","Cyprus","Georgia","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Qatar","Saudi Arabia","Palestinian Territory","Syrian Arab Republic (Syria)","Turkey","United Arab Emirates","Yemen")

for (i in 1:nrow(worldMap)){
  for(ix in (Eastern_Asia)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Eastern Asia")
      #print(paste(worldMap[i,5],ix)) 
    }
  }
}

####### Western Asia ###
Western_Asia <- c("Armenia","Azerbaijan","Bahrain","Cyprus","Georgia","Iraq","Israel","Jordan","Kuwait","Lebanon","Oman","Qatar","Saudi Arabia","Palestinian Territory","Syrian Arab Republic","Turkey","United Arab Emirates","Yemen")
for (i in 1:nrow(worldMap)){
  for(ix in (Western_Asia)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Western Asia")
    }
  }
}

####### Southern Asia ###
Southern_Asia <- c("Afghanistan","Bangladesh","Bhutan","India","Iran, Islamic Republic of","Maldives","Nepal","Pakistan","Sri Lanka")
for (i in 1:nrow(worldMap)){
  for(ix in (Southern_Asia)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Southern Asia")
    }
  }
}

####### South-Eastern Asia ###
South_Eastern_Asia <- c("Brunei Darussalam","Cambodia","Indonesia","Lao PDR","Malaysia","Myanmar","Philippines","Singapore","Thailand","Timor-Leste","Viet Nam")
for (i in 1:nrow(worldMap)){
  for(ix in (South_Eastern_Asia)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("South-Eastern Asia")
    }
  }
}

####### Sub-Saharan Africa ###
Sub_Saharan_Africa <- c("British Indian Ocean Territory","Burundi","Comoros","Djibouti","Eritrea","Ethiopia","French Southern Territories","Kenya","Madagascar","Malawi","Mauritius","Mayotte","Mozambique","Réunion", "Rwanda", "Seychelles",
                        "Somalia","South Sudan","Uganda","Tanzania, United Republic of","Zimbabwe","Zambia", "Angola","Cameroon","Chad","Central African Republic","Congo, (Kinshasa)","Congo (Brazzaville)","Equatorial Guinea","Gabon","Sao Tome and Principe","Botswana","Eswatini","Lesotho","Namibia","Togo" ,"Liberia","South Africa","Burkina Faso","Cabo Verde","Benin","Gambia","Ghana" ,"Guinea","Guinea-Bissau","Mali","Nigeria","Niger","Saint Helena", "Senegal","Sierra Leone","Mauritania","Côte d'Ivoire")


for (i in 1:nrow(worldMap)){
  for(ix in (Sub_Saharan_Africa)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Sub-Saharan Africa")
      #print(paste(worldMap[i,5],ix)) 
    }
  }
}

####### Northern Africa ###
Northern_Africa <- c("Algeria","Egypt","Libya","Morocco","Sudan","Tunisia","Western Sahara")

for (i in 1:nrow(worldMap)){
  for(ix in (Northern_Africa)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Northern Africa")
    }
  }
}

####### Southern Europe ###
Southern_Europe <- c("Albania","Andorra","Bosnia and Herzegovina","Croatia","Gibraltar","Greece","Holy See (Vatican City State)","Italy","Malta","Montenegro","North Macedonia","Portugal","San Marino","Serbia","Slovenia","Spain")

for (i in 1:nrow(worldMap)){
  for(ix in (Southern_Europe)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Southern Europe")
    }
  }
}

####### Eastern Europe ###
Eastern_Europe <- c("Belarus","Bulgaria","Czech Republic","Hungary","Poland","Moldova","Romania","Russian Federation","Slovakia","Ukraine")
for (i in 1:nrow(worldMap)){
  for(ix in (Eastern_Europe)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Eastern Europe")
    }
  }
}

####### Western Europe ###
Western_Europe <- c("Austria","Belgium","France","Germany","Liechtenstein","Luxembourg","Monaco","Netherlands","Switzerland")
for (i in 1:nrow(worldMap)){
  for(ix in (Western_Europe)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Western Europe")
    }
  }
}
####### Northern Europe ##
Northern_Europe <- c("Aland Islands","Guernsey","Jersey", "Sark", "Denmark","Estonia","Faroe Islands","Finland","Iceland","Ireland","Isle of Man","Latvia","Lithuania","Norway","Svalbard","Jan Mayen","Sweden","United Kingdom")
for (i in 1:nrow(worldMap)){
  for(ix in (Northern_Europe)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Northern Europe")
    }
  }
}
####### Northern America ##
Northern_America <- c("Bermuda","Canada","Greenland","Saint Pierre and Miquelon","United States of America")
for (i in 1:nrow(worldMap)){
  for(ix in (Northern_America)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Northern America")
    }
  }
}

####### Latin America and the Caribbean ##
Latin_America_and_the_Caribbean <- c("Anguilla","Antigua and Barbuda","Aruba","Bahamas","Barbados","Bonaire","British Virgin Islands","Cayman Islands","Cuba","Curaçao","Dominica","Dominican Republic","Grenada","Guadeloupe","Haiti","Jamaica","Martinique","Montserrat","Puerto Rico","Saint Barthélemy","Saint Kitts and Nevis","Saint Lucia","Saint Martin (French Part)","Saint Vincent and the Grenadines","Sint Maarten (Dutch part)","Trinidad and Tobago","Turks and Caicos Islands","United States Virgin Islands","Belize","Costa Rica","El Salvador","Guatemala","Honduras","Mexico","Nicaragua","Panama","Argentina","Bolivia","Bouvet Island","Brazil","Chile","Colombia","Ecuador","Falkland Islands (Malvinas)","French Guiana","Guyana","Paraguay","Peru","South Georgia and the South Sandwich Islands","Suriname","Uruguay","Venezuela (Bolivarian Republic)")
for (i in 1:nrow(worldMap)){
  for(ix in (Latin_America_and_the_Caribbean)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Latin America and the Caribbean" )
    }
  }
}

####### Micronesia ##
Micronesia <- c("Guam","Kiribati","Marshall Islands","Micronesia, Federated States of","Nauru","Northern Mariana Islands","Palau","United States Minor Outlying Islands")
for (i in 1:nrow(worldMap)){
  for(ix in (Micronesia)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Micronesia" )
    }
  }
}

####### Melanesia ###
Melanesia <- c("Fiji","New Caledonia","Papua New Guinea","Solomon Islands","Vanuatu")
for (i in 1:nrow(worldMap)){
  for(ix in (Melanesia)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Melanesia" )
    }
  }
}

####### Polynesia ##
Polynesia <- c("American Samoa","Cook Islands","French Polynesia","Niue","Pitcairn","Samoa","Tokelau","Tonga","Tuvalu", "Wallis and Futuna Islands")
for (i in 1:nrow(worldMap)){
  for(ix in (Polynesia)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Polynesia" )
    }
  }
}
####### Australia and New Zealand ###
Australia_and_New_Zealand <- c("Australia","Christmas Island","Cocos (Keeling) Islands","Heard and Mcdonald Islands", "New Zealand","Norfolk Island")
for (i in 1:nrow(worldMap)){
  for(ix in (Australia_and_New_Zealand)){
    if(worldMap[i,5] == ix){
      worldMap[i,9] <-c("Australia and New Zealand")
    }
  }
}

####### Cleaning up #######
rm(Central_Asia,Eastern_Asia,Western_Asia,Southern_Asia,South_Eastern_Asia,Sub_Saharan_Africa,
   Northern_Africa,Southern_Europe,Eastern_Europe,Western_Europe,Northern_Europe,Northern_America,
   Latin_America_and_the_Caribbean,Micronesia,Melanesia,Polynesia,Australia_and_New_Zealand,i,ix)

### Creating a polar chart ####
polarIndex <- as.data.frame(sdgsIndex[,c(9:25)])
polarIndex<- melt(data = polarIndex, measure.vars = c("Goal_1", "Goal_2","Goal_3", "Goal_4","Goal_5", "Goal_6","Goal_7", "Goal_8",
                                                      "Goal_9", "Goal_10","Goal_11", "Goal_12","Goal_13", "Goal_14","Goal_15",
                                                      "Goal_16","Goal_17"))

polarIndex$value<-replace_na(polarIndex$value, 0)
polarIndex <- polarIndex%>%
  select(variable,value)%>%
  group_by(variable) %>%
  summarise(value = mean(value))

polarIndex$value <- round(polarIndex$value, digits=2)
polarIndex <- as.data.frame(polarIndex)
polarIndex$variable <- factor(polarIndex$variable, labels = c( "Goal 1", "Goal 2","Goal 3", "Goal 4","Goal 5", "Goal 6","Goal 7", "Goal 8",
                                                               "Goal 9", "Goal 10","Goal 11", "Goal 12","Goal 13", "Goal 14","Goal 15",
                                                               "Goal 16","Goal 17" ))

#### Building the maps ####
# Index map
sdgsIndexMap<- sdgsIndex
names(sdgsIndexMap)[1] <- "region"
sdgsIndexMap <- as.data.frame(sdgsIndexMap [, c(1:4,9:25)])
sdgsIndexchoro <- merge(worldMap, sdgsIndexMap,  sort = FALSE, by = "region")
sdgsIndexchoro <- sdgsIndexchoro[order(sdgsIndexchoro$order), ]
str(sdgsIndexchoro)

# SDGs Map
worldMaps <- function(sdgsMap, worldMap, goal, period, indicator){
  
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
  worldMap['Goal'] <- rep(goal, nrow(worldMap))
  worldMap['Period'] <- rep(period, nrow(worldMap))
  worldMap['Indicator'] <- rep(indicator, nrow(worldMap))
  worldMap['Value'] <- plotsdgsMap$Value[match(worldMap$UN, plotsdgsMap$UN)]
  
  
  # Create caption with the data source to show underneath the map
  capt <- paste0("Source: ", ifelse(indicator == "SDGs", "United Nations" , "World Bank"))
  
  # Specify the plot for the world map
  library(RColorBrewer)
  library(ggiraph)
  g <- ggplot() + 
    geom_polygon_interactive(data = subset(worldMap, lat >= -60 & lat <= 90), color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s", ISO3, Value))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
    labs(fill = goal, color = goal, title = NULL, x = NULL, y = NULL, caption = capt) + 
    my_theme()
  
  return(g)
}




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

my_polar_theme <- function(){
  theme(
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
              fluidRow(
                valueBoxOutput("populationBox"),
                valueBoxOutput("povertyBox"),
              ),
              fluidRow(
                #################### buttons##############
                tags$button(
                  id = "wgoal_1",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-01.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_2",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-02.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_3",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-03.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_4",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-04.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_5",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-05.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_6",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-06.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_7",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-07.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_8",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-08.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_9",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-09.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_10",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-10.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_11",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-11.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_12",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-12.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_13",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-13.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_14",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-14.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_15",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-15.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_16",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-16.png",
                           height = "40px")),
                tags$button(
                  id = "wgoal_17",
                  class = "btn action-button",
                  tags$img(src = "http://109.255.170.99:8787/files/D.A_Project/SDG_Icons_2019/E-WEB-Goal-17.png",
                           height = "40px")),
                
              ),
              #################### outputs ##############
              fluidRow(
                column(width = 9,
                       box(h1( textOutput('title'),align = "center"),width = "100%",
                           girafeOutput("indexPlot",width = "100%"),
                       )
                ),
                column(width = 3,
                       box(h1("SDGs Progress",align = "center"),width = "100%",
                           girafeOutput("indexPolarBar")
                       )
                )
              )
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
                       )
                ),
                column(width = 7,
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
                           
                       )
                )
              ),
              fluidRow(
                column(width = 5,
                       box(title = "Country",width = NULL,status = "primary",
                           userOutput("countryProfile")
                       )
                ),
                column(width = 7,
                       
                       box(title = "Country",width = NULL,status = "primary",
                           girafeOutput("countryPlot")
                           
                       ),
                       girafeOutput("linePlot")
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
  output$title <- renderText({"Global Score Progress Of All SDGs"})
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
  #polar bar chart
  output$indexPolarBar <- renderGirafe({
    plotpolarIndex <- ggplot(polarIndex,
                             aes(
                               x = variable,
                               y = value,
                               fill = factor(variable),
                               tooltip = sprintf("%s<br/>%s", polarIndex$variable, polarIndex$value)
                             )) +
      geom_col_interactive(width = 1, color = "white")+
      coord_polar()+
    my_polar_theme()
    girafe(ggobj = plotpolarIndex)
  })
  
  ######################### actions buttons ############
  observeEvent(input$wgoal_1,{
    output$title <- renderText({"Global Index SDG Progress in Goal 1: No Poverty"})
    
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_1)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(1, "Reds"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_2,{
    output$title <- renderText({"Global Index SDG Progress in Goal 2: Zero Hunger"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_2)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(4, "YlOrRd"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_3,{
    output$title <- renderText({"Global Index SDG Progress in Goal 3: Good Health and Well-being"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_3)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(5, "Greens"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_4,{
    output$title <- renderText({"Global Index SDG Progress in Goal 4: Quality Education"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_4)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(5, "Reds"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_5,{
    output$title <- renderText({"Global Index SDG Progress in Goal 5: Gender Equality"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_5)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(5, "PuOr"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_6,{
    output$title <- renderText({"Global Index SDG Progress in Goal 6: Clean Water and Sanitation"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_6)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(5, "Blues"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_7,{
    output$title <- renderText({"Global Index SDG Progress in Goal 7: Affordable and Clean Energy"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_7)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(3, "YlOrRd"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_8,{
    output$title <- renderText({"Global Index SDG Progress in Goal 8: Decent Work and Economic Growth"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_8)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(3, "RdBu"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_9,{
    output$title <- renderText({"Global Index SDG Progress in Goal 9: Industry, Innovation and Infrastructure"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_9)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(1, "PuOr"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_10,{
    output$title <- renderText({"Global Index SDG Progress in Goal 10: Reduced Inequality"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_10)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(3, "PiYG"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_11,{
    output$title <- renderText({"Global Index SDG Progress in Goal 11: Sustainable Cities and Communities"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_11)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(1, "Oranges"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_12,{
    output$title <- renderText({"Global Index SDG Progress in Goal 12:  Responsible Consumption and Production"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_12)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(4, "YlOrBr"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_13,{
    output$title <- renderText({"Global Index SDG Progress in Goal 13: Climate Action"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_13)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(5, "Greens"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_14,{
    output$title <- renderText({"Global Index SDG Progress in Goal 14: Life Below Water"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_14)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(5, "Blues"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  
  observeEvent(input$wgoal_15,{
    output$title <- renderText({"Global Index SDG Progress in Goal 15: Life on Land"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_15)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(4, "Greens"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_16,{
    output$title <- renderText({"Global Index SDG Progress in Goal 16: Peace and Justice Strong Institutions"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_16)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(6, "Blues"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  observeEvent(input$wgoal_17,{
    output$title <- renderText({"Global Index SDG Progress in Goal 17: Partnerships to achieve the Goal"})
    output$indexPlot <- renderGirafe({
      m<-ggplot(sdgsIndexchoro, aes(long, lat)) +
        geom_polygon_interactive(aes(group = group, fill = sdgsIndexchoro$Global_Score,
                                     tooltip = sprintf("%s<br/>%s", sdgsIndexchoro$id, sdgsIndexchoro$Goal_17)),color = "white")+
        scale_fill_gradientn(colours = brewer.pal(7, "Blues"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 100), breaks = c()) + 
        scale_x_continuous(breaks = c()) +
        my_theme()
      girafe(ggobj = m, width = 15,height=9)
      
    })
  })
  ######################################################## 2nd tab server side ##########################################
  #country profile
  output$countryProfile <- renderUser({
    lower <- tolower(input$country)
    lower<- str_replace_all(lower, " ","-")
    path <- "http://109.255.170.99:8787/files/D.A_Project/country_icons/"
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
  output$countryPlot <- renderGirafe({
    
    countryMap <- map_data("world", region = input$country)
    countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
    c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Global_Score,
                                                                   tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Global_Score)))+
      scale_fill_gradientn(colours = "skyblue2", na.value = 'white') + 
      
      my_theme()+
      ggtitle(input$country)
    girafe(ggobj = c)
  })
  
  observeEvent(input$goal_1,{
    output$linePlot <- renderGirafe({
      #tmp<- sdgsMap$GeoArea == input$country
      tmp <- subset(sdgsMap, Indicator == "Employed population below international poverty line, by sex and age (%)" & GeoArea == input$country)
      wp<-ggplot(tmp) +
        aes(x = Period, y = Value, group=1,
            tooltip = sprintf("%s<br/>%s", tmp$Goal, tmp$Value)) +
        geom_point_interactive(size = 2L, colour = "#ef562d") +
        geom_line_interactive(color="blue", size=1)+
        #geom_smooth_interactive(stat = "smooth", method = "loess") +
        geom_text(aes(label = Value, y = Value +0.5), size = 2)+
        theme_minimal()
      
      girafe(ggobj = wp)
    })
    output$countryPlot <- renderGirafe({
      countryMap <- map_data("world", region = input$country)
      countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_1,
                                                                     tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_1)))+
        scale_fill_gradientn(colours = "red", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_2,{
    output$countryPlot <- renderGirafe({
      countryMap <- map_data("world", region = input$country)
      countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_2,
                                                                     tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_2)))+
        scale_fill_gradientn(colours = "orange", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_3,{
    output$countryPlot <- renderGirafe({
      countryMap <- map_data("world", region = input$country)
      countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_3,
                                                                     tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_3)))+
        scale_fill_gradientn(colours = "green3", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_4,{
    output$countryPlot <- renderGirafe({
      countryMap <- map_data("world", region = input$country)
      countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_4,
                                                                     tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_4)))+
        scale_fill_gradientn(colours = "red3", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_5,{
    output$countryPlot <- renderGirafe({
      countryMap <- map_data("world", region = input$country)
      countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_5,
                                                                     tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_5)))+
        scale_fill_gradientn(colours = "red2", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_6,{
    output$countryPlot <- renderGirafe({
      countryMap <- map_data("world", region = input$country)
      countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_6,
                                                                     tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_6)))+
        scale_fill_gradientn(colours = "steelblue2", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_7,{
    output$countryPlot <- renderGirafe({
      countryMap <- map_data("world", region = input$country)
      countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_7,
                                                                     tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_7)))+
        scale_fill_gradientn(colours = "gold2", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
  })
  observeEvent(input$goal_8,{
    output$countryPlot <- renderGirafe({
      countryMap <- map_data("world", region = input$country)
      countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
      c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_8,
                                                                     tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_8)))+
        scale_fill_gradientn(colours = "red4", na.value = 'white') + 
        
        my_theme()+
        ggtitle(input$country)
      girafe(ggobj = c)
      
    })
    observeEvent(input$goal_9,{
      output$countryPlot <- renderGirafe({
        countryMap <- map_data("world", region = input$country)
        countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_9,
                                                                       tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_9)))+
          scale_fill_gradientn(colours = "orangered1", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_10,{
      output$countryPlot <- renderGirafe({
        countryMap <- map_data("world", region = input$country)
        countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_10,
                                                                       tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_10)))+
          scale_fill_gradientn(colours = "deeppink3", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_11,{
      output$countryPlot <- renderGirafe({
        countryMap <- map_data("world", region = input$country)
        countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_11,
                                                                       tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_11)))+
          scale_fill_gradientn(colours = "orange3", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_12,{
      output$countryPlot <- renderGirafe({
        countryMap <- map_data("world", region = input$country)
        countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_12,
                                                                       tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_12)))+
          scale_fill_gradientn(colours = "orange4", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_13,{
      output$countryPlot <- renderGirafe({
        countryMap <- map_data("world", region = input$country)
        countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_13,
                                                                       tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_13)))+
          scale_fill_gradientn(colours = "green4", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_14,{
      output$countryPlot <- renderGirafe({
        countryMap <- map_data("world", region = input$country)
        countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_14,
                                                                       tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_14)))+
          scale_fill_gradientn(colours = "dodgerblue1", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_15,{
      output$countryPlot <- renderGirafe({
        countryMap <- map_data("world", region = input$country)
        countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_15,
                                                                       tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_15)))+
          scale_fill_gradientn(colours = "green2", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_16,{
      output$countryPlot <- renderGirafe({
        countryMap <- map_data("world", region = input$country)
        countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_16,
                                                                       tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_16)))+
          scale_fill_gradientn(colours = "dodgerblue4", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
    observeEvent(input$goal_17,{
      output$countryPlot <- renderGirafe({
        countryMap <- map_data("world", region = input$country)
        countryMap<- merge(countryMap,sdgsIndexMap, sort = FALSE, by = "region")
        c<- ggplot() + geom_polygon_interactive(data = countryMap, aes(x=long, y = lat, group = group,fill = countryMap$Goal_17,
                                                                       tooltip = sprintf("%s<br/>%s", countryMap$id, countryMap$Goal_17)))+
          scale_fill_gradientn(colours = "midnightblue", na.value = 'white') + 
          
          my_theme()+
          ggtitle(input$country)
        girafe(ggobj = c)
        
      })
    })
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
