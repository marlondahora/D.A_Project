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
###Creating R objects ####
#sdgsMap object#
sdgsMap <- sdgs
round(sdgsMap$Value, digits=3)
sdgsMap<- filter(sdgsMap, Units %in% "PERCENT") 

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


#sdgsIndexMap<- sdgsIndex
#names(sdgsIndexMap)[1] <- "region"
#sdgsIndexMap <- as.data.frame(sdgsIndexMap [, c(1:4,6,9:25)])
# Index map

cli <- subset(sdgs, Indicator == "Number of deaths due to disaster (number)" & Period == "2018")
save(cli, file = "cli.RData")


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

#tap1
save(sdgs, file="daproject/sdgs.RData")
save(cli, file = "daproject/cli.RData")

### Creating index polar chart ####
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


#ggplot them functions

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

