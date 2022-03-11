# Final Project
# Shiny Gallery
library(tidyverse)
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(rgdal)
library(stargazer)

# sch['var.interest'] <- sch[,'Attendance']

# place <- sch %>% 
#   filter(School_level=='Above.Middle.School') %>%
#   group_by(SCHOOL_ZIP) %>%
#   summarise(avg.map = mean(var.interest))

df = sch[, c('Attendance', 'Low_income_family', 'Drugs', 'School_level')]
df <- df %>%
  pivot_longer(cols=c('Low_income_family', 'Drugs'), 
               names_to = 'var',
               values_to = 'value')

setwd('G:/My Drive/Courses/At Penn/DATA401_AdvancedTopicsInDataAnalytics/Final project')
lea <- read.csv('LEA_data.csv')
sch <- read.csv('Philly_schools.csv')
sch['School_level'] <- 'Elementry School'
sch[sch$SCHOOL_LEVEL_NAME != 'ELEMENTARY SCHOOL', 'School_level'] <- 'Middle School or above'

place <- sch %>%
  filter(School_level %in% c('Elementry School', 'Middle School or above')) %>%
  group_by(SCHOOL_ZIP) %>%
  summarise(mean.att = mean(Attendance))
  
county <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly", encoding = "UTF-8")

county@data <- data.frame(county@data, place[match(county@data$CODE, place$SCHOOL_ZIP),])
# NOTE: here using merging function would cause a little bit trouble

county@data['Attendence'] = round(county@data$mean.att,2)
county@data['AttendenceLevel'] = 0
county@data[county@data$Attendence>92.32 & county@data$Attendence<=93.66 & !is.na(county@data$Attendence), 'AttendenceLevel'] = 1
county@data[county@data$Attendence>93.66 & county@data$Attendence<=94.82 & !is.na(county@data$Attendence), 'AttendenceLevel'] = 2
county@data[county@data$Attendence>94.82 & !is.na(county@data$Attendence), 'AttendenceLevel'] = 3
county@data[is.na(county@data$Attendence), 'AttendenceLevel'] = NA
# create the popup text
county_popup <- paste0("<strong>Zip Code: </strong>", 
                       county@data$CODE, 
                       "<br>",
                       "<br><strong>Attendence: </strong>", 
                       paste0(county@data$Attendence, '%'))
# 
# "<br><strong>Poverty: </strong>", 
# paste0(county@data$Poverty, '%'),
# "<br><strong>Education: </strong>", 
# paste0(county@data$Education, '%'),
# "<br><strong>Unemployment: </strong>", 
# paste0(county@data$Unemployment, '%'),
# "<br><strong>Crime: </strong>", 
# paste0(county@data$Crime, '%'),
# "<br><strong>ACEs: </strong>", 
# paste0(county@data$ACEs, '%')

# Choosing a color scheme
# https://html-color-codes.info/colors-from-image/
factpal <- colorFactor(c("#F9DC9C", "#F09F5D", "#DF5E65", "#C73642"),
                       county@data[,'AttendenceLevel'])

# Here's the map!
# map <- 
leaflet(county) %>% fitBounds(-124, 34, -62, 40) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(stroke = T,
              smoothFactor = 0.2, 
              fillOpacity = 1,
              color = ~factpal(county@data[,'AttendenceLevel']),
              weight = 1, 
              popup = county_popup) %>%
  addPolylines(color = 'grey', weight = 0.5) %>%
  addLegend("topright", 
            colors =c("#F9DC9C", "#F09F5D", "#DF5E65", "#C73642"),
            labels= c("1st Quantile", "2nd Quantile", "3rd Quantile", "4th Quantile"),  
            title= "Attendence: Lowest to Highest",
            opacity = 1) 

### make the plot of x vs. y
ggplot(data = df) +
      geom_point(aes(value, Attendance, color = School_level)) +
      scale_color_manual(values = c("Red","Blue")) +
      facet_wrap(~var,scales = "free_x") +
      theme_bw()+
      theme(axis.title.x=element_blank(), # remove X label
            axis.title.y=element_text(size=23),
            strip.text = element_text(size = 20), # Facet Panel label size
            # change x tick value size
            axis.text.x = element_text(size=20),
            axis.text.y = element_text(size=20),
            legend.position = "none")



### regression model
model = lm(Attendance~Low_income_family+School_level+Drugs+Assaults+Weapons+Thefts+
             Teacher_attendance+Special_education+Gifted_education, data=sch)
# outcome can be suspension, withdraw
model = lm(Withdrawals~Low_income_family+School_level+Drugs+Assaults+Weapons+Thefts+
             Teacher_attendance+Special_education+Gifted_education, data=sch)
# model = lm(Attendance~SCHOOL_LEVEL_NAME+Drugs+Assaults+Weapons+Thefts+
#              Teacher_attendance) 
# model = lm(One_suspension~Low_income_family+SCHOOL_LEVEL_NAME+Drugs+Assaults+Weapons+Thefts) 

# White+Asian+Latino+African_American+Other+Pacific_Islander
summary(model)
plot(Low_income_family, One_suspension)
plot(Low_income_family, Attendance)
