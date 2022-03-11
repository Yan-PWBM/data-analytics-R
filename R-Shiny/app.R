s
library(shiny)
library(shinythemes)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(rgdal)
library(stargazer)
# install.packages('rsconnect')

sch <- read.csv("Philly_schools.csv")
sch['School_level'] <- 'Elementary School'
sch[sch$SCHOOL_LEVEL_NAME != 'ELEMENTARY SCHOOL', 'School_level'] <- 'Middle School or above'

# This begins the app
############### Server ####################
###########################################
server <- function(input, output) {

  #-------------------------
  # Maps
  #-------------------------
  lat <- 39.8
  lng <- -75.15
    
  # filter data for mapping
  filteredData <- reactive({
    sch['var.interest'] <- sch[,input$map_var]
    place <- sch %>% 
      filter(School_level %in% input$sch_lvl) %>%
      group_by(SCHOOL_ZIP) %>%
      summarise(avg.map = mean(var.interest),
                White = round(mean(White),2),
                Black = round(mean(African_American),2),
                Asian = round(mean(Asian),2),
                Latino = round(mean(Latino),2),
                Other.Race = round(mean(Other),2),
                Pacific.Islander = round(mean(Pacific_Islander),2))
  })

  # output the map
  output$map <- renderLeaflet({
    county <- readOGR("Zipcodes_Poly", layer = "Zipcodes_Poly", encoding = "UTF-8")
    county@data <- data.frame(county@data, filteredData()[match(county@data$CODE, filteredData()$SCHOOL_ZIP),])
    county@data['var.interest'] = round(county@data$avg.map,2)
    
    county@data['Level'] = 0
    quantile25 <- quantile(county@data$avg.map, 0.25, na.rm=T)
    quantile50 <- quantile(county@data$avg.map, 0.5, na.rm=T)
    quantile75 <- quantile(county@data$avg.map, 0.75, na.rm=T)
    county@data[county@data$var.interest>quantile25 & county@data$var.interest<=quantile50 & !is.na(county@data$var.interest), 'Level'] = 1
    county@data[county@data$var.interest>quantile50 & county@data$var.interest<=quantile75 & !is.na(county@data$var.interest), 'Level'] = 2
    county@data[county@data$var.interest>quantile75 & !is.na(county@data$var.interest), 'Level'] = 3
    county@data[is.na(county@data$var.interest), 'Level'] = NA
    # create the popup text
    
    if (input$map_var %in% c('Attendance', 'Low_income_family', 'Teacher_attendance',
                             'Special_education', 'Gifted_education')){
      outcome <- paste0("<br><strong>", input$map_var, ": </strong>", 
                        paste0(county@data$var.interest, '%'))
    }
    else {
      outcome <- paste0("<br><strong>", input$map_var, ": </strong>", 
                        paste0(county@data$var.interest))
    }
    
    county_popup <- paste0("<strong>Zip Code: </strong>", 
                           county@data$CODE, 
                           "<br>",
                           outcome,
                           "<br><strong>White: </strong>", 
                           paste0(county@data$White, '%'),
                           "<br><strong>African American: </strong>", 
                           paste0(county@data$Black, '%'),
                           "<br><strong>Asian: </strong>", 
                           paste0(county@data$Asian, '%'),
                           "<br><strong>Latino: </strong>", 
                           paste0(county@data$Latino, '%'),
                           "<br><strong>Other Race: </strong>", 
                           paste0(county@data$Other.Race, '%'),
                           "<br><strong>Pacific Islander: </strong>", 
                           paste0(county@data$Pacific.Islander, '%')
                           )
    
    # Choosing a color scheme
    # https://html-color-codes.info/colors-from-image/
    factpal <- colorFactor(c("#F9DC9C", "#F09F5D", "#DF5E65", "#C73642"),
                           county@data$Level)
    
    # Here's the map!
    leaflet(county) %>% fitBounds(-124, 34, -62, 40) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(stroke = T,
                  smoothFactor = 0.2, 
                  fillOpacity = 1,
                  color = ~factpal(Level),
                  weight = 1, 
                  popup = county_popup) %>%
      addPolylines(color = 'grey', weight = 0.5) %>%
      addLegend("topright", 
                colors =c("#F9DC9C", "#F09F5D", "#DF5E65", "#C73642"),
                labels= c("1st Quantile", "2nd Quantile", "3rd Quantile", "4th Quantile"),  
                title= "Lowest to Highest",
                opacity = 1)

    })
  observe({ # zoom the map with given lng and lat
    input$reset_button
    new_zoom <- 10.3
    leafletProxy('map') %>%
      setView(lng = lng, lat = lat, zoom = new_zoom)
  })
  
  #-------------------------
  # Graphs
  #-------------------------
  output$plot <- renderPlot({
    df <- sch[, c('School_level', input$xvar, input$yvar)]
    df <- df %>%
      pivot_longer(cols=input$xvar, 
                   names_to = 'var',
                   values_to = 'value')
    df['yinterest'] <- df[input$yvar]
    df[df$var=='Low_income_family', 'var'] = 'Low Income Family'
    df[df$var=='Teacher_attendance', 'var'] = 'Teacher Attendance'
    df[df$var=='Special_education', 'var'] = 'Special Education'
    df[df$var=='Gifted_education', 'var'] = 'Gifted Education'
    
    ggplot(data = df) +
      geom_point(aes(value, yinterest, color = School_level)) +
      scale_color_manual(values = c("red", "blue")) +
      facet_wrap(~var, scales = "free_x") + # have different x-axis
      theme_bw()+
      theme(axis.title.x=element_blank(), # remove X label
            axis.title.y=element_text(size=23),
            strip.text = element_text(size = 20), # Facet Panel label size
            # change x tick value size
            axis.text.x = element_text(size=20),
            axis.text.y = element_text(size=20),
            legend.position = "none") +
      labs(y = input$yvar)
  })
  output$text <- renderText({
    paste0("<f><font size = 4>",
           "<strong>Low Family Income: </strong>", 
           "Percentage of student body that is from low income family",
           "<br><strong>Drugs/Morals/Weapons: </strong>", 
           "Drug/Morals/Weapons infractions per 100 students",
           "<br><strong>Assaults/Thefts: </strong>",  
           "Assaults/Thefts per 100 students",
           "<br><strong>Teacher Attendance: </strong>", 
           "Average percentage of teacher attendance",
           "<br><strong>Special Education: </strong>", 
           "Percentage of student body receiving special education",
           "<br><strong>Gifted Education: </strong>", 
           "Percentage of student body receiving gifted education"
    )
  })

  #-------------------------
  # Regression Tables
  #-------------------------  
  #creating a reactive regression forumula that uses inputs from the check list
  #as independent variables to predict the 3 dependent variables
  regFormula1 <- reactive({
    as.formula(paste('Attendance', " ~ ", paste(input$regxvar, collapse = "+")))
  })
  
  # then, put that formula into the lm() regression
  model1 <- reactive({lm(regFormula1(), sch)})

  regFormula2 <- reactive({
    as.formula(paste('Withdrawals', " ~ ", paste(input$regxvar, collapse = "+")))
  })
  
  # then, put that formula into the lm() regression
  model2 <- reactive({lm(regFormula2(), sch)})
  
  regFormula3 <- reactive({
    as.formula(paste('One_suspension', " ~ ", paste(input$regxvar, collapse = "+")))
  })
  
  # then, put that formula into the lm() regression
  model3 <- reactive({lm(regFormula3(), sch)})
  
  # Creating pretty labels for the stargazer table
  covar.label <- reactive({
    covars<-character()
    if ('School_level' %in% input$regxvar){
      covars <- c(covars,"Middle School or Above (ref:Elementary)")
    }
    if ('Low_income_family' %in% input$regxvar){
      covars <- c(covars,"Low Income Family Rate")
    }  
    if ('Drugs' %in% input$regxvar){
      covars <- c(covars,"Drugs")
    } 
    if ('Morals' %in% input$regxvar){
      covars <- c(covars,"Morals")
    } 
    if ('Weapons' %in% input$regxvar){
      covars <- c(covars,"Weapons")
    }
    if ('Thefts' %in% input$regxvar){
      covars <- c(covars,"Thefts")
    }
    if ('Assaults' %in% input$regxvar){
      covars <- c(covars,"Assaults")
    } 
    if ('Teacher_attendance' %in% input$regxvar){
      covars <- c(covars,"Teacher Attendance Rate")
    } 
    if ('Special_education' %in% input$regxvar){
      covars <- c(covars,"Special Education Rate")
    } 
    if ('Gifted_education' %in% input$regxvar){
      covars <- c(covars,"Gifted Education Rate")
    } 
    return(covars)
  }) # NOTE: here need to include labels of all potential independent variables
  
  #Create nice regression table output
  #stargazer() comes from the stargazer package
  output$regTab <- renderText({
    covars <- covar.label()
    stargazer(model1(),model2(), model3(), type = "html", 
              dep.var.labels = '',
              column.labels = c("Attendance Rate|", "|Withdrawals|","|Unique Suspensions"),
              object.names = F, model.numbers = T,
              covariate.labels = covars, omit.stat = c("f","ser","adj.rsq"))
  })
    
}

############### UI ####################
#######################################
ui <- shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  # This applies preset aesthetic decisions to the entire app
  navbarPage(
    "Philly School App",
    # navbarPage lets us set up a navigation bar to toggle between tabPanels
    
    #-------------------------
    # Welcome Tab
    #-------------------------  
    tabPanel("Welcome", # tabPanel() creates the tabs
             tags$head(
               tags$style("h2 {color: #04B4AE; }
               h1 {color: #04B4AE}; }")), # this is setting the color palette for our tab headers 1 and 2
             headerPanel("About the App"), # I haven't created a title for this page, try adding one!
             br(), #a break creates a space
             h2("How to Use This App"), # the number following h corresponds to size
             h4(tags$ul(
               tags$li("Mapping School Characteristics: Display the differences in school characteristics between different areas"), #bullet point
               tags$li("Explore Philly School Data: Explore the correlation between school outcomes and different characteristics"), #bullet point
               tags$li("Analyzing School Outcomes: Build a regression model and predict the Student Attendance rate, withdrawals and suspension") #bullet point
             )),
             h4("To begin, select \"Mapping School Characteristics\" on the navigation bar. You will be asked to choose 
                the school level and variable of interest, the map will display the average of the variable you are interested
                in by zip codes, based on the data of the school level you choose."),
             h4("The second tab \"Explore Philly School Data\" on the navigation bar will allow you to choose the school 
             outcomes and different school characteristic measurements, once you set your selection, the main panel will 
             present the correlation plots of the school outcome and the selected school characteristics. Each plot shows
             the data for both Elementary school and Middle School/above."),
             h4("In the third tab \"Analyzing School Outcomes\", users can select a series of independent variables from the sidebar on the 
             right to build the regression models. There are 3 OLS models where the dependent variables are Student Attendance
             rate, student withdrawals and unique suspended students per year. The table displays the regression results 
             for 3 models simultaneously."),
             
             h2("The Data"),
             h4("This app uses data from the Philly school data. The data includes all different school characteristics
                such as student race composition, school address. The data also includes some school outcome measurements
                such as student attendance rate, withdrawals and suspension numbers.")),
    #-------------------------
    # Mapping Tab
    #-------------------------  
    tabPanel(
      # First tab
      "Mapping School Characteristics",
      headerPanel("Mapping Philly school characteristics by school level"),
      # Side bar layout
      sidebarLayout(
        # Side panel
        sidebarPanel(width = 3,
        # Create the drop down CODE3
        actionButton("reset_button", "Reset view"), # one-click to zoom the map
        helpText("Click reset view button to zoom the map centering at Philly"),
        HTML('</br>'),
        selectInput("map_var",
                    "Variable of Interest:",
                    c('Student Attendance (%)' = 'Attendance', 
                      'Student Enrollment (#)' = 'Enrollment',
                      'New students/year'='New_student',
                      'Student Withdrawals (#)' = 'Withdrawals',
                      'Low income family (%)' = 'Low_income_family',
                      'Drugs infractions/100 students' = 'Drugs',
                      'Morals infractions/100 students' = 'Morals',
                      'Assaults infractions/100 students' = 'Assaults',
                      'Weapons infractions/100 students' = 'Weapons',
                      'Thefts infractions/100 students' = 'Thefts',
                      'Total Suspensions/year' = 'Total_suspensions',
                      'Unique suspended students/year' = 'One_suspension',
                      'Teacher attendance (%)' = 'Teacher_attendance',
                      'Special Education (%)' = 'Special_education',
                      'Gifted Education (%)' = 'Gifted_education',
                      'Average Teacher salary' = 'Average_salary'
                    ), 
                    selected = c('Student Attendance (%)' = 'Attendance')),
        helpText("The map displays the average of the variable of interest within each Zip Code"),
        HTML('</br>'),
        checkboxGroupInput(
          "sch_lvl",
          label = "Select school level",
          c('Elementary School', 'Middle School or above'), # c
          selected = c('Elementary School', 'Middle School or above')
        ),
        helpText("Choose both school levels will get average across all schools")
        ),
        # Main panel
        mainPanel(
          # Create the table.
          leafletOutput("map", width = "100%", height = "800px"))
        )), # map tab
    
    #-------------------------
    # Graphing Tab
    #-------------------------  
    tabPanel(
      # Second tab
      "Explore Philly School Data",
      headerPanel("Correlation Plot"),
      # Side bar layout
      sidebarLayout(position = "left",
        # Side panel
        sidebarPanel(width = 3,
                     selectInput("yvar",
                                 "School Outcome",
                                 c('Student Attendance (%)' = 'Attendance', 
                                   'Student Withdrawals (#)' = 'Withdrawals', 
                                   'Unique suspended students' = 'One_suspension'), 
                                 selected = ('Student Attendance (%)' = 'Attendance')),
                     helpText("Red dots are Elementary school, Blue dots are Middle school or above"),
                     # Create the drop down
                     checkboxGroupInput(
                       "xvar",
                       label = "School Characteristics",
                       c('Low Income Family(%)'='Low_income_family', 
                         'Drug infractions/100 students'='Drugs',
                         'Morals infractions/100 students'='Morals',
                         'Weapons infractions/100 students'='Weapons',
                         'Thefts/100 students'='Thefts',
                         'Assaults/100 students'='Assaults',
                         'Teacher Attendence (%)'='Teacher_attendance',
                         'Special education (%)'='Special_education',
                         'Gifted education (%)'='Gifted_education'), # c
                       selected = c('Low Income Family(%)'='Low_income_family',
                                    'Weapons infractions/100 students'='Weapons',
                                    'Teacher Attendence (%)'='Teacher_attendance',
                                    'Special education (%)'='Special_education')
                       # default selection
                     )
                     ),
        # Main panel
          mainPanel(
            # Create the table.
            plotOutput("plot"),
            htmlOutput('text')
      ))
    ), # Graph Tab
    
    #-------------------------
    # Regression Tab
    #-------------------------  
    tabPanel("Analyzing School Outcomes",
             tags$head(tags$style("h2 {color: #ee5500; }
                                             h1 {color: #04B4AE}; }")),
             headerPanel("School Outcome Prediction Model"), # TITLE
             #The sidebar allows me to select multiple independent variables
             sidebarLayout(
               position = "right",
               sidebarPanel(
                 h2("Customize your model"), #TITLE
                 br(), 
                 checkboxGroupInput(
                   "regxvar",
                   label = "Select the independent variables below to estimate your model. You can change your selection at any time.",
                   c('School Level' = 'School_level',
                     'Low Income Family(%)'='Low_income_family', 
                     'Drug infractions/100 students'='Drugs',
                     'Morals infractions/100 students'='Morals',
                     'Weapons infractions/100 students'='Weapons',
                     'Thefts/100 students'='Thefts',
                     'Assaults/100 students'='Assaults',
                     'Teacher Attendence (%)'='Teacher_attendance',
                     'Special education (%)'='Special_education',
                     'Gifted education (%)'='Gifted_education'), # c
                   selected = c('School Level' = 'School_level',
                                'Low Income Family(%)'='Low_income_family',
                                'Thefts/100 students'='Thefts',
                                'Teacher Attendence (%)'='Teacher_attendance',
                                'Special education (%)'='Special_education',
                                'Gifted education (%)'='Gifted_education')
                   )# checkboxGroupInput
                 ), 
               mainPanel(br(),
                         #create a 1st tab panel
                         tabsetPanel(
                           type = "tabs",
                           #first panel shows regression table
                           tabPanel("Regression Table",
                                    h3("Table of Regression Coefficients"),
                                    HTML('</br>'),
                                    tableOutput("regTab"),
                                    HTML('</br>'),
                                    helpText("The dependent variables in model (1), (2) and (3) are Student
                                    Attendance, Student Withdrawals, and unique suspended students per year."),
                                    helpText("The table displays the coefficients of the models and the standard error of the 
                  coeffcients in the (). Larger numbers (absolute) indicate that a variable has a greater effect. 
                  A positive number indicates that a variable increases the outcome measurement while a negative indicates 
                  that a variable decreases the outcome measurement. The P value determines statistical 
                  significance of the effect. P values below 0.05 are commonly accepted as significant, 
                  P values between 0.05 and 0.1 are considered marginally significant.")
                           )# tab panel
                         )# tabset
               )# mainpanel
             ) #sidebarlayout
    ) # regression
  )
))
shinyApp(ui = ui, server = server)

