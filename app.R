#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(plotly)
library(shinydashboard)
library(bslib)
library(fresh)
library(ggmosaic)
library(readr)
library(dplyr)

#create a custom colour for the theme
my_theme = create_theme(
  adminlte_color(
    light_blue = "#4898a8"
  )
)

#read in teh data and label appropriately
DIG.df <- read_csv("DIG.csv", show_col_types = FALSE)
head(DIG.df)
dig.df <- DIG.df %>% select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP, HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)
dig.df <- dig.df %>% mutate(ID = as.factor(ID), 
                            TRTMT = as.factor(TRTMT),
                            AGE = as.numeric(AGE),
                            SEX = as.factor(SEX),
                            BMI = as.numeric(BMI),
                            KLEVEL = as.numeric(KLEVEL),
                            CREAT = as.numeric(CREAT),
                            DIABP = as.numeric(DIABP),
                            SYSBP = as.numeric(SYSBP), 
                            HYPERTEN = as.factor(HYPERTEN),
                            CVD = as.factor(CVD),
                            WHF = as.factor(WHF),
                            DIG = as.factor(DIG),
                            HOSP = as.factor(HOSP),
                            HOSPDAYS = as.numeric(HOSPDAYS),
                            DEATH = as.factor(DEATH),
                            DEATHDAY = as.numeric(DEATHDAY)) %>%
  filter(!is.na(HYPERTEN)) %>%
  filter(!is.na(KLEVEL))#filter out na values of hyperten for Q 11

dig.df <- mutate(dig.df, MONTH = round(DEATHDAY/30))

## Insert your code here
dig.df$TRTMT <- recode_factor(dig.df$TRTMT, "0" = "Placebo", "1" = "Treatment")
dig.df$SEX <- recode_factor(dig.df$SEX, "1" = "Male", "2" = "Female")
dig.df$HYPERTEN <- recode_factor(dig.df$HYPERTEN, "0" = "Has not had", "1" = "Has had")
dig.df$CVD <- recode_factor(dig.df$CVD,"0" = "Does not have cardiovascular disease", 
                            "1" =  "Has cardiovasular disease")
dig.df$WHF <- recode_factor(dig.df$WHF, "0" = "Negative", "1" = "Positive")
dig.df$DIG <- recode_factor(dig.df$DIG, "0" = "Not Toxic", "1" = "Toxic")
dig.df$HOSP <- recode_factor(dig.df$HOSP, "0" = "Not Hospitalised", "1" = "Hospitalised")
dig.df$DEATH <- recode_factor(dig.df$DEATH, "0" = "Alive", "1" = "Dead")


#the user interface component that allows the user to alter the variables
#in the control panel
ui <- dashboardPage(
  dashboardHeader(title = "DIG Scatter Plot"),
  dashboardSidebar(
      sliderInput(
        inputId = "Age",
        label = "Select Age Range",
        min = 0, max = 100, value = c(20,50)
      ),
      sliderInput(
        inputId = "BMI",
        label = "Select BMI Value",
        min = 14, max = 65, value = c(20,50)
      ),
      sliderInput(
        inputId = "Creat",
        label = "Select Creat Level",
        min = 0.05, max = 4, value = c(2,3)
      ),
      sliderInput(
        inputId = "Klevel",
        label = "Select K Level",
        min = 4.0, max = 6.5, value = c(4.3,4.8)
      ),
      selectInput(
        inputId = "Sex",
        label = "Select the Sex:",
        choices = levels(dig.df$SEX) # Dynamically load SEX levels from the dataset
      ),
      selectInput(
        inputId = "Treatment",
        label = "Select the treatment option:",
        choices = levels(dig.df$TRTMT) 
      ),
      selectInput(
        inputId = "Hypertension",
        label = "Does the patient have hypertension? :",
        choices = levels(dig.df$HYPERTEN) 
      ),
      selectInput(
        inputId = "WHF",
        label = "Does this patient have worsening Heart Failure? :",
        choices = levels(dig.df$WHF)
        ) 
      ),
  #create the tabs and ids fro the plots, table and tabs
  dashboardBody(
    use_theme(my_theme),
    tabBox(
      width = 12, id = "tabs",
           tabPanel("Scatter Plot",
                    verbatimTextOutput("myBlockText"),
                    fluidRow(
                      box(
                      width = 12, 
                      title = "Scatter Plot",
                      collapsible = TRUE, status = "warning", 
                      solidHeader = TRUE,  
                      plotlyOutput("plot1")
                      ),   
                      )),
          tabPanel("Data",
                   verbatimTextOutput("myBlockText2"),
                   fluidRow(box(
                    width = 12, 
                    title = "Table",
                    collapsible = TRUE, 
                    status = "warning", 
                    solidHeader = TRUE,     
                    tableOutput("table1")))
   
    ),
    tabPanel("Mosaic Plots",                
             verbatimTextOutput("myBlockText3"),
             fluidRow(
               box(
                 width = 6, 
                 title = "Plot 2",
                 collapsible = TRUE, status = "primary", 
                 solidHeader = TRUE,  
                 plotOutput("plot2")
               ),
               box(
                 width = 6, 
                 title = "Plot 3",
                 collapsible = TRUE, status = "primary", 
                 solidHeader = TRUE,  
                 plotOutput("plot3")
               )
             ),
             fluidRow(
               box(
                 width = 6, 
                 title = "Plot 4",
                 collapsible = TRUE, status = "primary", 
                 solidHeader = TRUE,  
                 plotOutput("plot4")
               ),
                 box(
                   width = 6, 
                   title = "Plot 5",
                   collapsible = TRUE, status = "primary", 
                   solidHeader = TRUE,  
                   plotOutput("plot5")
                 )
             )
  )
    )
  )
)

  
  



server <- function(input, output) {
  #make the data reactive to allow for user input
  filtered_data <- reactive({
    filtered_data <- dig.df %>% 
      filter(SEX == input$Sex) %>% 
      filter(TRTMT == input$Treatment) %>% 
      filter(WHF == input$WHF) %>% 
      filter(HYPERTEN == input$Hypertension) %>% 
      #these are continuous variables
      filter(AGE >= input$Age[1] & AGE <= input$Age[2]) %>%
      filter(KLEVEL >= input$Klevel[1] & KLEVEL <= input$Klevel[2]) %>%
      filter(BMI >= input$BMI[1] & BMI <= input$BMI[2]) })
  
  #block which gives a description of each tab, to be displayed on each 
  #individual tab
  output$myBlockText <- renderPrint({
    cat("This Scatterplot shows the relationship between the number of days spent
        in the hospital and the age of the patient.\n",
        "If a patient is admitted and they are in this study the idea is that they
        can adjust the plot to match their baseline characteristics with other
        participants to give them an idea of how long they will be in the hospital.
        The plot could also be used to segment the study into cohorts easily.") })
    
    output$myBlockText2 <- renderPrint({
      cat("Here is a table of all data points visible on the scatterplot. \n
          This table updates depending on the conditions that you select.") })
      
      output$myBlockText3 <- renderPrint({
        cat("Here is a table of all data points visible on the scatterplot. \n
          This table updates depending on the conditions that you select.")
  })
  
      #renderPlotly for an interactive plot
  output$plot1 <- renderPlotly({
    #save the ggplot as an object
      p <- ggplot(filtered_data(), aes(x = AGE, y = HOSPDAYS)) +
      geom_point(alpha = 0.8, size = 3, colour = "lightblue") +
      labs(
        title = "Scatter Plot of Days Spent in Hospital vs Age of Patient",
        x = "Age",
        y = "Hospital Days",
      ) +
      theme_minimal()
      #turn the ggplot interactive
      ggplotly(p)
      
  })
  
  #display the data so that the user selects who they see
  output$table1 <- renderTable({
    filtered_data()
  })
  
  output$plot2 <- renderPlot({
    ggplot(dig.df) + geom_mosaic(aes(x = product(TRTMT),fill = SEX)) +
      scale_fill_manual(values = c("blue", "hotpink")) + 
      labs(title = "Mosaic Plot of SEX vs TREATMENT",
           x = "TREATMENT",
           y = "SEX")
  })
  output$plot3 <- renderPlot({
  ggplot(dig.df) + geom_mosaic(aes(x = product(TRTMT),fill = CVD)) +
    scale_fill_manual(values = c("red", "yellow")) + 
    labs(title = "Mosaic Plot of CVD vs TREATMENT",
         x = "TREATMENT",
         y = "CVD") +theme_get() })
  
  output$plot4 <- renderPlot({
  ggplot(dig.df) + geom_mosaic(aes(x = product(TRTMT),fill = DEATH)) + 
    scale_fill_manual(values = c("#80cdc1","#8c5")) + 
    labs(title = "Mosaic Plot of DEATH vs TREATMENT",
         x = "Treatment",
         y = "Death Status") +theme_cleveland() })
  
  output$plot5 <- renderPlot({
    ggplot(dig.df) + geom_mosaic(aes(x = product(CVD),fill = DEATH)) +
      theme_grey() +
      scale_fill_manual(values = c("navyblue", "cyan")) + 
      labs(title = "Mosaic Plot of DEATH vs CVD",
           x = "CVD",
           y = "Death Status") +theme_cleveland() })
}

shinyApp(ui = ui, server = server)


