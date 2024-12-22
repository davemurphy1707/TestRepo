#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


my_theme = create_theme(
  adminlte_color(
    light_blue = "#4898a8"
  )
)

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

library(shiny)
library(shinydashboard)
library(bslib)
library(fresh)
library(ggmosaic)

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
  dashboardBody(
    use_theme(my_theme),
    tabBox(
      width = 12, id = "tabs",
           tabPanel("Scatter Plot",
                    fluidRow(
                      box(
                      width = 12, 
                      title = "Scatter Plot",
                      collapsible = TRUE, status = "warning", 
                      solidHeader = TRUE,  
                      plotOutput("plot1")
                      )
                      )),
          tabPanel("Data",
                   fluidRow(box(
                    width = 12, 
                    title = "Table",
                    collapsible = TRUE, 
                    status = "warning", 
                    solidHeader = TRUE,     
                    tableOutput("table1")))
   
    ),
    tabPanel("Plot 2", plotOutput("plot2"))
  )
  )
  )



server <- function(input, output) {
  filtered_data <- reactive({
    filtered_data <- dig.df %>% 
      filter(SEX == input$Sex) %>% 
      filter(TRTMT == input$Treatment) %>% 
      filter(WHF == input$WHF) %>% 
      filter(HYPERTEN == input$Hypertension) %>% 
      filter(AGE >= input$Age[1] & AGE <= input$Age[2]) %>%
      filter(KLEVEL >= input$Klevel[1] & KLEVEL <= input$Klevel[2]) %>%
      filter(BMI >= input$BMI[1] & BMI <= input$BMI[2]) })
  
  output$plot1 <- renderPlot({ filtered_data() %>%
      ggplot(aes(x = AGE, y = HOSPDAYS, color = SEX)) +
      geom_point(alpha = 0.6, size = 3) +
      labs(
        title = "Scatter Plot of Age and HOSPDAYS by Sex",
        x = "Age",
        y = "Hospital Days",
        color = "SEX"
      ) +
      theme_minimal()
  })
  
  output$table1 <- renderTable({
    filtered_data()
  })
  
  output$plot2 <- renderPlotly({
    # Create the ggplot object
    mor_int <- ggplot(filtered_data(), aes(x = MONTH, y = mortalityrate, color = TRTMT, group = TRTMT)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        x = "Time (Months)",
        y = "Mortality Rate",
        title = "Mortality Rate Over Time by Treatment Group",
        color = "Treatment Group"
      ) +
      theme_minimal()
    
    # Apply ggplotly to make it interactive
    ggplotly(mor_int)  # Correctly use ggplotly here
  })
}

shinyApp(ui = ui, server = server)


