#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

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

## Insert your code here
dig.df$TRTMT <- recode_factor(dig.df$TRTMT, "0" = "Placebo", "1" = "Treatment")
dig.df$SEX <- recode_factor(dig.df$SEX, "1" = "Male", "2" = "Female")
dig.df$HYPERTEN <- recode_factor(dig.df$HYPERTEN, "0" = "Has_not_had", "1" = "Has_had")
dig.df$CVD <- recode_factor(dig.df$CVD,"0" = "Does not have cardiovascular disease", 
                            "1" =  "Has cardiovasular disease")
dig.df$WHF <- recode_factor(dig.df$WHF, "0" = "Negative", "1" = "Positive")
dig.df$DIG <- recode_factor(dig.df$DIG, "0" = "Not Toxic", "1" = "Toxic")
dig.df$HOSP <- recode_factor(dig.df$HOSP, "0" = "Not Hospitalised", "1" = "Hospitalised")
dig.df$DEATH <- recode_factor(dig.df$DEATH, "0" = "Alive", "1" = "Dead")

library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "DIG Scatter Plot"),
  dashboardSidebar(
      selectInput(
        inputId = "Sex",
        label = "Select the Sex:",
        choices = levels(dig.df$SEX) # Dynamically load SEX levels from the dataset
      ),
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
      plotOutput("distPlot") # Placeholder for the scatter plot
    )
  )


server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    dig.df %>% 
      filter(SEX == input$Sex) %>% 
      filter(TRTMT == input$Treatment) %>% 
      filter(WHF == input$WHF) %>% 
      filter(HYPERTEN == input$Hypertension) %>% 
      filter(AGE >= input$Age[1] & AGE <= input$Age[2]) %>%
      filter(BMI >= input$BMI[1] & BMI <= input$BMI[2]) %>%
      ggplot(aes(x = AGE, y = HOSPDAYS, color = "Sex")) +
      geom_point(alpha = 0.6, size = 3) +
      labs(
        title = "Scatter Plot of Age and HOSPDAYS by Sex",
        x = "Age",
        y = "Hospital Days",
        color = "Sex"
      ) +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)


