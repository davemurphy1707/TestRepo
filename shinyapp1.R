library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

DIG.df <- read_csv("DIG.csv", show_col_types = FALSE)
DIG <- DIG.df %>% filter(!is.na(AGE) & !is.na(DEATH))

DIG$TRTMT <- factor(DIG$TRTMT, levels = c(0, 1), labels = c("Placebo", "Treatment"))
DIG$SEX <- factor(DIG$SEX, levels = c(1, 2), labels = c("Male", "Female"))
DIG$RACE <- factor(DIG$RACE, levels = c(1, 2), labels = c("White", "Nonwhite"))


ui <- dashboardPage(
  dashboardHeader(title = "DIG Trial Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Summary", tabName = "summary", icon = icon("table")),
      menuItem("Gender Analysis", tabName = "gender", icon = icon("venus-mars")),
      menuItem("Survival Analysis", tabName = "survival", icon = icon("heartbeat")),
      menuItem("Treatment Effect", tabName = "treatment", icon = icon("vials"))
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "summary", 
              h2("Data Summary"),
              verbatimTextOutput("dataSummary")),
      
      
      tabItem(tabName = "gender",
              h2("Gender Distribution"),
              plotOutput("genderBarPlot"),
              plotOutput("ageGenderPlot")),
      
      
      tabItem(tabName = "survival",
              h2("Survival Analysis"),
              plotOutput("survivalScatterPlot"),
              plotOutput("survivalBoxPlot")),
      
      
      tabItem(tabName = "treatment",
              h2("Treatment Effectiveness"),
              plotOutput("effectivenessPlot"),
              dataTableOutput("treatmentTable"))
    )
  )
)


server <- function(input, output) {
  
  
  output$dataSummary <- renderPrint({
    str(DIG)
  })
  
  
  output$genderBarPlot <- renderPlot({
    ggplot(DIG, aes(x = SEX, fill = SEX)) +
      geom_bar() +
      labs(title = "Gender Distribution", x = "Gender", y = "Count") +
      scale_fill_manual(values = c("skyblue", "pink")) +
      theme_minimal()
  })
  
  
  output$ageGenderPlot <- renderPlot({
    ggplot(DIG, aes(x = AGE, fill = SEX)) +
      geom_density(alpha = 0.5) +
      labs(title = "Age Distribution by Gender", x = "Age", y = "Density") +
      scale_fill_manual(values = c("blue", "red")) +
      theme_minimal()
  })
  
  
  output$survivalScatterPlot <- renderPlot({
    ggplot(DIG, aes(x = AGE, y = DEATH, color = SEX)) +
      geom_jitter(alpha = 0.6) +
      labs(title = "Age vs. Survival Status", x = "Age", y = "Death (1=Yes, 0=No)") +
      scale_color_manual(values = c("green", "orange")) +
      theme_minimal()
  })
  
  
  output$survivalBoxPlot <- renderPlot({
    ggplot(DIG, aes(x = as.factor(DEATH), y = BMI, fill = as.factor(DEATH))) +
      geom_boxplot() +
      labs(title = "BMI Distribution by Survival Status", 
           x = "Death (1=Yes, 0=No)", y = "BMI") +
      scale_fill_manual(values = c("red", "blue")) +
      theme_minimal()
  })
  
  
  output$effectivenessPlot <- renderPlot({
    DIG %>%
      group_by(TRTMT) %>%
      summarise(DeathRate = mean(DEATH, na.rm = TRUE)) %>%
      ggplot(aes(x = TRTMT, y = DeathRate, fill = TRTMT)) +
      geom_col() +
      labs(title = "Mortality Rate by Treatment", x = "Treatment", y = "Mortality Rate") +
      scale_fill_manual(values = c("purple", "orange")) +
      theme_minimal()
  })
  
  
  output$treatmentTable <- renderDataTable({
    DIG %>%
      group_by(TRTMT, SEX) %>%
      summarise(
        Total = n(),
        Deaths = sum(DEATH, na.rm = TRUE),
        DeathRate = round(mean(DEATH, na.rm = TRUE) * 100, 2)
      )
  })
}


shinyApp(ui = ui, server = server)