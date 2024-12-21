#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#


library(tidyverse)
library(DT)
library(shiny)

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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
