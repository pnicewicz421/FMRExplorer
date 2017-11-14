library(shiny)
library(ggvis)

fmr <- read.csv("FY18FMR.csv")
states1 <- c("All", as.character(unique(fmr$state_alpha)))

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
    fluidPage(
     titlePanel("Fair Market Rent Explorer - 2018 Rates. Created 11/13/2017"),
     fluidRow(
         column(3,
                h3("Choose Attributes"),
                selectInput("bedrooms", "Number of Bedrooms",
                           choices=c(0, 1, 2, 3, 4), selected=0, multiple=FALSE),
                selectInput("states", "State/Territory", states1)
                ),
         column(9,
                ggvisOutput("fmrplot"),
                wellPanel(
                    span("Number of Counties selected:"),
                    textOutput("n_counties")
                ),
                wellPanel(
                  span("The Fair Market Rate Explorer allows users to see the 2018 Fair Market Rent Rates,
                       set by the U.S. Department of Housing and Urban Development (HUD). Fair Market Rates 
                       are based on the number of bedrooms, from 0 (efficiency) to 4. They are monthly rent amounts in the rental housing 
                        market and help guage affordability in the area and determine rates for various housing programs funded by the federal government.
                       Select the number of bedrooms and the state. The resulting scatterplot displays the population by fair market rent rate for each county or town in the state.
                       Metro areas are color coded blue and non-metro areas are green.")
                )
            )
         )
       )
    )
  )
    

