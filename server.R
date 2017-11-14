library(shiny)
library(ggvis)

# Set up data file
fmr <- read.csv("FY18FMR.csv")

# Scatterplot: pop2010 by fmr

# Convert categorical variables to factor
fmr$state <- as.factor(fmr$state)
fmr$county <- as.factor(fmr$county)
fmr$fmr_type <- as.factor(fmr$fmr_type)
fmr$metro <- as.factor(fmr$metro)
fmr$rates <- fmr$fmr_0
fmr$metroarea <- NA
fmr$metroarea[fmr$metro==0] <- "No"
fmr$metroarea[fmr$metro==1] <- "Yes"

function(input, output, session) {
  
    rentdata <- reactive({
      ##Prepare data frame based on input
      r <- fmr
      bdr <- paste0("fmr_", as.character(input$bedrooms))
      r$rates <- r[,bdr]
      #Pick states 
      if (input$states != "All"){
        r <- subset(r, subset=(fmr$state_alpha==input$states))
      }
    r
    })
    
    tooltip <- function(x){
      #Hover function
      if (is.null(x)) return (NULL)
      
      currentdata <- isolate(rentdata())
      datapoint <- currentdata[currentdata$fips2010 == x$fips2010, ]
      paste0("<b>", datapoint$county_town_name, "</b><br /><br /><b>Fair Market Rent (", 
             input$bedrooms, " bedrooms):</b> $", format(x$rates, big.mark=","), "<br /><b>Population (2010):</b> ", 
           format(x$pop2010, big.mark=","))
      
    }
    
    explorer <- reactive({
      currentdata <- rentdata()
      bdr <- paste0("fmr_", as.character(input$bedrooms))

      output$n_counties <- renderText({ nrow(rentdata())})

      currentdata <- dplyr::arrange(currentdata, desc(metroarea))
      
      currentdata %>% ggvis(~pop2010, ~rates) %>%  
      layer_points(size := 50, size.hover := 200,
                   fillOpacity :=0.2, fillOpacity.hover = 0.5,
                   stroke = ~metroarea, key := ~fips2010) %>%
      add_tooltip(tooltip, "hover") %>%
      add_axis("x", title = "Population (2010)") %>%
      add_axis("y", title = "Fair Market Rent ($)", title_offset=50) %>%
      add_legend("stroke", title = "Metro Area") %>%
      scale_nominal("stroke", range = c("blue", 'green')) %>%
      set_options(width=1000, height=500)
    
      
    })
    
    explorer %>% bind_shiny("fmrplot")

}

   