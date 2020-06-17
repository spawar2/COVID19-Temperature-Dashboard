library(shiny)
library(highcharter)
library(dplyr)


edata <- read.csv("feb.csv") %>% rename(lat=latitude,lon = longitude)


wmap <- hcmap()
# Define UI for application that draws a bubble map

ui <- fluidPage(
   titlePanel(title="COVID-19 & Temperature Dashboard"), # Application title
   sidebarLayout(
      sidebarPanel(
        selectInput("dataset", "Month", c("January", "February", "March")),
        sliderInput('mag','Confirmed cases more than', min = 1,max = 100,step = 0.5,value = 0),
        sliderInput('temp','Location temperature (Degree celsius)', min = -16.8,max = 50,step = 0.5,value = 0),
        selectInput('bubble','Bubble size selection',choices = c('Confirmed'= 'mag','Deaths of Confirmed' = 'depth','Recovered of Confirmed' = 'Recovered')),
        sliderInput('bublesize','Adjust bubble Size',min = 2,max = 10,step = 1,value = 6),
        hr(style = 'border-color:black;')
        
      ),
      
      # Show a MapBubble
      mainPanel(
        highchartOutput('eqmap',height = "500px"),
        hr(),   
        print("Dashboard Contact: Research, Clinical & Data Driven Responses to COVID-19, Yale University; Shrikant Pawar: shrikant.pawar@yale.edu."),
      )
   )
)

# Define server logic required to filter dataset and draw geo bubble map
server <- function(input, output, session) {
  
  
  
  data <- reactive(
    
    switch(input$dataset,
           "January" = edata, "February" = edata, "March" = edata)
                   %>% 
                  filter(mag >= input$mag) %>%  
                  filter(temp >= input$temp) %>% 
                  rename(z = input$bubble))


   
  output$eqmap <-renderHighchart(
    #hc_add_series_map(worldgeojson,zdata,value = 'value',joinBy = c('name')) %>%hc_legend(enabled = F) %>% 
    wmap %>% hc_legend(enabled = F) %>%
    hc_add_series(data = data(), type = "mapbubble", color = "red", name = "Number of Cases", maxSize = paste0(input$bublesize,'%')) %>% 
    hc_title(text = "COVID-19 Cases") %>% 
    hc_subtitle(text = paste('Number of observations:', nrow(data()),sep = '')) %>% 
    hc_mapNavigation(enabled = T)%>% 
    hc_exporting(enabled = TRUE,filename = 'custom'))
    
  
  
  
   

}

# Run the application 
shinyApp(ui = ui, server = server)

