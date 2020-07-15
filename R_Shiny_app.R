library(shiny)
library(ggplot2)
library(datasets)
library(dplyr)
library(magrittr)
library(leaflet)
library(rsconnect)

data <- read.csv('Ass_R_5147.csv')




ui <- fluidPage(
  titlePanel("FIT5147_R Tian ZHAI"),
  sidebarLayout(
    sidebarPanel("",
                 selectInput("coralType", 
                             label = tags$p(h3("Plz Select a coral name",style="color:coral")),
                             choices = list("soft corals"="soft corals",
                                            "blue corals"="blue corals",
                                            "hard corals"="hard corals",
                                            "sea pens"="sea pens",
                                            "sea fans"="sea fans"
                                            ),
                             selected = "blue corals"
                             
                 ),
                 img(src = "245-b.jpg", height = 120, width = 200),
                 tags$div(class="header", checked=NA,
                          tags$p(h4("wanna know more about coral?")),
                          tags$a(href="https://oceanservice.noaa.gov/education/tutorial_corals/coral11_protecting.html", "Click Here!", style = "color:blue;align: center"),
                 ),
                 
      
    ),
    
    
    mainPanel(tags$p(h3("Data Visualization",style="color:coral")),
              leafletOutput("siteMap"),
              h1(textOutput("coralName")),
              plotOutput("plot"),
    )
  )
)





server <- function (input,output)
{
  
  
  siteMap <- leaflet(data=data) %>%
    addTiles()%>% setView(147.6, -12.95,5)%>% addMarkers(~Longitude,~Latitude,popup = ~as.character(Name))
  
  output$siteMap = renderLeaflet(siteMap)
  
  
  
  output$plot <- renderPlot({
    
    requiredCoral <- data%>% filter(Coral==input$coralType)
    ggplot(requiredCoral,aes(Year,BleachingRate)) + geom_point() + facet_wrap(~reorder(Name,Latitude)) +geom_smooth()
    
  })
  
  output$coralName <- renderText({
    paste(input$coralType)
  })
  
  
}

shinyApp(ui = ui, server = server)