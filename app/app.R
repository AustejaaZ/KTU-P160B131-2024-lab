library(tidyverse)
library(ggplot2)
library(shiny)
ui <- fluidPage(
  titlePanel("Veiklos kodas 433900"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput("Kodas",
                     "Pasirinkite įmonę",
                     choices=NULL)
    ),
    
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  data<-read.csv("../data/433900.csv")
  names(data)=c("1","2","3","Pavadinimas","5","6","7","menuo","VidAtlyginimas","10","11","12","13")
  updateSelectizeInput(session,"Kodas",choices=data$Pavadinimas,server=TRUE)
  output$plot<-renderPlot(
    data%>%
      filter(Pavadinimas==input$Kodas)%>%
      ggplot(aes(x=ym(menuo),y=VidAtlyginimas))+
      geom_point()+
      geom_line()+
      theme_classic()+labs(x="Menuo",y="Vidutinis atlyginimas")
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
