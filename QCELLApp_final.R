#Packages
library(shiny)
library(tidyverse)
library(ggplot2)
library(shinythemes)


produced <- 10000 #range of values 
used <- 10000 #range of values
titles <- c("used", "produced") #names for plot
titles <- as.character(titles)
title_order <- c('used', 'produced') #reorder for plot

#User Interface 
ui <- fluidPage(
    theme=shinytheme("slate"), #theme
    #title 
    titlePanel("Evergreen House Energy Snapshot"),
    #subtitle / narrative
    mainPanel("The Evergreen House is located in Vancouver, Washington across from the PDX Airport (latitude: 45.6 degrees). 
    Construction will be completed in Summer 2021. The purpose of this app is to visualize energy use and production. 
    The house is fitted with 21 unobstructed, south-facing Q CELL solar panels (400 watt each) 
    at approximately 30 degrees."),
    mainPanel("Net cost is calculated as a function of cost per kilowatt-hour ($0.0816/kWh), 
    Vancouver city electric tax (6%), and a $12 monthly fee."),
    mainPanel("For example, in April 2021 89kWh were used and 1033kWh were produced. The net cost was -$68!"),
    mainPanel(img(src="qcel2.png", height=200, width=300, align="center")), #save as .png not .PNG when creating image
    #input boxes 
    numericInput('used', 'Energy Used (kWh)', used), #input value - used
    numericInput('produced','Energy Produced (kWh)', produced), #input value - produced
    textInput("date", "Date"), #input value - date
    plotOutput('plot', width=700, height=500) #plot it! and size it.
)

#Server 
server <- function(input, output){
    #barchart of energy used/produced
    output$plot <- renderPlot({
        kwh <- cbind(input$used,input$produced) #create data frame(?) of unput values 
        plot <- ggplot(mapping=aes(x=factor(titles, level=title_order), y=kwh, fill=titles)) #plot the inputs listed above
        plot+geom_bar(stat='identity')+
            labs(title="Evergreen House Energy Snapshot (kwh)", y="kWh",
                 caption=input$date, 
                 subtitle=paste0("Net Cost: $",(round(((input$used-input$produced)*0.0816)+12)*1.06)), digits=2)+ #calculate net cost
            scale_fill_manual(values=c("green4", "orange1"))+ #color the bars
            theme_minimal()+
            theme(plot.title=element_text(size=22, face="bold", color="midnightblue", hjust=0.5),
                  plot.subtitle=element_text(size=18, hjust=0.5, color="midnightblue"),
                  axis.title.x=element_blank(),
                  axis.title.y=element_text(size=16, face="bold", color="midnightblue"),
                  axis.text.x=element_text(size=16, face="bold", color="midnightblue"),
                  axis.text.y=element_text(size=14, color="midnightblue"),
                  legend.position="none",
                  plot.background=element_rect(fill="#7bbcd550"),
                  plot.caption=element_text(size=18, hjust=0, face="bold", color="midnightblue"))
    })
}


#run app
shinyApp(ui, server)