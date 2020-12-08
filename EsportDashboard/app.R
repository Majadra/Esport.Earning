##############################################################################################
# Preparation
library(shiny)
library(shinythemes)
library(RCurl)
library(tidyverse)
library(DT)

# Reading the datasets
General <- 
    read.csv(text = getURL("https://raw.githubusercontent.com/Majadra/Esport.Earning/master/GeneralEsportData.csv"))
Historical <- 
    read.csv(text = getURL("https://raw.githubusercontent.com/Majadra/Esport.Earning/master/HistoricalEsportData.csv"))


General$Game[which(Encoding(General$Game) == "UTF-8")] <- 
    iconv(General$Game[which(Encoding(General$Game) == "UTF-8")],from = "",to = "CP1255","byte")

Historical$Game[which(Encoding(Historical$Game) == "UTF-8")] <- 
    iconv(Historical$Game[which(Encoding(Historical$Game) == "UTF-8")],from = "",to = "CP1255","byte")

General$Game <- 
    as.character(parse(text=shQuote(gsub("<U\\+([A-Z0-9]+)>", "\\\\u\\1",General$Game))))

Historical$Game <- 
    as.character(parse(text=shQuote(gsub("<U\\+([A-Z0-9]+)>", "\\\\u\\1",Historical$Game))))

temp <- General %>% select(Game,Genre)
Historical <- left_join(Historical,temp)
##############################################################################################
ui <- fluidPage(theme = shinytheme("cerulean"),
    navbarPage("Test",
               tabPanel("By Game",
                        
    column(12,
           titlePanel(h1("Esport Analysis - Interactive dashboard",align = "center"))
    ),


    # Sidebar with a slider input for number of bins 
    fluidRow(
            column(4,
            selectInput(inputId = "Title1",
                    label = "Select game to analyse :",
                    choices = c(unique(Historical %>% count(Game, wt = Earnings,sort = TRUE))[[1]][])),offset = 4)),
          

    fluidRow(column(6,
            plotOutput("GameTimeline")),
            column(6,
            plotOutput("GameTimelineMonth"))), # Row,
    column(12,
           DT::dataTableOutput("GenData")), 
    
    ), # Tab n#1
    tabPanel("By Genre",
             fluidRow(
                 column(4,
                        selectInput(inputId = "Genre1",
                                    label = "Select genre to analyse :",
                                    choices = c(unique(Historical %>%
                                                           count(Genre, wt = Earnings,sort = TRUE))[[1]][])),offset = 4)
                 ),
             fluidRow(
                 column(6,
                 plotOutput("GenreTimeline")),
                 column(6,
                 plotOutput("GenreTournaments"))
             )
             # Row
             ) # Tab n#2
    
    
    

 
        
    ) # Nav bar
) # UI
##############################################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
output$GameTimeline <- renderPlot({
    Historical %>%
        select(Earnings,Date,Game) %>%
        mutate(Date = as.Date(Date, format = "%d %m %Y"),
               Year = format(Date,"%Y")) %>%
        filter(Game == input$Title1) %>%
        group_by(Year) %>%
        summarise(Earnings = sum(Earnings)) %>%
            ggplot(aes(x = Year,y = Earnings)) + 
            geom_col(fill = "lightblue",color = "black",alpha = 0.75,width = 0.75) +
            labs(title = "Earnings per year", x = element_blank(), y = element_blank()) +
            scale_y_continuous(labels = label_comma())
    })

output$GameTimelineMonth <- renderPlot({
    Historical %>%
        select(Earnings,Date,Game) %>%
        mutate(Date = as.Date(Date, format = "%d %m %Y"),
               Month = format(Date,"%m")) %>%
        filter(Game == input$Title1) %>%
        group_by(Month) %>%
        summarise(Earnings = sum(Earnings)) %>%
        ggplot(aes(x = Month,y = Earnings)) + 
        geom_col(fill = "tomato2",color = "black",alpha = 0.75,width = 0.75) +
        labs(title = "Earnings per month", x = element_blank(), y = element_blank()) +
        scale_y_continuous(labels = label_comma())
})

output$GenreTimeline <- renderPlot({
    Historical %>%
        select(Earnings,Date,Genre) %>%
        mutate(Date = as.Date(Date, format = "%d %m %Y"),
               Year = format(Date,"%Y")) %>%
        filter(Genre == input$Genre1) %>%
        group_by(Year) %>%
        summarise(Earnings = sum(Earnings)) %>%
        ggplot(aes(x = Year,y = Earnings)) + 
        geom_col(fill = "lightblue",color = "black",alpha = 0.75,width = 0.75) +
        labs(title = "Earnings", x = element_blank(), y = element_blank()) +
        scale_y_continuous(labels = label_comma())
})

output$GenreTournaments <- renderPlot({
    Historical %>%
        select(Tournaments,Date,Genre) %>%
        mutate(Date = as.Date(Date, format = "%d %m %Y"),
               Year = format(Date,"%Y")) %>%
        filter(Genre == input$Genre1) %>%
        group_by(Year) %>%
        summarise(Tournaments = sum(Tournaments)) %>%
        ggplot(aes(x = Year,y = Tournaments)) + 
        geom_col(fill = "tomato2",color = "black",alpha = 0.75,width = 0.75) +
        labs(title = "Tournaments", x = element_blank(), y = element_blank()) +
        scale_y_continuous(labels = label_comma())
})

output$GenData <- renderDataTable({
    datatable(General)

})
}
##############################################################################################
# Run the application 
shinyApp(ui = ui, server = server)
