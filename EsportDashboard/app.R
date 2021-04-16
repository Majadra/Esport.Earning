##############################################################################################
# Preparation
library(shiny)
library(shinythemes)
library(shinydashboard)
library(RCurl)
library(tidyverse)
library(scales)
library(DT)
theme_set(theme_bw())

##############################################################################################
# Reading the dataset
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
# Filtering games with low earnings
gamestorevmove <- General$Game[which(General$TotalEarnings <= 2500)]

General <- General[-which(General$Game %in% gamestorevmove),]
Historical <- Historical[-which(Historical$Game %in% gamestorevmove),]
temp <- General %>% select(Game,Genre)
Historical <- left_join(Historical,temp)
##############################################################################################
ui <- dashboardPage(
    dashboardHeader(title = "Esport Earnings Analysis"),
    dashboardSidebar(
        sidebarMenu(
            uiOutput("Kaggle_link",align = "center"),
            menuItem("The database",tabName = "db_tab",icon = icon("th"),
                     menuItem("General data",tabName = "gen_tab"),
                     menuItem("Historical data",tabName = "hist_tab")),
            menuItem("Genres",tabName = "genre_tab", icon = icon("dashboard"))
            
            
        ) 
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "gen_tab",
                    DTOutput("db_gen")),
            tabItem(tabName = "hist_tab",
                    DTOutput("db_hist")),
            tabItem(tabName = "genre_tab",
                    textOutput("Test"),
                fluidRow(column(width = 4,selectInput(inputId = "in_genre", 
                                                      label = "Select genre",
                                                      choices = c("All",Historical %>%
                                                                      count(Genre, wt = Earnings,sort = TRUE) %>%
                                                                      select(Genre)))),
                         column(width = 4,selectInput(inputId = "in_var",
                                                      label = "Select variable to measure",
                                                      choices = 
                                                          c("Total Earnings","Total tournaments","Total players"))),
                         column(width = 4,selectInput(inputId = "in_time",
                                                      label = "Select time",
                                                      choices = c("By year","By month")))), # Fluid row
                fluidRow(plotOutput("Genre_timeline"))) # genres tab
        )
    ) # Body
    
) # UI
##############################################################################################
# Define server logic required to draw a histogram
server <- function(input, output) {
    
Genre_filter <- reactive({
    if(input$in_genre == "All")
        {unique(Historical$Genre)}
    else
        {input$in_genre}
})    
variable <- reactive({
switch(input$in_var,
       "Total Earnings" =  sym("Earnings"),
       "Total tournaments" = sym("Tournaments"),
       "Total players" = sym("Players"))
})

plot1data <- reactive({
    Historical %>%
        select(variable(),Date,Genre) %>%
        mutate(Date = as.Date(Date, format = "%d %m %Y"),
               Year = format(Date,ifelse(input$in_time == "By year","%Y","%m"))) %>%
        filter(Genre %in% Genre_filter()) %>%
        group_by(Year) %>%
        summarise(Total = sum(!!(variable())))
})
output$Genre_timeline <- renderPlot({
    plot1data() %>%
        ggplot(aes(x = Year,y = Total)) + 
        geom_col(fill = "lightblue",color = "black",alpha = 0.75,width = 0.75) +
        labs(title = paste0(input$in_var," - ",input$in_genre), x = element_blank(), y = element_blank()) +
        scale_y_continuous(labels = label_comma())
})


output$Test <- renderText({paste0(Genre_filter(),input$in_genre)})

output$db_gen <- renderDataTable({
    datatable(General)
    
})
output$db_hist <- renderDataTable({
        datatable(Historical)
})

url <- tags$a(href="https://www.kaggle.com/rankirsh/esports-earnings-analysis",align = "center","The kaggle notebook")
output$Kaggle_link <- renderUI({
    tagList(url) })
}
##############################################################################################
# Run the application 
shinyApp(ui = ui, server = server)
