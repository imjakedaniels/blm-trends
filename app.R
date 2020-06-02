library(shiny)
library(lubridate)
library(stringr)
library(readr)
library(dplyr)
library(DT)
library(d3treeR)
library(treemap)
library(tidytext)
library(SnowballC)

#all relevant tweets for all trends
all_tweets <- readr::read_csv(here::here("blm_app/blm_all_tweets.csv"))
#all_tweets <- readr::read_csv(here::here("blm_all_tweets.csv"))

#top tweets for all trends
top_tweets <- readr::read_csv(here::here("blm_app/blm_top_tweets.csv"))
#top_tweets <- readr::read_csv(here::here("blm_top_tweets.csv"))

#popularity ranking for all trends
trend_popularity <- readr::read_csv(here::here("blm_app/blm_trend_popularity.csv"))
#trend_popularity <- readr::read_csv(here::here("blm_trend_popularity.csv"))

#data for datatable
table_data <- readr::read_csv(here::here("blm_app/blm_table_data.csv"))
#table_data <- readr::read_csv(here::here("blm_table_data.csv"))

ui <- fluidPage(
    theme = shinythemes::shinytheme("cosmo"),
    tags$head(HTML('<link href="https://fonts.googleapis.com/css?family=Permanent+Marker" rel="stylesheet">')),
    tags$head(HTML('<style>* {font-size: 100%; font-family: Permanent Marker; color:#FFFFFFF;}</style>')),
    tags$body(HTML('<link href="https://fonts.googleapis.com/css?family=Raleway" rel="stylesheet">')),
    tags$body(HTML('<style>* {font-size: 100%; font-family: Raleway Medium;color:#FFFFFFF;}</style>')),
    a(img(src="Black_Lives_Matter.png", height = 100, width = 100, align = "right"), href="https://secure.actblue.com/donate/ms_blm_homepage_2019"),
    titlePanel(div("It was leading up to this...", style = "font-family:Permanent Marker; color:#FFFFFFF")),
    fluidRow(
        column(12, 
               p("This tool was created to highlight Twitter trends with conversations mentioning #BlackLivesMatter, Police, and Racists."))),
    p("Choose the topic and click the different dates below to explore trends on that day."),
    fluidRow(
        column(12, 
               selectInput(inputId = "selection",
                           label = "Current Topic:", multiple = FALSE, selected = "#BlackLivesMatter",
                           choices = c("#BlackLivesMatter", "Police", "Racist")))),
    fluidRow(
        column(12, 
               div(d3tree3Output("hcontainer")))),
    fluidRow(
        column(12, 
               p("The same trends above are in the table below with more context."),
               p("You can also search for terms, like \"Protest\" in the search bar."),
               div(dataTableOutput("table"), 
                   style = "font-size:80%; font-family:Raleway Medium"), height = 500)
    ),
    p(),
    p("Trend data collected since May 7th, 2020 via Twitter's API by @datajake", align = "right")
)

server <- function(input, output) {
    
    ### treemap
    
    treemap_data <- reactive({
        
        trend_popularity %>%
            filter(stem %in% wordStem(tolower(input$selection))) %>%
            mutate(trend = str_remove_all(trend, '"')) %>%
            filter(str_detect(trend, "^[0-9A-Za-z#_\\.%\\-,’'&ー ]+$")) %>%
            mutate(trend = str_wrap(trend, 10))
        
        })
    
    output$hcontainer <- renderD3tree3({
        
        treemap(treemap_data(),
                index = c("date_label", "trend"),
                vSize = "n",
                vColor = "n",
                type = "manual",
                draw = FALSE,
                fontsize.labels=10,
                lowerbound.cex.labels=1,
                force.print.labels=FALSE,
                palette = c("#DDD92A", "#2D2A32"),
                algorithm = "pivotSize") %>%
            
                d3tree3(rootname = str_glue('{input$selection} Twitter Trends'))
        
    })
    
    
    dataset = reactive({
        
        table_data %>%
            filter(stem %in% wordStem(tolower(input$selection))) %>%
            select(-stem) %>%
            select(n, trend, text, keywords)
        
        })
    
    ### table
    
    output$table <- DT::renderDataTable(
        dataset(), colnames = c('Rank', 'Trend', 'Top Tweet', 'Keywords'), # move rank to 1
        rownames = FALSE,
        escape = FALSE,
        selection = "none",
        options = list(pageLength = 5,
                       autoHeight = FALSE,
                       dom = '<"top"fp>t<"bottom"s>',
                       initComplete = JS(
                           "function(settings, json) {",
                           "$(this.api().table().header()).css({'background-color': '#DDD92A', 'color': '#2D2A32'});",
                           "}"))
    )
    
}


shinyApp(ui, server)