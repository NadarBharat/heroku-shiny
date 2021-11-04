library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tosca)
library(LDAvis)
library(servr)
library(NLP)
library(tm)
library(udpipe)
library(dplyr)
library(tidytext)
library(DT)
library(lubridate)
library(readxl)
library(writexl)
library(tidyr)
library(tidytext)
library(xfun)
library(syuzhet)
library(sentimentr)
library(stringr)
library(stringi)
library(Unicode)
library(lexRankr)

ui = dashboardPage(skin = "blue",
                   # skin = "green",
                   dashboardHeader(title = "Text Analysis"),
                   dashboardSidebar(
                     # actionButton("display","Display Text"),
                     sidebarMenu(
                       menuItem("Data File", tabName = "files", icon = icon("file-excel")),
                       menuItem("Topic Modelling", tabName = "topicmodelling", icon = icon("trademark")),
                       menuItem("Sentitment", tabName = "sentiment", icon = icon("stripe-s")),
                       menuItem("Text Summarization", tabName = "summarization", icon = icon("twitch"))
                     )),
                   
                   
                   dashboardBody(
                     tabItems(
                       
                       tabItem(tabName = "files",
                               fluidPage(
                                 fluidRow(
                                   column(12,
                                          fileInput("datafile", label = "Choose CSV/XLSX File", accept = c("text/csv", ".csv", ".xlsx", "xlx"))
                                   ),
                                   fluidRow(column(12,
                                                   dataTableOutput("contents")))),
                                 mainPanel()
                               )),
                       
                       tabItem(tabName = "topicmodelling",
                               fluidPage(
                                 wellPanel(
                                   tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'), style = "background: lightgrey",
                                   id = "leftPanel",
                                   selectInput("ID","Select ID Column", choices = NULL),
                                   selectInput("Date","Select Date Column", choices = NULL),
                                   selectInput("Text","Select Text Column", choices = NULL),
                                   textInput("add_stopword", label = paste("Enter Stopwords:"),value = NULL),
                                   numericInput("nTopics", "Number of Topics", value = 7, min = 3, max = 500),
                                   numericInput("alpha", "Alpha Value", value = 0.1, min = 0, max = 500),
                                   numericInput("eta", "Eta Value", value = 0.01, min = 0, max = 500),
                                   sliderInput("nTerms", "Top Terms Per Topic", min = 10, max = 50, value = 30, step=5),
                                   sliderInput("nIteration", "Iteration Value", min = 500, max = 5000, value = 1000, step=500),
                                   tags$hr(),
                                   actionButton(inputId = "GoButton", label = "Go!!!",  icon("sync")),
                                   tags$hr(),
                                   numericInput("percentcf", "Percent Cut Off", min = 0.01, max = 0.99, value = 0.25),
                                   downloadButton("downloadtopicfile", "Consolidated Topic")
                                 ),
                                 mainPanel(
                                   visOutput('visChart')
                                 )
                               )),
                       tabItem(tabName = "sentiment",
                               fluidPage(
                                 fluidRow(
                                   column(12,
                                          downloadLink("sentimentoutput", "Download Your File Here...")
                                   ),
                                   fluidRow(column(12,
                                                   dataTableOutput("sentidata")))),
                                 mainPanel()
                               )),
                       
                       tabItem(tabName = "summarization",
                               fluidPage(
                                 wellPanel(
                                   tags$style(type="text/css", '#leftPanel { width:200px; float:left;}'), style = "background: lightgrey",
                                   id = "leftPanel",
                                   fileInput("datafileforsumm", label = "Choose CSV/XLSX File", accept = c("text/csv", ".csv", ".xlsx", "xlx")),
                                   tags$hr(),
                                   selectInput("idforsumm","Select ID Column", choices = NULL),
                                   selectInput("textforsumm","Select Text Column", choices = NULL),
                                   tags$hr(),
                                   radioButtons("methodselection", "Choose Method", choices = list("Extract" = 1, "Abstract" = 2), selected = 1 ),
                                   actionButton("btn","Go!!!"), 
                                   downloadButton("downloadsum", label = "Download")
                                 ),
                                 mainPanel(dataTableOutput("fileoutput"), style = "width: 75%")
                               ))
                     )
                   )
)