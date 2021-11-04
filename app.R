#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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

server = function(input, output, session) {
    options(shiny.maxRequestSize=5000*1024^2) # memory.limit(size = 500mb)
    
    shinyOptions(progress.style="old")
    
    #Function to store data
    datafile_org = reactive({
        inFile = input$datafile
        
        if (is.null(inFile)) {
            return(NULL) }
        
        ext = file_ext(inFile$name)
        
        importedfile = switch(ext,
                              csv = read.csv(inFile$datapath, stringsAsFactors=FALSE),
                              xlsx = , xls = read_xlsx(inFile$datapath, col_names = T, col_types = "text"),
                              stop("file extension not recognized"))
    })
    
    #Uploaded data output
    output$contents = renderDataTable({
        datatable(datafile_org(), 
                  options = list(
                      autoWidth = TRUE,
                      scrollY = TRUE,
                      columnDefs = list(list(width = '200px', targets = "_all"))))
    })
    
    #Column Selection
    observe({
        choices1 = colnames(datafile_org())
        updateSelectInput(session,"ID", choices =  choices1, selected = "id")
    })
    
    observe({
        choices1 = colnames(datafile_org())
        updateSelectInput(session,"Date", choices =  choices1, selected = "date")
    })
    
    observe({
        choices1 = colnames(datafile_org())
        updateSelectInput(session,"Text", choices =  choices1, selected = "text")
    })
    
    #Corpus Creation
    toscacorpus = reactive({
        datatable = datafile_org()
        ID = paste(input$ID)
        Date = paste(input$Date)
        Text = paste(input$Text)
        id = as.character(datatable[,ID]) #Input ID column Name from loaded file
        #date = format(as.Date(datatable[,Date],"%Y-%m-%d"), format = "%d/%m/%Y")
        dates = as.character(datatable[,Date])
        date = format(as.Date(dates, tryFormats = c("%m/%d/%Y", "%Y-%m-%d", "%m-%d-%Y", "%Y/%m/%d"))) #Input Date Column Name from loaded file
        title = datatable[,Text] #Input Column Name from loaded file
        
        text = datatable[,Text] #Input Text Column Name from Loaded file
        
        text = as.character(text) #Converting Text column to character format
        text = enc2utf8(text) #Coverting again to UTF-8 format
        
        #Creating Text list
        texts = lapply(text, function(x) {
            x = gsub("na na","", x, ignore.case = TRUE)  #Remove "na na"
            x = gsub("na","", x, ignore.case = TRUE)  #Remove "na"
            x = gsub("(https?://[^/\\s]+)[^\\s]*", replacement = "", x)  #Remove urls
            x = gsub("[^0-9A-Za-z///' ]", "'", x,ignore.case = TRUE)  #Remove special chars
            x = gsub("'","", x, ignore.case = TRUE)  #Remove '
            x = tolower(x)  # Transform to lowercase
            x = gsub("[[:digit:]]+", "", x)  #Remove digits
            x = gsub(pattern = "[/]", replacement = " ", x)  #Replace / with space
            x = gsub("^[[:space:]]+", "", x)  #Remove white space at the beginning of a sting
            x = gsub("[[:space:]]+$", "", x)  #Remove white space at the end of a string
            x = stri_trim(x, side = c("both", "left", "right"))  #Remove white space from start and end of a string
            x = str_squish(x)  # Reduce repeated white space inuniqueide a string
        })
        
        names(texts) = datatable[,ID] #Adding ID column to our text list
        
        toscacorpus = textmeta(meta = data.frame(id = id, date = date, title = title, stringsAsFactors = FALSE), text = texts)
    })
    
    #Cleaning Created Corpus
    corpusClean = reactive({
        corpus = toscacorpus()
        stopw = stop_words$word
        if(!is.null(input$add_stopword)){
            # Take the comma-delimited list of terms and split them out to be a
            # character vector. The ", ?" regEx is so that this will work with
            # or without a space following the comma
            terms <- unlist(strsplit(input$add_stopword,", ?"))
            stopw = c(stopw, terms)
        }
        #Cleaning our text column
        textClean = cleanTexts(text = corpus$text, sw = stopw ,lowercase = TRUE, 
                               rmPunctuation = TRUE, rmNumbers = TRUE,
                               checkUTF8 = TRUE, ucp = TRUE)
        
        #Creating new corpus with cleaned text
        corpusClean = textmeta(text = textClean, meta = corpus$meta)
        
        #Summary of our corpus
        print(corpusClean)
        summary(corpusClean)
    })
    
    #Creating Word table
    wordtable = reactive({
        cleancorpus = corpusClean()
        wordtable = makeWordlist(cleancorpus$text)
    })
    
    #Creating Data Compatible For LDA
    pagesLDA = reactive({
        cleancorpus = corpusClean()
        reqwordtable = wordtable()
        #Creating our LDA document necesary for Topic Modelling
        pagesLDA = LDAprep(text = cleancorpus$text, vocab = reqwordtable$words, reduce = TRUE)
    })
    
    #Running LDA Algorithm
    LDAresult = reactive({
        LDAprepfile = pagesLDA()
        reqwordtable = wordtable()
        ntopics = input$nTopics
        niteration = input$nIteration
        alpha = input$alpha
        eta = input$eta
        LDAresult = LDAgen(documents = LDAprepfile, K = ntopics, vocab = reqwordtable$words, seed = 1234,
                           num.iterations = niteration, alpha = alpha, eta = eta)
    })
    

    # Compute some statistics related to the data set for LDAvis:
    #Document Length
    doc.length = reactive({
        reqLDA = pagesLDA()
        #Calculating Document length for LDAvis
        doc.length = sapply(reqLDA, function(x) sum(x[2, ]))
    })
    
    #Term Frequency
    term.frequency = reactive({
        reqwordtable = wordtable()
        #Extraing frequency from word table we created
        term.frequency = as.integer(reqwordtable$wordtable)
    })
    
    #Theta
    theta = reactive({
        result = LDAresult()
        alpha = input$alpha
        #Calculating THETA
        theta = t(apply(result$document_sums + alpha, 2, function(x) x/sum(x)))
    })
    
    #Phi
    phi = reactive({
        result = LDAresult()
        eta = input$eta
        #Calculating PHI
        phi = t(apply(t(result$topics)+ eta, 2, function(x) x/sum(x)))
    })
    
    #Combining Statistic Created From Data
    finaloutput = reactive({
        doclen = doc.length()
        termfrequency = term.frequency()
        reqtheta = theta()
        reqphi = phi()
        reqwordtable = wordtable()
        finaloutput = list(phi = reqphi,
                           theta = reqtheta,
                           doc.length = doclen,
                           vocab = reqwordtable$words,
                           term.frequency = termfrequency)
    })
    #LDAvis Output
    observeEvent(input$datafile, {
        
        output$visChart = renderVis({
            
            progress = Progress$new(session, min=0, max=1)
            on.exit(progress$close())
            
            progress$set(message = 'Calculation in progress',
                         detail = 'This may take a while...')
            
            
            
            isolate({
                nterms    = input$nTerms
                lda_model = LDAresult()
                FinalOutput = finaloutput()
            })
            
            with(FinalOutput,
                 createJSON(phi = phi, 
                            theta = theta, 
                            doc.length = doc.length, 
                            vocab = vocab, 
                            term.frequency = term.frequency,
                            R = nterms,
                            mds.method = jsPCA,
                            plot.opts = list(xlab = "PC1", ylab = "PC2"),
                            reorder.topics = FALSE))
            
        })
        
        
        
    })
    #Updated LDAvis Output
    observeEvent(input$GoButton, {
        
        output$visChart = renderVis({
            
            progress = Progress$new(session, min=0, max=1)
            on.exit(progress$close())
            
            progress$set(message = 'Calculation in progress',
                         detail = 'This may take a while...')
            
            
            
            isolate({
                nterms    = input$nTerms
                lda_model = LDAresult()
                FinalOutput = finaloutput()
            })
            
            with(FinalOutput,
                 createJSON(phi = phi, 
                            theta = theta, 
                            doc.length = doc.length, 
                            vocab = vocab, 
                            term.frequency = term.frequency,
                            R = nterms,
                            mds.method = jsPCA,
                            plot.opts = list(xlab = "PC1", ylab = "PC2"),
                            reorder.topics = FALSE))
            
        })
        
        
        
    })
    
    #Document Extraction
    doc_with_topic = reactive({
        doc_topic_dist = as.data.frame(theta())
        toscacorpus = toscacorpus()
        corpusClean = corpusClean()
        doc_with_topic = datafile_org()
        Text = paste(input$Text)
        doc_with_topic$cleaned_text = lapply(doc_with_topic[, Text], function(x) {
            x = gsub("na na","", x, ignore.case = TRUE)  #Remove "na na"
            x = gsub("na","", x, ignore.case = TRUE)  #Remove "na"
            x = gsub("(https?://[^/\\s]+)[^\\s]*", replacement = "", x)  #Remove urls
            x = gsub("[^0-9A-Za-z///' ]", "'", x,ignore.case = TRUE)  #Remove special chars
            x = gsub("'","", x, ignore.case = TRUE)  #Remove '
            x = tolower(x)  # Transform to lowercase
            x = gsub("[[:digit:]]+", "", x)  #Remove digits
            x = gsub(pattern = "[/]", replacement = " ", x)  #Replace / with space
            x = gsub("^[[:space:]]+", "", x)  #Remove white space at the beginning of a sting
            x = gsub("[[:space:]]+$", "", x)  #Remove white space at the end of a string
            x = stri_trim(x, side = c("both", "left", "right"))  #Remove white space from start and end of a string
            x = str_squish(x)  # Reduce repeated white space inuniqueide a string
        })
        doc_with_topic$cleaned_text = as.character(doc_with_topic$cleaned_text)
        ID = paste(input$ID)
        doc_with_topic = doc_with_topic[ doc_with_topic[, ID] %in% names(corpusClean$text), ]
        ntopics = input$nTopics
        pcf = input$percentcf
        
        doc_with_topic$topic = apply(doc_topic_dist[1:ntopics], 1, function(x) names(which(x > pcf)))
        doc_with_topic2 = unnest(doc_with_topic, topic)
        isolate({
            lda_model = LDAresult()
            })
        topwords = as.list(as.data.frame(topWords(lda_model$topics, numWords = 10)))
        doc_with_topic2$topic_keywords = topwords[match(doc_with_topic2$topic, names(topwords))]
        doc_with_topic2$topic_keywords = as.character(doc_with_topic2$topic_keywords)
        return(doc_with_topic2)
    })
    
    output$downloadtopicfile = downloadHandler(
        filename = function() {
            paste("Topic Document", Sys.Date(), ".xlsx", sep="-")
        },
        content = function(file){
            doc_with_topic = doc_with_topic()
            write_xlsx(doc_with_topic, file)
        })
    
    # Summarization
    datafileforsumm = reactive({
        inFile = input$datafileforsumm
        
        if (is.null(inFile)) {
            return(NULL) }
        
        ext = file_ext(inFile$name)
        
        importedfile = switch(ext,
                              csv = read.csv(inFile$datapath, stringsAsFactors=FALSE),
                              xlsx = , xls = read_xlsx(inFile$datapath, col_names = T, col_types = "text"),
                              stop("file extension not recognized"))
    })
    
    #Select Your Text Column
    observe({
        choices1 = colnames(datafileforsumm())
        updateSelectInput(session,"textforsumm", choices =  choices1, selected = "ctext")
    })
    
    observe({
        choices1 = colnames(datafileforsumm())
        updateSelectInput(session,"idforsumm", choices =  choices1, selected = "ID")
    })
    
    summdata = eventReactive(input$btn,{
        progress <- Progress$new(session, min=0, max=1)
        on.exit(progress$close())
        
        progress$set(message = 'Calculation in progress',
                     detail = 'This may take a while...')
        
        data = datafileforsumm()
        text_column = paste(input$textforsumm)
        ID = paste(input$idforsumm)
        #text = data[,text_column] #Input Column Name from loaded file
        
        # Applying Extractive Method
        if(input$methodselection == "1") {
            summary_list = list()
            for(i in 1:nrow(data)) {       # for-loop over rows
                summary <- lexRank(text = data[i,text_column], docId = data[i,ID], threshold=0.1,
                                   usePageRank=TRUE, damping=0.85,
                                   continuous=TRUE, sentencesAsDocs=TRUE,
                                   removePunc=TRUE, removeNum=TRUE,
                                   toLower=TRUE, stemWords=TRUE,
                                   rmStopWords=TRUE, Verbose=TRUE,
                                   returnTies=TRUE)
                summary_list[[i]] = summary$sentence
            }
            data$extractive_summary = as.character(summary_list)
            
            return(data)
            
        }
        
        #Abstractive method
        else {
            influential_sentences = lexRank(text=sentences$sentence, docId = sentences$textrank_id, threshold=0.1,
                                            n=input$sentences, usePageRank=TRUE, damping=0.85,
                                            continuous=FALSE, sentencesAsDocs=FALSE,
                                            removePunc=TRUE, removeNum=TRUE,
                                            toLower=TRUE, stemWords=TRUE,
                                            rmStopWords=TRUE, Verbose=TRUE,
                                            returnTies=TRUE)
            return(influential_sentences)
        }
        
        
    })
    
    output$fileoutput = renderDataTable({
        summdata = summdata()
        text_column = paste(input$textforsumm)
        summdata_2 = summdata[, c(text_column, "extractive_summary")]
        datatable(summdata_2, options = list(searching = FALSE,pageLength = 10,lengthMenu = c(10, 50, 100, 500), scrollX = T))
    })
    
    output$downloadsum = downloadHandler(
        filename = function() {
            paste("Data_Summary", input$second, Sys.Date(), ".xlsx", sep="-")
        },
        content = function(file) {
            df = summdata()
            write_xlsx(df, file)
        }
    )
    
    # Sentiment
    sentidict = reactive({
        sentidict = read.csv("SentiDict.csv", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
        return(sentidict)
    })
    
    text_sent = reactive({
        datafileforsenti = datafile_org()
        Text = paste(input$Text)
        text_sent = datafileforsenti[,Text]
        text_sent = iconv(text_sent, "latin1", "ASCII", "") #Encode data where ever certain type of data that cannot be processed by R
        
        text_sent = lapply(text_sent, function(x) {
            x = gsub("na na","", x, ignore.case = TRUE)  #Remove "na na"
            x = gsub("(https?://[^/\\s]+)[^\\s]*", replacement = "", x)  #Remove urls
            x = gsub("[^0-9A-Za-z///' ]", "'", x,ignore.case = TRUE)  #Remove special chars
            x = gsub("'","", x, ignore.case = TRUE)  #Remove '
            x = tolower(x)  # Transform to lowercase
            x = gsub("[[:digit:]]+", "", x)  #Remove digits
            x = gsub(pattern = "[/]", replacement = " ", x)  #Replace / with space
            x = gsub("^[[:space:]]+", "", x)  #Remove white space at the beginning of a sting
            x = gsub("[[:space:]]+$", "", x)  #Remove white space at the end of a string
            x = stri_trim(x, side = c("both", "left", "right"))  #Remove white space from start and end of a string
            x = str_squish(x)  # Reduce repeated white space inuniqueide a string
        })
        
        # datafileforsenti$clean_text = as.character(datafileforsenti$clean_text)
        
        # striptext = function(x){
        # x = removeNumbers(stripWhitespace(tolower(x)))   }
        
        text_sent = as.character(text_sent)
        return(text_sent)
    })
    
    sentioutput = reactive({
        
        progress = Progress$new(session, min=0, max=1)
        on.exit(progress$close())
        
        progress$set(message = 'Calculation in progress',
                     detail = 'This may take a while...')
        
        # validate(input$sentidict, "Missing Sentiment Dictionary")
        sentidict = as_key(sentidict())
        text_s = text_sent()
        senti = sentiment_by(text_s, polarity_dt=sentidict, valence_shifters_dt = lexicon::hash_valence_shifters, by = NULL,
                             amplifier.weight = 1, n.before = 3, n.after = 3, missing_value = NULL)
        
        #Calculate sentiment scores
        senti$Sentiment <- ifelse(senti$ave_sentiment > 0.03,"Positive",
                                  ifelse(senti$ave_sentiment < 0.01,"Negative", "Neutral"))
        
        #Drop unused columns from "senti" data frame
        senti = subset(senti, select = -c(element_id, word_count, sd))
        
        #rename column name
        senti = rename(senti, Avg_Sentiment = ave_sentiment)
        senti$Avg_Sentiment = round(senti$Avg_Sentiment, digits = 2) 
        datafileforsenti = datafile_org()
        sentioutput = cbind(datafileforsenti,senti) #Add sentiment scores to the raw data
        return(sentioutput)
    })
    
    
    output$sentimentoutput = downloadHandler(
        filename = function() {
            paste("Sentiment_Output", ".xlsx", sep = "")
        },
        content = function(file){
            progress = Progress$new(session, min=0, max=1)
            on.exit(progress$close())
            
            progress$set(message = 'Calculation in progress',
                         detail = 'This may take a while...')
            sentioutput = sentioutput()
            write_xlsx(sentioutput, file)
        })
    
    observeEvent(input$datafile, {
        
        output$sentidata = renderDataTable({
            datatable(sentioutput(), 
                      options = list(
                          autoWidth = TRUE,
                          scrollY = TRUE
                      ))
        })
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
