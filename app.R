library(shiny)
library(arules)
library(arulesViz)
library(RColorBrewer)
library(dplyr, warn.conflicts = F)
library(lubridate, warn.conflicts = F)
library(knitr, warn.conflicts = F)
library(kableExtra)
library(ggpubr)
library(ggplot2)
library(plyr)
library(DT)
library(tidyverse)
library(readxl)
library(htmlwidgets)
library(htmltools)

options(shiny.maxRequestSize=30*1024^2)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Aplicación Basket"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        
        sidebarPanel(
            radioButtons(inputId = "file_type",label = "Selecciona el tipo de archivo a cargar",choices = c(Excel = "xls",CSV.file = "csv")),
            uiOutput(outputId = "read_file"),
            uiOutput(outputId = "tickets_col"),
            uiOutput(outputId = "art_col"),
            sliderInput("conf",
                        "Confidence:",
                        min = 0,
                        max = 1,
                        value = .2,
                        step = .1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel(
                    title = "Previsualización",
                    DT::dataTableOutput(outputId = "basket_file")),
                tabPanel(
                    title = "Ventas",
                    h3("Items más vendidos"),
                    br(),
                    plotOutput("freq_item", width = "100%"),
                    br(),
                    br(),
                    h3("Tickets de compra"),
                    plotOutput(outputId = "freq_ticket", width = "100%"),
                    
                ),
                tabPanel(
                    title = "Market Basket",
                    h3("Reglas de asociación"),
                    br(),
                    DT::dataTableOutput("rules"),  
                    br(),
                    h3("Comunidades de ITEMS"),
                    br(),
                    visNetwork::visNetworkOutput("grafo")
                ),
                tabPanel(
                    title = "Itemsets",
                    h3("Itemsets más frecuentes"),
                    br(),
                    plotOutput(outputId = "top10graf"),
                    br(),
                    DT::dataTableOutput(outputId = "top10")
                ),
                tabPanel(
                    title = "Top 10 Reglas",
                    h3("Reglas con mayor probabilidad"),
                    br(),
                    DT::dataTableOutput(outputId = "reglas_prob"),
                    br(),
                    h3("Reglas de items más frecuentes"),
                    DT::dataTableOutput(outputId = "reglas_freq")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$read_file <- renderUI({
        if (input$file_type == "csv") {
            fileInput(inputId = "basket",
                      label = "Selecciona el archivo .csv basket",
                      accept = c(
                          'text/csv',
                          'text/comma-separated-values',
                          '.csv')
            )
        }else{
            fileInput(inputId = 'basket', 
                      label = 'Selecciona el archivo .xls basket',
                      accept = c(".xlsx",
                                 ".xls")
            )
        }
    })
    
    
    df_upload <- reactive({
        inFile <- input$basket
        if (input$file_type == "csv") {
            if(is.null(inFile))
                return(NULL)
            df <- read.csv(inFile$datapath,header = TRUE,sep = ",")
            
        }else{
            if(is.null(inFile))
                return(NULL)
            df <- read_excel(path = inFile$datapath)
        }
        return(df)
    })
    
    output$tickets_col <- reactive({
        names(df_upload())
    })
    
    
    output$tickets_col <- renderUI({
        selectInput(inputId = "select_ticket_col",
                    label = "Selecciona la columna que contiene los tickets",
                    choices = names(df_upload()))
    })
    
    output$art_col <- renderUI({
        selectInput(inputId = "select_art_col",
                    label = "Selecciona la columna que contiene los artículos",
                    choices = names(df_upload()))
    })
    
    output$basket_file <- DT::renderDataTable({
        data <- df_upload()
        data <- data%>%
            select(input$select_ticket_col, input$select_art_col)
        DT::datatable(data)
    })
    
    file_basket <- reactive({
        data <- df_upload()
        data <- data%>%
            select(input$select_ticket_col, input$select_art_col)
        as.data.frame(data)
    })
    
    tr <- reactive({
        
        data <- file_basket()
        names(data) <- c("ticket","items")
        
        data <- data[complete.cases(data),]
        data$ticket <- as.character(data$ticket)
        data$items <- as.character(data$items)
        
        transaction <- ddply(data,
                             c("ticket"),
                             function(df1)paste(df1$items, collapse = ","))
        
        write.table(x = transaction,
                    file = tmp_cu <- file(),
                    row.names = FALSE,
                    quote = FALSE)
        
        read.transactions(file = tmp_cu,
                          format = "basket",
                          sep = ",", 
                          rm.duplicates=TRUE)
        
    })
    
    output$freq_item <- renderPlot({
        
        data <- file_basket()
        names(data) <- c("ticket","items")
        
        freq <- data.frame(table(data$items)) 
        freq <- freq[order(freq$Freq, decreasing = T),]
        total <- sum(freq$Freq)
        freq <- freq[1:10,]
        names(freq) <- c("items","count")
        relativo <- c(rel = round(freq$count/total,4))
        
        g <- ggplot(freq, aes(x= reorder(items, -count), y = count))
        
        g + geom_bar(fill="#F5CFDC", stat="identity")+
            geom_text(aes(label=paste(relativo*100,"%",sep = "")), vjust=-0.3, size = 2.5) +
            labs(x = "", y = "") +
            theme(axis.title.y = element_text(size = 10))
        
    })
    
    output$freq_ticket <- renderPlot({
        
        data <- file_basket()
        names(data) <- c("ticket","items")
        
        freq <- data.frame(table(data$ticket)) 
        freq <- freq[order(freq$Freq, decreasing = T),]
        freq <- data.frame(table(freq$Freq))
        freq <- freq[order(freq$Freq, decreasing = T),]
        if(nrow(freq)>10){
            freq <- freq[1:10,]
        }
        names(freq) <- c("size","count")
        total <- sum(freq$count)
        rela <- c(rel = round(freq$count/total,4))
        
        ggplot(freq, aes(x= reorder(size, -count), y= count))+
            geom_text(aes(label=paste(rela*100,"%",sep = "")), vjust = -0.3, size = 2.5) +
            geom_bar(fill="#FE6B6D", stat="identity")+
            labs(x = "", y = "") +
            theme(axis.title.y = element_text(size = 10))
        
    })
    
    
    output$grafo <- visNetwork::renderVisNetwork({
        
        tr <- tr()
        
        support <- 4/dim(tr)[1]
        
        rules <- apriori(data = tr,
                         parameter = list(supp=support, conf=input$conf, minlen = 3),
                         control = list(verbose = FALSE))
        
        rules <- rules[is.maximal(rules)]
        
        plot(top.count, method="graph",engine = "htmlwidget")
        
    })
    
    output$rules <- DT::renderDataTable({
        
        reglas <- reglas()
        
        top.count <- sort(reglas, by = "count", decreasing = T)
        
        cut <- unlist(strsplit(labels(top.count), "=>"))
        
        lhs <- data.frame(lhs = cut[seq(1,length(cut),2)])
        rhs <- data.frame(rhs = cut[seq(2,length(cut),2)])
        igual <- as.data.frame(matrix(nrow=(length(cut)/2), ncol = 1))
        igual[,1] <- "=>"
        names(igual) <- "=>"
        quality <- data.frame(top.count@quality)
        confidence <- data.frame(quality$confidence)
        count <- data.frame(quality$count)
        tabla <- data.frame(lhs, igual, rhs, confidence, count)
        names(tabla) <- c("lhs","=>","rhs","% Conf", "count")
        tabla <- tabla[order(tabla$count, decreasing = T),] %>%
            filter(tabla$count > 3)
        
        datatable(data = tabla, rownames = F) %>%
            formatPercentage(columns = "% Conf", digits = 1)
        
    })
    
    reglas <- reactive({
        trans <- tr()
        soporte <- 4/dim(trans)[1]
        reglas <- apriori(data = trans,
                          parameter = list(support = soporte,
                                           confidence = 0.02,
                                           maxlen = 2,
                                           target = "rules"))
        reglas <- reglas[is.maximal(reglas)]
        sort(reglas, by = "count", decreasing = T)
    })
    
    top <- reactive({
        trans <- tr()
        soporte <- 4/dim(trans)[1]
        itemsets <- apriori(data = trans,
                            parameter = list(support = soporte,
                                             minlen = 2,
                                             maxlen = 5,
                                             target = "frequent itemset"))
        
        itemsets <- itemsets[is.maximal(itemsets)]
        sort(itemsets, decreasing = T)[1:10]
    })
    
    
    output$top10 <- DT::renderDataTable({
        top_10 <- top()
        top_10 <- as(top_10, Class = "data.frame")
        rownames(top_10) <- c()
        top_10$support <- round(top_10$support,4)
        return(top_10)
    })
    
    
    output$top10graf <- renderPlot({
        top_10 <- top()
        as(top_10, Class = "data.frame") %>%
            ggplot(aes(x = reorder(items, count), y= count)) +
            geom_col(fill="#F5CFDC") +
            coord_flip() +
            labs(x = "", y = "") +
            theme_bw()
    })
    
    output$reglas_prob <- DT::renderDataTable({
        
        reglas <- reglas()
        
        metricas <- interestMeasure(x = reglas, measure = c("coverage", "fishersExactTest"),
                                    transactions = trans)
        quality(reglas) <- cbind(quality(reglas), metricas)
        df_reglas <- as(reglas, Class = "data.frame")
        
        df_fishers <- select(.data = df_reglas, rules, confidence, count, fishersExactTest)
        df_fishers <- df_fishers[order(df_fishers$fishersExactTest, decreasing = F),]
        top_10 <- df_fishers[1:10,]
        sec <- seq(1,10,2)
        top_5 <- as.data.frame(matrix(nrow = 0, ncol = 3))
        names(top_5) <- c("rules","confidence","count")
        for(i in sec){
            if(top_10[i,2]>top_10[i+1,2]){
                top_5 <- rbind(top_5, top_10[i,1:3])
            } else {
                top_5 <- rbind(top_5, top_10[i+1,1:3])
            }
        }
        
        rownames(top_5) <- c()
        
        top_5$confidence <- round(top_5$confidence,4)
        
        return(top_5)
        
    })
    
    output$reglas_freq <- DT::renderDataTable({
        
        reglas <- reglas()
        
        metricas <- interestMeasure(x = reglas, measure = c("coverage", "fishersExactTest"),
                                    transactions = trans)
        quality(reglas) <- cbind(quality(reglas), metricas)
        df_reglas <- as(reglas, Class = "data.frame")
        
        df_cov <- select(.data = df_reglas, rules, coverage, count, fishersExactTest)
        df_cov <- df_cov[order(-df_cov$coverage, df_cov$fishersExactTest),]
        df_cov <- df_cov[1:5,] 
        df_cov <- select(.data = df_cov, rules, coverage, count)
        df_cov$coverage <- round(df_cov$coverage,4)
        
        rownames(df_cov) <- c()
        
        return(df_cov)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
