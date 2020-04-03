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
library(htmlwidgets)
library(htmltools)


options(shiny.maxRequestSize=30*1024^2)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
        titlePanel("Market Basket Analysis"),
        
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
                                    max = 0.1,
                                    value = .01,
                                    step = .01)
                ),
                
                mainPanel(
                        tabsetPanel(
                           type = "tabs",
                           tabPanel(
                               title = "Previsualización",
                               tabsetPanel(
                                    type = "tabs",
                                    tabPanel(title = "Tabla Cargada",DT::dataTableOutput(outputId = "datacsv")),
                                    tabPanel(title = "Seleccionados",DT::dataTableOutput(outputId = "basket_file"))
                                )
                           ),
                           tabPanel(
                               title = "Análisis",
                               plotOutput("freq", width = "100%"),
                               visNetwork::visNetworkOutput("grafo"),
                               dataTableOutput("rules"))

                        )
                  )
        )
)

# SERVER
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
    
    output$datacsv <- DT::renderDataTable({
            data <- df_upload()
            DT::datatable(data)
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
   
    output$freq <- renderPlot({
                
                data <- file_basket()
                names(data) <- c("ticket","items")
                
                freq <- data.frame(table(data$items)) 
                freq <- freq[order(freq$Freq, decreasing = T),]
                total <- sum(freq$Freq)
                freq <- freq[1:10,]
                names(freq) <- c("items","count")
                relativo <- c(rel = round(freq$count/total,4))
                
                g <- ggplot(freq, aes(x= reorder(items, -count), y = count))
                
                trans <- g + geom_bar(fill="#F5CFDC", stat="identity")+
                        geom_text(aes(label=paste(relativo*100,"%",sep = "")), vjust=-0.3, size = 2.5) +
                        labs(title = "Frecuencia de ITEMS", x = "", y = "") +
                        theme(axis.title.y = element_text(size = 10))
                
                ##Gráfica de ticket
                ## Aquí debería de iniciar con un table(input$ticket)
                
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
                
                ticket <- ggplot(freq, aes(x= reorder(size, -count), y= count))+
                        geom_text(aes(label=paste(rela*100,"%",sep = "")), vjust = -0.3, size = 2.5) +
                        geom_bar(fill="#FE6B6D", stat="identity")+
                        labs(title = "Frecuencia tamaños de tickets") +
                        theme(axis.text.x = element_blank(), axis.text.y = element_blank(), axis.title.y = element_text(size = 10))
                
                ggarrange(trans, ticket , ncol = 2, nrow = 1, align = "v", widths = c(2,1))
                
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
    
    output$grafo <- visNetwork::renderVisNetwork({
        
        tr <- tr()
        
        support <- 4/dim(tr)[1]
        
        rules <- apriori(data = tr,
                         parameter = list(supp=0.0001, conf=input$conf, minlen = 3),
                         control = list(verbose = FALSE))
        
        rules <- rules[is.maximal(rules)]
        
        top.count <- sort(rules, by="count", decreasing=T)
        
        plot(top.count, method="graph",engine = "htmlwidget")
        
    })
    
    output$rules <- renderDataTable({
        
        tr <- tr()
        
        support <- 4/dim(tr)[1]
        rules <- apriori(data = tr, 
                         parameter = list(supp=support, conf=input$conf, maxlen = 2),
                         control = list(verbose = FALSE))
        
        top.count <- sort(rules, by="count", decreasing=T)
        
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
}
                   
# Run the application 
shinyApp(ui = ui, server = server)

