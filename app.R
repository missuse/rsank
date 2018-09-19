library(plotly)
library(shiny)
library(rpart)
library(rpart.utils)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$dataset)
    read.csv(input$dataset$datapath,
             header = input$header,
             sep = input$sep)
  })
  
  observeEvent(data(), {
    updateSelectInput(session,
                      "y",
                      choices = colnames(data()),
                      selected = colnames(data()[which(sapply(data(),
                                                              is.factor))[1]]))
  })
  
  observeEvent(data(), {
    updateSelectInput(session, "X",
                      choices = colnames(data()))
  })
  
  observeEvent(data(), {
    updateSelectInput(session,
                      "weights",
                      choices = colnames(data()))
  })
  
  
  filtereddata <- eventReactive({
    validate(need(input$dataset != "","Please select a data set in csv format only!!!"))# custom error message on opening the app
    input$update
  }, {
    req(data())
    if(is.null(input$X) || input$X == "" || is.null(input$y) || input$y ==""){
      data()
    } else {
      data()[, colnames(data()) %in% c(input$X, input$y)]
    }
  }
  )
  
  weights <- eventReactive({
    input$update
  }, {
    req(data())
    if(is.null(input$weights) || input$weights == "") {
      rep(1,
          nrow(filtereddata()))
    } else
      data()[,
             colnames(data()) == input$weights]
  }
  )
  
  output$plot <- renderPlotly({
    fit <- rpart(as.formula(paste(input$y, "~ .")),
                 data = filtereddata(),
                 method = "class",
                 weights = weights(),
                 parms = list(
                   split = input$split),
                 control = rpart.control(minsplit = input$minsplit,
                                         minbucket = input$minbucket,
                                         cp = input$cp,
                                         xval = 0))
    treeFrame <- fit$frame
    
    isLeave <- treeFrame$var == "<leaf>"
    nodes <- rep(NA, length(isLeave))
    ylevel <- attr(fit, "ylevels")
    nodes[isLeave] <- ylevel[treeFrame$yval][isLeave]
    nodes[!isLeave] <- labels(fit)[-1][!isLeave[-length(isLeave)]]
    
    treeRules <- rpart.utils::rpart.rules(fit)
    
    targetPaths <- sapply(as.numeric(row.names(treeFrame)),function(x)  
      strsplit(unlist(treeRules[x]),split=","))
    
    lastStop <-  sapply(1:length(targetPaths),function(x) targetPaths[[x]] 
                        [length(targetPaths[[x]])])
    
    oneBefore <-  sapply(1:length(targetPaths),function(x) targetPaths[[x]] 
                         [length(targetPaths[[x]])-1])
    
    target <- c()
    source <- c()
    values <- treeFrame$n
    for(i in 2:length(oneBefore)) { 
      tmpNode <- oneBefore[[i]]
      q <- which(lastStop == tmpNode)
      
      q <- ifelse(length(q) == 0,1,q)
      source <- c(source,q)
      target <- c(target,i)
    }
    source <- source - 1
    target <- target - 1
    
    p <- plot_ly(
      type = "sankey",
      orientation = "h",
      
      node = list(
        label = nodes,
        pad = 15,
        thickness = 20,
        line = list(
          color = "black",
          width = 0.5
        )
      ),
      
      link = list(
        source = source,
        target = target,
        value = values[-1]
        
      )
    ) %>% 
      layout(
        title = "Sankey Diagram",
        font = list(
          size = 10
        )
      )
    p
  })
}

ui <- fluidPage(
  titlePanel("Visualise rpart models with Sankey graphs"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("dataset", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
      
      helpText(em("Note:Select the inputs and click on button as given below to exectute the app")),
      
      checkboxInput("header", "Header", TRUE),
      
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      selectInput(
        "y",
        "select y",
        choices = ""
      ),
      selectInput(
        "X",
        "select X",
        choices = "",
        multiple = T
      ),
      selectizeInput(
        "weights",
        "Weights",
        choices = "",
        options = list(
          placeholder = 'Please select an option below',
          onInitialize = I('function() { this.setValue(""); }')
        )
      ),
      numericInput("minsplit",
                   "minsplit",
                   value = 20,
                   step = 1,
                   min = 2),
      numericInput("minbucket",
                   "minbucket",
                   value = 7,
                   step = 1,
                   min = 1) ,
      numericInput("cp",
                   "cp",
                   value = 0.01,
                   step = 0.001,
                   min = 0,
                   max = 1) ,
      radioButtons("split", "Split",
                   choices = c(Information = "information",
                               Gini = "gini"),
                   selected = "gini"),
      actionButton("update",
                   "fit model",
                   class = "btn-primary",
                   style='padding:4px; font-size:100%')
    ),
    
    mainPanel(
      plotly::plotlyOutput('plot', height = "100%")
      
    )
  )
)



shinyApp(ui = ui, server = server)
