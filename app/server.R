function(input, output, session) {
  
  rValues <- reactiveValues(params = list(), myData = list())
  
  observeEvent(input$defaultRadio, {
    if("1" == input$defaultRadio) {
      data <- read.table(Data$data.txt,sep="")
      
      k <- length(unique(data[[2]]))
      n <- nrow(data)
      labels <- unique(data[[2]])
      
      params <- Functions$fitGNB(k,n,labels,data)
      yMax <- Functions$getYMax(params$pi,params$mu,params$sigma)
      
      xlim <- c(0.9*min(data[,1]),max(data[,1])*1.1)
      ylim <- c(0,yMax)
      
      rValues$myData <- list(
        data = data,
        n = n,
        k = k,
        labels = labels
      )
      
      rValues$params <- list(
        xlim = xlim,
        ylim = ylim,
        pi = params$pi,
        mu = params$mu,
        sigma = params$sigma
      )
      
    } else {
      showModal(modalDialog(
        title = "Upload files",
        size = "s",
        tags$a(href = "data.zip","Download sample data"),
        fileInput("dataFileInput","Upload data",accept = "text/plain"),
        footer = list(
          actionButton("useFile","Use file"),
          actionButton("cancelFile","Cancel")
        ),
        easyClose = FALSE
      ))
      message <- HTML("Please select a valid text file: 
                        <br><b>first column</b> - input data (real numbers)<br>
                        <b>second column</b> - output data (labels; discrete - any type of labels: strings, numbers, ...)<br>
                        Example of contents of such a file:<br>
                        <code>
                        0 A<br>
                        2 A<br>
                        3 B<br>
                        4 B<br>
                        5 B<br>
                        6 B<br>
                        7 B
                        </code>")
        sendSweetAlert(
          session = session,
          title = "Info",
          text = message,
          type = "info",
          html = TRUE
        )
    }
    
  })
  
  observeEvent(input$useFile, {
    dataFile <- input$dataFileInput
    
    if (is.null(dataFile)) {
      sendSweetAlert(
        session = session,
        title = "Error",
        text = "Data file not uploaded.",
        type = "error"
      )
      return()
    }
    
    data <- try(read.table(dataFile$datapath,sep=""), 
             silent = TRUE)
    if (class(data) == "try-error" || !Functions$checkDataFile(data)) {
      message <- HTML("Please select a valid text file: 
                        <br><b>first column</b> - input data (real numbers)<br>
                      <b>second column</b> - output data (labels; discrete - any type of labels: strings, numbers, ...)<br>
                      Example of contents of such a file:<br>
                      <code>
                      0 A<br>
                      2 A<br>
                      3 B<br>
                      4 B<br>
                      5 B<br>
                      6 B<br>
                      7 B
                      </code>")
      sendSweetAlert(
        session = session,
        title = "Error",
        text = message,
        type = "error",
        html = TRUE
      )
      return()
    }
    
    k <- length(unique(data[[2]]))
    n <- nrow(data)
    labels <- unique(data[[2]])
    
    params <- Functions$fitGNB(k,n,labels,data)
    yMax <- Functions$getYMax(params$pi,params$mu,params$sigma)
    
    xlim <- c(0.9*min(data[,1]),max(data[,1])*1.1)
    ylim <- c(0,yMax)
    
    rValues$myData <- list(
      data = data,
      n = n,
      k = k,
      labels = labels
    )
    
    rValues$params <- list(
      xlim = xlim,
      ylim = ylim,
      pi = params$pi,
      mu = params$mu,
      sigma = params$sigma
    )
    removeModal()
  })
  
  observeEvent(input$cancelFile, {
    updateRadioButtons(session, "defaultRadio",
                       selected = "1"
    )
    removeModal()
  })
  
  output$distPlot <- renderPlot({
    Functions$plotOneIteration(rValues$myData$data[,1],
     rep(0,rValues$myData$n),
     as.numeric(rValues$myData$data[,2]),
      rValues$params$xlim,
      rValues$params$ylim,
      rValues$myData$k,
      rValues$params$pi,
      rValues$params$mu,
      rValues$params$sigma)
  })
  
  output$dataParamsText <- renderPrint({
    cat("Input data:",rValues$myData$data[[1]],"\n")
    cat("Output data:",as.character(rValues$myData$data[[2]]),"\n")
    print(rValues$params)
  })
  
  output$latex <- renderUI({
    pi <- rValues$params$pi
    mu <- rValues$params$mu
    sigma <- rValues$params$sigma
    k <- rValues$myData$k
    
    text <- "$$"
    for(i in 1:k) {
      text <- paste0(text,pi[i],"\\cdot \\mathcal{N}(x|",mu[i],",",sigma[i],"^2)")
      if(i != k) {
        text <- paste0(text," + ")
      }
    }
    text <- paste0(text,"$$")
    withMathJax(
      helpText(text)
    )
  })
  
  output$predictedLabel <- renderPrint({
    x <- input$numberToPredict
    
    if(is.null(x) || length(x) == 0 || x == "") {
      return()
    }
    
    if(is.na(as.numeric(x))) {
      message <- HTML("Please type a number.")
      sendSweetAlert(
        session = session,
        title = "Error",
        text = message,
        type = "error",
        html = TRUE
      )
      return()
    }
    
    x <- as.numeric(x)
    Functions$predictGNB(rValues$myData$labels,
                               rValues$myData$n,
                               rValues$myData$k,
                               x,
                               rValues$params$pi,
                               rValues$params$mu,
                               rValues$params$sigma)
  })
  
}
