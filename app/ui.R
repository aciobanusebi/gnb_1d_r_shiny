fluidPage(
  
  # mandatory
  Variables$toIncludeList,
  
  # Application title
  titlePanel("Gaussian (Naive) Bayes 1D"),
  tags$p("Unfortunately, it can give NaN/Inf results..."),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      radioButtons("defaultRadio", label = h3("Input"),
                   choices = list("Default example" = 1, "Upload your own files" = 2), 
                   selected = 1),
      verbatimTextOutput("dataParamsText"),
      textInput("numberToPredict","Predict class label for:"),
      verbatimTextOutput("predictedLabel")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tags$h2("The pdfs are multiplied by their selection probability and then plotted:"),
      tags$style(HTML("
                  #latex {
                      width:100%;
                      overflow-x:scroll
                      }
                      ")),
      uiOutput("latex"),
      plotOutput("distPlot",height = "570px")
    )
  )
)
