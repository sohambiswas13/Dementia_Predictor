
# Import libraries
library(shiny)
library(data.table)
library(randomForest)
library(tibble)
# Read in the RF model
#setwd('C:\\Users\\Soham\\Documents\\DEMENTIA_PROJ')
v <- readRDS('model1.RDS')



####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Dementia Predictor'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    textInput('M.F', 
                 label = 'Sex', 
                 value = 'Female'),
    textInput("Age", 
                 label = 'Age', 
                 value = 74),
    numericInput("EDUC", 
                 label = 'Years of education', 
                 value = 2),
    numericInput("SES", 
                 label = "Socioeconomic Status", 
                 value = "4"),
    numericInput("MMSE", 
                 label = "Mini Mental State Examination", 
                 value = 29),
    numericInput("eTIV", 
                 label = "Estimated Total Intracranial Volume", 
                 value = 1344),
    numericInput("nWBV", 
                 label = "Normalize Whole Brain Volume", 
                 value = 0.743),
    numericInput("ASF", 
                 label = "Atlas Scaling Factor", 
                 value = 1.306),
    
    actionButton("submitbutton", "Run", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("M.F"  ,"Age"  ,"EDUC" ,"SES" , "MMSE" , "eTIV" ,"nWBV" ,"ASF" ),
      Value = as.character(c(input$M.F,
                             input$Age,
                             input$EDUC,
                             input$SES,
                             input$MMSE,
                             input$eTIV,
                             input$nWBV,
                             input$ASF)),
      stringsAsFactors = FALSE)
    
    input <- transpose(df)
    
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = F)
    
    
    test <- read.csv("input.csv",header = T)
    test <- add_column(test,CDR=as.factor(0),.after="MMSE")
    test$M.F <- as.factor(test$M.F)
    test1 <- data.frame("M.F"=as.factor(c("Male","Female"))  ,"Age"=as.integer(c(74,76))  ,"EDUC"=as.integer(c(2,2)) ,"SES"=as.integer(c(3,3)) , "MMSE"=as.integer(c(29,29)) ,'CDR'=as.factor(c(0,0)), "eTIV"=as.integer(c(1344,1244)) ,"nWBV"=as.numeric(c(0.743,0.87)) ,"ASF"=as.numeric(c(1.306,1.44)) )
    test<- rbind(test,test1)
    r <- predict(v,test)[1]
    
    if(r==0){
      r1 <- "No Dementia"
    }else if(r == 0.5){
      r1 <- "Mild Dementia"
    }else{
      r1 <- "Severe Dementia"
    }
    Output <- data.frame(Prediction=r1, round(predict(v,test,type="prob"), 3))[1,]
    print(Output)
    
  })
  
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete. X0 -> P(No Dementia) ; X0.5 -> P(Mild Dementia) ; X1 -> P(Severe Dementia)") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)

