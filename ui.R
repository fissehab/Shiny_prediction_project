
library(shiny)


shinyUI(fluidPage(
    
    sidebarLayout(
        
        sidebarPanel( 
            
            selectInput("help", 
                        label = em("Help",style="color:#004700"),
                        choices = c("Summary", "Video","Datasets","--"),
                        selected = "--",selectize=FALSE),
                          
                          br(),
            
            
        
                       
            h4(strong(em("Predictand",style="color:blue;text-align:center"))),
                      selectInput("dataset", 
                                  label = "Choose Dataset",
                                  choices = c("CHIRPS", "CRU","--"),
                                  selected = "--"),
            
            fileInput('file1', strong('Upload Predictand',style="color:blue"),
                      accept=c('text/csv','text/comma-separated-values,text/plain', '.csv')),
            
                      
           dateInput("startdate", "Starting period (yyyy-mm):",value ="1948-01", min ="1948-01",max="2013-12",format="yyyy-mm"),
                      
           dateInput("enddate", "Ending period (yyyy-mm):", value ="2013-12",min ="1948-01-01",max="2013-12",format="yyyy-mm"),
                      
                      
                      br(),
                      
                      
                  
    
    selectInput("pcpstat", 
                label = em("Precipitation Over Selected Region",style="font-size:14px;color:darkblue"),
                choices = c("Climatology", "Boxplot","Trend","Histogram","--"),
                selected = "--",selectize=FALSE),
                  
      br(), 
    
   
    
    selectInput("months", 
                label = em("Select Months of Interest",style="color:#004700"),
                choices = c("January", "February","March","April","May","June","July","August","September","October","November","December"),
                selected = "January",selectize=FALSE,multiple=TRUE),
    
    br(),
    
    
    sliderInput("leadmonths",
                em("Lead Time in Months",style="color:#004700;background-color:#FFFFB2"),
                min = 0,
                max = 6,
                value = 0),
    br(),
    
    br(),
    
    tags$hr(style="height:1.5px;border:none;color:#66FF66;background-color:#999966"),
    
    br(),
    h4(strong(tags$u(em("Supply Predictors",style="color:#7A00CC;background-color:#CCFFB2")))),
    
    br(),
    
    selectInput("sst", 
                label = strong("Sea Surface Temperature",style="color:#CC0000;background-color:lightgrey"),
                choices = c("select", "--"),
                selected = "--"),
    
    br(),
    
    
    selectInput("slp", 
                label = strong("Mean Sea Level Pressure",style="color:#CC0000;background-color:lightgrey"),
                choices = c("select", "--"), selected = "--"),
    br(),
    
    
    selectInput("phgt", 
                label = strong("Select Pressure Level for HGT",style="color:#CC0000;background-color:lightgrey"),
                choices = c("--","1000", "925","850","700","600","500","400","300","250","200","150"),
                selected = "--",selectize=FALSE,multiple=TRUE),
    
    
    br(),
    
    selectInput("pwind", 
                label = strong("Select Pressure Level for wind",style="color:#CC0000;background-color:lightgrey"),
                choices = c("--","1000", "925","850","700","600","500","400","300","250","200","150"),
                selected = "--",selectize=FALSE,multiple=TRUE),
    
    br(),
    
    
    selectInput("pair", 
                label = strong("Select Pressure Level for airT",style="color:#CC0000;background-color:lightgrey"),
                choices = c("--","1000", "925","850","700","600","500","400","300","250","200","150"),
                selected = "--",selectize=FALSE,multiple=TRUE),
    
    
    br(),
    
    
    
    br(),

    
    
  
    
    fileInput('file1', strong('Upload predictand',style="color:#CC0000;background-color:lightgrey"),
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    
    br(),
    br(),
    
    
    selectInput("prep", 
                label = strong(em("Preprocessing Options",style="color:#CC0000;background-color:lightgrey")),
                choices = c("Detrend", "Standardize","PCA"),
                selected = "Standardize",selectize=FALSE,multiple=TRUE),
    br(),
    tags$hr(style="height:1.5px;border:none;color:#66FF66;background-color:#999966"),
    br(),
    
    h4(strong(tags$u(em("Select Algorithms",style="color:blue;background-color:#FFFF66")))),
    selectInput("models", 
                label = "",
                choices = c("GLM", "SGLM","GAM","SGAM","Bagging","RF","Boosting","SVM","ANN","MARS","ALL"),
                selected = "ALL",selectize=FALSE,multiple=TRUE,size=12),
    
    br(),
    
    selectInput("overview", 
                label = em("Quick Overview of Model Reults",style="color:#004700"),
                choices = c("Summary", "TrainTest","ModelPlot","--"),
                selected = "--",selectize=FALSE),
    br(),
    
    actionButton("submit",em(strong("Submit",style="color:blue;background-color:#FFFF66;font-size:120%"))),
    
    br(),
    
    tags$hr(style="height:1.5px;border:none;color:#66FF66;background-color:#999966"),
    
    br(),
    
    
    checkboxInput("present", em("Download results",style="font-size:14px;color:darkblue"), FALSE),
    
    
    br(),
    br(),
    br(),
    br()
                      ),
        
        
        
        mainPanel(
             br(),
            h5(em(strong("Semi-Automated Interactive Prediction Models", style="color:darkblue;font-size:210%")),align = "center"),
            
        ## help: start
            
            conditionalPanel(
                 condition = "input.help == 'Summary'",

             p(textOutput("help"))),
            
            conditionalPanel(
                condition = "input.help == 'Video'",
        
              uiOutput("tb")),
            
            conditionalPanel(
                condition = "input.help == 'Datasets'",
            
            tableOutput("dataatt")),
            
            
        ## help: end
        
                  # map for zooming in and region selection
            plotOutput("selectRegion",dblclick='plot_dblclick', click = "plot_click",width = "100%", height = "400px"),
            
        
            
        
    ## Summary of rainfall over selected region: start
            
            conditionalPanel(
                condition = "input.pcpstat == 'Climatology'", 
                       # rainfall climatology
                plotOutput('climatology',width = "100%", height = "400px")),
            
            conditionalPanel(
                condition = "input.pcpstat == 'Trend'",
                      #trend of rainfall over selected region
                plotOutput('trend',width = "100%", height = "400px")),
            
            
            conditionalPanel(
                condition = "input.pcpstat == 'Boxplot'",
                      #boxplot of rainfall for selected month(s)
                plotOutput('boxplot',width = "100%", height = "400px")),
            
    
            conditionalPanel(
                condition = "input.pcpstat == 'Histogram'",
                      #histogram of rainfall for selected month(s)
                plotOutput('histogram',width = "100%", height = "400px")),
            
  ## Summary of rainfall over selected region: end
  
    
            #Correlation map of SST with rainfall over selected region
            plotOutput("distPlotsst",dblclick='plot_dblclicksst', click = "plot_clicksst",width = "100%", height = "400px"),
            
            #Correlation map of SLP with rainfall over selected region
            plotOutput("distPlotslp",dblclick='plot_dblclickslp', click = "plot_clickslp",width = "100%", height = "400px"),
            
            
  
  
  ## models summary: start
  
  conditionalPanel(
      condition = "input.overview == 'Summary'",
      
      verbatimTextOutput("summary")),
  
  
  conditionalPanel(
      condition = "input.overview == 'TrainTest'",
      plotOutput("fitpred")),
  
  conditionalPanel(
      condition = "input.overview == 'ModelPlot'",
      plotOutput("qqplot"))
  
  
  
  ## models summary: end
  
  
  
             ))))

