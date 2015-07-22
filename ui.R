# start user interface ----
library(shiny)


shinyUI(fluidPage(
    
    sidebarLayout(
        
        sidebarPanel(width = 3,
            
            selectInput("help", 
                        label = em("Help",style="color:#004700"),
                        choices = c("Architecture","Summary", "Video","Datasets","--"),
                        selected = "--",selectize=FALSE),
                          
                          br(),
            
# Giving the user the option to select rainfall data =====

        selectInput("dataset", 
                                  label = tags$h4(strong(em("Predictand",style="color:blue;text-align:center"))),
                                  choices = c("CHIRPS", "CRU","--"),
                                  selected = "--"),

# Giving the user the option to upload rainfall data =====
            fileInput('file1', strong('Upload Predictand',style="color:blue"),
                      accept=c('text/csv','text/comma-separated-values,text/plain', '.csv')),
            

# starting and end dates =====                   
           dateInput("startdate", "Starting year & month (yyyy-mm):",value ="1948-01", min ="1948-01",max="2013-12",format="yyyy-mm"),
                      
           dateInput("enddate", "Ending year & month (yyyy-mm):", value ="2013-12",min ="1948-01-01",max="2013-12",format="yyyy-mm"),
                      
                      
                      br(),
                      
                      
                  

# which rainfall summaries do you want to look at? ====   
    selectInput("pcpstat", 
                label = em("Summary of Precipitation Over Selected Region",style="font-size:14px;color:darkblue"),
                choices = c("Climatology", "Boxplot","Trend","Histogram","--"),
                selected = "--",selectize=FALSE),
                  
      br(), 
    
   

# For which months do you want to predict? =====    
    selectInput("months", 
                label = em("Select Months of Interest",style="color:#004700"),
                choices = c("January", "February","March","April","May","June","July","August","September","October","November","December"),
                selected = "January",selectize=FALSE,multiple=TRUE),
    
    br(),
    

# How long in advance do you want to predict? ====   
    sliderInput("leadmonths",
                em("Lead Time in Months for Prediction",style="color:#004700;background-color:#FFFFB2"),
                min = 0,
                max = 6,
                value = 0),
    br(),
    

# Predictors you can select from ====  
    br(),
    h4(strong(tags$u(em("Choose Predictors",style="color:#7A00CC;background-color:#CCFFB2")))),
    
    br(),
    
    checkboxInput("sst", 
                label = strong("Sea Surface Temperature",style="color:#CC0000")),
    br(),
    
    checkboxInput("slp", label = strong("Mean Sea Level Pressure",style="color:#CC0000")),
    
    br(),
    
    
    selectInput("phgt", 
                label = strong("Geopotential Pressure Level",style="color:#CC0000"),
                choices = c("--","1000", "925","850","700","600","500","400","300","250","200","150"),
                selected = "--",selectize=FALSE,multiple=TRUE),
    
    
    br(),
    
    selectInput("pwind", 
                label = strong("Wind Pressure Level",style="color:#CC0000"),
                choices = c("--","1000", "925","850","700","600","500","400","300","250","200","150"),
                selected = "--",selectize=FALSE,multiple=TRUE),
    
    br(),
    
    
    selectInput("pair", 
                label = strong("Air Temperature Pressure Level",style="color:#CC0000"),
                choices = c("--","1000", "925","850","700","600","500","400","300","250","200","150"),
                selected = "--",selectize=FALSE,multiple=TRUE),
    
    
    br(),
    
    
    
    br(),

# Do you have your own predictor? Upload it ====
    fileInput('file1', strong('Upload Predictor',style="color:blue"),
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    
    br(),
  
    

# preprocessing options ====  

    selectInput("prep", 
                label = strong(em("Preprocessing Options",style="color:#CC0000")),
                choices = c("Detrend", "Standardize","PCA"),
                selected = "Standardize",selectize=FALSE,multiple=TRUE),
    br(),
    tags$hr(style="height:1.5px;border:none;color:#66FF66;background-color:#999966"),
    br(),
    

# choose models ====

    h4(strong(tags$u(em("Select Models",style="color:blue")))),
    selectInput("models", 
                label = "",
                choices = c("GLM", "SGLM","GAM","SGAM","Bagging","RF","Boosting","SVM","ANN","MARS","ALL"),
                selected = "ALL",selectize=FALSE,multiple=TRUE,size=12),
    
    br(),

# choose model results to see ====
    selectInput("overview", 
                label = em("Quick Overview of Model Reults",style="color:#004700"),
                choices = c("Summary", "TrainTest","ModelPlot","--"),
                selected = "--",selectize=FALSE),
    br(),

# Enables shiny to wait until you click this ====

    actionButton("submit",em(strong("Submit",style="color:blue;background-color:#FFFF66;font-size:120%"))),
    
    br(),
    
    tags$hr(style="height:1.5px;border:none;color:#66FF66;background-color:#999966"),
    
    br(),
    
# Download full analysis and prediction steps

    checkboxInput("present", em("Download results",style="font-size:14px;color:darkblue"), FALSE),
    
    
    br(),
    br(),
    br(),
    br()
                      ),
        

# help ====        
        
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
            condition = "input.help == 'Architecture'",
            
            uiOutput("app")),
            
            conditionalPanel(
                condition = "input.help == 'Datasets'",
            
            tableOutput("dataatt")),
            
            
        ## help: end
        
        
# Map for zooming in and region selection =====
        
            plotOutput("selectRegion",dblclick='plot_dblclick', click = "plot_click",width = "100%", height = "400px"),
            

# Summary of rainfall over selected region =====
            
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
                      # histogram of rainfall for selected month(s)
                plotOutput('histogram',width = "100%", height = "400px")),
            
  ## Summary of rainfall over selected region: end
  
    

# Correlation map of SST with rainfall over selected region ====
  
            plotOutput("distPlotsst", click = "plot_clicksst",width = "100%", height = "400px"),
            

# Correlation map of SLP with rainfall over selected region ====

            plotOutput("distPlotslp", click = "plot_clickslp",width = "100%", height = "400px"),
            

# Correlation map of wind with rainfall over selected region ====

# Correlation map of geopotential with rainfall over selected region ====

# Correlation map of air temperature with rainfall over selected region ====

# Model output ====
  
  conditionalPanel(
      condition = "input.overview == 'Summary'",
      
      verbatimTextOutput("summary")),
  
  
  conditionalPanel(
      condition = "input.overview == 'TrainTest'",
      plotOutput("fitpred")),
  
  conditionalPanel(
      condition = "input.overview == 'ModelPlot'",
      plotOutput("qqplot"))
  
  

  
  
  
             ))))

