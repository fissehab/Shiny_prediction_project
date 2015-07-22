# Souces of datasets used ----
 # shapefile: http://baruch.cuny.edu/geoportal/data/esri/world/continent.zip
 # Geopotential: http://apps.ecmwf.int/datasets/data/interim-full-invariant/
 # SST:  http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.html
 # SLP, HGT, UWND, VWND, AirT:  http://www.esrl.noaa.gov/psd/data/gridded/data.ncep.reanalysis.html
 # CHG rainfall:  http://chg.geog.ucsb.edu/data/chirps/
 # CRU rainfall:  http://www.cru.uea.ac.uk/data
# Load libraries ====

library(shiny)

library(chron)
library(RColorBrewer)
library(lattice)
library(lubridate)
library(ncdf)
library(ggplot2)
library(caret)
library(maptools)
library(sp)
library(fields)
# helper functions ====

source("lonlat_indices.R")
source("nearestIndx.R")
# Load data sets  ====

data(wrld_simpl)
cont <- readShapeSpatial("data/continent.shp")

era.cdf <- open.ncdf("data/era_hgt.cdf")
era <- get.var.ncdf(era.cdf, "HGT") 
era.cdf$dim$LON$vals -> lon_era
era.cdf$dim$LAT$vals -> lat_era

sst.cdf <- open.ncdf("data/sst.mnmean2.v4.nc")
sst <- get.var.ncdf(sst.cdf, "SST") 
sst.cdf$dim$LON$vals -> lon_sst
sst.cdf$dim$LAT$vals -> lat_sst
sst.cdf$dim$TIME$vals -> time_sst
time_sst<-as.Date(time_sst, origin = "1800-01-01")
time_sst<-update(time_sst,days=01)

cru.cdf <- open.ncdf("data/cru_ts3.22.1948.2013.pre.dat1by1.nc")
cru <- get.var.ncdf(cru.cdf, "PRE") 
cru.cdf$dim$LON$vals -> lon_cru
cru.cdf$dim$LAT$vals -> lat_cru
cru.cdf$dim$TIME$vals -> time_cru
time_cru<-as.Date(time_cru,origin = "1900-01-01")
time_cru<-update(time_cru,days=01)

ch.cdf <- open.ncdf("data/chirps_1981_2015_monthly_course.cdf")
ch <- get.var.ncdf(ch.cdf, "PRE") 
ch.cdf$dim$LON$vals -> lon_ch
ch.cdf$dim$LAT$vals -> lat_ch
ch.cdf$dim$TIME$vals -> time_ch
time_ch<-as.Date(time_ch,origin = "1980-01-01")
time_ch<-update(time_ch,days=01)


slp.cdf<-open.ncdf("data/slp.mon.mean.nc")
slp<-get.var.ncdf(slp.cdf, "SLP") 
slp.cdf$dim$LON$vals -> lon_slp
slp.cdf$dim$LAT$vals -> lat_slp
time_slp = seq(as.Date("1948-01-01"), as.Date("2015-06-30"), by="1 month")


close.ncdf(era.cdf)
close.ncdf(cru.cdf)
close.ncdf(sst.cdf)
close.ncdf(slp.cdf)

# start of shinyServer function ====
shinyServer(function(input, output) {
 

    lon_l <- reactive({  
        lon_index<-which(lon_era==round(double_clicked$center[1],0))
        lonl(lon_index)
        })
    
    lon_r <- reactive({  
        lon_index<-which(lon_era==round(double_clicked$center[1],0))
        lonr(lon_index)
    
    })
    
    
    lat_s <- reactive({  
        lat_index<-which(lat_era==round(double_clicked$center[2],0))
        lats(lat_index)
    })
    
    lat_n <- reactive({  
        lat_index<-which(lat_era==round(double_clicked$center[2],0))
        latn(lat_index)
    })
    
  
  output$selectRegion <- renderPlot({
      
      mypalette<-brewer.pal(3,"BuGn")
      
    
    if(!exists("selected") | length(selected$coords)<=3){
            
              if (length(double_clicked$center)==2) {
                 
                  image.plot(lon_era[lon_l():lon_r()],lat_era[lat_s():lat_n()],
                             era[lon_l():lon_r(),lat_s():lat_n()],
                             col = mypalette,xlab ="Longitude",
                             ylab="Latitude",legend.width=0.001, legend.shrink=.001,
                             legend.mar=0.001,legend.cex=0.0,horizontal=T)
                  
                  
                  plot(wrld_simpl, add=TRUE, axes=TRUE, bg='azure2', 
                       col='khaki',border='#AAAAAA', lwd=0.4)
                  
                }
               
                
        else {
                    
                   
                   image.plot(lon_era,lat_era,era,
                              col = mypalette,xlab ="Longitude",
                              ylab="Latitude",legend.width=0.001, legend.shrink=.001,
                              legend.mar=0.001,legend.cex=0.0,horizontal=T)
                   
                   
        plot(wrld_simpl, add=TRUE, axes=TRUE, bg='azure2', 
                        col='khaki',border='#AAAAAA', lwd=0.4)
                   
                    
                   }
             }
      
     
      else if (length(selected$coords)>=4 
               & length(selected$coords)%%2==0){
          
          if (length(double_clicked$center)==2) {
              
              image.plot(lon_era[lon_l():lon_r()],lat_era[lat_s():lat_n()],
                         era[lon_l():lon_r(),lat_s():lat_n()],
                         col = mypalette,xlab ="Longitude",
                         ylab="Latitude",legend.width=0.001, legend.shrink=.001,
                         legend.mar=0.001,legend.cex=0.0,horizontal=T)
              
              
    plot(wrld_simpl, add=TRUE, axes=TRUE, bg='azure2', 
                   col='khaki',border='#AAAAAA', lwd=0.4)
              
            
            
            z<-selected$coords
            for (i in seq(1,length(z),4)){
                a<-z[c(i,i+2,i+2,i,i)]
                b<-z[c(i+1,i+1,i+3,i+3,i+1)]
                
                lines(a,b,
                      type='l',col="blue",lwd=2)   
        
        
               }}
          
          
          else {
              
              
              image.plot(lon_era,lat_era,era,
                         col = mypalette,xlab ="Longitude",
                         ylab="Latitude",legend.width=0.001, legend.shrink=.001,
                         legend.mar=0.001,legend.cex=0.0,horizontal=T)
              
              
      plot(wrld_simpl, add=TRUE, axes=TRUE, bg='azure2', 
                   col='khaki',border='#AAAAAA', lwd=0.4)
              
                
              z<-selected$coords
              
              for (i in seq(1,length(z),4)){
                  a<-z[c(i,i+2,i+2,i,i)]
                  b<-z[c(i+1,i+1,i+3,i+3,i+1)]
                  
                  lines(a,b,
                        type='l',col="blue",lwd=2)   
                  
                  
              }}
      }
          
      
  })
  
  
# help ====
  
  output$help <- renderText({
      
      'This Package helps to predict precipitation 
      over any geographic region of interest. The user can extract predictand by drawing 
      a polygon over a region. They can also upload a time series rainfall data. 
      Then, they select data sets to extract features from.  They can also upload their own predictors. 
      The package generates correlations of the variables selected with the predictand. 
      The user also has the option to generate composites of the variables.
      Next, the user can extract predictors by drawing polygons over the regions
      that show strong correlations (composites).
      Then, the user can select some or all of the machine learning algorithms provided.
      Finally, the user can download a presentation of 
      the results in PDF or HTML format.
      
      (1) Choose predictand 
      
      (2) Click two corners along a diagonal of a rectangular region of your interest  
      
      (3) If you want to regionalize the selected region, choose a regionalization method
      
      (4) If you regionalized your selected region, select sub-regions 
      
      (5) You can also upload your own predictand
      
      (6) Select the months that you want to predict for 
      
      (7) Specify lead time and feature selection method 
      
      (8) Extract features by drawing polygons over the regions that show stronger correlations (composites)
      (9) Choose algothims(s)'
      
      
  })
  
  
  output$dataatt <- renderTable({
      
      dataatt=list()
      dataatt$Dataset=c('CRU','CHIRPS','SST','SLP','HGT','UWND','VWND','AIR')
      dataatt$Start=c('Jan-1948','Jan-1981','Jan-1948','Jan-1948','Jan-1948','Jan-1948','Jan-1948','Jan-1948')
      dataatt$End=c('Dec-2013','May-2015','Present','Present','Present','Present','Present','Present')
      dataatt$Resolution=c('0.5','0.25','2','2.5','2.5','2.5','2.5','2.5')
      dataatt$Coverage=c('Global Landmass','50N-50S','Global','Global','Global','Global','Global','Global')
      
      data.frame(dataatt)
  })
  
  
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
      
      tags$video(src='demo.mp4',
                 type="video/mp4", width="600px", height="350px", controls="controls")
  }) 
  
  
  output$app <- renderUI({
      
      tags$img(src='flow.png',
               width="780px", height="390px")
  }) 
  
  
  
# Zooming in on the map ==== 
  
  double_clicked <- reactiveValues(
      
      center = NULL 
  )
  
  # Handle clicks on the plot
  
  observeEvent(input$plot_dblclick, {
      
      double_clicked$center <- c(input$plot_dblclick$x,input$plot_dblclick$y)
      
  })
  
  
  
  
# Save mouse clicks for rainfall extraction ==== 
  
  selected <- reactiveValues(
    coords = NULL 
  )
  
          # Handle clicks on the base plot
  observeEvent(input$plot_click, {
      m<-c(input$plot_click$x,input$plot_click$y)
      selected$coords <- c(selected$coords,m)
  })
  
  

# selected time period ====
  
  monthsPredictor <- reactive({
      
      list_months<- c("January" ,"February","March","April","May",
                      "June","July","August","September","October","November",
                      "December")  
      
      selected<-input$months
      
      indices<-seq(1,12)
      
      for (i in seq(1,length(selected))){
          
          a<-which(list_months==selected[i])
          selected[i]=indices[a]
      }
      as.numeric(selected)-input$leadmonths
      
  })
  
  
  monthsPredictand <- reactive({
      
      list_months<- c("January" ,"February","March","April","May",
                      "June","July","August","September","October","November",
                      "December")  
      
      selected<-input$months
      
      indices<-seq(1,12)
      
      for (i in seq(1,length(selected))){
          
          a<-which(list_months==selected[i])
          selected[i]=indices[a]
      }
      as.numeric(selected)
      
  })
  
  
  ## Lead time
  
  leadtime<-reactive({
      input$leadmonths
  })
  
  
  ### months selected 
  
  month_indicespredictand<-reactive({
      indices= matrix(seq(1,length(pcp())),nrow=12,ncol=length(pcp())/12)
      indices[monthsPredictand(),]
      
  })
  
  
  month_indicespredictor<-reactive({
      indices= matrix(seq(1,length(pcp())),nrow=12,ncol=length(pcp())/12)
      indices[monthsPredictor(),]
      
  })
  
  ####
  
  startingdatecru<-reactive({
      
      which(time_cru==update(input$startdate, days=01))
  })
  
  endingdatecru<-reactive({
      which(time_cru==update(input$enddate, days=01))
  })
  
  
  startingdatech<-reactive({
      
      which(time_ch==update(input$startdate, days=01))
  })
  
  endingdatech<-reactive({
      which(time_ch==update(input$enddate, days=01))
  })
  

#  selected rainfall data ----

datasetInputpre <- reactive({
    switch(input$dataset,
           "CHIRPS" = ch,
           "CRU" = cru,
           "--"=c())
})
  
  
  
  
# Extract rainfall data over the selected region ----

 pcp<-reactive({
     
    if (!is.null(input$file1)){
        
        read.csv(inFile$datapath)
        }
       else {
          dataset <- datasetInputpre()
 
             if(!is.null(dataset) & length(selected$coords==4)){
                 if(input$dataset=="CRU"){
                     
     
     lonl<-nearestIndx(selected$coords[1],lon_cru)
     lonr<-nearestIndx(selected$coords[3],lon_cru)
     lats<-nearestIndx(selected$coords[2],lat_cru)
     latn<-nearestIndx(selected$coords[4],lat_cru)
     
     apply(dataset[lonl:lonr,lats:latn,startingdatecru():endingdatecru()],3,mean,na.rm=T)
                 }
    else if(input$dataset=="CHIRPS"){
                     
        lonl<-nearestIndx(selected$coords[1],lon_ch)
        lonr<-nearestIndx(selected$coords[3],lon_ch)
        lats<-nearestIndx(selected$coords[2],lat_ch)
        latn<-nearestIndx(selected$coords[4],lat_ch)
      
      apply(dataset[lonl:lonr,lats:latn,startingdatech():endingdatech()],3,mean,na.rm=T)
                 }
             }
       }
})
    

# Rainfall trend over selected region ----

output$trend <- renderPlot({

        if (!is.null(pcp())){
             
            rain<-pcp()
             
             years<-seq(year(input$startdate),year(input$enddate))
             
             annual<-apply(matrix(rain,ncol=length(rain)/12,nrow=12),2,mean,na.rm=T)
              
             pcp=data.frame('year'=years, 'rainfall'=annual)
             
             g=ggplot(pcp, aes(x=year, y=rainfall))
             p=g+geom_line(aes(group=1),color='darkblue',alpha=0.5)
             
             p+ylab('Rainfall (mm/month)')+
                 theme(text = element_text(size=17, color='darkred'),
                       axis.text.x = element_text(angle=45, vjust=0.5))+ theme(axis.title.x = element_blank())+
                 theme(axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=1,face="plain"),
                       axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),
                       axis.text.x = element_text(colour="grey20",size=14,angle=45,hjust=.5, vjust=.5,face="plain"))+
                 stat_smooth(method='lm',color='darkred')+
                 ggtitle('Annual Rainfall')
        }
 })

# Long term mean of rainfall over the selected region ====

output$climatology <- renderPlot({
        
        if (!is.null(pcp())){
            rain<-pcp()
            clim<-round(apply(matrix(rain,ncol=length(rain)/12,nrow=12),1,mean,na.rm=T),0)
       
    
        labels<-c("January","February","March","April","May","June","July","August","September","October","November","December")
        
        pre=data.frame('months'=labels, 'rainfall'=clim)
        
        pre$months=factor(pre$months,order=T,levels=c("January","February","March","April","May","June","July","August","September","October","November","December"))
        
        ggplot(pre, aes(x=months, y=rainfall)) +
            geom_bar(stat="identity", fill="white",color='orange') +
            ggtitle('Climatology')+ theme(plot.title = element_text(size = 18,colour="blue"))+
            ylab("Rainfall (mm/month)") +coord_cartesian(ylim=c((min(pre$rainfall)-10),(max(pre$rainfall)+20)))+
            theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 45, hjust = 1))+
            theme(axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=1,face="plain"),
                  axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0,face="plain"),
                  axis.text.x = element_text(colour="grey20",size=14,angle=60,hjust=.5,vjust=.5,face="plain"))
        }
})

# Distribution of rainfall over the selected month(s) and region  ----

output$boxplot <- renderPlot({
        
        if (!is.null(pcp())){
            
            years<-seq(year(input$startdate),year(input$enddate))
            
            rain<-pcp()[month_indicespredictand()]
            pcp=data.frame('year'=years, 'rainfall'=rain)
            
           p= ggplot(pcp,aes(y=rainfall,x=''))+geom_boxplot() + geom_jitter()
           p+ylab('Rainfall (mm/month)')+
               theme(axis.title.y = element_text(colour="grey20",size=16,angle=90,hjust=.5,vjust=1,face="plain"),
                     axis.text.y = element_text(colour="grey20",size=16,angle=0,hjust=1,vjust=0,face="plain"))+
                   theme(axis.title.x = element_blank())
        }         
})


output$histogram <- renderPlot({
    
    if (!is.null(pcp())){
        
        years<-seq(year(input$startdate),year(input$enddate))
        
        rain<-pcp()[month_indicespredictand()]
        pcp=data.frame('year'=years, 'rainfall'=rain)
        
        q=ggplot(data=pcp, aes(rainfall)) + 
            
         geom_histogram(col="gray50", 
                           fill="blue", 
                           alpha = .4)
             q+xlab('Rainfall (mm/month)')+ylab('Frequency')+
                 theme(axis.title.y = element_text(colour="grey20",size=15,angle=90,hjust=.5,vjust=1,face="plain"),
                       axis.title.x = element_text(colour="grey20",size=15,angle=0,hjust=.5,vjust=1,face="plain"),
                       axis.text.y = element_text(colour="grey20",size=15,angle=0,hjust=1,vjust=0,face="plain"),
                       axis.text.x = element_text(colour="grey20",size=15,angle=60,hjust=.5,vjust=.5,face="plain"))
    }
})

### pcp over selected region:end

 
 

# Extracting features (predictors) ====
 
 dataInputsst <- reactive({
     if(input$sst==T) {
         sst
         }
 })
 
 startingdatesst<-reactive({
     
     which(time_sst==update(input$startdate, days=01))
 })
 
 endingdatesst<-reactive({
     which(time_sst==update(input$enddate, days=01))
 })
 
 
 
# Extracing SLP
 
 dataInputslp <- reactive({
     if(input$slp==T){
            slp
         }
 })
 
 startingdateslp<-reactive({
     
     which(time_slp==update(input$startdate, days=01))
 })
 
 endingdateslp<-reactive({
     which(time_slp==update(input$enddate, days=01))
 })
 
 
 
 ###
 ##### clicks on SLP map
 
 selectedslp <- reactiveValues(
     
     coords = NULL 
 )
 
 # Handle clicks on slp map
 
 observeEvent(input$plot_clickslp, {
     
     m<-c(input$plot_clickslp$x,input$plot_clickslp$y)
     selectedslp$coords <- c(selectedslp$coords,m)
     
 })
 
 
 output$distPlotslp <- renderPlot({
     
     slp   <- dataInputslp()
     slp<-slp[ , ,startingdateslp():endingdateslp()]
     
 if (length(selectedslp$coords)>=4 
              & length(selectedslp$coords)%%2==0){ 
         
             corr_pcp_slp<-matrix(data = NA, nrow = dim(dataInputslp())[1], ncol=dim(dataInputslp())[2] )
             
             for (i in 1:dim(dataInputslp())[1]){
                 for (j in 1:dim(dataInputslp())[2]){
                     
                     corr_pcp_slp[i,j]<-cor(slp[i,j,month_indicespredictor()],pcp()[month_indicespredictand()])
                 }
             }
             title<-"Correlation of Precipitation over selected Region with SLP"
             mypalette<-brewer.pal(9,"BrBG")
             lon_slp1=lon_slp
             lon_slp1[which(lon_slp > 180)]=lon_slp1[which(lon_slp > 180)]-360
             lon_slp2=lon_slp1[order(lon_slp1)]
             corr_pcp_slp1 <- corr_pcp_slp[order(lon_slp1), ]
             image.plot(lon_slp2,lat_slp,corr_pcp_slp1, col = mypalette,
                        xlab ="Longitude", ylab="Latitude",
                        main =title)
             
             plot(cont,add=T,border='#000000', lwd=0.4)
             
             
             zslp<-selectedslp$coords
             
             for (i in seq(1,length(zslp),4)){
                 a<-zslp[c(i,i+2,i+2,i,i)]
                 b<-zslp[c(i+1,i+1,i+3,i+3,i+1)]
                 
                 lines(a,b,
                       type='l',col="blue",lwd=2)
             }
 }
 
 
 else if(!exists("selectedslp") | length(selectedslp$coords)<=3){
     
     if(length(slp) > 0 & length(pcp())> 0){
         
         
         corr_pcp_slp<-matrix(data = NA, nrow = dim(dataInputslp())[1], ncol=dim(dataInputslp())[2] )
         
         
         for (i in 1:dim(dataInputslp())[1]){
             for (j in 1:dim(dataInputslp())[2]){
                 
                 corr_pcp_slp[i,j]<-cor(slp[i,j,month_indicespredictor()],pcp()[month_indicespredictand()])
             }
         }
         
         title<-"Correlation of Precipitation over selected Region with SLP"
         mypalette<-brewer.pal(9,"BrBG")
         lon_slp1=lon_slp
         
         lon_slp1[which(lon_slp > 180)]=lon_slp1[which(lon_slp > 180)]-360
         lon_slp2=lon_slp1[order(lon_slp1)]
         corr_pcp_slp1 <- corr_pcp_slp[order(lon_slp1), ]
         image.plot(lon_slp2,lat_slp,corr_pcp_slp1, col = mypalette,
                    xlab ="Longitude", ylab="Latitude",
                    main =title)
         plot(cont,add=T,border='#000000', lwd=0.4)
         
     }   
 }
 
 })

 
 
 ### SST
 
 ####
 ##### clicks on SST map
 
 selectedsst <- reactiveValues(
     
     coords = NULL 
 )
 
 # Handle clicks on sst map
 
 observeEvent(input$plot_clicksst, {
     
     m<-c(input$plot_clicksst$x,input$plot_clicksst$y)
     selectedsst$coords <- c(selectedsst$coords,m)
     
 })
 
 
 output$distPlotsst <- renderPlot({
     
     sst   <- dataInputsst()
     sst<-sst[ , ,startingdatesst():endingdatesst()]
     
     if (length(selectedsst$coords)>=4 
         & length(selectedsst$coords)%%2==0){ 
         
         corr_pcp_sst<-matrix(data = NA, nrow = dim(dataInputsst())[1], ncol=dim(dataInputsst())[2] )
         
         for (i in 1:dim(dataInputsst())[1]){
             for (j in 1:dim(dataInputsst())[2]){
                 
                 corr_pcp_sst[i,j]<-cor(sst[i,j,month_indicespredictor()],pcp()[month_indicespredictand()])
             }
         }
         title<-"Correlation of Precipitation over selected Region with SST"
         mypalette<-brewer.pal(9,"BrBG")
         lon_sst1=lon_sst
         lon_sst1[which(lon_sst > 180)]=lon_sst1[which(lon_sst > 180)]-360
         lon_sst2=lon_sst1[order(lon_sst1)]
         corr_pcp_sst1 <- corr_pcp_sst[order(lon_sst1), ]
         image.plot(lon_sst2,lat_sst,corr_pcp_sst1, col = mypalette,
                    xlab ="Longitude", ylab="Latitude",
                    main =title)
         
         plot(cont,add=T,border='#000000', lwd=0.4)
         
         
         zsst<-selectedsst$coords
         
         for (i in seq(1,length(zsst),4)){
             a<-zsst[c(i,i+2,i+2,i,i)]
             b<-zsst[c(i+1,i+1,i+3,i+3,i+1)]
             
             lines(a,b,
                   type='l',col="blue",lwd=2)
         }
     }
     
     
     else if(!exists("selectedsst") | length(selectedsst$coords)<=3){
         
         if(length(sst) > 0 & length(pcp())> 0){
             
             
             corr_pcp_sst<-matrix(data = NA, nrow = dim(dataInputsst())[1], ncol=dim(dataInputsst())[2] )
             
             
             for (i in 1:dim(dataInputsst())[1]){
                 for (j in 1:dim(dataInputsst())[2]){
                     
                     corr_pcp_sst[i,j]<-cor(sst[i,j,month_indicespredictor()],pcp()[month_indicespredictand()])
                 }
             }
             
             title<-"Correlation of Precipitation over selected Region with SST"
             mypalette<-brewer.pal(9,"BrBG")
             lon_sst1=lon_sst
             
             lon_sst1[which(lon_sst > 180)]=lon_sst1[which(lon_sst > 180)]-360
             lon_sst2=lon_sst1[order(lon_sst1)]
             corr_pcp_sst1 <- corr_pcp_sst[order(lon_sst1), ]
             image.plot(lon_sst2,lat_sst,corr_pcp_sst1, col = mypalette,
                        xlab ="Longitude", ylab="Latitude",
                        main =title)
             plot(cont,add=T,border='#000000', lwd=0.4)
             
         }   
     }
     
 })
 
 
 
#### SST feature extraction
 
sst_feature<-reactive({
     if (length(selectedsst$coords)>=4 
         & length(selectedsst$coords)%%2==0){
         
         coordinates<-selectedsst$coords
         
         lon_sst_shifted<-ifelse(lon_sst>180 , lon_sst-360, lon_sst)
         sorted<-sort(lon_sst_shifted)
         
         a<-nearestIndx(coordinates[1],sorted )
         lonl_sst<-which(lon_sst_shifted==sorted[a])
         
         a<-nearestIndx(coordinates[3],sorted )
         lonr_sst<-which(lon_sst_shifted==sorted[a])
         
         lats_sst<-nearestIndx(coordinates[2],lat_sst)
         latn_sst<-nearestIndx(coordinates[4],lat_sst)
         
         as.vector(apply(sst[lonl_sst:lonr_sst,lats_sst:latn_sst,month_indicespredictor()],3,mean,na.rm=T))
          
     }
 })  
          

##### SLP feature extraction

slp_feature<-reactive({
    if (length(selectedslp$coords)>=4 
        & length(selectedslp$coords)%%2==0){
        
        coordinates<-selectedslp$coords
        
        lon_slp_shifted<-ifelse(lon_slp>180 , lon_slp-360, lon_slp)
                   
        sorted<-sort(lon_slp_shifted)
        
        a<-nearestIndx(coordinates[1],sorted )
        lonl_slp<-which(lon_slp_shifted==sorted[a])
        
        a<-nearestIndx(coordinates[3],sorted )
        lonr_slp<-which(lon_slp_shifted==sorted[a])
        
        lats_slp<-nearestIndx(coordinates[2],lat_slp)
        latn_slp<-nearestIndx(coordinates[4],lat_slp)
        
        as.vector(apply(slp[lonl_slp:lonr_slp,lats_slp:latn_slp,month_indicespredictor()],3,mean,na.rm=T))
        
    }
})
 
 

# Choosing models ====

model <- reactive({
    switch(input$models,
           "GLM" = 'glm',
           "RF"='rf',
           "ANN"='ann',
           "GAM"='gam',
           "SGAM"='sgam',
           "SGLM"='sglm',
           "Bagging"='bagging',
           "Boosting"='boosting',
           "SVM"='svm',
           "MARS"='mars')
})

# Model training and prediction ====

output$summary <- renderPrint({

     slp1=slp_feature()
     sst1=sst_feature()
 
    rain<-pcp()[month_indicespredictand()]
    pcp=data.frame('slp'=slp1,'sst'=sst1, 'rainfall'=rain)
    
    inTrain <- createDataPartition(y=pcp$rainfall, p=0.6, list=FALSE)
    training <- pcp[inTrain,]
    testing <- pcp[-inTrain,]
    modelFit <- train(rainfall ~.,data=training, method=model(),preProcess=c("center", "scale"))
    predictions <- predict(modelFit,newdata=testing)
    summary(modelFit)
})

##
output$fitpred <- renderPlot({
    
    slp1=slp_feature()
    sst1=sst_feature()
    
    rain<-pcp()[month_indicespredictand()]
    pcp=data.frame('slp'=slp1,'sst'=sst1, 'rainfall'=rain)
    
    inTrain <- createDataPartition(y=pcp$rainfall, p=0.6, list=FALSE)
    training <- pcp[inTrain,]
    testing <- pcp[-inTrain,]
    modelFit <- train(rainfall ~., data=training, method=model(),preProcess=c("center", "scale"))
    predictions <- predict(modelFit,newdata=testing)
    
    pcp_train=data.frame('fitted'=modelFit$finalModel$fitted.values,'observed'=training$rainfall)
    
    pcp_test=data.frame('predicted'=predictions,'observed'=testing$rainfall)
    
    cols <- c("Training"="blue","Testing"="red")
    
    ggplot(pcp_train, aes(x=fitted, y=observed,color='Training')) +
        geom_point(pch=18,size=3)+
        geom_point(data = pcp_test,aes(x=predicted, y=observed,colour = "Testing"))+ylab("Observation")+
        xlab("model")+ ggtitle('Observation Vs Model')+ theme(plot.title = element_text(size = 18,colour="black"))+
        theme(axis.text.x = element_text(angle = 45, hjust = 1))+
        scale_colour_manual(name=" ",values=cols)+stat_smooth(method='lm',color='blue')+
        theme(axis.title.y = element_text(colour="grey20",size=14,angle=90,hjust=.5,vjust=1),
              axis.text.y = element_text(colour="grey20",size=14,angle=0,hjust=1,vjust=0),
              axis.text.x = element_text(colour="grey20",size=14,angle=60,hjust=.5,vjust=.5))
    
    
    })


output$qqplot <- renderPlot({
    
    slp1=slp_feature()
    sst1=sst_feature()
    
    rain<-pcp()[month_indicespredictand()]
    pcp=data.frame('slp'=slp1,'sst'=sst1, 'rainfall'=rain)
    
    inTrain <- createDataPartition(y=pcp$rainfall, p=0.6, list=FALSE)
    training <- pcp[inTrain,]
    testing <- pcp[-inTrain,]
    modelFit <- train(rainfall ~., data=training, method=model(),preProcess=c("center", "scale"))
    
    par(mfrow=c(2,2))
    plot(modelFit$finalModel)
    
})


})

 




  
  


