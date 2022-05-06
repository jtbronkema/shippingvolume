################################################################################
# MGMT 590 Using R for Analytics
# Team Lab #3
# Due: 12/05/2021 11:59pm
#
#
# Team #:9
### we added progress indicator under lab2:scatter plot. #############

######################################################################################
#load library
library(shiny)
library(shinythemes)
#load library
library(tidyr) #datawrangling
library(ggplot2) #ploting
library(dplyr) #datawrangling
library(zoo) #forecast
library(lubridate) #datetimethings
library(forecast) #forecast
library(imputeTS) #impute
library(reshape2) #reshape
options(shiny.maxRequestSize=100*1024^2)
library(DT)

#Custom Functions
data_cleaning = function(filename,topnum,minVol,minYear){
  #read in data
  # rh.data <- paste0(filename, ".csv")
  df <- data.frame(filename)
  colsneeded <- c("BL_AWB_PRO",
                  "container_num",
                  "shipment_id",
                  "origin_city",
                  "pol_city",
                  "pod_city",
                  "bl_city",
                  "Service.Type",
                  "Carrier.Name",
                  "Actual.Cargo.Receipt.Date.Date",
                  "Departed.Date",
                  "Arrived.at.Delivery.Location.Date",
                  "Container.Type",
                  "Actual.Volume..CBM.")
  small_df <- df[colsneeded]
  # Renaming the cols for easy access
  names(small_df) <- c("BL_num",
                       "Container_num",
                       "Shipment_id",
                       "Origin_city",
                       "POL_city",
                       "POD_city",
                       "BL_city",
                       "Service_type",
                       "Carrier",
                       "Cargo_receipt_date",
                       "Departed_date",
                       "Delivery_date",
                       "Container_type",
                       "Volume")
  ## create new lane
  small_df$Lane = paste(small_df$Origin_city,"_",small_df$POL_city,"_",small_df$POD_city,"_",small_df$BL_city)
  
  #combine Vung Tau _ Vung Tau _ Oakland _ Oakland with Vung Tau _ Vung Tau _ Oakland _ Patterson
  small_df$Lane[which(small_df$Lane=="Vung Tau _ Vung Tau _ Oakland _ Oakland")] <- "Vung Tau _ Vung Tau _ Oakland _ Patterson"
  #combine Ningbo _ Ningbo _ Oakland _ Oakland with Ningbo _ Ningbo _ Oakland _ Patterson
  small_df$Lane[which(small_df$Lane=="Ningbo _ Ningbo _ Oakland _ Oakland")] <- "Ningbo _ Ningbo _ Oakland _ Patterson"
  #combine Yantian _ Yantian _ Oakland _ Oakland with Ningbo _ Ningbo _ Oakland _ Patterson
  small_df$Lane[which(small_df$Lane=="Yantian _ Yantian _ Oakland _ Oakland")] <- "Yantian _ Yantian _ Oakland _ Patterson"
  
  ## coerce the categorical column to factor format
  colnames_df <- as.vector(colnames(small_df))
  categorical_vars<-c("BL_num",
                      "Container_num",
                      "Shipment_id",
                      "Origin_city",
                      "POL_city",
                      "POD_city",
                      "BL_city",
                      "Service_type",
                      "Carrier",
                      # "Cargo_receipt_date",
                      # "Departed_date",
                      # "Delivery_date",
                      "Container_type",
                      "Lane")
  small_df[categorical_vars] <- lapply(small_df[categorical_vars], as.factor)
  
  # coerce the Date column to Date format
  date_vars <- c("Cargo_receipt_date",
                 "Departed_date",
                 "Delivery_date")
  small_df[date_vars] <- lapply(small_df[date_vars], function(x) as.Date(x,"%m/%d/%Y"))
  
  # Filtering out rows with 0 volume
  small_df <- filter(small_df, Volume > as.numeric(minVol))
  print(small_df)
  str(small_df)
  # Getting month wise grouping
  small_df$Dep_month <- floor_date(small_df$Departed_date, "month")
  
  #group by shipping lanes
  volume_by_lanes <- small_df %>%
    select("Lane","Volume","Dep_month") %>%
    group_by(Lane) %>%
    summarize(total = sum(Volume),
              minyear=min(year(Dep_month)),
              maxyear=max(year(Dep_month)))%>%
    arrange(desc(total))
  #drop lanes that has no shipment at/after 2019
  volume_by_lanes <- volume_by_lanes %>% filter(maxyear >= as.numeric(minYear))
  
  ##### generate dataframe for each lane
  lane_result = list()
  i=1
  print(paste("length is",length(volume_by_lanes$Lane[1:as.numeric(topnum)])))
  for(laneName in volume_by_lanes$Lane[1:as.numeric(topnum)]){
    laneDf <- small_df%>%
      filter(Lane==laneName)%>%
      select("Lane","Volume","Departed_date","Dep_month")%>%
      arrange(Departed_date)%>%
      group_by(Dep_month)%>%
      summarize(monthly_volume=sum(Volume))
    # generate a sequence of complete months between the start date and end date in the data
    complete_month <- data.frame(seq(laneDf$Dep_month[1], laneDf$Dep_month[nrow(laneDf)], "month"))
    names(complete_month) <- "Dep_month"
    # combine the datasets and reveal the missing values
    complete_laneDf <- complete_month %>% left_join(laneDf, by = "Dep_month")
    # write.csv(complete_laneDf,paste("/Users/yuxuanli/Desktop/备份/spring 2022/Industry Practium/top20lanes_0319/",i,laneName,".csv"))
    lane_result[[laneName]] = complete_laneDf
    # plot(complete_laneDf,type="o",main=laneName)
    # print(laneName)
    i=i+1
    
  }
  
  return(lane_result)
}


foralllane = function(name, alllanesfinal, alllanesplot, x){
  
  #read data
  lanedt <- lanedata[[name]]
  print(paste0("lane data:",length(lanedt)))
  if (nrow(lanedt)<24 | mean(is.na(lanedt[2]))>0.5){
    print("no enough data for modeling")
    print(alllanesfinal)
    print(alllanesplot)
    alllanesfinal[nrow(alllanesfinal)+1,] <- c(name,"NA",
                                               "NA","0","0","0","0","0","0","0","0","0","0","0","0")
    print(alllanesfinal)
    alllanesplot[[x]] <- list()
    # print(alllanesplot)
  }else{
    print("enter else")
    #getting the start and end years and months of the time series
    mindt <- min(lanedt$Dep_month)
    maxdt <- max(lanedt$Dep_month)
    startts <- c(year(mindt), month(mindt))
    endts <- c(year(maxdt), month(maxdt))
    
    #Impute missing values if any using the mean of the whole time series
    fullts_mean <- ts(na_mean(lanedt$monthly_volume), start = startts,
                      end = endts, freq = 12)
    # no imputation: leave the missing value there
    fullts_no <- ts(lanedt$monthly_volume, start = startts,
                    end = endts, freq = 12)
    
    #Forecasting with the best model
    nums_mean <- forecastnplot(fullts_mean, timehorizon = 12, name)
    nums_no <- forecastnplot(fullts_no, timehorizon = 12, name)
    
    print(paste0("forecast result with mean imp:",nums_mean))
    print(paste0("forecast result with no imp:",nums_no))
    
    if (round(as.numeric(nums_mean[2]),2) < round(as.numeric(nums_no[2]),2)){
      alllanesfinal[nrow(alllanesfinal)+1,] <- c(name,nums_mean[1],
                                                 round(as.numeric(nums_mean[2]),2),
                                                 round(as.numeric(nums_mean[3]),2),
                                                 round(as.numeric(nums_mean[4]),2),
                                                 round(as.numeric(nums_mean[5]),2),
                                                 round(as.numeric(nums_mean[6]),2),
                                                 round(as.numeric(nums_mean[7]),2),
                                                 round(as.numeric(nums_mean[8]),2),
                                                 round(as.numeric(nums_mean[9]),2),
                                                 round(as.numeric(nums_mean[10]),2),
                                                 round(as.numeric(nums_mean[11]),2),
                                                 round(as.numeric(nums_mean[12]),2),
                                                 round(as.numeric(nums_mean[13]),2),
                                                 round(as.numeric(nums_mean[14]),2)
      )
      
      # # #plotting
      print(paste("alllanesfinal:",alllanesfinal))
      print(paste("modelused:",alllanesfinal$ModelUsed[x]))
      alllanesplot[[x]]  <- list(fullts_mean,alllanesfinal$ModelUsed[x],name)
      
    } else{
      alllanesfinal[nrow(alllanesfinal)+1,] <- c(name,nums_no[1],
                                                 round(as.numeric(nums_no[2]),2),
                                                 round(as.numeric(nums_no[3]),2),
                                                 round(as.numeric(nums_no[4]),2),
                                                 round(as.numeric(nums_no[5]),2),
                                                 round(as.numeric(nums_no[6]),2),
                                                 round(as.numeric(nums_no[7]),2),
                                                 round(as.numeric(nums_no[8]),2),
                                                 round(as.numeric(nums_no[9]),2),
                                                 round(as.numeric(nums_no[10]),2),
                                                 round(as.numeric(nums_no[11]),2),
                                                 round(as.numeric(nums_no[12]),2),
                                                 round(as.numeric(nums_no[13]),2),
                                                 round(as.numeric(nums_no[14]),2)
                                              
                                                 
      )
      
      alllanesplot[[x]] <- list(fullts_no,alllanesfinal$ModelUsed[x],name)
      
    }
    
  }
  return(list(alllanesfinal, alllanesplot))
}


forecastnplot = function(fullts, timehorizon, nameofplot) {
  
  #defining ets and autoarima functions for tsCV use
  fets <- function(x, h) {
    forecast(ets(x, lambda = 0), h = h)
  }
  farima <- function(x, h) {
    forecast(auto.arima(x, lambda = 0), h = h)
  }
  
  # Compute CV errors for ETS as e1
  e1 <- tsCV(fullts, fets, h = timehorizon)
  # Compute CV errors for ARIMA as e2
  e2 <- tsCV(fullts, farima, h = timehorizon)
  
  # Find MAPE of each model class
  mape1 <- sum(abs(e1)/fullts, na.rm = TRUE)*100/length(e1)
  mape2 <- sum(abs(e2)/fullts, na.rm = TRUE)*100/length(e2)
  print(paste0("mape1 is :",mape1))
  print(paste0("mape2 is :",mape2))
  
  if (mape1 <= mape2){
    #Model and forecast
    modelused <- "fets"
    mod <- fullts %>% ets(lambda = 0) 
    fct <- mod %>% forecast(h = 12, level = 0)
    #give options to specify CI
    
    forecastvalue<-data.frame(Volume=fct$mean)
    print(forecastvalue)
    contractval1<-forecastvalue[1,]
    contractval2<-forecastvalue[2,]
    contractval3<-forecastvalue[3,]
    contractval4<-forecastvalue[4,]
    contractval5<-forecastvalue[5,]
    contractval6<-forecastvalue[6,]
    contractval7<-forecastvalue[7,]
    contractval8<-forecastvalue[8,]
    contractval9<-forecastvalue[9,]
    contractval10<-forecastvalue[10,]
    contractval11<-forecastvalue[11,]
    contractval12<-forecastvalue[12,]
    print(contractval2)
    print(contractval6)
    print(contractval12)

    mapeval <- mape1
    

    
  } else {
    #Model and forecast
    modelused <- "farima"
    mod <- fullts %>% auto.arima(lambda = 0) 
    fct <- mod %>% forecast(h = 12, level = 0)
    
    forecastvalue<-data.frame(Volume=fct$mean)
    print(forecastvalue)
    contractval1<-forecastvalue[1,]
    contractval2<-forecastvalue[2,]
    contractval3<-forecastvalue[3,]
    contractval4<-forecastvalue[4,]
    contractval5<-forecastvalue[5,]
    contractval6<-forecastvalue[6,]
    contractval7<-forecastvalue[7,]
    contractval8<-forecastvalue[8,]
    contractval9<-forecastvalue[9,]
    contractval10<-forecastvalue[10,]
    contractval11<-forecastvalue[11,]
    contractval12<-forecastvalue[12,]

    print(contractval2)
    print(contractval6)
    print(contractval12)
    
    mapeval <- mape2
    

  }
  return(c(modelused,mapeval,contractval1,contractval2,contractval3,contractval4,contractval5,contractval6,
           contractval7,contractval8,contractval9,contractval10,contractval11,contractval12))
}


fplots = function(fullts, modelused,nameofplot) {
  
  if (modelused=="fets"){
    mod <- fullts %>% ets(lambda = 0) 
    fct <- mod %>% forecast(h = 12, level = 0)
    #plotting
    plot<-plot(fct, main = nameofplot, xlab = "Years",
               ylab = "Volume (CBM)")
    lines(fct$fitted, col=3)
    
  } else {
    mod <- fullts %>% auto.arima(lambda = 0) 
    fct <- mod %>% forecast(h = 12, level = 0)
    
    #plotting
    plot<- plot(fct, main = nameofplot, xlab = "Years",
                ylab = "Volume (CBM)")
    lines(fct$fitted, col=3)
  }
  return(plot)
}


##################################################### shiny! ##################################################################
ui <- fluidPage (theme = shinytheme("united"),
                 navbarPage(title = "RH Shipping Volume Forecasting Dashboard",
                            ######################################### Shiny App Introduction Tab ########################################
                            tabPanel("User Manual", mainPanel(
                              
                              h2("Welcome to Shiny! Before start, please read the following instructions carefully", 
                                 style = "font-family: 'times'; font-size:16pt;bold"),
                              h2("1. Please make sure your input data file is in '.csv' format and has the same column names and order as below:", 
                                 style = "font-family: 'times'; font-size:12pt"),
                              img(src = "column names.png", height = 400, width = 350),
                              h2("2. If you are running the Shiny App locally in RStudio, make sure the input data file and the Shiny App file are in the same working directory.
                                 If the file upload goes wrong, please terminate the current running session by clicking 'Stop' on the top right of the Console panel, and restart the program by clicking 'Run App' on the top right of the Script panel.", 
                                 style = "font-family: 'times'; font-size:12pt"),
                              img(src = "stop.png", height = 170, width = 380),
                              img(src = "run.png", height = 170, width = 380),
                              width=12, 
                              h2("3. If you are running the Shiny App on a website and the file upload goes wrong, please refresh the website and re-upload the correct data file.", 
                                 style = "font-family: 'times'; font-size:12pt"),
                              h2("4. When uploading a file in the Lanes Overview Dashboard, please wait until the upload has been 100% completed then make any changes. File uploading time depends on file size.", 
                                 style = "font-family: 'times'; font-size:12pt"),
                              h2("5. Model runtime reference: the model running time depends on the number of Lanes you select and the amount of data per Lane. The test data we use contains shipping records from 2014 to 2021, and the model running time is an average of 1.3 minutes per Lane.", 
                                 style = "font-family: 'times'; font-size:12pt"),
                              h2("6. 'NA' in model result is due to insufficient data for modeling", 
                                 style = "font-family: 'times'; font-size:12pt"),
                              
                              br(),
                              br(),
                              br(),
                            
                            )),
                            
                            
                            
                            ######################################### Lanes Overview Tab ########################################
                            tabPanel("Lanes Overview",
                                     
                                     sidebarPanel( width = 3,
                                                   fileInput('file1',"Upload a CSV File:", multiple = FALSE,accept = ".csv"),
                                                   numericInput("topnum","Number of Lanes:",20,min=1,max=100),
                                                   numericInput("minVol","Min Volume Threshold:",50,min=0,max=10000),
                                                   numericInput("minYear","Lanes that has no shipment at and after this year will be removed from modeling",2019,min=2012),
                                                   actionButton(inputId = "button1", label = "Show Lanes")
                                     ),
                                     
                                     mainPanel(
                                       h2("Lanes Overview"),
                                       tableOutput("table1"))),
                            
                            ######################################## Model Result Tab########################################
                            tabPanel("Model Results",
                                    sidebarPanel( width = 3,
                                                   actionButton(inputId = "button2", label = "Show Model Results")
                                     ),
                                     
                                     mainPanel( 
                                       h3("Model Result Table"),
                                       dataTableOutput("table2"),
                                       h3("Aggregated Monthly Contract Volume (12 Months)"),
                                       dataTableOutput("final"),
                                       h3("Yearly Contract Volume"),
                                       tableOutput("yearly"),
                                       h3("Forecasted Plots"),
                                       uiOutput("plots")
                                       ))
                            
                            
                 ))


server <- function(input, output) {
  
  ################################### dashboard 1--lanes overview server#######################################################
  
  Lanes <- reactive({
    infile <- input$file1
    if (is.null(infile)) {
      # User has not uploaded a file yet
      return(NULL)
    }
    df<-read.csv(input$file1$datapath)
    
    #run the data_cleaning function
    lanedata <- data_cleaning(df,input$topnum,input$minVol,input$minYear)
    return(lanedata)
  })
  
  #This previews all the selected lanes
  observeEvent(input$button1,{
    lanedata <<- Lanes()
    output$table1 <- renderTable({
      names(lanedata)
    })
    print(lanedata)
    
  })
  
  ################################### dashboard 2--model result server#######################################################3
  alllanesfinal <- data.frame(Name = character(), ModelUsed = character(), MAPE = double(),
                              Forecast1 = double(),Forecast2 = double(),Forecast3 = double(),
                              Forecast4 = double(),Forecast5 = double(),Forecast6 = double(),
                              Forecast7 = double(),Forecast8 = double(),Forecast9 = double(),
                              Forecast10 = double(),Forecast11 = double(),Forecast12 = double())
  
  alllanesplot <- list()
  observeEvent(input$button2,{
    for (x in 1:length(lanedata)){
      print(x)
      result <- foralllane(names(lanedata)[x], alllanesfinal, alllanesplot, x)
      alllanesfinal <- data.frame(result[[1]])
      alllanesplot <- result[[2]]
    }
   
    output$table2 <- DT::renderDataTable({datatable(
      alllanesfinal[,-2],
      option=list(autoWidth = TRUE,scrollX = TRUE,
                  columnDefs = list(list(targets=c(0), visible=TRUE, width='15'),
                                    list(targets=c(1), visible=TRUE, width='300'),
                                    list(targets=c(2,3,4,5,6,7,8,9,10,11,12,13,14), visible=TRUE, width='45')),
      filter = list(position = "top"), style = "bootstrap", class = "table-bordered"))
    })
    
    output$final <- DT::renderDataTable({datatable(
      monthlyVol <- data.frame(Forecast1=sum(as.numeric(alllanesfinal[,4])),Forecast2=sum(as.numeric(alllanesfinal[,5])),Forecast3=sum(as.numeric(alllanesfinal[,6])),Forecast4=sum(as.numeric(alllanesfinal[,7])),
                 Forecast5=sum(as.numeric(alllanesfinal[,8])),Forecast6=sum(as.numeric(alllanesfinal[,9])),Forecast7=sum(as.numeric(alllanesfinal[,10])),Forecast8=sum(as.numeric(alllanesfinal[,11])),
                 Forecast9=sum(as.numeric(alllanesfinal[,12])),Forecast10=sum(as.numeric(alllanesfinal[,13])),Forecast11=sum(as.numeric(alllanesfinal[,14])),Forecast12=sum(as.numeric(alllanesfinal[,15]))),
      option=list(autoWidth = TRUE,scrollX = TRUE))
      })
    output$yearly <- renderTable({
      sum(data.frame(Forecast1=sum(as.numeric(alllanesfinal[,4])),Forecast2=sum(as.numeric(alllanesfinal[,5])),Forecast3=sum(as.numeric(alllanesfinal[,6])),Forecast4=sum(as.numeric(alllanesfinal[,7])),
                     Forecast5=sum(as.numeric(alllanesfinal[,8])),Forecast6=sum(as.numeric(alllanesfinal[,9])),Forecast7=sum(as.numeric(alllanesfinal[,10])),Forecast8=sum(as.numeric(alllanesfinal[,11])),
                     Forecast9=sum(as.numeric(alllanesfinal[,12])),Forecast10=sum(as.numeric(alllanesfinal[,13])),Forecast11=sum(as.numeric(alllanesfinal[,14])),Forecast12=sum(as.numeric(alllanesfinal[,15]))))
    })
    
    # Insert the right number of plot output objects into the web page
    output$plots <- renderUI({
      plot_output_list <- lapply(1:length(lanedata), function(i) {
        plotname <- paste("plot", i, sep="")
        if(length(alllanesplot[[i]])==0){
          tableOutput(plotname)
        }else{
          plotOutput(plotname, height = 280, width = 400)
        }
      })
      
      # Convert the list to a tagList - this is necessary for the list of items
      # to display properly.
      do.call(tagList, plot_output_list)
    })
    
    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    for (i in 1:length(lanedata)) {
      # Need local so that each item gets its own number. Without it, the value
      # of i in the renderPlot() will be the same across all instances, because
      # of when the expression is evaluated.
      local({
        my_i <- i

        plotname <- paste("plot", my_i, sep="")
        if(length(alllanesplot[[my_i]])==0){
          print("enter no plot")
          output[[plotname]] <- renderText({"Insufficient data for modeling"})
        }else{ 
          output[[plotname]] <- renderPlot({
            args = alllanesplot[[my_i]]
            print(paste0("args:",args))
            fplots(args[[1]], args[[2]], args[[3]])
            
          })
        }
      })
    }
  })
}
shinyApp(ui = ui, server = server)

