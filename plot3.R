library("lubridate")

message("Call function plot3(makePNG = TRUE) to create plot 3")
message ("While makePNG is true, a file plot3.png is created in working directory")
message("else it is shown on screen")

## while makePNG is true, a file plot1.png is created in working directory
## else it is shown on screen
plot3<-function(makePNG = TRUE)
{
  ## Read dataset. Loaddata function (defined below) loads data and returns 
  ## a data frame used in following plot
  
    epc<-LoadData()
    
    ## Get the maximum value for the three series in current chart
    ## this will be required as the upper limit of yAxis
    ymax<-max(epc[,c(7,8,9)]);
  
  if(makePNG == TRUE)
  {
    if(file.exists("plot3.png"))
    {
      message("plot3.png already exists. Removing current file and re-creating file")
      file.remove("plot3.png")
    }
    
      png("plot3.png");
      
      plot(epc$DateTime,epc$Sub_metering_1,type="l",col="black", xlab=""
           ,ylab="Energy sub metering", ylim=c(0,ymax));
      par(new=T); 
      plot(epc$DateTime,epc$Sub_metering_2,type="l",col="red", xlab=""
           ,ylab="Energy sub metering" ,ylim=c(0,ymax));
      par(new=T); 
      plot(epc$DateTime,epc$Sub_metering_3,type="l",col="blue", xlab=""
         ,ylab="Energy sub metering" ,ylim=c(0,ymax));
      legend("topright",legend=names(epc[,c(7,8,9)]),lty=c(1,1,1)
             ,col=c("black","red","blue"));
      par(new=F);
      dev.off() 
    
    if(file.exists("plot3.png"))
    {
      message("Created plot3.png in working directory")
    }
    else
    {
      message("Failed to create plot3.png") 
    }
    
  }
  else {
    plot(epc$DateTime,epc$Sub_metering_1,type="l",col="black", xlab=""
         ,ylab="Energy sub metering", ylim=c(0,ymax));
    par(new=T); 
    plot(epc$DateTime,epc$Sub_metering_2,type="l",col="red", xlab=""
         ,ylab="Energy sub metering" ,ylim=c(0,ymax));
    par(new=T); 
    plot(epc$DateTime,epc$Sub_metering_3,type="l",col="blue", xlab=""
         ,ylab="Energy sub metering" ,ylim=c(0,ymax));
    legend("topright",legend=names(epc[,c(7,8,9)]),lty=c(1,1,1)
           ,col=c("black","red","blue"));
    par(new=F);
  }
}


LoadData<-function()
{
  ##  Read the data 
  epc_raw<-read.csv("household_power_consumption.txt",sep=";")
  
  ## Select only for days 2007-02-01 and 2007-02-02
  epc_select<-epc_raw[(epc_raw$Date=="1/2/2007" | epc_raw$Date=="2/2/2007"),]
  
  ## Remove the massive orignial dataset, as the rest deals only with data for selected days
  rm("epc_raw")
  
  ## Convert the date and time into a single column date time format 
  DateTime<-dmy_hms(paste(epc_select$Date,epc_select$Time))
  
  ## find the weekday names - required for plot 2,3,4
  DayOfWeek<-substr(weekdays(DateTime),1,3)
  
  ## Convert the measurement part of dataset into numeric
  epc_numeric<-apply(epc_select[,c(-1,-2)],2,as.numeric)
  epc_data<-as.data.frame(epc_numeric)
  names(epc_data)<-names(epc_select[,c(-1,-2)])
  
  
  ## Create the new dataset with DateTime, Day and measurements converted to numeric
  epc1<-cbind(DateTime,DayOfWeek,epc_data)
  
  ## Remove all intermediate datasets
  rm("epc_select","epc_numeric","epc_data")
  
  epc1
}


