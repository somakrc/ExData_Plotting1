library("lubridate")

message("Call function plot4(makePNG = TRUE) to create plot 4")
message ("While makePNG is true, a file plot4.png is created in working directory")
message("else it is shown on screen")

## while makePNG is true, a file plot1.png is created in working directory
## else it is shown on screen
plot4<-function(makePNG = TRUE)
{
  ## Read dataset. Loaddata function (defined below) loads data and returns 
  ## a data frame used in following plot
  
    epc<-LoadData()
    
  if(makePNG == TRUE)
  {
    if(file.exists("plot4.png"))
    {
      message("plot4.png already exists. Removing current file and re-creating file")
      file.remove("plot4.png")
    }
    
      png("plot4.png");
      plotcharts(epc)  
      dev.off() 
    
    if(file.exists("plot4.png"))
    {
      message("Created plot4.png in working directory")
    }
    else
    {
      message("Failed to create plot4.png") 
    }
    
  }
  else {
          plotcharts(epc)
  }
}

## This function includes all the plotting instructions
plotcharts<-function(epc)
{  
  ## Plot 4
  ## 4 plots into 1
  par(mfcol=c(2,2))
  
  ## Chart 1
  plot(epc$DateTime,epc$Global_active_power,type="l",xlab="",ylab="Global Active Power (kilowatts)");
  par(new=F);
  
  ## Chart 2
  ## Get the maximum value for the three serieses in the bottom left chart 
  ## this will be required as the upper limit of yAxis of that chart
  ymax<-max(epc[,c(7,8,9)]);
  plot(epc$DateTime,epc$Sub_metering_1,type="l",col="black", xlab="",ylab="Energy sub metering", ylim=c(0,ymax));
  par(new=T); 
  plot(epc$DateTime,epc$Sub_metering_2,type="l",col="red", xlab="",ylab="Energy sub metering",ylim=c(0,ymax));
  par(new=T); 
  plot(epc$DateTime,epc$Sub_metering_3,type="l",col="blue", xlab="",ylab="Energy sub metering",ylim=c(0,ymax));
  par(new=T);
  legend("topright",legend=names(epc[,c(7,8,9)]),lty=c(1,1,1)
         ,col=c("black","red","blue"));
  par(new=F);

  ## Chart 3
  plot(epc$DateTime,epc$Voltage,type="l",xlab="datetime",ylab="Voltage");
  par(new=F);
  
  ## Chart 4
  plot(epc$DateTime,epc$Global_reactive_power,type="l",xlab="datetime",ylab="Global_reactive_power");
  par(new=F);
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


