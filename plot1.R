library("lubridate")

message("Call function plot1(makePNG = TRUE) to create plot 1")
message ("While makePNG is true, a file plot1.png is created in working directory")
message("else it is shown on screen")

## while makePNG is true, a file plot1.png is created in working directory
## else it is shown on screen
plot1<-function(makePNG = TRUE)
{
  ## Read dataset. Loaddata function (defined below) loads data and returns 
  ## a data frame used in following plot
  
    epc<-LoadData()
  
  if(makePNG == TRUE)
  {
    if(file.exists("plot1.png"))
    {
      message("plot1.png already exists. Removing current file and re-creating file")
      file.remove("plot1.png")
    }
    
      png("plot1.png");
      hist(epc$Global_active_power,breaks=seq(0,8, by=0.5),
       xlab="Global Active Power (kilowatts)",
       ylab="Frequency", xlim = c(0,6), ylim = c(0,1200), 
       main="Global Active Power",col="red");
      dev.off() 
    
    if(file.exists("plot1.png"))
    {
      message("Created plot1.png in working directory")
    }
    else
    {
      message("Failed to create plot1.png") 
    }
    
  }
  else {
    hist(epc$Global_active_power,breaks=seq(0,8, by=0.5),
         xlab="Global Active Power (kilowatts)",
         ylab="Frequency", xlim = c(0,6), ylim = c(0,1200), 
         main="Global Active Power",col="red");
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


