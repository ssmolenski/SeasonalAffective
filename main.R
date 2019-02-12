library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)

setwd("C:/Users/Sarah/Documents/DataScience/SeasonalAffective")

#Read in Weather data
#Data can be acquired from https://www7.ncdc.noaa.gov/CDO/cdoselect.cmd?datasetabbv=GSOD&countryabbv&georegionabbv
if (!file.exists("tempdata.txt")){
    cat("Downloading data...\n")
    url<- "https://www1.ncdc.noaa.gov/pub/orders/CDO125287832548.txt"
    #"https://www1.ncdc.noaa.gov/pub/orders/CDO7909037746350.txt" #From Jan2015
    # url<- "https://www1.ncdc.noaa.gov/pub/orders/CDO5132857743160.txt" #From Sept2015
    download.file(url,"tempdata.txt")
    Downloadtime<-Sys.time()
}
cat("Reading weather data...\n")
widths<-c(-14,8,-4,4,-73,5,-4,4,-3,4,-9,6)
cnames<-c("Date","AvgTemp", "High", "Low", "Prcp", "FRSHTT")
weather<-read.fwf("tempdata.txt",widths=widths,na.strings=c(999.9,9999.9),skip=1, col.names=cnames)
weather$Date<-ymd(weather$Date)
weather<-weather[5:1242,] #Drop first four days of September, since I started taking mood data on Sept 5.

#Read in Mood data
cat("Reading mood data...\n")
read.csv("iMoodJournal.csv", stringsAsFactors=FALSE,fileEncoding="UTF-8-BOM") %>%
        select(c(1,5,214,7)) %>% 
        rename(Wellbutrin=wellbutrin) -> moods
moods$Date<-as.Date(moods$Date,"%B %d, %Y")

#Wellbutrin column only gives which days are tagged with "Wellbutrin". I want to extract the dose information for each day:
cat("Extracting Wellbutrin doses...\n")
meds<- which(moods$Wellbutrin==1,arr.ind=TRUE)
meddates<- as.Date(moods$Date[meds])
dose<-integer(0)
for (i in 1:length(meds)){
    index<-meds[i]
    str<- moods$Comment[index]
    dose<-as.numeric(c(dose,str_extract(str,"[[:digit:]]+")))
}
dose[2]<-0 #This is the day I stopped, but data does not include "0mg"
for (i in 0:(length(meds))){
    if(i==0){
        WellbutrinDose<-rep(0,(meds[1]-1))
    }else if (i==length(meds)) {
        n<-(length(moods$Wellbutrin) - meds[i]+1)
        WellbutrinDose<- c(WellbutrinDose, rep(dose[i], n ))
    }else{
        n<- meds[i+1] - meds[i]
        WellbutrinDose<- c(WellbutrinDose, rep(dose[i], n ))
    }
}


###############Medication Estimates##################

cat("Filling missing data...\n")
WellbutrinDose[1135:1189] <- 100
WellbutrinDose[1190:1243] <- 150
WellbutrinDose[1244:1430] <- 200
WellbutrinDose[1430:1484] <- 100
WellbutrinDose[1485:1541] <- 200
WellbutrinDose[1542:1562] <- 100
WellbutrinDose[1563:1574] <- 50


####################################################################

moods$Wellbutrin<-WellbutrinDose

#Now put everything in one table.
cat("Merging all data...\n")
merge(moods, weather, by="Date", all=TRUE) %>%
    select(Date, Level, High, Wellbutrin, Prcp, Comment, AvgTemp, Low, FRSHTT) -> data


model1 <- lm(Level~High,data) #1 and 2 produce sme High-Level relation
model2 <- lm(Level~High+Prcp,data)
model3 <- lm(Level~High+Wellbutrin+Prcp,data) #3 and 4 produce same High-Level relation
model4 <- lm(Level~High+Wellbutrin,data) 
model5 <- lm(Level~Prcp+Wellbutrin,data) #Bad

anova(model1,model2,model3)

#residual plots
par(mfrow=c(3,2))
plot(model1, which=1)
plot(model2, which=1)
plot(model3, which=1)
plot(model4, which=1)
plot(model5, which=1) #Terrible fit

g <- ggplot(data)
g + geom_point(aes(High,Level,color=Wellbutrin)) + geom_smooth(method="lm", mapping=aes(High,Level))