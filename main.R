library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)

setwd("C:/Users/Sarah/Documents/DataScience/Seasonal Affective")

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

#Read in Mood data
cat("Reading mood data...\n")
read.csv("iMoodJournal.csv", stringsAsFactors=FALSE) %>%
        select(c(1,5,214,7)) %>% 
        rename(Date="ï..Date", Wellbutrin=wellbutrin) -> moods
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


#Plot some shit
cat("Plotting...\n")
g<-ggplot(data)

# g +
#     theme_light(base_family="serif") +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     geom_point(aes(Date,Prcp, color = "Precipitation"), 
#                 shape = 20, 
#                 alpha = 1/2
#                 ) +
#     geom_smooth(mapping=aes(Date, Prcp, color = "Precipitation"),
#                 se    = FALSE
#                 ) +
#     geom_point(aes(Date, (Level/10 - .25), color = "Mood"), 
#                 shape = 20, 
#                 alpha = 1/2
#                 ) +
#     geom_smooth(mapping=aes(Date, (Level/10 - .25), color = "Mood"),
#                 se    = FALSE
#                 ) +
#     geom_point(aes(Date, .1, color= "Fog"),
#                 data = subset(data, FRSHTT>=100000),
#                 shape=17,
#                 ) +
#     ylim(0,.75) +
#     ylab("Precipitation (in)") +
#     ggtitle("Precipitation and Mood") +
#     scale_colour_manual( name   = "",
#                          values = c(Precipitation = "cadetblue", 
#                                     Mood = "grey47",
#                                     Fog = "seagreen"
#                                     )
#                         )


# g +
#     theme_light(base_family="serif") +
#     theme(plot.title = element_text(hjust = 0.5)) +
#     geom_point(aes(Date,High, color = "HighTemp"), 
#                 shape = 20, 
#                 alpha = 1/2
#                 ) +
#     geom_smooth(mapping=aes(Date, High, color = "HighTemp"),
#                 se    = FALSE
#                 ) +
#     geom_point(aes(Date, (Level*8+60), color = "Mood"), 
#                 shape = 20, 
#                 alpha = 1/2
#                 ) +
#     geom_smooth(mapping=aes(Date, (Level*8+60), color = "Mood"),
#                 se    = FALSE
#                 ) +
#     ylab("Temperature (F)") +
#     ggtitle("Temperature and Mood") +
#     scale_colour_manual( name   = "",
#                          values = c(HighTemp = "indianred", 
#                                     Mood="grey47"
#                                     )
#                         )

g +
    theme_light(base_family="serif") +
    theme(plot.title = element_text(hjust = 0.5)) +
    geom_point(aes(Date,Wellbutrin, color = "Antidepressant"), 
                shape = 20, 
                alpha = 1/2
                ) +
    geom_point(aes(Date, (Level-5.5)*150, color = "Mood"), 
                shape = 20, 
                alpha = 1/2
                ) +
    geom_smooth(mapping=aes(Date, (Level-5.5)*150, color = "Mood"),
                se    = FALSE
                ) +
    ylab("Dose (mg)") +
    ggtitle("Medication and Mood") +
    scale_colour_manual( name   = "",
                         values = c(Antidepressant = "darkorchid", 
                                    Mood="grey47"
                                    )
                        )

