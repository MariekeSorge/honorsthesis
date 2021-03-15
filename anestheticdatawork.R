setwd("C:/Users/sorge/Downloads")
getwd()
library(tidyverse)
install.packages("lubridate")
library(lubridate)
anesdata <- read.csv("anestheticdata1.csv")
animals <- read.csv("Allanimalscohabitationdatesraw.csv")
rm(list = ls())
setwd("C:/Users/sorge/Downloads/")
getwd()
euthanasiadata <- read.csv("Euthanasiaconcentrationdata.csv")
seasons <- read.csv("seasons.csv")
refusedfood <- read.csv("daysrefusedtoeat.csv")

#adding euthanasia data
names(euthanasiadata)[names(euthanasiadata) == "ï..Specimen"] <- "Specimen"
names(euthanasiadata)[names(euthanasiadata) == "Date"] <- "Date.Euthanized"
anesdataeuthrf <- merge(anesdatarf1, euthanasiadata, by = c("Specimen"), all.x = TRUE)

#rownames(refusedfood) <- refusedfood[,1]
#refusedfood <- data.frame(t(refusedfood[-1]))
names(refusedfood)[names(refusedfood) == "ï..B2"] <- "Specimen"
anesdata$numberofdays <- NA
daysrefusefood2 <-as.data.frame(matrix(ncol = 3, nrow = 1))
daysrefusefood2$Specimen <- NA
daysrefusefood2$numberofdays <- NA
daysrefusefood2$Date.of.Experiments <- NA
m<-15
for (m in (1:length(anesdata$Date.of.Experiments))){
  animal <- as.character(anesdata[m,1])
  experimentaldays <- anesdata[which(anesdata$Specimen == animal),]
  experimentaldays <- experimentaldays$Date.of.Experiments
  rfdates <- refusedfood[which(refusedfood$Specimen == animal),]
  rfdates <- data.frame(t(rfdates[-1]))
  compdates <- as.data.frame(rfdates[!(is.na(rfdates) | rfdates ==""), ])
  daysrefusefood <-as.data.frame(matrix(ncol = 3, nrow = length(experimentaldays)))
  if (length(compdates) == 0|| length(compdates[,1]) == 0) {
    daysrefusefood$numberofdays <- 0 } else {
    before <- as.data.frame(matrix(ncol = length(compdates[,1]), nrow = (length(experimentaldays))))
    exdate <- as.data.frame(matrix(ncol = 1, nrow = (length(experimentaldays))))
    rfdate <-as.data.frame(matrix(ncol = 1, nrow = (length(compdates[,1]))))
  for (i in (1:length(experimentaldays))) {
    exdate[i,1] <- as.numeric(as.Date(experimentaldays[i], format = "%m/%d/%Y"))
    for (p in (1:length(compdates[,1]))){
      rfdate[p,1] <- as.numeric(as.Date(compdates[p,1], format = "%m/%d/%Y"))
      before[i,p] <- rfdate[p,1] < exdate[i,1]
  }
  }
daysrefusefood$numberofdays <- rowSums(before == "TRUE")
} 

daysrefusefood$Date.of.Experiments <- experimentaldays
daysrefusefood$Specimen <- animal
anesdatarf <- merge(anesdata, daysrefusefood, by = c("Specimen", "Date.of.Experiments"), all.x = TRUE)
daysrefusefood2 <- rbind(daysrefusefood2, daysrefusefood)
anesdatarf <- merge(anesdata, daysrefusefood2, by.x = c("Specimen", "Date.of.Experiments"), all.x = TRUE, all.y = FALSE)
}
anesdatarf1 <- anesdatarf %>% distinct()
anesdatarf1$V1 <- NULL
anesdatarf1$V2 <- NULL
anesdatarf1$V3 <- NULL
anesdatarf1$numberofdaysx <- NULL


write.csv(anesdataeuthrf, "C:/Users/sorge/Documents/anestheticdataeuthanasiarf.csv", row.names = FALSE )
#length(unique(anesdata$Specimen))
#f <- subset(anesdata, anesdata$Sex.Determination == "F" & anesdata$Previous.Anesthetic.Events == "0")
seasons$Start <- as.Date(seasons$Start, format = "%m/%d/%Y",)
seasons$End <- as.Date(seasons$End, format = "%m/%d/%Y",)

#fill this in with correct dates once more information found
anesdataeuthrf$Date.Received <- as.IDate(anesdataeuthrf$Date.Received, format = "%m/%d/%Y")
anesdataeuthrf$Date.Received <- as.numeric(anesdataeuthrf$Date.Received)
library(data.table)
dt <-data.table(anesdataeuthrf$Date.Received)
dt[ , season := 0.0]
dt[ V1 > as.IDate("9/1/2018", format = "%m/%d/%Y") & V1 < as.IDate("12/1/2018", format = "%m/%d/%Y"), season := 1]
dt[ V1 > as.IDate("12/1/2018", format = "%m/%d/%Y") & V1 < as.IDate("3/1/2019", format = "%m/%d/%Y"), season := 2]
dt[ V1 > as.IDate("3/1/2019", format = "%m/%d/%Y") & V1 < as.IDate("5/1/2019", format = "%m/%d/%Y"), season := 3]
dt[ V1 > as.IDate("5/1/2019", format = "%m/%d/%Y") & V1 < as.IDate("9/1/2019", format = "%m/%d/%Y"), season := 4]
dt[ V1 > as.IDate("9/1/2019", format = "%m/%d/%Y") & V1 < as.IDate("12/1/2019", format = "%m/%d/%Y"), season := 1]

anesdataeuthrf$seasonaquired <- dt$season
anesdataeuthrf$Date.Received <- anesdatarf1$Date.Received
write.csv(anesdataeuthrf, "C:/Users/sorge/Documents/anesdataeuthrfseason.csv", row.names = FALSE )

#https://www.ncdc.noaa.gov/cdo-web/

chuckdataf <- subset(anesdata, select = c(Specimen, Date.Received ))
final <- chuckdataf[!duplicated(chuckdataf$Specimen),]
#write.csv(final,"C:/Users/sorge/Documents/animalsdaterecieved.csv", row.names = FALSE )
# cohabitation 
#days in raceway with cohabitation
animals$Date.Received <- as.Date(animals$Date.Received, format = "%m/%d/%Y")

animals$Final.date <- as.Date(animals$Final.date, format = "%m/%d/%Y")
animals$Moved.to.Raceway.1 <- as.Date(animals$Moved.to.raceway1, format = "%m/%d/%Y")
animals$moved.to.raceway2 <- as.Date(animals$Moved.to.raceway.2, format = "%m/%d/%Y")

raceway1animals <- animals[animals$Raceway == "1",]
raceway2animals <- animals[animals$Raceway == "2",]


dates1 <- raceway1animals[!duplicated(raceway1animals$Specimen),]
dates2 <- raceway2animals[!duplicated(raceway2animals$Specimen),]
dates1$Final.date <- as.Date(dates1$Final.date, format = "%m/%d/%Y")
dates1$Date.Received <- as.Date(dates1$Date.Received, format = "%m/%d/%Y")
#dates1$Date.of.Experiments <- as.Date(dates1$Date.of.Experiments, format = "%m/%d/%Y")
dates2$Final.date <- as.Date(dates2$Final.date, format = "%m/%d/%Y")
dates2$Date.Received <- as.Date(dates2$Date.Received, format = "%m/%d/%Y")
#dates2$Date.of.Experiments <- as.Date(dates2$Date.of.Experiments, format = "%m/%d/%Y")

dates1$datesinraceway <- interval(dates1$Date.Received, dates1$Final.date)
dates2$datesinraceway <- interval(dates2$Date.Received, dates2$Final.date)

rows <- (1:(length(dates1$Specimen)))
overlaps <- data.frame()
periodoverlap <- matrix ( ncol = length(dates1$datesinraceway), nrow = length(dates1$datesinraceway))
periodoverlaps <- matrix ( ncol = length(dates1$datesinraceway), nrow = length(dates1$datesinraceway))
peroidoverlaps <- data.frame()
x <- 1
y <- 1
for (x in rows){
  for (y in rows){
  a <- dates1$datesinraceway[x]
  b <- dates1$datesinraceway[y]
    periodoverlap <- intersect(a,b)
    periodoverlaps[x,y] <- as.data.frame.vector(periodoverlap)
  overlaps[x,y] <- (as.duration(intersect(dates1$datesinraceway[x], dates1$datesinraceway[y]))/(60*60*24))
  }
  }
selfoverlap <- as.data.frame(diag(as.matrix(overlaps[,])))
totaldaysduplicates <- (rowSums(overlaps, na.rm = TRUE) - selfoverlap$`diag(as.matrix(overlaps[, ]))`) 

notna <- data.frame()
for (m in (1:length(dates1$Specimen))){
  notna[m,] <- length(which(!is.na(overlaps[m,])-1 ))
}
notna <- notna$V1  



maxanimals <- as.data.frame(notna)
#list of columns without NAs 
#Sum the rows without NAs 
#subtract the overlap of the rows without NAs
m <- 18
for (m in (1:animals$Specimen)) {
  thisanimal <- overlaps[m,]
  notnarows <- as.data.frame(colnames(overlaps[which( !(overlaps[m,] == "NA"))]))
  notna <- overlaps[which( !(overlaps[m,] == "NA"))]
  notnaanimal <- notna[m ,]
  animalranking <- as.data.frame(matrix (ncol=3, nrow                                         = length(notnaanimal)))
  
  #x <- 5
  n <- length(notnaanimal)
  for (x in (1:(n))){
  animal <- sort(notnaanimal, decreasing = TRUE)
  animalID <- as.data.frame(colnames(animal))
  animalranking$ID <- animalID$`colnames(animal)`
  animalranking$index[x] <- which(names(overlaps)== animalID[x,])
  animalranking$days[x] <- animal[1, x]
  #animalranking <- animalranking[seq(dim(animalranking)[1],1),]
   }

#comparedvalues <- data.frame()
#for (x in (1:length(animalranking$ID))){
  #for (y in (1:length(animalranking$ID))){
    #comparedvalues[x,y] <- overlaps[animalranking$index[x], animalranking$index[y]]
#  }
#}

addeddays <- data.frame()
doublecount <- data.frame()
totaldays <- as.data.frame(matrix(ncol = length(animalranking)))
# I need to get the peroid overlap to be in interval not a duration
n <- 4
p <- 1
#in this loop n corresponds to the index of the 2nd 3rd fourth... animals by order of the number of days they overlap with the animal of interest
#m corresponds to the index of the animal of interest in overlaps
#p corresponds to the indices of the other animals with overlapping time periods with the animal of interest
for (p in (3:length(animalranking))){
  for (n in (3:length(animalranking))){
    animalofinterest <- m
    animalcompared <- animalranking$index[n]
    animalcomparedto <- animalranking$index[p]
    intersect(dates1$datesinraceway[animalofinterest],dates1$datesinraceway[animalcompared], dates1$datesinraceway[animalcomparedto])
    
  doublecount[n,p] <- as.duration(intersect(periodoverlap[p,m], periodoverlap[n,m]))
  doublecountdays <- rowSums(doublecount[n, c((n+1):length(animalranking))], na.rm = true)
  addeddays[n,1] <- (overlap[m,n]- doublecountdays[n,])
  #daystoadd <- as.data.frame(colSums(addeddays, na.rm = TRUE))
  }
}
totaldays[m,1] <- as.data.frame(animalranking$days[1] + addeddays[1,])
}
#this is giving me an NA value 
animalofinterest <- m
animalcompared <- animalranking$index[n]
animalcomparedto <- animalranking$index[p]
intersect(dates1$datesinraceway[animalofinterest],dates1$datesinraceway[animalcompared], dates1$datesinraceway[animalcomparedto])

#for (x in rows){
  #for (y in rows){
    #overlaps[x,y] <- (as.duration(intersect(dates1$datesinraceway[x], dates1$datesinraceway[y]))/(60*60*24))}
}
totaldays <- maxdays + (maxdays - compvalues) #this give us the days from the animals with the max shared days plus any days that do not overlap with that time period 
#still need to address the ones that not in the max but might overlap with others not in the max 
#for all in notnaanimal rank 3 or less 




dates1$maximumanimals <- (maxanimals$`rowSums(is.na(overlaps))` - 1)/2
dates1$dayscohabitation <- (dates1$totaldays/ dates1$maximumanimals)/2

rows <- (length(dates2$Specimen))
overlaps2 <- data.frame()
for (x in rows){
  for (y in rows){
    overlaps2[x,y] <- (as.duration(intersect(dates2$datesinraceway[x], dates2$datesinraceway[y]))/(60*60*24))}}
selfoverlap <- as.data.frame(diag(as.matrix(overlaps2[,])))
dates2$totaldays <- rowSums(overlaps, na.rm = TRUE) - selfoverlap$`diag(as.matrix(overlaps2[, ]))` 
dates2$maximumanimals <- (length(dates2$Specimen)-as.data.frame(rowSums(is.na(overlaps2))))

maxanimals <- as.data.frame(length(dates2$Specimen)-(as.data.frame(rowSums(is.na(overlaps)))))
dates2$maximumanimals <- maxanimals$`rowSums(is.na(overlaps))` - 1
dates2$dayscohabitation <- dates2$totaldays/ dates2$maximumanimals

d1 <- dates1 %>% select( c("Specimen", "totaldays", "maximumanimals", "dayscohabitation"))
d2 <- dates2 %>% select(c("Specimen", "totaldays", "maximumanimals", "dayscohabitation"))



anesdatat <- merge(anesdata, d1, by = "Specimen", all.y = TRUE)
anesdatax <- merge(anesdata, d2, by = "Specimen", all.y = TRUE)
anesdata3 <- rbind(anesdatat, anesdatax)

write.csv(anesdata3,"C:/Users/sorge/Documents/anestheticdata3.1.csv", row.names = TRUE )


anesdata3$timeinvivarium <- as.numeric(as.Date(anesdata3$Date.Euthanized..or.Found.Dead.,format = "%m/%d/%Y")) - as.numeric(as.Date(anesdata3$Date.Received, format = "%m/%d/%Y"))

anesdata$TimeinVivarium <- as.numeric(anesdata$Date.Euthanized..or.Found.Dead.) - as.numeric(anesdata$Date.Received)
anesdata$Date.of.Experiments <- as.Date(anesdata$Date.of.Experiments, format = "%m/%d/%Y")
anesdata$Date.Received <- as.Date(anesdata$Date.Received, format = "%m/%d/%Y")



#finding the number of days cohabitating before experiment

#raceway 1 animals prior to experimental dates
dates1$exdates <- interval(dates1$Date.Received, dates1$Date.of.Experiments)
dates2$exdates <- interval(dates2$Date.Received, dates2$Date.of.Experiments)
rowsx <- (1:45)
rowsy <- (1:15)
for (x in rowsx){
  for (y in rowsy){
    overlaps[x,y] <- (as.duration(intersect(raceway1animals$exdates[x], dates1$datesinraceway[y]))/(60*60*24))}}

selfoverlap <- as.data.frame(diag(as.matrix(overlaps[,])))
dates1$priorexdates <- rowSums(overlaps, na.rm = TRUE) - selfoverlap$`diag(as.matrix(overlaps[, ]))` 
dates1$maximumanimals <- 45-as.data.frame(rowSums(is.na(overlaps)))


#raceway 2 animals prior to experient dates 
rowsx <- (1:28)
rowsy <- (1:10)
overlaps <- data.frame()
for (x in rows){
  for (y in rows){
    overlaps[x,y] <- (as.duration(intersect(dates1$datesinraceway[x], dates1$datesinraceway[y]))/(60*60*24))}}
selfoverlap <- as.data.frame(diag(as.matrix(overlaps[,])))
anesdata$dayscohabitationpriortoexperiment <- rowSums(overlaps, na.rm = TRUE) - selfoverlap$`diag(as.matrix(overlaps[, ]))` 
dates1$maxanimalscohabitating <- 15-as.data.frame(rowSums(is.na(overlaps)))
merge <- data.frame()
dates <- merge(dates1, dates2)

anesdata1 <- merge(anesdata, dates1, by = c("Specimen"), all.y = TRUE)

exboth <- rbind(raceway1animals, raceway2animals)
anesdata <- merge(anesdata, exboth, by = "Specimen")

