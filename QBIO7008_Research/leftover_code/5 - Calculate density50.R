########
library(adehabitatHR)
library(maptools)
library(rgdal)
library(spatstat)
library(rgeos)
library(reshape2)
library(raster)
library(devtools)
library(dplyr)
setwd("~/Desktop/Masters student")
## Get data
data<-read.csv("FinalCelineStudentData.csv", stringsAsFactors=FALSE, header = TRUE)
# data[1]<-NULL
# data[6]<-NULL # dont need location


## Read the date and time properly in R
data$Time<-gsub("7am-10am", "07:00", data$Time)
data$Time<-gsub("10am-1pm", "07:00", data$Time)
data$Time<-gsub("1pm-3pm", "13:00", data$Time)
data$datetime<-paste(data$Date, data$Time, sep=" ")
str(data)

# data$datetime<-strptime(data$datetime, format=c("%d/%m/%Y %H:%M"))
# data$datetime<-as.POSIXct(data$datetime)
head(data$datetime)

data$Lat<- ((((data$Lat/1000)+27)/60)+27)*-1
data$Long<-as.numeric(data$Long)
data$Long<-(((data$Long/1000)+1)/60)+153
names(data)[4:5]<-c("Y","X")
data<-data[c(3,6,4,5,7,2,1)]
head(data)
data<-na.omit(data)


# trim outliers from spatial data
plot(data$X, data$Y, col=as.factor(data$Name))

data1 <- data[data$X < 153.022,]
data1 <- data1[data1$Y > -27.518,]
data1 <- data1[data1$X < 153.021,]
data1 <- data1[data1$Y < -27.458,]
plot(data1$X, data1$Y, col=as.factor(data1$Name))

data=data1
# count number of sightings per season 
# Group by count of multiple columns
head(data)
sight <- data %>% dplyr::group_by(Season, Name) %>% 
  dplyr::summarise(total_count=n(),.groups = 'drop') %>% 
  as.data.frame()
colnames(sight)[3] <- "nsight"


## Only keep dragons with at least 20 sightings

dat<-merge(data, sight, by=c("Name","Season"))
#write.csv(dat,"Latest_database_2012-2021_season.csv")
rm(sight)
rm(data)
rm(data1)




data<-subset(dat, dat$nsight > 19)
data<-droplevels(data)



xydata<-cbind(data$X,data$Y)
xydata2 <- as.data.frame(project(xydata, "+proj=tmerc +lat_0=-28 +lon_0=153 +k=0.99999 +x_0=50000 +y_0=100000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
xydata3 <- data.frame(data$Name,data$datetime,xydata2, data$Season)


colnames(xydata3)<-c("Name","Datetime","X","Y", "Season")

x <- seq(min(xydata3[,"X"])-20,max(xydata3[,"X"])+20,by=1) # where resolution is the pixel size you desire
y <- seq(min(xydata3[,"Y"])-20,max(xydata3[,"Y"])+20,by=1)
xy <- expand.grid(x=x,y=y)
coordinates(xy) <- ~x+y
gridded(xy) <- TRUE
class(xy)


uniq <- unique(dat$Season)

results <- data.frame(1,1,1,1)
colnames(results) <- c( "Name", "NumOverlap50", "kdarea", "Season")
results[,1:4] <- NA
i=1

for(i in 1:length(uniq)){
  xydata4 <- xydata3[xydata3$Season == uniq[i],]
  
  # remove this subsetting before running
  names <- unique(xydata3$Name)
  xydata3 <- xydata3[xydata3$Name %in% names[1:2],]

  
  hrxydata <- SpatialPointsDataFrame(xydata4[,3:4],xydata4[c("Name","Datetime")])
  hrxydata.df <- as.data.frame(hrxydata, xydata4$Season)
  hrxydata.df.names <- as.data.frame(table(hrxydata.df$Name))
 
  uds7 <- kernelUD(hrxydata[,1], grid=xy, h= 7) ## h = 7 means a smoothing parameter of 7 metres, this has been 
  
  # calculate home range area
  kdareas <- kernel.area(uds7, percent = 50, unin = c("m"), unout = c("m2"))
  kdareas <- data.frame(t(kdareas), row.names = colnames(kdareas))
  # colnames(kdareas)<-c("HR50","Name")
  
  # calculate the HR overlap - the area of overlap that each focal has with all other inds
  UDOIoverlap50 <- kerneloverlaphr(uds7, method = c("UDOI"), percent=50)
  NumOverlap50 <- data.frame(colSums(UDOIoverlap50 > 0))
  
  df <- merge(NumOverlap50, kdareas, by= 0)
  colnames(df) <- c("Name", "NumOverlap50", "kdarea")
  df$Season <- uniq[i]
  results <- rbind(results, df)
  } 

