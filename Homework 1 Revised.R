
#Note: I collaborated with Christina Bi during this assignment
#first import the data
audubon.data<- read.csv("~/Desktop/BIO 1 Cred/Data Analysis with R/Homework Assignment 1/audubon_data.csv", header=TRUE, sep = ",", na.strings="NA")
audubon.data[audubon.data==""]<-NA
audubon.data[audubon.data=="N/A"]<-NA
audubon.data$Date<- as.Date(audubon.data$Date, format="%m/%d/%y")
audubon.data$Lattitude<-gsub("N", "", audubon.data$Latitude)
audubon.data$Longitude<-gsub("W","-", audubon.data$Longitude)
audubon.data$Longitude<-as.numeric(audubon.data$Longitude)
audubon.data$Latitude<-as.numeric(audubon.data$Latitude)

audubon.data$Survey_Type[grep("non", audubon.data$Survey_Type, ignore.case = TRUE)]<-"NA"
audubon.data$Survey_Type[grep("trans", audubon.data$Survey_Type, ignore.case = TRUE)]<-"transect"
audubon.data.revised<-subset(audubon.data, Survey_Type="transect")
audubon.data.revised<-subset(audubon.data.revised, Date>= "2010-01-01")
audubon.data.revised<-audubon.data.revised[,c("Longitude", "Latitude", "Date", "Survey_Type")]


#import in gw.data
gw.data <- read.csv("~/Desktop/BIO 1 Cred/Data Analysis with R/Homework Assignment 1/gw_data_mac.csv", header=TRUE, sep = ",", na.strings="NA")
gw.data$Date<-as.Date(gw.data$Date, format="%d-%b-%y")
gw.data$Survey_Type[grep("trans", gw.data$Survey_Type, ignore.case = TRUE)]<-"transect"
gw.data$Longitude<- as.character(gw.data$Longitude)
list1<-strsplit(gw.data$Longitude, "°")
longitude<- data.frame(degrees=sapply(list1, "[[", 1),
                       minutes=sapply(list1, "[[", 2))
as.character(longitude$minutes)
longitude$minutes<-substr(longitude$minutes, 1, longitude$minutes-2) #getting rid of two characters at end
longitude$minutes<-as.numeric(longitude$minutes)
longitude$minutes<-(longitude$minutes/60)
longitude$degrees<-as.numeric(longitude$degrees)
gw.data$Longitude<-((longitude$degrees+longitude$minutes)*(-1)) #negative one to account for westward direction

gw.data$Latitude<-as.character(gw.data$Latitude)
list2<-strsplit(gw.data$Latitude, "°")
latitude<-data.frame(degrees=sapply (list2, "[[", 1), 
                      minutes=sapply(list2, "[[", 2))
latitude$minutes<-substr(latitude$minutes, 1, latitude$minutes-2)
latitude$minutes<-as.numeric(latitude$minutes)
latitude$minutes<-(latitude$minutes/60)
latitude$degrees<-as.numeric(latitude$degrees)
gw.data$Latitude<-((latitude$degrees+latitude$minutes))
gw.data.final<-subset(gw.data, Date>= "2010-01-01")
gw.data.final<-subset(gw.data.final, Survey_Type=="transect")
gw.data.final<-gw.data.final[,c("Longitude", "Latitude", "Date", "Survey_Type")]


nat_geo.data<-read.csv("~/Desktop/BIO 1 Cred/Data Analysis with R/Homework Assignment 1/nat_geo_data.csv", header=TRUE, sep = ",", na.strings="NA")
nat_geo.data$Survey_Type[grep("trans", nat_geo.data$Survey_Type, ignore.case = TRUE)]<-"transect"
nat_geo.data$Date<-as.Date(nat_geo.data$Date)
nat_geo.data.final<-subset(nat_geo.data, Date>= "2010-01-01")
nat_geo.data.final<-subset(nat_geo.data, Survey_Type=="transect")
nat_geo.data.final<- nat_geo.data.final[,c("Longitude", "Latitude", "Date", "Survey_Type")]


total_data<-rbind(audubon.data.revised, gw.data.final, nat_geo.data.final)
row.names(total_data)<-1:nrow(total_data)
na.omit(total_data)

write.csv(total_data, file = "clean_data.csv", row.names = FALSE)

setwd("~/Desktop/BIO 1 Cred/Data Analysis with R/Homework Assignment 1")


##Assignment 1 Map Plotting Template

library(sp)
library(rgdal)

clean_data <- read.csv("clean_data.csv")

plotting_data <- SpatialPoints(clean_data[, c("Longitude", "Latitude")])

#Map of DC neighborhoods from maps2.dcgis.dc.gov
dc <- readOGR("Neighborhood_Clusters-shp", "Neighborhood_Clusters")

#Plot the map of DC


par(mar = c(1, 1, 1, 1))

plot(
  dc,
  col = "darkgrey",
  border = "white",
  main = "District of Columbia Bird Sightings"
)
plot(dc[46, ],
     add = TRUE,
     col = "#718BAE80",
     border = "white")


#Add your data

plot(plotting_data,
     add = TRUE,
     pch = 16,
     cex = 0.25)
