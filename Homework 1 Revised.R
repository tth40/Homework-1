#Note: I collaborated with Christina Bi during this assignment
#first import the data
audubon.data<- read.csv("audubon_data.csv", header=TRUE, sep = ",", na.strings="NA")
#audubon.data[audubon.data==""]<-NA
#audubon.data[audubon.data=="N/A"]<-NA
#Correction: need to reformat data
audubon.data$Date<- as.Date(audubon.data$Date, format="%m/%d/%y")
#received an error about "nchar needs a character vector", this command should not have any errors
#coordinate conversoins, actual numerical values are ok though
audubon.data$Latitude<-gsub("N", "", audubon.data$Latitude)
audubon.data$Longitude<-gsub("W","-", audubon.data$Longitude)
#audubon.data$Longitude<-as.numeric(audubon.data$Longitude)
#audubon.data$Latitude<-as.numeric(audubon.data$Latitude)
#coordinates now formatted properly

#Filtration
audubon.data$Survey_Type[grep("non", audubon.data$Survey_Type, ignore.case = TRUE)]<-"NA"
audubon.data$Survey_Type[grep("trans", audubon.data$Survey_Type, ignore.case = TRUE)]<-"transect"
#audubon.data.revised<-subset(audubon.data, Survey_Type="transect")
audubon.data.revised<-subset(audubon.data, Date>= "2010-01-01")
audubon.data.revised<-audubon.data.revised[,c("Longitude", "Latitude", "Date", "Survey_Type")]
#double check for dataset:
summary(audubon.data.revised)
#looks good
#initial filtering
audubon.data.revised<-audubon.data.revised[grepl("transect", audubon.data.revised$Survey_Type, ignore.case=TRUE), ]
audubon.data.revised[!grepl("nontransect", 
                            audubon.data.revised$Survey_Type), ]

#import in gw.data
#Need to find typo here
gw.data <- read.csv("gw_data_mac.csv", header=TRUE, sep = ",", na.strings="NA")
gw.data$Date<-as.Date(gw.data$Date, format="%d-%b-%y")
head(gw.data)
gw.data$Survey_Type[grep("non", gw.data$Survey_Type, ignore.case = TRUE)]<-"NA"
#gw.data$Survey_Type[grep("trans", gw.data$Survey_Type, ignore.case = TRUE)]<-"transect"
#gw.data$Longitude<- as.character(gw.data$Longitude)
gw.data$Longitude<-as.character(gw.data$Longitude)
list1<-sapply(strsplit(gw.data$Longitude, "°"), "[[", 2)
typeof(list1)
#get rid of the 2 cahracters at end
list1<-gsub("'W", "", list1)
list1
#now make numeric in order to perform calculations
list1<-as.numeric(list1)
#conversion of degrees (divide by 60 as indicated by the assignment)
list1<-list1/60
#subtract from the base of -77
gw.data$Longitude<-(-77 -list1)


#Old code that is not needed right now
#longitude<- data.frame(degrees=sapply(list1, "[[", 1),
#minutes=sapply(list1, "[[", 2))
#as.character(longitude$minutes)
#longitude$minutes<-substr(longitude$minutes, 1, longitude$minutes-2) #getting rid of two characters at end
#longitude$minutes<-as.numeric(longitude$minutes)
#longitude$minutes<-(longitude$minutes/60)
#longitude$degrees<-as.numeric(longitude$degrees)
#gw.data$Longitude<-((longitude$degrees+longitude$minutes)*(-1)) #negative one to account for westward direction


#same method for latitude
gw.data$Latitude<-as.character(gw.data$Latitude)
list2<-sapply(strsplit(gw.data$Latitude, "°"), "[[", 2)
typeof(list2)
#get rid of the 2 cahracters at end
list2<-gsub("'N", "", list2)
list2
#now make numeric in order to perform calculations
list2<-as.numeric(list2)
#conversion of degrees (divide by 60 as indicated by the assignment)
list2<-list2/60

gw.data$Latitude<-(38+list2)
#this can be done since all latitudes start with 38
#old code
#latitude<-data.frame(degrees=sapply (list2, "[[", 1), 
#                    minutes=sapply(list2, "[[", 2))
#latitude$minutes<-substr(latitude$minutes, 1, latitude$minutes-2)
#latitude$minutes<-as.numeric(latitude$minutes)
#latitude$minutes<-(latitude$minutes/60)
#latitude$degrees<-as.numeric(latitude$degrees)
#gw.data$Latitude<-((latitude$degrees+latitude$minutes))


gw.data.final<-subset(gw.data, Date>= "2010-01-01")
#gw.data.final<-subset(gw.data.final, Survey_Type=="transect")
gw.data.final<-gw.data.final[,c("Longitude", "Latitude", "Date", "Survey_Type")]
#double check dataset now:
summary(gw.data.final)
#filter here also
gw.data.final<-total_data[grepl("transect", gw.data.final$Survey_Type, ignore.case=TRUE), ]
gw.data.final[!grepl("nontransect", 
                     gw.data.final$Survey_Type), ]

nat_geo.data<-read.csv("nat_geo_data.csv", header=TRUE, sep = ",", na.strings="NA")
#nat_geo.data$Survey_Type[grep("trans", nat_geo.data$Survey_Type, ignore.case = TRUE)]<-"transect"
nat_geo.data$Survey_Type[grep("non", nat_geo.data$Survey_Type, ignore.case = TRUE)]<-"NA"
nat_geo.data$Date<-as.Date(nat_geo.data$Date)
nat_geo.data.final<-subset(nat_geo.data, Date>= "2010-01-01")
nat_geo.data.final<-subset(nat_geo.data, Survey_Type=="transect")
nat_geo.data.final<- nat_geo.data.final[,c("Longitude", "Latitude", "Date", "Survey_Type")]

#double check dataset now:
summary(nat_geo.data.final)
#max longitude is 38.93, and min latitude in -77.05, this causes concern that a few datapoints have switched coordiantes.
#fix this by reversing these points

#first: ID these points(find out which longitude points are positive!)
nat_geo.data.final[which(nat_geo.data.final$Longitude > 0), ]

reverse<-nat_geo.data.final[which(nat_geo.data.final$Longitude > 0), ]
#swap by setting longtitude to latitude and vice versa for the three points that have this error
nat_geo.data.final$Longitude[which(nat_geo.data.final$Longitude >= 0)]<-reverse$Latitude
nat_geo.data.final$Latitude[which(nat_geo.data.final$Longitude >= 0)]<-reverse$Longitude


total_data<-rbind(audubon.data.revised, gw.data.final, nat_geo.data.final)
row.names(total_data)<-1:nrow(total_data)
#filter transects
#total_data<-subset(total_data, Survey_Type="transect")
#filter data again
total_data<-total_data[total_data$Date >= "2010-01-01",]
total_data<-total_data[grepl("transect", total_data$Survey_Type, ignore.case=TRUE), ]
total_data[!grepl("nontransect", 
                  total_data$Survey_Type), ]

#write file for second part, please set the working directory to your own desired folder

write.csv(total_data, file = "clean_data.csv", row.names = FALSE)




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

dim(total_data)