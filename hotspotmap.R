rm(list = ls())
library(sf)
library(mapview)
library(ggmap)
sales <- read.csv("C:\\Users\\zzr\\Downloads\\Iowa_Liquor_Sales.csv")
total <- cbind(sales$Store.Number,sales$Store.Location,sales$Sale..Dollars.)
colnames(total) <- c("num","location","dollar")
total <- as.data.frame(total)
total$dollar <- as.numeric(total$dollar)

# get store sum
total1<- matrix(0,nrow = 2218,ncol = 2)
total1<-aggregate(total$dollar,list(total$num), FUN=sum)
colnames(total1) <- c("store","dollar")

#get location
total3 <- cbind(sales$Store.Number,sales$Store.Location)
colnames(total3) <- c("num","location")
total3 <- as.data.frame(total3)
total3$location<-substr(total3$location,8,nchar(total3$location)-1 )
total3 <- unique(total3)
total3 <- total3[order(total3$num),]
total3$location[total3$location==""] <- NA
total3 <- na.omit(total3)

#add sales value
total1 <- cbind(total1,rep(0,2218))
colnames(total3) <- c("num","location","dollar")
for (i in 1:2215) {
  j <- which(total3[i,1]==total1[,1])
  total3[i,3] <- total1[j,2]
}

#wrong geo location 4722 9936 5876 

total3$lon<- c(1:length(total3$location))
total3$lat<- c(1:length(total3$location))
for (i in 1:length(total3$location)) {
  total3$lon[i] <- sapply(strsplit(total3$location[i], " "), "[", 1)
  total3$lat[i] <- sapply(strsplit(total3$location[i], " "), "[", 2)
}

#initial
locations_sf <- st_as_sf(total3, coords = c("lon", "lat"), crs = 4326)
locations_sf <- locations_sf[-c(871,1867,2212),]
mapview(locations_sf)


#first 200 










#using ggmap
map <- get_stamenmap(bbox = c(left=-96.90981,bottom=40.00000, right=-89.75335,top=43.81075),
                     zoom=7,
                     maptype = "toner-hybrid")
ggmap(map) +
  geom_point(data = total3, aes(x = as.numeric(lon), y = as.numeric(lat),size = dollar))



#get sale per day
total3$perday<- total3$dollar/(365*4)
ggmap(map) +
  geom_point(data = total3, aes(x = as.numeric(lon), y = as.numeric(lat),size = perday))
#des moines
mapdm <- get_stamenmap(bbox = c(left=-93.0155,bottom=41.39638, right=-93.34671,top=41.80305),
                     zoom=11,
                     maptype = "toner-hybrid")
ggmap(mapdm) +
  geom_point(data = total3, aes(x = as.numeric(lon), y = as.numeric(lat),size = dollar))


