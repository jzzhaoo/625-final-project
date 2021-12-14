rm(list = ls())
library(sf)
library(mapview)
library(ggmap)
sales <- read.csv("C:\\Users\\zzr\\Downloads\\Iowa_Liquor_Sales.csv")
total <- cbind(sales$Store.Number,sales$Store.Location,sales$Sale..Dollars.,sales$Date)
colnames(total) <- c("store","location","dollar","date")
total <- as.data.frame(total)
total$dollar <- as.numeric(total$dollar)


#sort date
total$date1 <- substr(total$date,1,6)
total$date2 <- substr(total$date,9,10)
total$date <- paste0(total$date1,total$date2)
total <- total[-c(5,6)]
total$date <- as.Date(total$date,"%m/%d/%y")
total <- total[order(total$date),]

#2 years before covid
before <- total[1:4720306,]
nstoreb <- length(unique(before$store))
storesumb <- matrix(0,nrow = nstoreb,ncol = 2)
storesumb <- aggregate(before$dollar,list(before$store), FUN=sum)
colnames(storesumb) <- c("store","dollar")

##add location
before <- cbind(sales$Store.Number,sales$Store.Location)
colnames(before) <- c("store","location")
before <- as.data.frame(before)
before$location<-substr(before$location,8,nchar(before$location)-1 )


before <- unique(before)
before <- before[order(before$store),]
before$location[before$location==""] <- NA
before <- na.omit(before)
before <- before[!duplicated(before$store),]


##add sales value
storesumb <- cbind(storesumb,rep(0,1834))
colnames(storesumb) <- c("store","dollar","location")
for (i in 1:1834) {
  j <- which(storesumb[i,1]==before[,1])
  if(length(j) == 0){
  }else{
    storesumb[i,3] <- before[j,2]
  }
}

before$lon<- c(1:length(before$location))
before$lat<- c(1:length(before$location))
for (i in 1:length(before$location)) {
  before$lon[i] <- sapply(strsplit(before$location[i], " "), "[", 1)
  before$lat[i] <- sapply(strsplit(before$location[i], " "), "[", 2)
}


###store with no location
nolocation <- which(before$location == "")
nolocation <- before[nolocation,1]
nolocation <- length(unique(nolocation))



# covid 2 years
covid <- total[4720307:9913937,]
nstorec <- length(unique(covid$store))

# get store sum
storesum <- matrix(0,nrow = 2218,ncol = 2)
storesum <-aggregate(total$dollar,list(total$stote), FUN=sum)
colnames(storesum) <- c("store","dollar")

#get location
total3 <- cbind(sales$Store.Number,sales$Store.Location)
colnames(total3) <- c("store","location")
total3 <- as.data.frame(total3)
total3$location<-substr(total3$location,8,nchar(total3$location)-1 )
total3 <- unique(total3)
total3 <- total3[order(total3$store),]
total3$location[total3$location==""] <- NA
total3 <- na.omit(total3)

#add sales value
total1 <- cbind(total1,rep(0,2218))
colnames(total3) <- c("store","location","dollar")
for (i in 1:2215) {
  j <- which(total3[i,1]==total1[,1])
  total3[i,3] <- total1[j,2]
}
#get sale per day




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












#using ggmap
map <- get_stamenmap(bbox = c(left=-96.90981,bottom=40.00000, right=-89.75335,top=43.81075),
                     zoom=7,
                     maptype = "toner-hybrid")
total3$sales <- log(total3$V3)
total3$perday<- total3$V3/(365*4)

ggmap(map) +
  geom_point(data = total3, aes(x = as.numeric(lon), y = as.numeric(lat),color = sales ))+

  scale_color_gradient(low="blue", high="red")





