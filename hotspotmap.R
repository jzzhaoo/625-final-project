#install the following library
library(sf)
library(mapview)
library(data.table)


names <- fread("C:\\Users\\zzr\\Downloads\\Iowa_Liquor_Sales.csv", nrows = 0)
total <- fread("C:\\Users\\zzr\\Downloads\\Iowa_Liquor_Sales.csv", select = c("Store Number","Store Location","Sale (Dollars)", "Date"))
colnames(total) <- c("store","location","dollar","date")
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
before <- cbind(total$store,total$location)
colnames(before) <- c("store","location")
before <- as.data.frame(before)
before$location<-substr(before$location,8,nchar(before$location)-1 )


before <- unique(before)
before <- before[order(before$store),]
before$location[before$location==""] <- NA
before <- na.omit(before)
before <- before[!duplicated(before$store),]


storesumb <- cbind(storesumb,rep(0,nstoreb))
colnames(storesumb) <- c("store","dollar","location")
for (i in 1:nstoreb) {
  j <- which(storesumb[i,1]==before[,1])
  if(length(j) == 0){
  }else{
    storesumb[i,3] <- before[j,2]
  }
}

before<- storesumb

###store with no location
nolocationb <- which(before$location == "0")
before <- before[-c(nolocationb),] #removed stores with no geo info
nolocationb <- before[nolocationb,1] #114 store with no geo info


before$lon<- rep(0,length(before$location))
before$lat<- rep(0,length(before$location))
for (i in 1:length(before$location)) {
  before$lon[i] <- sapply(strsplit(before$location[i], " "), "[", 1)
  before$lat[i] <- sapply(strsplit(before$location[i], " "), "[", 2)
}

#map with sf
before_sf <- st_as_sf(before, coords = c("lon", "lat"), crs = 4326)
before_sf <- before_sf[-c(768,1718),] #two wrong geo info
before_sf$log_sales_perday <- log(before_sf$dollar/730)
mapview(before_sf,zcol = "log_sales_perday" )










# covid 2 years
covid <- total[4720307:9913937,]
nstorec <- length(unique(covid$store))

storesumc <- matrix(0,nrow = nstorec,ncol = 2)
storesumc <- aggregate(covid$dollar,list(covid$store), FUN=sum)
colnames(storesumc) <- c("store","dollar")

##add location
covid <- cbind(total$store,total$location)
colnames(covid) <- c("store","location")
covid <- as.data.frame(covid)
covid$location<-substr(covid$location,8,nchar(covid$location)-1 )


covid <- unique(covid)
covid <- covid[order(covid$store),]
covid$location[covid$location==""] <- NA
covid <- na.omit(covid)
covid <- covid[!duplicated(covid$store),]



storesumc <- cbind(storesumc,rep(0,2044))
colnames(storesumc) <- c("store","dollar","location")
for (i in 1:2044) {
  j <- which(storesumc[i,1]==covid[,1])
  if(length(j) == 0){
  }else{
    storesumc[i,3] <- covid[j,2]
  }
}

covid<- storesumc

###store with no location
nolocationc <- which(covid$location == "0")
covid <- covid[-c(nolocationc),] #removed stores with no geo info
nolocationc <- covid[nolocationc,1] #132 store with no geo info


covid$lon<- rep(0,length(covid$location))
covid$lat<- rep(0,length(covid$location))
for (i in 1:length(covid$location)) {
  covid$lon[i] <- sapply(strsplit(covid$location[i], " "), "[", 1)
  covid$lat[i] <- sapply(strsplit(covid$location[i], " "), "[", 2)
}

#map with sf
covid_sf <- st_as_sf(covid, coords = c("lon", "lat"), crs = 4326)
covid_sf <- covid_sf[-c(740),] #one wrong geo info
covid_sf$log_sales_perday <- log(covid_sf$dollar/730)
mapview(covid_sf,zcol = "log_sales_perday" )

