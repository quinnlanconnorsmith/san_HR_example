#Example code for SCWH 2017 fish
#I've commented the code below, but feel free to reach out with any questions! -Q

#It's fairly easy to do KDE, but within small and complex systems (like this lake)
#There's lots of overlap with the surrounding land, so I used LKDE

packagelist <- c("adehabitatHR", "rgdal", "sp", "ggmap", "mapproj", "rgeos", "maptools", "raster", "leaflet", "move", "ctmm", "BBMM", "PBSmapping", "spatstat", "sf", "geosphere", "ggplot2", "SpatialPosition")
lapply(packagelist, require, character.only = TRUE)

#Below will format the .csv to implement LKDE bounds
#The points for limits have to correspond to specific guidelines - read the .pdf I attached

Sanlim <-read.csv("Sanlim.csv", header=T)

coordinates(Sanlim) <- c("x_coords","y_coords")
proj4string(Sanlim) <- CRS("+proj=longlat +datum=WGS84")

#Transform to UTM
utmlim <- spTransform(Sanlim, CRS("+proj=utm +zone=16 ellps=WGS84"))
utm_limpts <-data.frame(as(utmlim, "SpatialPoints"))
colnames(utm_limpts) <- c("UTM_W", "UTM_N")

#Create final data frame
finallimpts <- data.frame(UTM_W=utm_limpts$UTM_W,    UTM_N=utm_limpts$UTM_N)

#Defining coordinates and projection in the new data frame
coordinates(finallimpts) <- c("UTM_W","UTM_N")
projection(finallimpts) <- CRS("+proj=utm +zone=16 ellps=WGS84")

San_limits <- as(finallimpts,"SpatialLines")

####2017 All HRE####
#Here's MCP and LKDE for the fish in 2017

TDdata2017 <- read.csv("SanfordTD2017.csv", header =T)

TwentySeventeenTD <- leaflet(data = TDdata2017) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white")

TwentyThree17 <- TDdata2017[1:26,]
NinetyThree17 <- TDdata2017[27:47,]
OneEightyThree17 <- TDdata2017[48:68,]
TwoZeroThree17 <- TDdata2017[69:89,]
TwoFortyFour17 <- TDdata2017[90:114,]
TwoSixtyFour17 <- TDdata2017[115:135,]
TwoEightyThree17 <- TDdata2017[136:156,]
ThreeThirtyThree17 <- TDdata2017[157:182,]
ThreeFortyThree17 <- TDdata2017[183:203,]
ThreeFiftyThree17 <- TDdata2017[204:229,]
ThreeEightyFour17 <- TDdata2017[230:255,]
FourOneThree17 <- TDdata2017[256:280,]
FourTwentyThree17 <- TDdata2017[281:306,]
FourThirtyThree17 <- TDdata2017[307:332,]
FourFourtyOne17 <- TDdata2017[333:357,]
FourFiftyThree17 <- TDdata2017[358:382,]

coordinates(TDdata2017) <- c("Longitude","Latitude")
proj4string(TDdata2017) <- CRS("+proj=longlat +datum=WGS84")

#Transform to UTM
utm2017 <- spTransform(TDdata2017, CRS("+proj=utm +zone=16 ellps=WGS84"))
utm_locs2017 <-data.frame(as(utm2017, "SpatialPoints"))
colnames(utm_locs2017) <- c("UTM_W", "UTM_N")

#Create final data frame
TDdata_Final2017 <- data.frame(ID=as.factor(TDdata2017$Tag),UTM_W=utm_locs2017$UTM_W,UTM_N=utm_locs2017$UTM_N)

#Defining coordinates and projection in the new data frame
coordinates(TDdata_Final2017) <- c("UTM_W","UTM_N")
projection(TDdata_Final2017) <- CRS("+proj=utm +zone=16 ellps=WGS84")

#####MCPs#####

TD_hr_mcp <- mcp.area(TDdata_Final2017[,1], percent=c(50,95), unout=c("m2"),plot=F)


#Creating home range polygon
TD_mcp_poly <- mcp(TDdata_Final2017[,1],percent=95)
#Transform to lat long for mapping
TD_mcp_latlong <- spTransform(TD_mcp_poly, CRS("+proj=longlat +datum=WGS84"))

#####LIMITED Kernel density estimates#####

#Depending on computing power, the following lines will take a while to run

#Creating utilization distribution

#Depending on your scale, you can change h, extent, and grid size
#Read .pdf for more info regarding these values, there's also a semi-workaround depending
#on robustness of your data. For mine, I had to manually calculate these to adhere to the boundaries

TD_hr_kde_UD_Two <- kernelUD(TDdata_Final2017[,1], h=15, extent=3, boundary=San_limits, grid=1000)
#Using UD to make estimates
TD_hr_kde_Two <- kernel.area(TD_hr_kde_UD_Two, percent=c(50,95), unout=c("m2"))

#Creating the polygon
TD_kde_poly <- getverticeshr(TD_hr_kde_UD_Two, percent=95)
TD_kde_latlong <- spTransform(TD_kde_poly, CRS("+proj=longlat +datum=WGS84"))

TD_mcp_hr_23 <- fortify(subset(TD_mcp_latlong, id=="23"))
TD_mcp_hr_93 <- fortify(subset(TD_mcp_latlong, id=="93"))
TD_mcp_hr_183 <- fortify(subset(TD_mcp_latlong, id=="183"))
TD_mcp_hr_203 <- fortify(subset(TD_mcp_latlong, id=="203"))
TD_mcp_hr_244 <- fortify(subset(TD_mcp_latlong, id=="244"))
TD_mcp_hr_264 <- fortify(subset(TD_mcp_latlong, id=="264"))
TD_mcp_hr_283 <- fortify(subset(TD_mcp_latlong, id=="283"))
TD_mcp_hr_333 <- fortify(subset(TD_mcp_latlong, id=="333"))
TD_mcp_hr_343 <- fortify(subset(TD_mcp_latlong, id=="343"))
TD_mcp_hr_353 <- fortify(subset(TD_mcp_latlong, id=="353"))
TD_mcp_hr_384 <- fortify(subset(TD_mcp_latlong, id=="384"))
TD_mcp_hr_413 <- fortify(subset(TD_mcp_latlong, id=="413"))
TD_mcp_hr_423 <- fortify(subset(TD_mcp_latlong, id=="423"))
TD_mcp_hr_433 <- fortify(subset(TD_mcp_latlong, id=="433"))
TD_mcp_hr_441 <- fortify(subset(TD_mcp_latlong, id=="441"))
TD_mcp_hr_453 <- fortify(subset(TD_mcp_latlong, id=="453"))

TD_kde_hr_23 <- fortify(subset(TD_kde_latlong, id=="23"))
TD_kde_hr_93 <- fortify(subset(TD_kde_latlong, id=="93"))
TD_kde_hr_183 <- fortify(subset(TD_kde_latlong, id=="183"))
TD_kde_hr_203 <- fortify(subset(TD_kde_latlong, id=="203"))
TD_kde_hr_244 <- fortify(subset(TD_kde_latlong, id=="244"))
TD_kde_hr_264 <- fortify(subset(TD_kde_latlong, id=="264"))
TD_kde_hr_283 <- fortify(subset(TD_kde_latlong, id=="283"))
TD_kde_hr_333 <- fortify(subset(TD_kde_latlong, id=="333"))
TD_kde_hr_343 <- fortify(subset(TD_kde_latlong, id=="343"))
TD_kde_hr_353 <- fortify(subset(TD_kde_latlong, id=="353"))
TD_kde_hr_384 <- fortify(subset(TD_kde_latlong, id=="384"))
TD_kde_hr_413 <- fortify(subset(TD_kde_latlong, id=="413"))
TD_kde_hr_423 <- fortify(subset(TD_kde_latlong, id=="423"))
TD_kde_hr_433 <- fortify(subset(TD_kde_latlong, id=="433"))
TD_kde_hr_441 <- fortify(subset(TD_kde_latlong, id=="441"))
TD_kde_hr_453 <- fortify(subset(TD_kde_latlong, id=="453"))

#To view the 50% and 95% percent home ranges for minimum convex polygon and limited KDE, pull these two objects

TD_hr_mcp
TD_hr_kde_Two

#The following code will create maps, then if you call the object it will display
TwentyThreePFS <- leaflet(data = TwentyThree17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_23, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_23, lng=~long, lat=~lat, group="id", weight=1, color="blue")

TwentyThreePFS

NinetyThreePFS <- leaflet(data = NinetyThree17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_93, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_93, lng=~long, lat=~lat, group="id", weight=1, color="blue")


TwoZeroThreePFS <- leaflet(data = TwoZeroThree17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_203, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_203, lng=~long, lat=~lat, group="id", weight=1, color="blue")

TwoFortyFourPFS <- leaflet(data = TwoFortyFour17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_244, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_244, lng=~long, lat=~lat, group="id", weight=1, color="blue")

TwoSixtyFourPFS <- leaflet(data = TwoSixtyFour17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_264, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_264, lng=~long, lat=~lat, group="id", weight=1, color="blue")


TwoEightyThreePFS <- leaflet(data = TwoEightyThree17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_283, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_283, lng=~long, lat=~lat, group="id", weight=1, color="blue")


ThreeThirtyThreePFS <- leaflet(data = ThreeThirtyThree17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_333, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_333, lng=~long, lat=~lat, group="id", weight=1, color="blue")


ThreeFortyThreePFS <- leaflet(data = ThreeFortyThree17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_343, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_343, lng=~long, lat=~lat, group="id", weight=1, color="blue")


ThreeEightyFourPFS <- leaflet(data = ThreeEightyFour17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_384, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_384, lng=~long, lat=~lat, group="id", weight=1, color="blue")

FourOneThreePFS <- leaflet(data = FourOneThree17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_413, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_413, lng=~long, lat=~lat, group="id", weight=1, color="blue")

FourTwentyThreePFS <- leaflet(data = FourTwentyThree17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_423, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_423, lng=~long, lat=~lat, group="id", weight=1, color="blue")


FourFourtyOnePFS <- leaflet(data = FourFourtyOne17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_441, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_441, lng=~long, lat=~lat, group="id", weight=1, color="blue")


FourFiftyThreePFS <- leaflet(data = FourFiftyThree17) %>%
  addTiles(group="Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite") %>%
  addCircleMarkers(~Longitude, ~Latitude, popup = ~as.character(Tag.Date), radius=2, fillOpacity=0.9, color="white") %>%
  addPolygons(data=TD_mcp_hr_453, lng=~long, lat=~lat, group="id", weight=1, color="aquamarine")%>%
  addPolygons(data=TD_kde_hr_453, lng=~long, lat=~lat, group="id", weight=1, color="blue")
