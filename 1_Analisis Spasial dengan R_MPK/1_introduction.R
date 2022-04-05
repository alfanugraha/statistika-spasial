#mendefinisikan library yang digunakan
#install.packages(c("sp", "gstat"))
library(sp)
library(gstat)

#praktikum jenis data spasial dengan data meuse (sp)
data(meuse)
class(meuse)
summary(meuse)
str(meuse)
help(meuse)
meuse
View(meuse)

#titik
coordinates(meuse) <- c("x", "y")
class(meuse)
coordinates(meuse) <- ~x+y
plot(meuse,xlim=c(178600,181400),ylim=c(329000,333700))
title("points")

#polygon
data(meuse.riv)
View(meuse.riv)
help(meuse.riv)
meuse.1st <- list(Polygons(list(Polygon(meuse.riv)),"meuse.riv"))
meuse.sr <- SpatialPolygons(meuse.1st)
plot(meuse.sr, col = "grey")
title("polygons")

#grid
data(meuse.grid)
View(meuse.grid)
coordinates(meuse.grid) <- c("x", "y")
meuse.grid <- as(meuse.grid, "SpatialPixels")
image(meuse.grid, col = "grey")
title("grid")

#gabungan point, polygon, and grid
image(meuse.grid, col = "lightgrey")
plot(meuse.sr, col = "grey", add = TRUE)
plot(meuse,add = TRUE)


#PETA TEMATIK
#Loading data Set
library(spData)
library(rgdal)

data(columbus)
help(columbus)

#Data Columbus
str(columbus)
help(columbus)
View(columbus)

#Plot peta columbus
columbusmap <- readOGR(system.file("shapes/columbus.shp", package="spData"))
plot(columbusmap)
class(columbusmap)

spplot(columbusmap,"HOVAL",sub="Map of HOVAL")

#Input data eksternal
library(readxl)
setwd("C:\\asprak\\2022\\Spasial MPK\\1. Analisis Spasial dengan R")
data.giziburuk<-read_excel("data giziburuk jabar.xlsx", sheet="Sheet1")

#pengecekan data
dim(data.giziburuk)
str(data.giziburuk)
View(data.giziburuk)

#membuka data shp
jabar<-readOGR(dsn="petajabar27", layer="Peta Jabar 27")
plot(jabar)

library(raster)
text(jabar,'KABKOT',cex=0.5) #menambahkan nama wilayah pada peta

palette(rainbow(6))
plot(jabar,col=jabar$IDKAB2) #memberi warna pada peta

#Memberi warna peta berdasarkan data
jabar$gizi<-data.giziburuk$Giziburuk
spplot(jabar,"gizi",main="Peta Gizi Buruk Jawa Barat")
colfunc<-colorRampPalette(c("green", "yellow","red"))
spplot(jabar, "gizi", col.regions=colfunc(6), cuts = 5, main="Peta Gizi Buruk Bogor")



