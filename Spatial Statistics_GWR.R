install.packages("spgwr")
library(spgwr)
library(sp)
library(spData)

library(spdep)
library(sf)

install.packages("tmap")
library(tmap)

#Input data yang akan digunakan
gizi.buruk<-read.csv(file.choose(), sep=";", header=TRUE)
head(gizi.buruk)

library(spdep);library(maptools);library(raster);library(maptools)
library(spatialreg);library(leaflet);library(RColorBrewer)
library(raster);library(ggplot2)
library(Matrix)
library(spatialreg)

Indonesia<-getData('GADM', country='IDN', level=3) # memanggil shape.file Bandung
Bandung<-Indonesia[Indonesia$NAME_2 == "Kota Bandung",]
Bandung@data<-gizi.buruk

UTM = CRS("+proj=robin +datum=WGS84") #Transform to Meters 
BandungUTM = spTransform(Bandung, UTM)

library(spgwr)
#u/ mendapatkan bandwith optimum
gwr.b1<-gwr.sel(Gizi.Buruk ~ PHBS, BandungUTM)

#Running GWR
gwr.fit1<-gwr(Gizi.Buruk ~ PHBS, data =BandungUTM, bandwidth = gwr.b1, se.fit=T, hatmatrix=T)
gwr.fit1

gwr.b2<-gwr.sel(Gizi.Buruk ~ PHBS, BandungUTM, gweight = gwr.bisquare)
gwr.fit2<-gwr(Gizi.Buruk ~ PHBS, data =BandungUTM, bandwidth = gwr.b2, gweight = gwr.bisquare, se.fit=T, hatmatrix=T)
gwr.b2
gwr.fit2

gwr.b3<-gwr.sel(Gizi.Buruk ~ PHBS, data=BandungUTM, adapt = TRUE)
#UNTUK MENENTUKAN KERNEL ADAPTIF, TENTUKAN ADAPT = TRUE SAAT MENGUNAKAN BANDWIDTH OPTIMAL MENGGUNAKAN gwr.sel()
gwr.b3
gwr.fit3<-gwr(Gizi.Buruk ~ PHBS, data=BandungUTM, adapt = gwr.b3, se.fit=T, hatmatrix=T)
gwr.fit3

gwr.fit3$bandwidth
gizi.buruk$bwadapt <- gwr.fit3$bandwidth
tm_shape(gizi.buruk, unit = "mi") +
  tm_polygons(col = "bwadapt", style = "quantile",palette = "Reds", 
              border.alpha = 0, title = "") +
  tm_scale_bar(breaks = c(0, 1, 2), size = 1, position = c("right", "bottom")) +
  tm_compass(type = "4star", position = c("left", "top")) + 
  tm_layout(main.title = "GWR bandwidth",  main.title.size = 0.95, frame = FALSE, legend.outside = TRUE)











