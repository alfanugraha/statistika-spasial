#MATRIKS PENIMBANG SPASIAL DENGAN DATA JAWA BARAT

library(spdep)

#membuka file shp Jabar
jabar<-readOGR(dsn="petajabar27", layer="Peta Jabar 27")

#Queen Contiguity
queenjabar.w <- poly2nb(jabar,queen = T)
summary(queenjabar.w)

#menampilkan dalam bentuk matriks
queenjabar.w1<-nb2mat(queenjabar.w,style = "B") #untuk melihat bentuk matriks bobot
View(queenjabar.w1)

#untuk masuk sintaks sebagai penimbang
queenjabar.w2<-nb2listw(queenjabar.w,style = "B") 

#queen weights - plot
plot(jabar, border="white",col='gray')
coords<-coordinates(jabar)
plot(queenjabar.w, coords, add = TRUE, col = "red")

#Rook Contiquity
rookjabar.w <- poly2nb(jabar, queen=FALSE)
summary(rookjabar.w)

#menampilkan dalam bentuk matriks
rookjabar.w1<-nb2mat(rookjabar.w,style = "B") #untuk melihat bentuk matriks bobot
View(rookjabar.w1)

#untuk masuk sintaks sebagai penimbang
rookjabar.w2<-nb2listw(rookjabar.w,style = "B") 

#rook weights - plot
plot(jabar, border="white",col='gray')
coords<-coordinates(jabar)
plot(rookjabar.w, coords, add = TRUE, col = "blue")

#Perbandingan
plot(jabar, border="white",col='gray')
plot(queenjabar.w, coords, add = TRUE, col = "red")
plot(rookjabar.w, coords, add = TRUE, col = "blue")

#Spasial Weighted
coords<-coordinates(jabar)
#K Nearest Neighbour 
#k=2
w.knn2<-knearneigh(coords, k=2, longlat = TRUE)

knn2.w<-nb2listw(knn2nb(w.knn2))
summary(knn2.w)

#K-nn weight matrix - plot
plot(jabar, border="white",col='gray')
plot(knn2.w, coords, add = TRUE, col = "red")

#k=4
w.knn4<-knearneigh(coords, k=4, longlat = TRUE)

knn4.w<-nb2listw(knn2nb(w.knn4))
summary(knn4.w)

#K-nn weight matrix - plot
plot(jabar, border="white",col='gray')
plot(knn4.w, coords, add = TRUE, col = "red")

#k=7
w.knn7<-knearneigh(coords, k=7, longlat = TRUE)

knn7.w<-nb2listw(knn2nb(w.knn7))
summary(knn7.w)

#K-nn weight matrix - plot
plot(jabar, border="white",col='gray')
plot(knn7.w, coords, add = TRUE, col = "blue")

#D Near Neighbour
#dnearneight dengan jarak tertentu batasannya
w.dnn1 <- dnearneigh(coords, 0, 1)
summary(w.dnn1)

#D-nn weight matrix - plot
plot(jabar, border="white",col='gray')
plot(w.dnn1, coords, add = TRUE, col = "blue")

#Metode dengan Nilai Jarak (Eucledian)
D<-as.matrix(dist(coordinates(jabar),method = "euclidean"))
head(D)

#inverse weight matrix
w=1/D
head(w)

# inverse weight matrix - row-normalized
diag(w)<-0
rtot<-rowSums(w, na.rm =T)
w_std<-w/rtot
head(w_std)
rowSums(w_std, na.rm=T)

#matriks penimbang invers jarak
invers.w<-mat2listw(w, style="W") #untuk melihat matriks W
summary(invers.w)

invers.w2<-mat2listw(w_std) #untuk melihat matriks W
summary(invers.w2)

#inverse weight matrix - plot
plot(jabar, border="white",col='gray')
coords<-coordinates(jabar)
plot(invers.w, coords, add = TRUE, col = "red")

#Eksponential weight matrix
alpha<-2
w.expo<-exp(-alpha*D)

#Eksponential weight matrix - row-normalized
diag(w.expo)<-0
rtotexpo<-rowSums(w.expo, na.rm =T)
wexpo_std<-w.expo/rtotexpo

#matriks penimbang eksponensial
eksp.w=mat2listw(w.expo, style="W") #untuk melihat matriks W
summary(eksp.w)

eksp.w2=mat2listw(wexpo_std) #untuk melihat matriks W
summary(eksp.w2)

#inverse weight matrix - plot
plot(jabar, border="white",col='gray')
coords<-coordinates(jabar)
plot(eksp.w, coords, add = TRUE, col = "red")


#Autokorelasi Indeks Moran 
moran(jabar$gizi, invers.w, n=length(invers.w$neighbours), S0=Szero(invers.w))
moran(jabar$gizi, queenjabar.w2, n=length(invers.w$neighbours), S0=Szero(invers.w))
moran(jabar$gizi, knn7.w, n=length(invers.w$neighbours), S0=Szero(invers.w))

moran.test(jabar$gizi, invers.w)
moran.test(jabar$gizi, knn7.w)
moran.test(jabar$gizi, invers.w,alternative="two.sided")

#Local Moran/LISA
local <- localmoran(jabar$gizi, listw = invers.w)
head(local)
moran.plot(jabar$gizi,invers.w, labels=jabar$KABKOT)
