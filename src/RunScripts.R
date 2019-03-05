## Aim: Minimal empirical model for Holocene Asian precipitation  
## Status/belongs to: Position and orientation of the westerly jet determined Holocene rainfall patterns in China, Nature Communications, 2019
## tlaepple@awi.de 


#Load paleolibrary
basedrive="/Users/XXX/data/"  ##Set to local path
path<-paste(basedrive,"paleoLibrary/src/",sep="")
source(paste(path,"header.R",sep=""))


setwd(paste(basedrive,"MonsoonMinimalModel/",sep=""))
sourceDir("./src/lib")

library(ncdf4)
library(e1071)


##Model Parameters:

## "The daily position of the westerly jet stream was estimated by taking the zonal mean of the u-wind field between 100°E and 120°E "
kLonBound1=100 #this results in averaging the boxes with mid-longitude of  115.0 117.5
kLonBound2=120
                                        
kInsolLatitude=35 #Latitude for the insolation used in the prediction
kLabelTarget="upos 250hpa" #Label of the target variable


kTolerance=5 #For that purpose, all days (= analog days) in the modern dataset (1951–2007, ~20,000 days) with a westerly jet stream position within ±5 degrees of latitude of the modeled jet stream position were chosen

kRangeOneSided=60 #We restricted our selection to days within the same season as the targeted historical day (i.e. to ± 60 days) in order to avoid a mixture of spring and autumn precipitation patterns. 

kBPolynomial=TRUE #Use polynomial for prediction (TRUE) or linear (FALSE)
kPolynom.order=2 #Order of polynomial


cYear<-seq(from=2,to=10,by=0.25) #timepoints in kyr BP that will be simulated




##Read NCEP Wind data
FILENAME.UWND="/Users/tlaepple/data/MonsoonAnalogue/data/NCEP/uwnd.250hpa.nc"
FILENAME.VWND="/Users/tlaepple/data/MonsoonAnalogue/data/NCEP/vwnd.250hpa.nc"

uwnd<-ReadDailyNCEP(FILENAME.UWND,varname="uwnd")
vwnd<-ReadDailyNCEP(FILENAME.VWND,varname="vwnd")

##Read Aphrodite data
precip<-ReadDailyAphro(FILENAME="/Users/tlaepple/data/Ulrike/data/APHRO/merged.cutn.2x2.large55.con.nc")
pp.clim<-pField(aperm(apply(precip$data,c(2,3,4),mean),c(1,2,3)),time=1:365,lat=precip$lat,lon=precip$lon) #Create daily climatology
         
#Fit the transfer function
# Fit the relationship of modern insolation and wind


#Helper function
LatOfMaxPosition<-function(data,lat) return(lat[which.max(data)])

index.lat<-seq(uwnd$lat)
index.lon<-which((uwnd$lon>kLonBound1) & (uwnd$lon<kLonBound2))
uwnd$zmean<-apply(uwnd$data[,index.lon,index.lat,],c(1,3,4),mean)

target<-apply(uwnd$zmean,c(1,3),LatOfMaxPosition,lat=uwnd$lat)

## Calculate the climatology target
t<-apply(uwnd$data,c(2,3,4),mean)
clim.zmean<-apply(t[index.lon,index.lat,],c(2,3),mean)
climatology.target<-apply(clim.zmean,c(2),LatOfMaxPosition,lat=uwnd$lat)

    
quartz()
quartz(width=8,height=3)
par(mfcol=c(1,3))

transfer<-AnalyseTransferfunction(climatology.target,latitude=kInsolLatitude,negative=FALSE,clab=kLabelTarget,bPlot=TRUE,polynom.order=kPolynom.order,bLinPlot=FALSE,col1="dodgerblue")
uwnd.clim<-pField(aperm(apply(uwnd$data,c(2,3,4),mean),c(1,2,3)),time=1:365,lat=uwnd$lat,lon=uwnd$lon) #daily climatology

anomalyRef<-climatology.target-SimYear(0,transfer=transfer,latitude=kInsolLatitude,bPolynomial=kBPolynomial)


dev.copy2pdf(file="./plots/Transferfunction.pdf")
    

## Run the model; takes some time

sim.precip<-pField(NA,cYear,lat=precip$lat,lon=precip$lon)
sim.wind<-pField(NA,cYear,lat=precip$lat,lon=precip$lon)

for (ikYear in 1:length(cYear))
    {
    
        print(paste("Working in kyr",cYear[ikYear]))
        fields<-SimulatePastField(kyear=cYear[ikYear],transfer=transfer,target=target,data=precip,rangeOneSided=kRangeOneSided,tolerance=kTolerance,latitude=kInsolLatitude,bPolynomial=kBPolynomial,anomalyRef=anomalyRef)
        sim.precip[ikYear,]<-applytime(fields,mean)


 }




#Prepare wind plots by simulating the wind fields at 2 and 9kyr BP
wind.2.u<-SimulatePastField(kyear=2,transfer=transfer,target=target,data=uwnd,rangeOneSided=kRangeOneSided,tolerance=kTolerance,latitude=kInsolLatitude,bPolynomial=kBPolynomial)
wind.9.u<-SimulatePastField(kyear=9,transfer=transfer,target=target,data=uwnd,rangeOneSided=kRangeOneSided,tolerance=kTolerance,latitude=kInsolLatitude,bPolynomial=kBPolynomial)

wind.2.v<-SimulatePastField(kyear=2,transfer=transfer,target=target,data=vwnd,rangeOneSided=kRangeOneSided,tolerance=kTolerance,latitude=kInsolLatitude,bPolynomial=kBPolynomial)
wind.9.v<-SimulatePastField(kyear=9,transfer=transfer,target=target,data=vwnd,rangeOneSided=kRangeOneSided,tolerance=kTolerance,latitude=kInsolLatitude,bPolynomial=kBPolynomial)

maxDay.2<-which.max(SimYear(2,transfer=transfer,latitude=kInsolLatitude,bPolynomial=kBPolynomial,anomalyRef=anomalyRef))
maxDay.9<-which.max(SimYear(9,transfer=transfer,latitude=kInsolLatitude,bPolynomial=kBPolynomial,anomalyRef=anomalyRef))

wind.2.u.maxDay<-wind.2.u[maxDay.2,]
wind.9.u.maxDay<-wind.9.u[maxDay.9,]

wind.2.v.maxDay<-wind.2.v[maxDay.2,]
wind.9.v.maxDay<-wind.9.v[maxDay.9,]

quartz(height=3,width=3)
tmp.wind<-plot.preparation(wind.2.u.maxDay)
contour(tmp.wind$lon,tmp.wind$lat,tmp.wind$data,levels=quantile(tmp.wind$data,c(0.7,0.9,0.99)),main="")
tmp.wind<-plot.preparation(wind.9.u.maxDay)
contour(tmp.wind$lon,tmp.wind$lat,tmp.wind$data,levels=quantile(tmp.wind$data,c(0.7,0.9,0.99)),col="red",add=TRUE)

addland()
dev.copy2pdf(file="./plots/Winds.pdf")



quartz(height=6,width=6)
tmp.wind<-plot.preparation(wind.2.u.maxDay)
contour(tmp.wind$lon,tmp.wind$lat,tmp.wind$data,levels=quantile(tmp.wind$data,c(0.7,0.9,0.99)),main="")
tmp.wind<-plot.preparation(wind.9.u.maxDay)
contour(tmp.wind$lon,tmp.wind$lat,tmp.wind$data,levels=quantile(tmp.wind$data,c(0.7,0.9,0.99)),col="red",add=TRUE)
addland()
addwind(wind.2.u.maxDay,wind.2.v.maxDay,scale=2.5,l=0.8)
addwind(wind.9.u.maxDay,wind.9.v.maxDay,scale=2.5,l=0.8,col="red")
dev.copy2pdf(file="./plots/WindWithVectors.pdf")



clusterPlotCMeans(sim.precip,c(time(sim.precip)))
dev.copy2pdf(file="./plots/cluster.pdf")
graphics.off()

save(sim.precip,file="./output/minimalModelOutput.dat") #Save output for the model-data comparison (script in the supplementary information)


