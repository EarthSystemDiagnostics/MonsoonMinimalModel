##' .. Read Aphrodite precip data
##'
##' .. content for \details{} ..
##' @title Read Aphrodite precip data
##' @param FILENAME 
##' @return  field = as pField object, data(year,lon,lat,day) ,year,lat,lon as matrix/vectors
##' @author Thomas Laepple 
ReadDailyAphro<- function(FILENAME="./data/APHRO/merged.cutn.2x2.nc")
{
  temp.nc = nc_open(FILENAME)
  #Read out the data
  temp.time <- ncvar_get(temp.nc,"time")
  temp.year<-temp.time%/%10000

  year<-seq(from=min(temp.year),to=max(temp.year),by=1)

  temp.data <-ncvar_get(temp.nc,"precip")  #Take zonal means
  temp.lat <-ncvar_get(temp.nc,"lat")
  temp.lon <-ncvar_get(temp.nc,"lon")
  nc_close(temp.nc)

  temp.matrix<-array(NA,c(length(year),length(temp.lon),length(temp.lat),365))
  for (iYear in 1:length(year))
      {
          index<-which(temp.year==year[iYear])
          nDay<-length(index)
          if (nDay<365 | nDay>366) stop("data not consistent")
          if (nDay==365)  temp.matrix[iYear,,,]<-temp.data[,,index]
          if (nDay==366)  temp.matrix[iYear,,,]<-temp.data[,,index[-60]] #Remove day 60 on leap years
      }
  field<-pField(aperm(temp.matrix,c(2,3,4,1)),time=1:(length(year)*365),lat=temp.lat,lon=temp.lon) 

  return(list(field=field,data=temp.matrix,year=year,lat=temp.lat,lon=temp.lon))
}
  
