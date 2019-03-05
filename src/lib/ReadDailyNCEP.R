##' .. Read daily 2D NCEP data as obtained from the subselection web interface
##'
##' .. content for \details{} ..
##' @title Read daily 2D NCEP data as obtained from the subselection web interface
##' @param FILENAME 
##' @param varname 
##' @param firstYear Limit the results to firstYear:lastYear
##' @param lastYear 
##' @return field = as pField object, data(year,lon,lat,day) ,year,lat,lon as matrix/vectors
##' @author Thomas Laepple 
ReadDailyNCEP<-function(FILENAME="",varname="",firstYear=1951,lastYear=2007)
{
    temp.nc = nc_open(FILENAME)
    temp.time <- ncvar_get(temp.nc,"time")
    temp.year<-temp.time%/%10000
    temp.day<-temp.time-10000*temp.year

    year<-seq(from=min(temp.year),to=max(temp.year),by=1)
    temp.data <-ncvar_get(temp.nc,varname)  
    

    temp.lat <-ncvar_get(temp.nc,"lat")
    temp.lon <-ncvar_get(temp.nc,"lon")

    nc_close(temp.nc)
    index.lat<-sort(temp.lat,index.return=TRUE)$ix
    temp.lat<-temp.lat[index.lat]

    if (min(year)>firstYear) stop("firstYear not in data")
    if (max(year)<lastYear) stop("lastYear not in data")
    year<-firstYear:lastYear
    
    temp.matrix<-array(NA,c(length(year),length(temp.lon),length(temp.lat),365))
    for (iYear in 1:length(year))
        {
            index<-which(temp.year==year[iYear])
            nDay<-length(index)
            if (nDay<365 | nDay>366) stop("data not consistent")
            if (nDay==365)  temp.matrix[iYear,,,]<-(temp.data[,index.lat,index]) 
            if (nDay==366)  temp.matrix[iYear,,,]<-(temp.data[,index.lat,index[-60]]) #Remove day 60 on leap years
        }

    field<-pField(aperm(temp.matrix,c(2,3,1,4)),time=1:(length(year)*365),lat=temp.lat,lon=temp.lon)
    return(list(field=field,data=temp.matrix,year=year,lat=temp.lat,lon=temp.lon))
}
