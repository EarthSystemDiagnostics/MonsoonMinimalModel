##' @title Find an analogue field
##' @param target.point scalar
##' @param target.field 2D Field, yearsxdays
##' @param data 
##' @param indexDay days of the year that are searched
##' @param tolerance tolerance of the target
##' @return 
##' @author Thomas Laepple 
AnalogueField<-function(target.point,target,data,indexDay=1:365,tolerance=5)
    {
        field<-array(0,dim(data$data)[2:3])
        iAnalogue<-SearchAnalogues(target.point,target[,indexDay],tolerance=tolerance)
        for (i in 1:nrow(iAnalogue)) field<-field+data$data[iAnalogue[i,1],,,indexDay[iAnalogue[i,2]]]
        return(pField(field/nrow(iAnalogue),1,lat=data$lat,lon=data$lon))
    }
