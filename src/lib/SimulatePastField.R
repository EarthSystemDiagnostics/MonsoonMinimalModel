
##' @title Simulate a past daily field
##' @param kyear time kyr BP
##' @param transfer transfer function
##' @param target 
##' @param data 
##' @param rangeOneSided 
##' @param tolerance 
##' @param latitude latitude of the insolation calculation
##' @param bPolynomial (TRUE) use polynomial, FALSE = linear
##' @param anomalyRef NULL for absolute mode or vector of 365 anomaly correction values
##' @return 
##' @author Thomas Laepple 
SimulatePastField<-function(kyear,transfer=transfer,target=uwnd,data=precip,rangeOneSided=NULL,tolerance=5,latitude=35,bPolynomial=TRUE,anomalyRef=NULL)
    {
        paleo.pos<-SimYear(kyear,transfer=transfer,latitude=latitude,bPolynomial=bPolynomial,anomalyRef=anomalyRef)
        dailyField<-pField(NA,1:365,lat=data$lat,lon=data$lon)
        
        index=1:365
        for (i in 1:365) {
            if (!is.null(rangeOneSided)) index=IndexAround(i,rangeOneSided)
            dailyField[i,]<-AnalogueField(target.point=paleo.pos[i],target=target,data=data,indexDay=index,tolerance=tolerance)
        }
        return(dailyField)
    }
