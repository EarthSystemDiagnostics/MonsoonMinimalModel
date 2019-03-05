
##' @title Simulate a year from a parametric relationship to insolation
##' @param kyear point in time (kyr BP)
##' @param transfer transfer function
##' @param latitude latitude of the insolation calculation
##' @param bPolynomial (TRUE) use polynomial, FALSE = linear
##' @param anomalyRef NULL for absolute mode or vector of 365 anomaly correction values
##' @return 
##' @author Thomas Laepple 
SimYear<-function(kyear=0,transfer,latitude,bPolynomial=FALSE,anomalyRef=NULL)
    {
        insol<-(TLag(daily_insolation(kyear,latitude,1:365)$Fsw,transfer$lag))
        if (transfer$negative) insol<--1*insol
        if (bPolynomial)
            {
                
                ins.1<-insol
                ins.2<-insol^2
                ins.3<-insol^3
                if (length(transfer$coeff)==4) result<- transfer$coeff[1]+transfer$coeff[2]*ins.1+transfer$coeff[3]*ins.2+transfer$coeff[4]*ins.3
                if (length(transfer$coeff)==3) result<- transfer$coeff[1]+transfer$coeff[2]*ins.1+transfer$coeff[3]*ins.2
            } else
                {
                    result<- transfer$coeff.lin[1]+transfer$coeff.lin[2]*insol

                }
        if (!is.null(anomalyRef)) result=result+anomalyRef
        return(result)       
    }



