##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##' @title timelag with rolling over at 365 days
##' @param data vector of values at 365 days
##' @param ilag lag in days
##' @return lagged values
##' @author Thomas Laepple 
TLag<-function(data,ilag)
{
	temp<-rep(data,3)
	return(temp[366:730-ilag])
    }
