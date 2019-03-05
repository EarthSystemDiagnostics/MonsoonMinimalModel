##' @title index of days around the day 
##' @param day 
##' @param rangeOneSided 
##' @return index of days   (day-rangeOneSided)..(day+rangeOneSided) wrapped at 365 days
##' @author Thomas Laepple 
IndexAround<-function(day,rangeOneSided)
{
    rep(1:365,3)[(day+365-rangeOneSided):(day+365+rangeOneSided)]
}
