#' SimpleOverTime
#'
#' Creates a data.frame with time points for measurements.
#'
#' @param numberOfDays number of 'days'
#' @param meanMeasPerDay mean number of measurements per 'day'
#' @param sdMeasPerDay sd of measurements per 'day'
#'
#' @return data.frame with a 'time' columne
#' @export
#'
#' @examples
#' simpleOverTime(10, 4, 2)
simpleOverTime <- function(numberOfDays, meanMeasPerDay,
                           sdMeasPerDay){
  plyr::ldply(1:numberOfDays, function(x){
    data.frame(time = rep(x,
      times=max(1, round(rnorm(1, mean = meanMeasPerDay,
                               sd=sdMeasPerDay)))),
               type = "pat")
  })
}
