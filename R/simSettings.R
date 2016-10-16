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
  df <- data.frame(time= rep(1:numberOfDays,
          times=pmax(1, round(rnorm(numberOfDays, mean = meanMeasPerDay,
                                                   sd=sdMeasPerDay)))))
  df$type = 'pat'
  df
}
