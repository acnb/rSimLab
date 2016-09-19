#' Analyte
#'
#' Creates analytes from settings.
#'
#' The function analyte uses settings and additional params to
#' create true values for an analyte. First, settings and params
#' need to be supplied. The functions in the praeHook list
#' are executed to modify params. Settings and params are in
#' scope of these functions directly. The sim.f function creates
#' true values for the analyte. The functions in the postHook list
#' are executed to modify these true values. Again, settings and
#' params are in scope of these functions directly.
#'
#' @param settings a data.frame with settings for analytes
#' @template mainSteps
#'
#' @return object of class analyte
#' @export
analyte <- function(settings = data.frame(),
                    params = data.frame("resultName" =
                                         "trueValue"),
                    praeHook = list(),
                    sim.f = NULL,
                    postHook = list()){


  ana <- rSimLab(settings, params, praeHook, sim.f, postHook)
  class(ana) <- append(class(ana), "analyte")
  ana
}


#' Distribution of true values
#'
#' Creates log normal or normal distribution of values. Spread
#' and center are the real center and spread not
#' (log-) tranformed in any way for any distribution.
#'
#' @template analyte-block
#' @param center center of distribution
#' @param spread spread of distribution
#'
#' @export
#' @importFrom stats rlnorm
#' @references Haeckel, Rainer, and Werner Wosniok.
#'    "Observed, unknown distributions of clinical chemical
#'    quantities should be considered to be log-normal: a
#'    proposal." Clinical chemistry and laboratory
#'    medicine 48.10 (2010): 1393-1396.
ana_distrLnorm <- function(ana, center , spread){
  ana[["params"]]$spread <- spread
  ana[["params"]]$center <- center
  func <- function(params){
    s <- params[['spread']]^2
    m <- params[['center']]
    sdlog <- (log(1+s/m^2))^.5
    meanlog <- log(m/(1+s/m^2)^.5)
    vfunc <- Vectorize(function(sdlog, meanlog){
      rlnorm(1, sdlog=sdlog, meanlog=meanlog)
      })
    vfunc(sdlog, meanlog)
  }
  ana[['sim.f']] <-  func

  ana
}

#' @rdname ana_distrLnorm
#' @importFrom stats rnorm
#' @export
ana_distrNorm <- function(ana, center , spread){
  ana[["params"]]$spread <- spread
  ana[["params"]]$center <- center
  func <- function(params){
    sd <- params[['spread']]
    mean <- params[['center']]
    vfunc <- Vectorize(function(sd, mean){
      rnorm(1, sd=sd, mean=mean)
    })
    vfunc(sd, mean)
  }
  ana[['sim.f']] <-  func

  ana
}


# Prae-execution Modifiers with conditions
#
#
# #@template analyte-block
# #@param mod string to execute. All settings and params of the
# analyte are in scope and are modifiable.
# #@param cond condition when to execute modification function.
xx_ana_condPraeModifyers <- function(ana, mod, cond = TRUE){
  cond_str <- substitute(cond)
  modFunc <- function(setting, params){
    condMet <- eval(cond_str, setting)
    if (nrow(setting) != nrow(params)){
      params <- params[rep(seq_len(nrow(params)),
                           each=round(nrow(setting)/
                                        nrow(params))),]
    }
    params[condMet,] <- mod(ana[condMet,], params[condMet,])
    params
  }
  ana[['praeHook']] <- append(ana[['praeHook']],
                                     modFunc)
  ana
}

xx_ana_timeCyle <- function(ana, cycles, maxChange,
                         paramToChange = "center"){
  rangeT <- max(ana[['setting']][["time"]]) -
    min(ana[['setting']][["time"]])
  p <- (2*3.14*cycles)/rangeT
  func <- function(setting, param){
    param[[paramToChange]] <- (1 + sin(setting$time*p*maxChange))*
      param[[paramToChange]]
    param
  }
  ana[['praeHook']] <- append(ana[['praeModifyers']],
                                   func)
  ana
}

#' Add Quality Control (QC) measurement
#'
#' This function adds QC measurements with known true values.
#'
#' One QC measurement is added to each 'time' periode. Therefore
#' the time variable needs to be present in settings. The type
#' variable is used to differentiate between patient and QC
#' measurements and needs to be present in settings as well.
#' QC measurements are added after everthing else is done
#' (postHook).
#'
#' @template analyte-block
#' @param value true value of the QC measurement
#' @param type identifier of the QC measurement
#'
#' @export
ana_addQC <- function(ana, value, type="qc"){
  func <- function(a){
    qc <- data.frame("time" = unique(a$time),
                     "trueValue" = value, "type" = type)
    missingCols <- colnames(a)[!colnames(a) %in% colnames(qc)]
    qc[missingCols] <- NA
    rbind(a, qc)
  }
  ana[['postHook']] <- append(ana[['postHook']],
                                   func)
  ana
}

