#' Measurements
#'
#' The function measurement uses settings and additional params to
#' simulates measurements from true values of analytes. First,
#' settings and params need to be supplied. The functions in
#' the praeHook list are executed to modify params. Settings
#' and params are in scope of these functions directly. The
#' sim.f function simulates measurements from true values.
#' The functions in the postHook list are executed to
#' modify these measuerments. Again, settings and
#' params are in scope of these functions directly.
#'
#' @param settings data.frame with 'trueValue colume' or analyte
#' object
#' @param f.imprec function to simulate imprecision
#' @param f.trueness function to simulate trueness
#' @template mainSteps
#' @return measurement object
#' @export
measurement <- function(settings = data.frame(),
                   params = data.frame("measuredName" =
                                       "trueValue",
                                       "resultName" =
                                         "measurement"),
                   praeHook = list(),
                   sim.f = NULL,
                   postHook = list(),
                   f.imprec = function(...) 0,
                   f.trueness = function(...) 0){


  if (is.null(sim.f)){
    sim.f <- function(x, f.imprec, f.trueness){
      mname <- as.character(x$measuredName[1])
      x[[mname]] + f.imprec(x) + f.trueness(x)
    }
  }

  measurement <- rSimLab(settings, params, praeHook,
                        sim.f, postHook)
  measurement[['f.imprec']] <- f.imprec
  measurement[['f.trueness']] <- f.trueness

  class(measurement) <- append(class(measurement),
                               "measurement")
  measurement
}

#' Simulates imprecision using the characteristic function
#'
#' Imprecision at the concentration c is specified
#' using the characteristic function sd = (a^2+(cb)^2)^.5
#' Parameter a describes the near constant sd at low
#' concentrations, parameter b describes a relative sd at higher
#' concentrations. A relative sd is often expressed as 'coefficient
#' of variation' (CV), sd/c.
#'
#' @param a parameter a of characteristic function.
#' @param b parameter b of characteristic function.
#' @template measurement-block
#'
#' @export
#' @importFrom stats rnorm
#' @references Thompson, Michael. "The characteristic function,
#' a method-specific alternative to the Horwitz Function."
#' Journal of AOAC International 95.6 (2012): 1803-1806.
mm_precCharFunc <- function(measurement, a, b){

  measurement[["params"]]$a <- a
  measurement[["params"]]$b <- b

  measurement[['f.imprec']] <- function(x){
    mname <- as.character(x$measuredName[1])
    rnorm(length(x[[mname]]), sd=(x$a^2+x$b^2*x[[mname]]^2)^.5)
  }
  measurement
}


#' @export
#' @rdname mm_truenessFunc
mm_accFunc <- function(measurement, constD = 0, relD = 0){
  mm_truenessFunc(measurement, constD, relD)
}

#' Simulates constant and relative trueness.
#'
#' Systematic errors lead to a (constant or relativ) bias.
#' The performance characteristics measuring bias is called "trueness".
#' "Accuracy" is an outdated term for the same concept. See the
#' reference for more details.
#'
#' @param constD constant deviation
#' @param relD relative deviation
#' @template measurement-block
#'
#' @export
#' @references Menditto, Antonio, Marina Patriarca, and Bertil Magnusson.
#' "Understanding the meaning of accuracy, trueness and precision."
#'  Accreditation and quality assurance 12.1 (2007): 45-47.
mm_truenessFunc <- function(measurement, constD = 0, relD = 0){
  measurement[["params"]]$constD <- constD
  measurement[["params"]]$relD <- relD

  measurement[['f.trueness']] <- function(x){
    mname <- as.character(x$measuredName[1])
    x$constD + x[[mname]] * x$relD
  }
  measurement
}


#' @export
#' @rdname runSim
runSim.measurement <- function(rSimLab){
  setting <- rSimLab[["setting"]]
  if (!is.data.frame(setting)){
    setting <- runSim(setting)
  }
  params <- rSimLab[['params']]
  if (nrow(setting) != nrow(params)){
    params <- params[rep(seq_len(nrow(params)),
                         each=round(nrow(setting)/
                                      nrow(params))),]
  }

  mname <- as.character(params[1, "measuredName"])
  rname <- as.character(params[1, "resultName"])

 # params[[mname]] <- setting[[mname]]

  for(f in rSimLab[["praeHook"]]){
    params <- f(setting, params)
  }

  results <- cbind(setting[, !colnames(setting) %in%
                             colnames(params), drop=F], params)


  results[[rname]]  <-
    rSimLab[['sim.f']](results, rSimLab[['f.imprec']] ,
                     rSimLab[['f.trueness']])

  for(f in rSimLab[["postHook"]]){
    results <- f(results)
  }

  results$resultName <- NULL
  results$measuredName <- NULL

  results

}
