#' rSimLab: A package for the simulation of measurements
#' in laboratory medicine.
#'
#' The rSimLab package simulates measurements in laboratory
#' medicine. The general idea is to first create analytes with
#' true values. Measured values are based on these true values.
#' Complex settings can be created from simple building
#' blocks. Hooks allow for even more flexibility. The '\%>\%'
#' operator from the magrittr package is strongly recomended for
#' chaining everything together.
#'
#'
#' @docType package
#' @name rSimLab
#'
#' @examples
#' # makes the code a lot more readable
#' library(magrittr)
#' # Simulation of one year of albumin measurements
#' setting <- simpleOverTime(365,500, 50)
#' ana <- analyte(setting) %>%
#'    ana_distrNorm(3.5 , .886)
#' mm <- measurement(ana) %>%
#'    mm_precCharFunc(.067, 0.031) %>%
#'    mm_accFunc(0, .03)
#' runSim(mm)
#'
#' # Simulation of one year of Vitamin D3 with seasonal
#' # fluctuations.
#' ana <- analyte(setting) %>%
#'    ana_distrLnorm(19.5, 11.4) %>%
#'    addPraeHook("center", center *(sin(time/365*3.14*2)*.075+1))
#' runSim(ana)
#'
#' # Repeated measurements of the same sample with different
#' # methods (Analyte simulation is run explicitly.)
#' ana <- analyte(setting) %>%
#'    ana_distrLnorm(19.5, 11.4)
#' ana_concrete <- runSim(ana)
#' mm1 <- measurement(ana_concrete) %>%
#'    mm_precCharFunc(.05, 0.02)
#' mm2 <- measurement(ana_concrete) %>%
#'    mm_precCharFunc(.03, 0.1)
#' runSim(mm1)
#' runSim(mm2)
#'
#' # Repeated measurements of the different samples
#' # (of the same analyte) with different
#' # methods (Analyte simulation is not run explicitly.)
#' ana <- analyte(setting) %>%
#'    ana_distrLnorm(19.5, 11.4)
#' mm1 <- measurement(ana) %>%
#'    mm_precCharFunc(.05, 0.02)
#' mm2 <- measurement(ana) %>%
#'    mm_precCharFunc(.03, 0.1)
#' runSim(mm1)
#' runSim(mm2)
NULL

# Simulation of analytes and measurements in a (clinical)
# laboratory. This is the parent class. Do not use it directly.
rSimLab <- function(setting = data.frame(),
                   params = data.frame(),
                   praeHook = list(),
                   sim.f = function(...){},
                   postHook = list()){
  rSimLab <- list(
    setting = setting,
    params = params,
    praeHook = praeHook,
    sim.f = sim.f,
    postHook = postHook
  )
  class(rSimLab) <- append(class(rSimLab), "rSimLab")
  rSimLab
}

#' Executes the simulation
#'
#' @param rSimLab object of class rSimLab
#' @return data.frame
#' @export
runSim <- function(rSimLab){
  UseMethod("runSim",rSimLab)
}

#' @export
#' @rdname runSim
runSim.default <- function(rSimLab){
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

  for(f in rSimLab[["praeHook"]]){
    params <- f(setting, params)
  }

  results <- cbind(setting, params)

  rname <- as.character(params[1, "resultName"])

  results[[rname]]  <-
    rSimLab[['sim.f']](results)

  results$resultName <- NULL

  for(f in rSimLab[["postHook"]]){
    results <- f(results)
  }
  results
}


#' Hook for functions that are executed before the simulation.
#'
#'
#'
#' @param rSimLab object of class rSimLab
#' @param paramToModify name of the parameter that should
#' be modified
#' @param modification formular for modification
#' @param cond condition, that must hold TRUE for the modification
#' to be executed
#' @param addEnv additional environment for the modification
#'
#' @return object of class rSimLab
#' @export
#'
#' @examples
#' #' # makes the code a lot more readable
#' library(magrittr)
#' analyte(data.frame('time' = 1:10, 'type' = 'pat')) %>%
#'   ana_distrNorm(center = 5, spread = 3) %>%
#'   ana_addQC(5, 'highQC') %>%
#'   addPraeHook('center',
#'                center + 5,
#'                type != 'highQC')
addPraeHook <- function(rSimLab, paramToModify,
                            modification, cond = TRUE,
                        addEnv = environment()){
  cond_str <- substitute(cond)
  mod_str <- substitute(modification)

  modFunc <- function(envir, params){
    condMet <- eval(cond_str, cbind(envir, params), addEnv)
    params[condMet, paramToModify] <-
      eval(mod_str, cbind(envir[condMet, ,drop=F],
                          params[condMet, ,drop=F]),
                          addEnv)
    params
  }
  rSimLab[['praeHook']] <- append(rSimLab[['praeHook']], modFunc)
  rSimLab
}
