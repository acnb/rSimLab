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
#'    mm_truenessFunc(0, .03)
#' runSim(mm)
#'
#' # Simulation of one year of Vitamin D3 with seasonal
#' # fluctuations.
#' ana <- analyte(setting) %>%
#'    ana_distrLnorm(19.5, 11.4) %>%
#'    addPraeHook(TRUE, center = center *(sin(time/365*3.14*2)*.075+1))
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
#' Functions are executed row by row. At first, variables are evaluated
#' in the environment of the calling function, then in the environment of
#' the parameters. PraeHook Functions can only modify parameters, not
#' settings.
#'
#' @param rSimLab object of class rSimLab
#' @param cond condition, that must hold TRUE for the modification
#' to be executed
#' @param ... Name-value pairs of expressions. Use NULL to drop a variable.
#'
#' @return object of class rSimLab
#' @export
#'
#' @importFrom magrittr %>%
#' @examples
#' # makes the code a lot more readable
#' library(magrittr)
#' diff <- 5
#' analyte(data.frame('time' = 1:10, 'type' = 'pat')) %>%
#'   ana_distrNorm(center = 5, spread = 3) %>%
#'   ana_addQC(5, 'highQC') %>%
#'   addPraeHook(type != 'highQC', center = center + 5) %>%
#'   addPraeHook(type != 'highQC', spread = runif(1,2,3)) %>%
#'   addPraeHook(type != 'highQC', spread = spread + diff)
#'
addPraeHook <- function(rSimLab, cond = TRUE, ...){
  cond_str <- lazyeval::f_capture(cond)
  env <- parent.frame()
  while(!identical(env, emptyenv())){
    cond_str <- lazyeval::interp(cond_str, .values=env)
    env <- parent.env(env)
  }
  dots <- lazyeval::lazy_dots(...)
  modFunc <- function(settings, params){
    comBObj <- cbind(settings, params)
    condMet <- lazyeval::f_eval(cond_str, comBObj)

    if(nrow(comBObj[condMet, ]) > 0){
      comBObj[condMet, ] <- comBObj[condMet, ] %>%
        dplyr::rowwise() %>%
        dplyr::mutate(...)
    }
    comBObj[, !colnames(comBObj) %in% colnames(settings)]
  }
  rSimLab[['praeHook']] <- append(rSimLab[['praeHook']], modFunc)
  rSimLab
}


#' Hook for functions that are executed after the simulation.
#'
#' Functions are executed row by row. At first, variables are evaluated
#' in the environment of the calling function, then in the environment of
#' the parameters and settings.
#'
#' @param rSimLab object of class rSimLab
#' @param cond condition, that must hold TRUE for the modification
#' to be executed
#' @param ... Name-value pairs of expressions. Use NULL to drop a variable.
#'
#' @return object of class rSimLab
#' @export
addPostHook <- function(rSimLab, cond = TRUE, ...){
  cond_str <- lazyeval::f_capture(cond)
  env <- parent.frame()
  while(!identical(env, emptyenv())){
    cond_str <- lazyeval::interp(cond_str, .values=env)
    env <- parent.env(env)
  }
  dots <- lazyeval::lazy_dots(...)

  modFunc <- function(results){
    condMet <- lazyeval::f_eval(cond_str, results)
    if(nrow(results[condMet, ]) > 0){
      results[condMet, ] <- results[condMet, ] %>%
        dplyr::rowwise() %>%
        dplyr::mutate_(.dots=dots)
    }
    results
  }
  rSimLab[['postHook']] <- append(rSimLab[['postHook']], modFunc)
  rSimLab
}


#' Setter for the settings for analytes or measurements
#'
#' A setter function to supply settings (e.g. data.frame with true values) to
#' measurements or analytes objects.
#'
#' @param rSimLab object of class rSimLab
#' @param settings data.frame with settings
#' @return rSimLab object of class rSimLab
#' @examples
#' # makes the code a lot more readable
#' library(magrittr)
#'  ana <- analyte() %>%
#'    ana_distrLnorm(center = 5, spread = 5) %>%
#'    setSettings(data.frame('time' = 1:10, 'type' = 'pat'))
#' @export
setSettings <- function(rSimLab, settings){
  UseMethod("setSettings",rSimLab)
}

#' @export
#' @rdname setSettings
setSettings.default <- function(rSimLab, settings){
  rSimLab[["setting"]] <- settings
  rSimLab
}
