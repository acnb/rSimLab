#' Measurements
#'
#' The function measurement uses settings and additional params to
#' simulates measurements from true values of analytes. First,
#' settings and params need to be supplied. The functions in
#' the praeHook list are executed to modify params. Settings
#' and params are in scope of these functions directly. The
#' sim.fxs functions simulate measurements from true values.
#' The functions in the postHook list are executed to
#' modify these measuerments. Again, settings and
#' params are in scope of these functions directly.
#'
#' @param settings data.frame with 'trueValue colume' or analyte
#' object
#' @template mainSteps
#' @return measurement object
#' @export
measurement <- function(settings = data.frame(),
                        params = data.frame("measuredName" =
                                              "trueValue",
                                            "resultName" =
                                              "measurement"),
                        praeHook = list(),
                        postHook = list()) {

  measurement <- rSimLab(settings, params, praeHook, postHook)

  class(measurement) <- append(class(measurement), "measurement")
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
mm_precCharFunc <- function(measurement, a, b) {
  measurement[["params"]]$a <- a
  measurement[["params"]]$b <- b

  fn <- function(x) {
    mname <- as.character(x$measuredName[1])
    rnorm(length(x[[mname]]), sd = (x$a ^ 2 + x$b ^ 2 * x[[mname]] ^ 2) ^
            .5)
  }
  measurement[['sim.fxs']] <- append(measurement[['sim.fxs']], fn)

  measurement
}


#' @export
#' @rdname mm_truenessFunc
mm_accFunc <- function(measurement,
                       constD = 0,
                       relD = 0) {
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
mm_truenessFunc <- function(measurement,
                            constD = 0,
                            relD = 0) {
  measurement[["params"]]$constD <- constD
  measurement[["params"]]$relD <- relD


  fn <- function(x) {
    mname <- as.character(x$measuredName[1])
    x$constD + x[[mname]] * x$relD
  }
  measurement[['sim.fxs']] <- append(measurement[['sim.fxs']], fn)

  measurement
}

#' Adds a bias from a reagent lot
#'
#' @param relDLot relative deviation attributable to a reagent lot
#'
#' @export
#' @template measurement-block
mm_lotBiasFunc <- function(measurement, relDLot = 0) {
  measurement[["params"]]$relDLot <- relDLot

  fn <- function(x) {
    mname <- as.character(x$measuredName[1])
    x[[mname]] * x$relDLot
  }

  measurement[['sim.fxs']] <- append(measurement[['sim.fxs']], fn)

  measurement
}


#' Adds reagent lots with different bias. Distribution of lot biases can be
#' specified as normal distribution
#'
#'
#' @param sdRelDLot standard deviation of distribution of lot biases
#' @param systemicRelDLot mean of distribution of lot biases; systemic bias
#' @param nTimesPerLot times (e.g. days) each lot is in use
#'
#' @export
#' @template measurement-block
#' @importFrom rlang .data
mm_lotBiasVariationFunc <- function(measurement,
                                    sdRelDLot,
                                    systemicRelDLot = 0,
                                    nTimesPerLot = NULL) {

  measurement[["params"]]$sdRelDLot <- sdRelDLot
  #measurement[["params"]]$systemicRelDLot <- systemicRelDLot

  if (!is.null(nTimesPerLot)){
    fnPrae <- function(x){
      if(is.null(x$time)) return(x)
      res <- x %>% dplyr::mutate(lot = ceiling(.data$time/nTimesPerLot))

      lots <- tibble::tibble(lot = unique(res$lot)) %>%
        dplyr::mutate(systemicRelDLot = systemicRelDLot,
                      relDLot = rnorm(n = dplyr::n(), sd = sdRelDLot,
                                      mean = systemicRelDLot))
      res %>%
        dplyr::left_join(lots, by="lot") %>%
        return()
    }

    measurement[['praeHook']] <- append(measurement[['praeHook']],
                                        fnPrae)
  } else {
    measurement[["params"]]$relDLot <- rnorm(n = 1, sd = sdRelDLot,
                                             mean = systemicRelDLot)
  }

  fn <- function(x) {
    mname <- as.character(x$measuredName[1])
    x[[mname]] * x$relDLot
  }

  measurement[['sim.fxs']] <- append(measurement[['sim.fxs']], fn)

  measurement
}


#' @export
#' @rdname runSim
runSim.measurement <- function(rSimLab) {

  setting <- rSimLab[["setting"]]
  if (!is.data.frame(setting)) {
    setting <- runSim(setting)
  }
  params <- rSimLab[['params']]
  if (nrow(setting) != nrow(params)) {
    params <- params[rep(seq_len(nrow(params)),
                         each = round(nrow(setting) /
                                        nrow(params))), ]
  }

  mname <- as.character(params[1, "measuredName"])
  rname <- as.character(params[1, "resultName"])

  results <- cbind(setting[,!colnames(setting) %in%
                             colnames(params), drop = F], params)

  for (f in rSimLab[["praeHook"]]) {
    results <- f(results)
  }

  rx <- purrr::map(rSimLab[['sim.fxs']], function(fx){
    fx(results) %>% tibble::as_tibble_col()
  }) %>%
    purrr::list_cbind(name_repair = "unique_quiet")

  results[[rname]]  <- results[[mname]] + rowSums(rx)

  for (f in rSimLab[["postHook"]]) {
    results <- f(results)
  }

  results$resultName <- NULL
  results$measuredName <- NULL

  results

}
