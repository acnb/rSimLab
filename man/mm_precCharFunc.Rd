% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simMeasurement.R
\name{mm_precCharFunc}
\alias{mm_precCharFunc}
\title{Simulates imprecision using the characteristic function}
\usage{
mm_precCharFunc(measurement, a, b)
}
\arguments{
\item{measurement}{(unevaluated) object of class measurement}

\item{a}{parameter a of characteristic function.}

\item{b}{parameter b of characteristic function.}
}
\value{
(unevaluated) object of class measurement
}
\description{
Imprecision at the concentration c is specified
using the characteristic function sd = (a^2+(cb)^2)^.5
Parameter a describes the near constant sd at low
concentrations, parameter b describes a relative sd at higher
concentrations. A relative sd is often expressed as 'coefficient
of variation' (CV), sd/c.
}
\references{
Thompson, Michael. "The characteristic function,
a method-specific alternative to the Horwitz Function."
Journal of AOAC International 95.6 (2012): 1803-1806.
}
\seealso{
Other measurement blocks: 
\code{\link{mm_accFunc}()},
\code{\link{mm_lotBiasFunc}()},
\code{\link{mm_lotBiasVariationFunc}()}
}
\concept{measurement blocks}
