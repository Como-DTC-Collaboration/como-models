% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/intervention.R
\name{intervention_protocol}
\alias{intervention_protocol}
\title{Creates protocols for the interventions using parameters of the model; used
in the 'run' method of compartmental model with interventions.}
\usage{
intervention_protocol(int_parms, sim_parms, tanh_slope)
}
\arguments{
\item{int_parms}{parameters of the interventions as according to a InterventionParameters
object.}

\item{sim_parms}{parameters of the simulation timeframe as according to a SimulationParameters
object.}

\item{tanh_slope}{sharpness of the intervention waves used for function
continuity purposes.}
}
\description{
Creates protocols for the interventions using parameters of the model; used
in the 'run' method of compartmental model with interventions.
}
