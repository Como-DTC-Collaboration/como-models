library(tidyverse)
check <-function(object) {
  #' Checks that the format of the InterventionParameters is correct:
  #' only whole intervals are allowed; intervals do not overlap; no backwards
  #' intervals.
  start <- object@start
  stop <- object@stop
  coverage <- object@coverage
  
  if(!length(start) == length(stop))
    return("Must be equal numbers of intervention starts and ends.")
  if(!length(start) == length(coverage))
    return("Must be equal numbers of intervention starts and coverage levels.")
  
  for(i in seq_along(start))
    if(start[i] >= stop[i])
      return("Intervention must start before it ends.")
  
  len_start <- length(start)
  if(len_start > 1) {
    last_starts <- start[2:len_start]
    first_stops <- stop[1:(len_start - 1)]
    start_before_end <- (last_starts - first_stops) <= 0
    if(sum(start_before_end) > 0)
      return("Each subsequent intervention must start after the last ends.")
  }
  
  for(i in seq_along(coverage))
    if(coverage[i] < 0 | coverage[i] > 1)
      return("Intervention coverage must not be outside [0, 1].")
}

#' An S4 object representing the InterventionParameters,
#'
#' This class hosts the intervention parameters used to create intevention protocols
#' for a SEIRD-class type of model.
#'
#' @slot start list of values for start points of intervention intervals
#'       (double).
#' @slot stop list of values for stop points of intervention intervals
#'       (double).
#' @slot coverage list of values for effect levels of intervention intervals
#'       (double).
#'
#' @import tidyverse
#'
#' @export Intervention
InterventionParameters <-
  setClass("InterventionParameters",
           slots = c(start="numeric",
                     stop="numeric",
                     coverage="numeric"),
           validity = check)

#' An S4 object representing the SimulationParameters.
#'
#' This class represents the simulator of a SEIRD-class type of model.
#'
#' @slot start value for start point of simulation run
#'       (numeric).
#' @slot stop value for end point of simulation run
#'       (numeric).
#' @slot coverage value for time step of simulation run
#'       (numeric).
#'
#' @import tidyverse
#'
#' @export
SimulationParameters <-
  setClass("SimulationParameters",
           slots = c(start="numeric",
                     stop="numeric",
                     tstep="numeric"))

tanh_coverage_smoother <- function(t, start, stop, coverage, tanh_slope) {
  #' Smoother function for the curve of the interventions. Used to avoid breaks
  #' in the ode solver of compartamental models with non-smooth interventions.
  0.5 * coverage * (tanh(tanh_slope * (t - start)) - tanh(tanh_slope * (t - stop)))
}

stack_intervention_coverages <- function(times, int_parms, tanh_slope) {
  for(i in seq_along(int_parms@start)) {
    single_coverage <- map_dbl(
      times, ~tanh_coverage_smoother(.,
                                     int_parms@start[i],
                                     int_parms@stop[i],
                                     int_parms@coverage[i],
                                     tanh_slope))
    if(i == 1)
      total_coverage <- single_coverage
    else
      total_coverage <- total_coverage + single_coverage
  }
  if_else(total_coverage > 1, 1, total_coverage)
}

intervention_protocol <- function(int_parms,
                                  sim_parms,
                                  tanh_slope) {
  #' Creates protocols for the interventions using parameters of the model; used
  #' in the 'run' method of compartamental model with interventions.
  times <- seq(sim_parms@start, sim_parms@stop, sim_parms@tstep)
  coverage <- stack_intervention_coverages(times, int_parms, tanh_slope)
  tibble(time=times, coverage=coverage)
}