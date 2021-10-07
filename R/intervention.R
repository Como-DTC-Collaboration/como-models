#' Checks that the format of the
#' InterventionParameters is correct: only whole intervals are allowed; intervals
#' do not overlap; no backwards intervals.
#' 
#' @param object InterventionParameters object whose shape we check.
#' 
#' @return error messages.
#' 
#' @import tidyverse
check <-function(object) {
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
#' This class hosts the intervention parameters used to create intervention protocols
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
#' @export InterventionParameters
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
#' @slot tstep value for the time step of the grid on which the smoothed
#'       intervention protocol is discretized (numeric).
#'
#' @import tidyverse
#'
#' @export SimulationParameters
SimulationParameters <-
  setClass("SimulationParameters",
           slots = c(start="numeric",
                     stop="numeric",
                     tstep="numeric"))

#' Smoother function for the curve of the interventions. Used to avoid breaks
#' in the ode solver of compartmental models with non-smooth interventions.
#' 
#' @param t time at which the intervention effect is computed.
#' @param start time at which the intervention started.
#' @param stop time at which the intervention ended.
#' @param coverage height of intervention effect.
#' @param tanh_slope sharpness of the intervention wave used for function
#'                   continuity purposes;larger values of this parameter cause
#'                   the curve to more closely approach the step function.
#'                   
#' @return value of intervention effect.
#' 
#' @import tidyverse
#' 
#' @export tanh_coverage_smoother
tanh_coverage_smoother <- function(t, start, stop, coverage, tanh_slope) {
  # check that the tangent of the slope is always positive
  if (tanh_slope <= 0) {
    stop("Tangent of the slope must always be positive.")
  }
  
  0.5 * coverage * (tanh(tanh_slope * (t - start)) - tanh(tanh_slope * (t - stop)))
}

#' Creates stacks of interventions using parameters of the model; used
#' in the 'run' method of compartmental model with interventions.
#' 
#' @param times times between which we simulate.
#' @param int_parms parameters of the interventions as according to a InterventionParameters
#'                  object.
#' @param tanh_slope sharpness of the intervention waves used for function
#'                   continuity purposes.
#' 
#' @import tidyverse
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

#' Creates protocols for the interventions using parameters of the model; used
#' in the 'run' method of compartmental model with interventions.
#' 
#' @param int_parms parameters of the interventions as according to a InterventionParameters
#'                  object.
#' @param sim_parms parameters of the simulation timeframe as according to a SimulationParameters
#'                  object.
#' @param tanh_slope sharpness of the intervention waves used for function
#'                   continuity purposes.
#'                   
#' @import tidyverse
#' 
#' @export intervention_protocol
intervention_protocol <- function(int_parms,
                                  sim_parms,
                                  tanh_slope) {
  times <- seq(sim_parms@start, sim_parms@stop, sim_parms@tstep)
  coverage <- stack_intervention_coverages(times, int_parms, tanh_slope)
  tibble(time=times, coverage=coverage)
}