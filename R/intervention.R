library(tidyverse)
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

InterventionParameters <-
  setClass("InterventionParameters",
           slots = c(start="numeric",
                     stop="numeric",
                     coverage="numeric"),
           validity = check)

SimulationParameters <-
  setClass("SimulationParameters",
           slots = c(start="numeric",
                     stop="numeric",
                     tstep="numeric"))

tanh_coverage_smoother <- function(t, start, stop, coverage, tanh_slope) {
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
  times <- seq(sim_parms@start, sim_parms@stop, sim_parms@tstep)
  coverage <- stack_intervention_coverages(times, int_parms, tanh_slope)
  tibble(time=times, coverage=coverage)
}

# example: remove when going in
int_parms <- InterventionParameters(start=c(75, 130, 170),
                                    stop=c(120, 150, 180),
                                    coverage=c(0.3, 0.8, 0.1))
sim_parms <- SimulationParameters(start=0, stop=200, tstep = 0.1)

intervention_protocol(int_parms, sim_parms, 1) %>%
  ggplot(aes(x=time, y=coverage)) +
  geom_line()
