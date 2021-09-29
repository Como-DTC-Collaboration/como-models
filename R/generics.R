#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generics
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' Defines generics which are shared between classes within comomodels packages.
#' Class-specific Generics are defined in each class separately.

#' Retrieves initial conditions of a class within comomodels package(one of SEIRD, SEIRDAge, SEIaImIsRD, SEIRD_RU).
#'
#' @param object An object of the an S4 class within comomodels package(one of SEIRD, SEIRDAge, SEIaImIsRD, SEIRD_RU)
#' 
#' 
#' @export
setGeneric("initial_conditions",
           function(object) standardGeneric("initial_conditions"))

#' Retrieves transmission parameters of a class within comomodels package.
#'
#' @param object An object of one of the classes of the como model
#' 
#' 
#' @export
setGeneric("transmission_parameters",
           function(object) standardGeneric("transmission_parameters"))

#' Set initial conditions of a class within comomodels package.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of a class within comomodels package
#' @param value (list) list of initial conditions
#'
#' @return object of a class within comomodels package with initial conditions assigned.
#' 
#' 
#' @export
setGeneric(
  "initial_conditions<-",
  function(object, value) {
    standardGeneric("initial_conditions<-")
  })

#' Set transmission parameters for a class within comomodels package.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (a class within comomodels package)
#' @param value (list) list of values for each transmission parameter, respectively
#'
#' @return object of a class within comomodels package with transmission parameter values
#' assigned.
#' 
#' 
#' @export
setGeneric(
  "transmission_parameters<-",
  function(object, value) {
    standardGeneric("transmission_parameters<-")
  })

#' Solves ODEs of a class within comomodels package specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of a class within the comomodels
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function
#'
#' @return two dataframes: one with the time steps, age range, time series of each
#' group of population fractions, and one with the time steps, age range,
#' time series of incidences and deaths population fraction.
#' 
#' 
#' @export
setGeneric(name = "run",
           def = function(object, times = seq(0, 100, by = 1),
                          solve_method = "lsoda") {
             standardGeneric("run")})

#' Calculates basic reproduction number
#'
#' @param model a model object from comomodels package
#'
#' @return an R0 value
setGeneric("R0", def = function(model) {
  standardGeneric("R0")
})

