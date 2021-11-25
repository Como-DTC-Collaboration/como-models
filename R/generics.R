#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generics
# Defines generics which are shared between classes within comomodels packages.
# Class-specific Generics are defined in each class separately.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#' Retrieves initial conditions of a class within comomodels package
#'
#' @param object An object of the an S4 class within comomodels package
#' 
#' 
#' @export
setGeneric("initial_conditions",
           function(object) standardGeneric("initial_conditions"))

#' Retrieves transmission parameters of a class within comomodels package
#'
#' @param object An object of one of the classes of the como model
#' 
#' 
#' @export
setGeneric("transmission_parameters",
           function(object) standardGeneric("transmission_parameters"))

#' Set initial conditions of a class within comomodels package.
#'
#' All initial conditions must sum up to 1. If the initial conditions provided to do not sum to 1, an error is thrown.
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

#' Set transmission parameters for a class within comomodels package
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

#' Solves ODEs for a given model
#' 
#' Solves ODEs of a model class within comomodels package and returns solutions
#' at the time points specified using a specified integration method.
#' Numerical solution of the ODEs is carried out by the deSolve package.
#'
#' @param object an object of a class within the comomodels
#' @param times (double) a vector of time points at which the solution to
#' the system of ODEs should be returned. Default time series is seq(0, 100, by = 1)
#' @param solve_method (string) a string specifying the chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function
#' @param ... other parameters
#'
#' @return a named list of two dataframes: 'states' which describes the time evolution of
#' compartment proportions; and 'changes' which describes the time evolution of daily quantities
#' like cases and deaths
#' 
#' 
#' @export
setGeneric(name = "run",
           def = function(object, times = seq(0, 100, by = 1),
                          solve_method = "lsoda", ...) {
             standardGeneric("run")})

#' Calculates basic reproduction number
#'
#' @param model a model object from comomodels package
#'
#' @return an R0 value
setGeneric("R0", def = function(model) {
  standardGeneric("R0")
})



#' Retrieves intervention parameters of SEIRDV or SEIRDAge_interventions model.
#'
#' @param object An object of a model class within comomodels package (SEIRDV or SEIRDAge_interventions).
#' 
#' @export
setGeneric("intervention_parameters",
           function(object) standardGeneric("intervention_parameters"))


#' Set intervention parameters of the SEIRV or SEIRDAge model.
#'
#' Intervention parameters have same size. This class is designed for interventions
#' which last several days at least and have several days between them.
#'
#' @param object An object of a model class within comomodels package (SEIRDV or SEIRDAge_interventions).
#' @param value (list) list of intervention parameters: starts, stops and coverages.
#'
#' @return object of class within comomodels package
#' @export
setGeneric(
  "intervention_parameters<-",
  function(object, value) {
    standardGeneric("intervention_parameters<-")
  })