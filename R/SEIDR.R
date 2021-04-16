#' An S4 object representing the SEIDR.
#'
#' This class represents the SEIR model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time.
#'
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I0", "D0", R0").
#' @slot initial_cases_names name for initial cases (characters). Default is 
#'       list("C0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("b", "k", "g", "a").
#' @slot initial_conditions list of values for initial conditions (double). 
#' @slot initial_cases list of values for initial cases (double). Default 
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#'
#' @import deSolve
#' @import glue

setClass("SEIDR",
         # slots
         slots = c(
           output_names = 'list',
           initial_condition_names = 'list',
           transmission_parameter_names = 'list',
           initial_conditions = 'list',
           transmission_parameters = 'list'
         ),
         # prototypes for the slots, automatically set parameter names and
         # its data type
         prototype = list(
           output_names = list("S", "E", "I", "R", "Incidences", "Deaths"),
           initial_condition_names = list("S0", "E0", "I0", "D0", "R0"),
           initial_cases_names = list("C0"),
           transmission_parameter_names = list("b", "k", "g", "a"),
           initial_conditions = vector(mode = "list", length = 4),
           initial_cases = vector(mode = "list", length = 2),
           transmission_parameters = vector(mode = "list", length = 3)
         )
)

#' @describeIn SEIDR retrieves initial conditions for SEIR model.
#'
#' @param object An object of the class SEIDR.

setGeneric("initial_conditions",
           function(object) standardGeneric("initial_conditions"))

setMethod("initial_conditions", "SEIDR",
          function(object) object@initial_conditions)

#' @describeIn SEIDR retrieves initial cases for SEIR model.
#'
#' @param object An object of the class SEIDR.

setGeneric("initial_cases",
           function(object) standardGeneric("initial_cases"))

setMethod("initial_cases", "SEIDR",
          function(object) object@initial_cases)

#' @describeIn SEIDR retrieves transmission parameters for SEIR model.
#'
#' @param object An object of the class SEIDR.
setGeneric("transmission_parameters",
           function(object) standardGeneric("transmission_parameters"))

setMethod("transmission_parameters", "SEIDR",
          function(object) object@transmission_parameters)

#' @describeIn SEIDR setter and getter method for initial conditions (S0, 
#' E0, I0 and R0) of the SEIR model.
#'
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIDR
#' @param S0 (double) initial fraction of the population that is susceptible
#' @param E0 (double) initial fraction of the population that has been exposed
#' @param I0 (double) initial fraction of the population that is infectious
#' @param D0 (double) initial fraction of the population that is dead from the
#' disease
#' @param R0 (double) initial fraction of the population that has recovered#' All initial conditions must sum up to 1.
#'
#' @return object of class SEIDR with initial conditions assigned.
#' @export

setGeneric(
  "initial_conditions<-",
  function(object, S0, E0, I0, D0, R0){
    standardGeneric("initial_conditions<-")
  })

setMethod(
  "initial_conditions<-", "SEIDR",
  function(object, S0, E0, I0, D0, R0) {
    
    # create list of parameter values
    init_cond <- list(S0, E0, I0, D0, R0)
    
    # add names to each value
    names(init_cond) = object@initial_condition_names
    
    # raise errors if age category dimensions do not match initial state vectors
    # also raise errors if initial state and parameter values are not doubles
    for (p in list("S0", "E0", "I0", "D0", "R0")){
      if(!is.numeric(init_cond[[p]])){
        stop(glue("{p} format must be numeric"))}
    }
    
    # check that the initial conditions are properly normalized
    if (sum(S0, E0, I0, D0, R0) != 1) {
      stop("Invalid initial conditions. Must add up to 1.")
    }
    
    # if all above tests are passed, assign the init_cond namelist to the object
    object@initial_conditions <- init_cond
    
    return(object)
  })

#' @describeIn SEIDR setter and getter method for initial cases (C0)
#' of the SEIR model.
#'
#' If the initial cases provided to are not integers an error is thrown.
#' 
#' @param C0 (double) initial fraction of the population that got infected
#'
#' @return object of class SEIDR with initial cases value assigned.
#' @export

setGeneric(
  "initial_cases<-",
  function(object, C0 = 0){
    standardGeneric("initial_cases<-")
  })

setMethod(
  "initial_cases<-", "SEIDR",
  function(object, C0) {
    
    # create list of parameter values
    init_cases <- list(C0)
    
    # add names to each value
    names(init_cases) = object@initial_cases_names
    
    if(!is.numeric(init_cases[["C0"]])){
      stop("C0 format must be numeric")}
    
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@initial_cases <- init_cases
    
    return(object)
  })

#' @describeIn SEIDR setter and getter method for transmission parameters 
#' (b, k, g and a) of the SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#' 
#' @param b (double) rate at which an infected individual exposes susceptible
#' @param k (double) rate at which exposed individuals become infectious
#' @param g (double) rate at which infected individuals recover
#' @param a (double) rate at which infected individuals die from the disease
#'
#' @return object of class SEIDR with transmission parameter values
#' assigned.
#' @export

setGeneric(
  "transmission_parameters<-",
  function(object, b, k, g, a){
    standardGeneric("transmission_parameters<-")
  })

setMethod(
  "transmission_parameters<-", "SEIDR",
  function(object, b, k, g, a) {
    
    # create list of parameter values
    trans_params <- list(b, k, g, a)
    
    # add names to each value
    names(trans_params) = object@transmission_parameter_names
    
    # check format of parameters b, k and g
    if(length(b) != 1 | length(k) != 1 | length(g) != 1 | length(a) != 1){
      stop("The parameter values should be 1-dimensional.")
    }
    
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params
    
    return(object)
  })

# SEIDR class specific functions

#' @describeIn SEIDR solves ODEs of the SEIDR specified in object
#' for the time points specified in times and integration method specified in 
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = - b S(t) I(t)}
#' \deqn{\frac{dE(t)}{dt} =  b S(t) I(t) - k E(t)}
#' \deqn{\frac{dI(t)}{dt} = k E(t) - (g + a) I(t)}
#' \deqn{\frac{dR(t)}{dt} = g I(t)}
#' \deqn{\frac{dC(t)}{dt} = b S(t) I(t)}
#' \deqn{\frac{dD(t)}{dt} = a I(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIDR
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form 
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method 
#' for solving the ode system. Default is "lsoda" which is also the default for 
#' the ode function in the deSolve package used in this function.
#'
#' @return a dataframe with the time steps, time series of S, E, I and R
#' population fractions and incidence numbers of the SEIR model.
#' @export

setGeneric(name = "simulate_SEIR",
           def = function(object, times = seq(0, 100, by = 1),
                          solve_method = "lsoda") {
             standardGeneric("simulate_SEIR")})

setMethod(
  "simulate_SEIR", "SEIDR",
  function(object, times, solve_method = "lsoda") {
    if (!is.double(times)) {
      stop("Evaluation times of the model storage format must be a vector.")
    }
    
    # set initial state vector
    state <- c(S = initial_conditions(object)$S0,
               E = initial_conditions(object)$E0,
               I = initial_conditions(object)$I0,
               R = initial_conditions(object)$R0,
               C = initial_cases(object)$C0,
               D = initial_cases(object)$D0)
    # set transmission parameters vector
    parameters <- c(b = transmission_parameters(object)$b,
                    k = transmission_parameters(object)$k,
                    g = transmission_parameters(object)$g,
                    a = transmission_parameters(object)$a)
    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters) {
      with(
        as.list(c(state, parameters)), {
          s <- state[1]
          e <- state[2]
          i <- state[3]
          r <- state[4]
          c <- state[5]
          d <- state[6]
          # rate of change
          ds <- -b * s * i
          de <- b * s * i - k * e
          di <- k * e - (g + a) * i
          dr <- g * i
          dc <- b * s * i
          d_death <- a * i
          # return the rate of change
          list(c(ds, de, di, dr, dc, d_death))
        })
    }
    
    # call ode solver
    out <- ode(
      y = state, times = times, func = right_hand_side,
      parms = parameters, method = solve_method)
    
    # output as a dataframe
    output <- as.data.frame.array(out)
    
    return(output)
  })
