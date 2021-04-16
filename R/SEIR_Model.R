#' An S4 object representing the SEIR_model.
#'
#' This class represents the SEIR model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time.
#'
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I0", "R0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("b", "k", "g").
#' @slot initial_conditions list of values for initial conditions(double). 
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#'
#' @import deSolve
#' @import glue

setClass("SEIR_model",
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
           output_names = list("S", "E", "I", "R", "Incidence"),
           initial_condition_names = list("S0", "E0", "I0", "R0"),
           transmission_parameter_names = list("b", "k", "g"),
           initial_conditions = vector(mode = "list", length = 4),
           transmission_parameters = vector(mode = "list", length = 3)
         )
)

#' @describeIn SEIR_model retrieves initial conditions for SEIR model.
#'
#' @param object An object of the class SEIR_model.

setMethod("initial_conditions", "SEIR_model",
          function(object) object@initial_conditions)

#' @describeIn SEIR_model retrieves transmission parameters for SEIR model.
#'
#' @param object An object of the class SEIR_model.

setMethod("transmission_parameters", "SEIR_model",
          function(object) object@transmission_parameters)

#' @describeIn SEIR_model setter and getter method for initial conditions (S0, 
#' E0, I0 and R0) of the SEIR model.
#'
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIR_model
#' @param S0 (double) initial fraction of the population that is susceptible
#' @param E0 (double) initial fraction of the population that has been exposed
#' @param I0 (double) initial fraction of the population that is infectious
#' @param R0 (double) initial fraction of the population that has recovered#' All initial conditions must sum up to 1.
#'
#' @return object of class SEIR_model with initial conditions assigned.
#' @export

setMethod(
  "initial_conditions<-", "SEIR_model",
  function(object, value) {
    
    # create list of parameter values
    init_cond <- value
    
    # add names to each value
    names(init_cond) = object@initial_condition_names
    
    # raise errors if age category dimensions do not match initial state vectors
    # also raise errors if initial state and parameter values are not doubles
    for (p in list("S0", "E0", "I0", "R0")){
      if(!is.numeric(init_cond[[p]])){
        stop(glue("{p} format must be numeric"))}
    }
    
    # check that the initial conditions are properly normalized
    if (sum(value$S0, value$E0, value$I0, value$R0) != 1) {
      stop("Invalid initial conditions. Must add up to 1.")
    }
    
    # if all above tests are passed, assign the init_cond namelist to the object
    object@initial_conditions <- init_cond
    
    return(object)
  })

#' @describeIn SEIR_model setter and getter method for transmission parameters 
#' (b, k and g) of the SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#' 
#' @param b (double) rate at which an infected individual exposes susceptible
#' @param k (double) rate at which exposed individuals become infectious
#' @param g (double) rate at which infected individuals recover
#'
#' @return object of class SEIR_model with transmission parameter values
#' assigned.
#' @export

setMethod(
  "transmission_parameters<-", "SEIR_model",
  function(object, value) {
    
    # create list of parameter values
    trans_params <- value
    
    # add names to each value
    names(trans_params) = object@transmission_parameter_names
    
    # check format of parameters b, k and g
    if(length(value$b) != 1 | length(value$k) != 1 | length(value$g) != 1){
      stop("The parameter values should be 1-dimensional.")
    }
    
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params
    
    return(object)
  })

# SEIR_model class specific functions

#' @describeIn SEIR_model solves ODEs of the SEIR_model specified in object
#' for the time points specified in times and integration method specified in 
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = - b S(t) I(t)}
#' \deqn{\frac{dE(t)}{dt} =  b S(t) I(t) - k E(t)}
#' \deqn{\frac{dI(t)}{dt} = k E(t) - g I(t)}
#' \deqn{\frac{dR(t)}{dt} = g I(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIR_model
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
  "simulate_SEIR", "SEIR_model",
  function(object, times, solve_method = "lsoda") {
    if (!is.double(times)) {
      stop("Evaluation times of the model storage format must be a vector.")
    }

    # set initial state vector
    state <- c(S = initial_conditions(object)$S0,
               E = initial_conditions(object)$E0,
               I = initial_conditions(object)$I0,
               R = initial_conditions(object)$R0)
    # set transmission parameters vector
    parameters <- c(b = transmission_parameters(object)$b,
                    k = transmission_parameters(object)$k,
                    g = transmission_parameters(object)$g)
    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters) {
      with(
        as.list(c(state, parameters)), {
          s <- state[1]
          e <- state[2]
          i <- state[3]
          r <- state[4]
          # rate of change
          ds <- -b * s * i
          de <- b * s * i - k * e
          di <- k * e - g * i
          dr <- g * i
          # return the rate of change
          list(c(ds, de, di, dr))
        })
    }

    # call ode solver
    out <- ode(
      y = state, times = times, func = right_hand_side,
      parms = parameters, method = solve_method)

    # output as a dataframe
    output <- as.data.frame.array(out)
    
    # compute incidence number
    total_inf <- output$I + output$R
    n_inc <- c(0,total_inf[2:length(total_inf)]-total_inf[1:length(total_inf)-1])

    output$Incidence <- n_inc
    
    return(output)
  })
