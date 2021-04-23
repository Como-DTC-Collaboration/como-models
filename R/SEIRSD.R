#' An S4 object representing the SEIRSD.
#'
#' This class represents the SEIRSD model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time.
#'
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I0", R0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("b", "k", "g", "m", "a").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#'
#' @import deSolve
#' @import glue
#' @import reshape2
#' @export SEIRSD
SEIRSD <- setClass("SEIRSD",
         # slots
         slots = c(
           output_names = "list",
           initial_condition_names = "list",
           transmission_parameter_names = "list",
           initial_conditions = "list",
           transmission_parameters = "list"
         ),
         # prototypes for the slots, automatically set parameter names and
         # its data type
         prototype = list(
           output_names = list("S", "E", "I", "R", "Incidences", "D"),
           initial_condition_names = list("S0", "E0", "I0", "R0"),
           transmission_parameter_names = list("b", "k", "g", "m", "a"),
           initial_conditions = vector(mode = "list", length = 4)
         )
)

#' Retrieves initial conditions of SEIRSD model.
#'
#' @param object An object of the class SEIRSD.
#' @export
setGeneric("initial_conditions",
           function(object) standardGeneric("initial_conditions"))


#' @describeIn SEIRSD Retrieves initial conditions of SEIRSD model.
#'
#' @param object An object of the class SEIRSD.
#' @aliases initial_conditions,ANY,ANY-method
#' @export
setMethod("initial_conditions", "SEIRSD",
          function(object) object@initial_conditions)

#' Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIRSD.
#' @export
setGeneric("transmission_parameters",
           function(object) standardGeneric("transmission_parameters"))

#' @describeIn SEIRSD Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIRSD.
#' @aliases transmission_parameters,ANY,ANY-method
#' @export

setMethod("transmission_parameters", "SEIRSD",
          function(object) object@transmission_parameters)

#' Setter method for initial conditions (S0, E0, I0 and R0) of the SEIR model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIRSD
#' @param value (list) list of initial conditions S0, E0, I0, R0.
#'
#' @return object of class SEIRSD with initial conditions assigned.
#'
#' @export

setGeneric(
  "initial_conditions<-",
  function(object, value) {
    standardGeneric("initial_conditions<-")
  })

#' @describeIn SEIRSD Setter method for initial conditions (S0, E0, I0 and R0)
#' of the SEIR model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIRSD
#' @param value (list) list of initial conditions S0, E0, I0, R0.
#'
#' @return object of class SEIRSD with initial conditions assigned.
#'
#' @aliases initial_conditions<-,ANY,ANY-method
#' @export
setMethod(
  "initial_conditions<-", "SEIRSD",
  function(object, value) {
    
    # create list of parameter values
    init_cond <- value
    
    # add names to each value
    names(init_cond) <- object@initial_condition_names
    
    # raise errors if age category dimensions do not match initial state vectors
    # also raise errors if initial state and parameter values are not doubles
    for (p in list("S0", "E0", "I0", "R0")) {
      if (!is.numeric(init_cond[[p]])) {
        stop(glue("{p} format must be numeric"))
      }
    }
    
    # check that the initial conditions are properly normalized
    if (init_cond$S0 + init_cond$E0 + init_cond$I0 + init_cond$R0 != 1) {
      stop("Invalid initial conditions. Must add up to 1.")
    }
    
    # if all above tests are passed, assign the init_cond namelist to the object
    object@initial_conditions <- init_cond
    
    return(object)
  })

#' Setter method for transmission parameters
#' (b, k, g, m, a) of the SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIRSD model)
#' @param value (list) list of values for b, k, g, m, a respectively.
#'
#' @return object of class SEIRSD with transmission parameter values
#' assigned.
#' @export

setGeneric(
  "transmission_parameters<-",
  function(object, value) {
    standardGeneric("transmission_parameters<-")
  })


#' @describeIn SEIRSD Setter method for transmission parameters
#' (b, k, g and m) of the SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIRSD model)
#' @param value (list) list of values for b, k, g, m, respectively.
#'
#' @return object of class SEIRSD with transmission parameter values
#' assigned.
#' @aliases transmission_parameters<-,ANY,ANY-method
#' @export

setMethod(
  "transmission_parameters<-", "SEIRSD",
  function(object, value) {
    
    # create list of parameter values
    trans_params <- value
    
    # add names to each value
    names(trans_params) <- object@transmission_parameter_names
    
    # check format of parameters b, k, g, m, a
    if (length(trans_params$b) != 1
        | length(trans_params$k) != 1
        | length(trans_params$g) != 1
        | length(trans_params$m) != 1
        | length(trans_params$a) != 1) {
      stop("The parameter values should be 1-dimensional.")
    }
    
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params
    
    return(object)
  })

# SEIRSD class specific functions

#' Solves ODEs of the SEIRSD specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = a R(t) - b S(t) I(t)}
#' \deqn{\frac{dE(t)}{dt} = b S(t) I(t) - k E(t)}
#' \deqn{\frac{dI(t)}{dt} = k E(t) - (g + m) I(t)}
#' \deqn{\frac{dR(t)}{dt} = g I(t) - a R(t)}
#' \deqn{\frac{dC(t)}{dt} = b S(t) I(t)}
#' \deqn{\frac{dD(t)}{dt} = m I(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIRSD
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return a dataframe with the time steps, time series of S, E, I and R
#' population fractions, and incidence numbers and deaths of the SEIRSD model.
#' @export

setGeneric(name = "run",
           def = function(object, times = seq(0, 100, by = 1),
                          solve_method = "lsoda") {
             standardGeneric("run")})

#' @describeIn SEIRSD Solves ODEs of the SEIRSD specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = a R(t) - b S(t) I(t)}
#' \deqn{\frac{dE(t)}{dt} = b S(t) I(t) - k E(t)}
#' \deqn{\frac{dI(t)}{dt} = k E(t) - (g + m) I(t)}
#' \deqn{\frac{dR(t)}{dt} = g I(t) - a R(t)}
#' \deqn{\frac{dC(t)}{dt} = b S(t) I(t)}
#' \deqn{\frac{dD(t)}{dt} = m I(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIRSD
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return a dataframe with the time steps, time series of S, E, I and R
#' population fractions, and incidence numbers and deaths of the SEIRSD model.
#' @aliases run,ANY,ANY-method
#' @export

setMethod(
  "run", "SEIRSD",
  function(object, times, solve_method = "lsoda") {
    if (!is.double(times)) {
      stop("Evaluation times of the model storage format must be a vector.")
    }
    
    # set initial state vector
    state <- c(S = initial_conditions(object)$S0,
               E = initial_conditions(object)$E0,
               I = initial_conditions(object)$I0,
               R = initial_conditions(object)$R0,
               C = 0,
               D = 0)
    # set transmission parameters vector
    parameters <- c(b = transmission_parameters(object)$b,
                    k = transmission_parameters(object)$k,
                    g = transmission_parameters(object)$g,
                    m = transmission_parameters(object)$m,
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
          ds <- a * r - b * s * i
          de <- b * s * i - k * e
          di <- k * e - (g + m) * i
          dr <- g * i - a * r
          dc <- b * s * i
          d_death <- m * i
          # return the rate of change
          list(c(ds, de, di, dr, dc, d_death))
        })
    }
    
    # call ode solver
    out <- ode(
      y = state, times = times, func = right_hand_side,
      parms = parameters, method = solve_method)
    
    output <- as.data.frame.array(out)
    
    colnames(output) <- c("time", object@output_names)
    
    # Create long format of output
    output <- melt(output, id.vars = "time")
    names(output) <- c("time", "compartment", "value")
    
    # Added for consistency of output format across models
    output$age_group <- rep("0-150", length(output$time))
    
    return(output)
  })