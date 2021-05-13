#' An S4 object representing the SEIRD.
#'
#' This class represents the SEIR model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time.
#'
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I0", R0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("beta", "kappa", "gamma", "mu").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#'
#' @import deSolve
#' @import glue
#' @import reshape2
#' 
#' @export SEIRD
SEIRD <- setClass("SEIRD",
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
           output_names = list("S", "E", "I", "R", "D", "Incidence", "Deaths"),
           initial_condition_names = list("S0", "E0", "I0", "R0"),
           transmission_parameter_names = list("beta", "kappa", "gamma", "mu"),
           initial_conditions = vector(mode = "list", length = 4),
           transmission_parameters = vector(mode = "list", length = 4)
         )
)
#' Retrieves initial conditions of SEIRD model.
#'
#' @param object An object of the class SEIRD.
#' @export

setGeneric("initial_conditions",
           function(object) standardGeneric("initial_conditions"))


#' @describeIn SEIRD Retrieves initial conditions of SEIRD model.
#'
#' @param object An object of the class SEIRD.
#' @aliases initial_conditions,ANY,ANY-method
#' @export
setMethod("initial_conditions", "SEIRD",
          function(object) object@initial_conditions)

#' Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIRD.
#' @export
setGeneric("transmission_parameters",
           function(object) standardGeneric("transmission_parameters"))

#' @describeIn SEIRD Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIRD.
#' @aliases transmission_parameters,ANY,ANY-method
#' @export
setMethod("transmission_parameters", "SEIRD",
          function(object) object@transmission_parameters)

#' Set initial conditions (S0, E0, I0 and R0) of the SEIR model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIRD
#' @param value (list) list of initial conditions S0, E0, I0, R0.
#'
#' @return object of class SEIRD with initial conditions assigned.
#'
#' @export
setGeneric(
  "initial_conditions<-",
  function(object, value) {
    standardGeneric("initial_conditions<-")
  })

#' @describeIn SEIRD Setter method for initial conditions (S0, E0, I0 and R0)
#' of the SEIR model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIRD
#' @param value (list) list of initial conditions S0, E0, I0, R0.
#'
#' @return object of class SEIRD with initial conditions assigned.
#'
#' @aliases initial_conditions<-,ANY,ANY-method
#' @export
setMethod(
  "initial_conditions<-", "SEIRD",
  function(object, value) {

    if(mean(names(value) %in% object@initial_condition_names) != 1)
      stop(paste0("Initial conditions must contain: ",
                  object@initial_condition_names))
    init_cond <- value
    
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

    object@initial_conditions <- init_cond

    return(object)
  })

#' Set transmission parameters for SEIRD model
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIRD model)
#' @param value (list) list of values for beta, kappa, gamma, mu, respectively.
#'
#' @return object of class SEIRD with transmission parameter values
#' assigned.
#' @export
setGeneric(
  "transmission_parameters<-",
  function(object, value) {
    standardGeneric("transmission_parameters<-")
  })


#' @describeIn SEIRD Set transmission parameters (beta, kappa, gamma and mu) of the SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIRD model)
#' @param value (list) list of values for beta, kappa, gamma, mu, respectively.
#'
#' @return object of class SEIRD with transmission parameter values
#' assigned.
#' @aliases transmission_parameters<-,ANY,ANY-method
#' @export
setMethod(
  "transmission_parameters<-", "SEIRD",
  function(object, value) {

    # create list of parameter values
    if(mean(names(value) %in% object@transmission_parameter_names) != 1)
      stop(paste0("Transmission parameters must contain: ",
                  object@transmission_parameter_names))
    trans_params <- value

    # check format of parameters
    if (length(trans_params$b) != 1
        | length(trans_params$k) != 1
        | length(trans_params$g) != 1
        | length(trans_params$m) != 1) {
      stop("The parameter values should be 1-dimensional.")
    }

    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params

    return(object)
  })

# SEIRD class specific functions

#' Solves ODEs of the SEIRD specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = - beta S(t) I(t)}
#' \deqn{\frac{dE(t)}{dt} =  beta S(t) I(t) - kappa E(t)}
#' \deqn{\frac{dI(t)}{dt} = kappa E(t) - (gamma + mu) I(t)}
#' \deqn{\frac{dR(t)}{dt} = gamma I(t)}
#' \deqn{\frac{dC(t)}{dt} = beta S(t) I(t)}
#' \deqn{\frac{dD(t)}{dt} = mu I(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIRD
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return two dataframes: one with the time steps, age range, time series of S,
#' E, I and R population fractions, and one with the time steps, age range,
#' time series of incidences and deaths population fraction.
#' @export
setGeneric(name = "run",
           def = function(object, times = seq(0, 100, by = 1),
                          solve_method = "lsoda") {
             standardGeneric("run")})

#' @describeIn SEIRD Solves ODEs of the SEIRD specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = - beta S(t) I(t)}
#' \deqn{\frac{dE(t)}{dt} =  beta S(t) I(t) - kappa E(t)}
#' \deqn{\frac{dI(t)}{dt} = kappa E(t) - (gamma + mu) I(t)}
#' \deqn{\frac{dR(t)}{dt} = gamma I(t)}
#' \deqn{\frac{dC(t)}{dt} = beta S(t) I(t)}
#' \deqn{\frac{dD(t)}{dt} = mu I(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIRD
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return two dataframes: one with the time steps, age range, time series of S,
#' E, I and R population fractions, and one with the time steps, age range,
#' time series of incidences and deaths population fraction.
#' @aliases run,ANY,ANY-method
#' @export
setMethod(
  "run", "SEIRD",
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
                    m = transmission_parameters(object)$m)
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
          di <- k * e - (g + m) * i
          dr <- g * i
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

    # Compute incidences and deaths
    cases <- c(0, diff(output$C))
    deaths <- c(0, diff(output$D))
    output$Incidence <- cases
    output$Deaths <- deaths
    output <- output[, c("time", unlist(object@output_names))]

    # Create long format of output
    output <- melt(output, id.vars = "time")
    output <- output[ , c("time", "value", "variable")]
    names(output) <- c("time", "value", "compartment")

    # Added for consistency of output format across models
    output$age_range <- rep("0-150", length(output$time))
    
    # Split output into 2 dataframes: one with S,E,I, and R and one with C and D
    states <- subset(output, !output$compartment %in% c("Incidence", "Deaths"))
    states <- droplevels(states)
    daily <- subset(output, output$compartment %in% c("Incidence", "Deaths"))
    daily <- droplevels(daily)
    
    return(list("states" = states, "daily" = daily))
  })