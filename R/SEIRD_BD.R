#' @include generics.R
#'
NULL

#' An S4 object representing the SEIRD_BD model.
#'
#' This class represents the SEIRD_BD model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time.
#'
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I0", R0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("beta", "kappa", "gamma", "mu", "lambda", "nu", "delta").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#'
#' @import deSolve
#' @import glue
#' @import reshape2
#'
#' @export SEIRD_BD
#' 
SEIRD_BD <- setClass("SEIRD_BD",
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
           transmission_parameter_names = list("beta", "kappa", "gamma", "mu", "lambda", "nu", "delta"),
           initial_conditions = vector(mode = "list", length = 4),
           transmission_parameters = vector(mode = "list", length = 7)
         )
)

# Setter and getter methods for initial_conditions

#' @describeIn SEIRD_BD Retrieves initial conditions of SEIRD_BD model.
#'
#' @param object An object of the class SEIRD_BD.
#' 
#' @export
setMethod("initial_conditions", "SEIRD_BD",
          function(object) object@initial_conditions)


#' @describeIn SEIRD_BD Retrieves transmission parameters of SEIRD_BD model.
#'
#' @param object An object of the class SEIRD_BD.
#' 
#' @export
setMethod("transmission_parameters", "SEIRD_BD",
          function(object) object@transmission_parameters)

# SEIRD_BD class specific functions

#' @describeIn SEIRD_BD Setter method for initial conditions (S0, E0, I0 and R0)
#' of the SEIRD_BD model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIRD_BD
#' @param value (list) list of initial conditions S0, E0, I0, R0.
#'
#' @return object of class SEIRD_BD with initial conditions assigned.
#' 
#' @export
setMethod(
  "initial_conditions<-", "SEIRD_BD",
  function(object, value) {

    if (mean(names(value) %in% object@initial_condition_names) != 1)
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

    object
  })


#' @describeIn SEIRD_BD Set transmission parameters (beta, kappa, gamma, mu, lambda, nu)
#' of the SEIRD_BD model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIRD_BD model)
#' @param value (list) list of values for beta, kappa, gamma, mu, lambda, nu
#'
#' @return object of class SEIRD_BD with transmission parameter values
#' assigned.
#' 
#' @export
setMethod(
  "transmission_parameters<-", "SEIRD_BD",
  function(object, value) {

    # create list of parameter values
    if (mean(names(value) %in% object@transmission_parameter_names) != 1)
      stop(paste0("Transmission parameters must contain: ",
                  object@transmission_parameter_names))
    trans_params <- value

    # check format of parameters
    if (length(trans_params$beta) != 1
        | length(trans_params$kappa) != 1
        | length(trans_params$gamma) != 1
        | length(trans_params$mu) != 1
        | length(trans_params$lambda) != 1
        | length(trans_params$nu) != 1
        | length(trans_params$delta) != 1) {
      stop("The parameter values should be 1-dimensional.")
    }

    # if all above tests are passed, assign the trans_params named list to the
    # object
    object@transmission_parameters <- trans_params

    object
  })


#' @describeIn SEIRD_BD Solves ODEs of the SEIRD_BD specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = lambda + delta R(t) - beta S(t) I(t) - nu S(t)}
#' \deqn{\frac{dE(t)}{dt} =  beta S(t) I(t) - kappa E(t) - nu E(t)}
#' \deqn{\frac{dI(t)}{dt} = kappa E(t) - (gamma + mu) I(t) - nu I(t)}
#' \deqn{\frac{dR(t)}{dt} = gamma I(t) - (nu + delta) R(t)}
#' \deqn{\frac{dD(t)}{dt} = mu I(t) + nu (S(t) + E(t) + I(t) + R(t))}
#' \deqn{\frac{dC(t)}{dt} = beta S(t) I(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIRD_BD
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is lsoda which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return two dataframes: one with the time steps, age range, time series of S,
#' E, I, R and D population fractions, and one with the time steps, age range,
#' time series of incidences and deaths population fraction.
#' 
#' @export
setMethod(
  "run", "SEIRD_BD",
  function(object, times, solve_method = "lsoda") {
    if (!is.double(times)) {
      stop("Evaluation times of the model storage format must be a vector.")
    }

    if (is.null(unlist(object@transmission_parameters)))
      stop("Transmission parameters must be set before running.")
    if (is.null(unlist(object@initial_conditions)))
      stop("Initial conditions must be set before running.")

    # set initial state vector
    state <- c(S = initial_conditions(object)$S0,
               E = initial_conditions(object)$E0,
               I = initial_conditions(object)$I0,
               R = initial_conditions(object)$R0,
               C = 0,
               D = 0)
    # set transmission parameters vector
    parameters <- c(beta = transmission_parameters(object)$beta,
                    kappa = transmission_parameters(object)$kappa,
                    gamma = transmission_parameters(object)$gamma,
                    mu = transmission_parameters(object)$mu,
                    lambda = transmission_parameters(object)$lambda,
                    nu = transmission_parameters(object)$nu,
                    delta = transmission_parameters(object)$delta)
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
          ds <- lambda - beta * s * i - nu * s + delta * r
          de <- beta * s * i - kappa * e - nu * e
          di <- kappa * e - (gamma + mu) * i - nu * i
          dr <- gamma * i - nu * r - delta * r
          dc <- beta * s * i
          d_death <- mu * i + nu * (s + e + i + r)
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
    output <- output[, c("time", "value", "variable")]
    names(output) <- c("time", "value", "compartment")

    # Added for consistency of output format across models
    output$age_range <- rep("0-150", length(output$time))

    # Split output into 2 dataframes: one with SEIRD, another with incidence and deaths
    states <- subset(output, !output$compartment %in% c("Incidence", "Deaths"))
    states <- droplevels(states)
    changes <- subset(output, output$compartment %in% c("Incidence", "Deaths"))
    changes <- droplevels(changes)

    list("states" = states, "changes" = changes)
  })

#' @describeIn SEIRD_BD Calculates basic reproduction number for SEIRD_BD model
#'
#' The R0 parameter is given by:
#' \deqn{R_0 = \beta \kappa / (\kappa + \nu) * 1/(\gamma + \mu + \nu)}
#'
#' @param model an SEIRD_BD model
#'
#' @return an R0 value
#' 
#' @export
setMethod("R0", "SEIRD_BD", function(model) {
  beta <- model@transmission_parameters$beta
  gamma <- model@transmission_parameters$gamma
  mu <- model@transmission_parameters$mu
  kappa <- model@transmission_parameters$kappa
  nu <- model@transmission_parameters$nu
  beta * kappa / (kappa + nu) * 1 / (gamma + mu + nu)
})


#' @describeIn SEIRD_BD Prints a compartmental diagram for the SEIRD model
#'
#' @param model an SEIRD_BD model
#'
#' @return An ODE-compartmental structure diagram object of class html
#' 
#' @export
setMethod("ode_structure_diagram", "SEIRD_BD", function(model) {
  htmltools::HTML(comomodels:::seird_bd_structure)
})