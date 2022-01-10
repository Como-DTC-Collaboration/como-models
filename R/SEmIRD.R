#' @include generics.R
#'
NULL

#' An S4 object representing the SEmIRD.
#'
#' This class represents the SEmIRD model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time. Here, there are
#' multiple exposed compartments
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
#' @export SEmIRD
SEmIRD <- setClass("SEmIRD",
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


#' @describeIn SEmIRD Retrieves initial conditions of SEmIRD model.
#'
#' @param object An object of the class SEmIRD.
#' 
#' @export
setMethod("initial_conditions", "SEmIRD",
          function(object) object@initial_conditions)


#' @describeIn SEmIRD Retrieves transmission parameters of SEmIRD model.
#'
#' @param object An object of the class SEmIRD.
#' 
#' @export
setMethod("transmission_parameters", "SEmIRD",
          function(object) object@transmission_parameters)

# SEmIRD class specific functions

#' @describeIn SEmIRD Setter method for initial conditions (S0, E0, I0 and R0)
#' of the SEmIRD model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEmIRD
#' @param value (list) list of initial conditions S0, list(E10, E20, E30, ..., En0), I0, R0.
#'
#' @return object of class SEmIRD with initial conditions assigned.
#' 
#' @export
setMethod(
  "initial_conditions<-", "SEmIRD",
  function(object, value) {
    
    # if (mean(names(value) %in% object@initial_condition_names) != 1)
    #   stop(paste0("Initial conditions must contain: ",
    #               object@initial_condition_names))
    init_cond <- value
    # check inits are numeric
    for (p in list("S0", "I0", "R0")) {
      if (!is.numeric(init_cond[[p]])) {
        stop(glue("{p} format must be numeric"))
      }
    }
    for(v in init_cond[["E0"]]){
      if (!is.numeric(v)) {
        stop("each component in E must be numeric")
      }
    }
    # check that the initial conditions are properly normalized
    if (init_cond$S0 + sum(unlist(init_cond$E0)) + init_cond$I0 + init_cond$R0 != 1) {
      stop("Invalid initial conditions. Must add up to 1.")
    }
    
    object@initial_conditions <- init_cond
    # update output names for the exposed compartments: E1, E2, ... E{n_exposed_comp}
    n_exposed_comp <- length(init_cond[["E0"]])
    object@output_names <- list("S", paste0("E", seq(n_exposed_comp)), "I", "R", "D", "Incidence", "Deaths")
    
    object
  })


#' @describeIn SEmIRD Set transmission parameters (beta, kappa, gamma and mu)
#' of the SEmIRD model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEmIRD model)
#' @param value (list) list of values for beta, kappa, gamma, mu, respectively.
#'
#' @return object of class SEmIRD with transmission parameter values
#' assigned.
#' 
#' @export
setMethod(
  "transmission_parameters<-", "SEmIRD",
  function(object, value) {
    
    # create list of parameter values
    if (mean(names(value) %in% object@transmission_parameter_names) != 1)
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
    
    object
  })


#' @describeIn SEmIRD Solves ODEs of the SEmIRD specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = - beta S(t) I(t)}
#' \deqn{\frac{dE_i(t)}{dt} =  beta S(t) I(t) - kappa E_i(t), (if i=1)}
#' \deqn{\frac{dEi(t)}{dt} =  kappa E_{i-1}(t) - kappa E_i(t), (if i>1)}
#' \deqn{\frac{dI(t)}{dt} = kappa E_n(t) - (gamma + mu) I(t)}
#' \deqn{\frac{dR(t)}{dt} = gamma I(t)}
#' \deqn{\frac{dC(t)}{dt} = beta S(t) I(t)}
#' \deqn{\frac{dD(t)}{dt} = mu I(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEmIRD
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is lsoda which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return two dataframes: one with the time steps, age range, time series of S,
#' E1, E2, ..., En, I and R population fractions, and one with the time steps, age range,
#' time series of incidences and deaths population fraction.
#' 
#' @export
setMethod(
  "run", "SEmIRD",
  function(object, times, solve_method = "lsoda") {
    if (!is.double(times)) {
      stop("Evaluation times of the model storage format must be a vector.")
    }
    
    if (is.null(unlist(object@transmission_parameters)))
      stop("Transmission parameters must be set before running.")
    if (is.null(unlist(object@initial_conditions)))
      stop("Initial conditions must be set before running.")
    # set initial state vector
    state <- c(initial_conditions(object)$S0,
               unlist(initial_conditions(object)$E0),
               initial_conditions(object)$I0,
               initial_conditions(object)$R0,
               0,
               0)
    n_exposed_comp = length(initial_conditions(object)$E0)
    names(state) <- c("S", paste0("E", seq(n_exposed_comp)), "I", "R", "C", "D")
    # set transmission parameters vector
    parameters <- c(b = transmission_parameters(object)$beta,
                    k = transmission_parameters(object)$kappa,
                    g = transmission_parameters(object)$gamma,
                    m = transmission_parameters(object)$mu,
                    n_exposed_comp = length(initial_conditions(object)$E0))
    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters) {
      with(
        as.list(c(state, parameters)), {
          s <- state[1]
          e <- state[2:(n_exposed_comp+1)]
          i <- state[n_exposed_comp+2]
          r <- state[n_exposed_comp+3]
          c <- state[n_exposed_comp+4]
          d <- state[n_exposed_comp+5]
          de <- c()
          # rate of change
          ds <- -b * s * i
          for(idx in seq(n_exposed_comp)){
            if(idx == 1){
              de[idx] <- - k * e[idx] + b * s * i
            }
            else{
              de[idx] <- k * e[idx - 1] - k * e[idx]
            }
          }
          di <- k * e[n_exposed_comp] - (g + m) * i
          dr <- g * i
          dc <- b * s * i
          d_death <- m * i
          list(c(ds, unlist(de), di, dr, dc, d_death))
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
    
    # Split output into 2 dataframes: one with S, E1, E2, ..., En, I, R, D and one with
    # incidence and deaths
    states <- subset(output, !output$compartment %in% c("Incidence", "Deaths"))
    states <- droplevels(states)
    changes <- subset(output, output$compartment %in% c("Incidence", "Deaths"))
    changes <- droplevels(changes)
    
    list("states" = states, "changes" = changes)
  })

#' @describeIn SEmIRD Calculates basic reproduction number for SEmIRD model
#'
#' The R0 parameter is given by:
#' \deqn{R_0 = \beta/(\gamma + \mu)}
#'
#' @param model an SEmIRD model
#'
#' @return an R0 value
#' 
#' @export
setMethod("R0", "SEmIRD", function(model) {
  beta <- model@transmission_parameters$beta
  gamma <- model@transmission_parameters$gamma
  mu <- model@transmission_parameters$mu
  beta / (gamma + mu)
})
