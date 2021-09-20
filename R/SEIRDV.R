#' An S4 object representing the SEIRDV.
#'
#' This class represents the SEIRV model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time. Vaccinated
#' individuals are considered in their own compartment
#'
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I0", R0", "V0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("beta", "kappa", "gamma", "mu",  "nu",
#'       "delta_V", "delta_R").
#' @slot intervention_parameter_names list of names of intervention parameters
#'       (characters). Default is list ("starts", "stops", "coverages")
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#' @slot intervention_parameters list of values for intervention parameters
#'       (double).
#'
#' @import deSolve
#' @import glue
#' @import reshape2
#'
#' @export SEIRDV
SEIRDV <- setClass("SEIRDV",
                  # slots
                  slots = c(
                    output_names = "list",
                    initial_condition_names = "list",
                    transmission_parameter_names = "list",
                    intervention_parameter_names = "list",
                    initial_conditions = "list",
                    transmission_parameters = "list",
                    intervention_parameters = "list"
                  ),
                  # prototypes for the slots, automatically set parameter names and
                  # its data type
                  prototype = list(
                    output_names = list("S", "E", "I", "R", "V", "D", "Incidence", "Deaths"),
                    initial_condition_names = list("S0", "E0", "I0", "R0", "V0"),
                    transmission_parameter_names = list("beta", "kappa", "gamma",
                                                        "mu", "nu", "delta_V",
                                                        "delta_R"),
                    intervention_parameter_names = list("starts", "stops", "coverages"),
                    initial_conditions = vector(mode = "list", length = 5),
                    transmission_parameters = vector(mode = "list", length = 7),
                    intervention_parameters = vector(mode = "list", length = 3)
                  )
)

#' @describeIn SEIRDV Retrieves initial conditions of SEIRV model.
#'
#' @param object An object of the class SEIRDV.
#' 
#' @export
setMethod("initial_conditions", "SEIRDV",
          function(object) object@initial_conditions)

#' @describeIn SEIRDV Retrieves transmission parameters of SEIRV model.
#'
#' @param object An object of the class SEIRDV.
#' 
#' @export
setMethod("transmission_parameters", "SEIRDV",
          function(object) object@transmission_parameters)

#' @describeIn SEIRDV Setter method for initial conditions (S0, E0, I0, R0 and V0)
#' of the SEIRV model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIRDV
#' @param value (list) list of initial conditions S0, E0, I0, R0, V0.
#'
#' @return object of class SEIRDV with initial conditions assigned.
#' 
#' @export
setMethod(
  "initial_conditions<-", "SEIRDV",
  function(object, value) {
    
    if (mean(names(value) %in% object@initial_condition_names) != 1)
      stop(paste0("Initial conditions must contain: ",
                  object@initial_condition_names))
    init_cond <- value
    
    # raise errors if age category dimensions do not match initial state vectors
    # also raise errors if initial state and parameter values are not doubles
    for (p in list("S0", "E0", "I0", "R0", "V0")) {
      if (!is.numeric(init_cond[[p]])) {
        stop(glue("{p} format must be numeric"))
      }
    }
    
    # check that the initial conditions are properly normalized
    if (init_cond$S0 + init_cond$E0 + init_cond$I0 +
        init_cond$R0 + init_cond$V0 != 1) {
      stop("Invalid initial conditions. Must add up to 1.")
    }
    
    object@initial_conditions <- init_cond
    
    object
  })

#' @describeIn SEIRDV Set transmission parameters (beta, kappa, gamma, mu, nu,
#' delta_V, delta_R) of the SEIRV model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIRDV model)
#' @param value (list) list of values for beta, kappa, gamma, mu, nu, delta_V and
#' delta_R respectively.
#'
#' @return object of class SEIRDV with transmission parameter values
#' assigned.
#' 
#' @export
setMethod(
  "transmission_parameters<-", "SEIRDV",
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
        | length(trans_params$nu) != 1
        | length(trans_params$delta_V) != 1
        | length(trans_params$delta_R) != 1) {
      stop("The parameter values should be 1-dimensional.")
    }
    
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params
    
    object
  })

# SEIRDV class specific functions

#' Retrieves intervention parameters of SEIRDV model.
#'
#' @param object An object of the class SEIRDV.
#' 
#' @export
setGeneric("intervention_parameters",
           function(object) standardGeneric("intervention_parameters"))


#' @describeIn SEIRDV Retrieves intervention parameters of SEIRDV model.
#'
#' @param object An object of the class SEIRDV.
#' 
#' @export
setMethod("intervention_parameters", "SEIRDV",
          function(object) object@intervention_parameters)

#' Set intervention parameters of the SEIRV model.
#'
#' Intervention parameters have same size.
#'
#' @param object an object of the class SEIRDV
#' @param value (list) list of intervention parameters: starts, stops and
#'              coverages.
#'
#' @return object of class SEIRDV with intervention parameters assigned.
#' 
#' @export
setGeneric(
  "intervention_parameters<-",
  function(object, value) {
    standardGeneric("intervention_parameters<-")
  })

#' @describeIn SEIRDV Setter method for intervention parameters of the SEIRV model.
#'
#' Intervention parameters have same size.
#'
#' @param object an object of the class SEIRDV
#' @param value (list) list of intervention parameters: starts, stops and
#'              coverages.
#'
#' @return object of class SEIRDV with intervention parameters assigned.
#' 
#' @export
setMethod(
  "intervention_parameters<-", "SEIRDV",
  function(object, value) {
    
    if (mean(names(value) %in% object@intervention_parameter_names) != 1)
      stop(paste0("Intervention parameters must contain: ",
                  object@intervention_parameter_names))
    interv_par <- value
    
    # raise errors if intervention parameters are not doubles
    for (p in list("starts", "stops", "coverages")) {
      if (!is.numeric(interv_par[[p]])) {
        stop(glue("{p} format must be numeric"))
      }
    }
    
    # check that the intervention parameters are all of the same size
    if (length(interv_par$starts) != length(interv_par$stops)|
        length(interv_par$starts) != length(interv_par$coverages)|
        length(interv_par$coverages) != length(interv_par$stops)) {
      stop("Invalid intervention parameters. Must have same size.")
    }
    
    object@intervention_parameters <- interv_par
    
    object
  })

#' @describeIn SEIRDV Solves ODEs of the SEIRDV specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = - beta S(t) I(t) - nu Inter(t) S(t) + delta_V V(t) + delta_R R(t)}
#' \deqn{\frac{dE(t)}{dt} =  beta S(t) I(t) - kappa E(t)}
#' \deqn{\frac{dI(t)}{dt} = kappa E(t) - (gamma + mu) I(t)}
#' \deqn{\frac{dR(t)}{dt} = gamma I(t) - delta_R R(t)}
#' \deqn{\frac{dV(t)}{dt} = nu Inter(t) S(t) - delta_V V(t)}
#' \deqn{\frac{dC(t)}{dt} = beta S(t) I(t)}
#' \deqn{\frac{dD(t)}{dt} = mu I(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIRDV
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return two dataframes: one with the time steps, age range, time series of S,
#' E, I, R and V population fractions, and one with the time steps, age range,
#' time series of incidences and deaths population fraction.
#' 
#' @export
#' 
setMethod(
  "run", "SEIRDV",
  function(object, times, solve_method = "lsoda") {
    if (!is.double(times)) {
      stop("Evaluation times of the model storage format must be a vector.")
    }
    if (is.null(unlist(object@transmission_parameters)))
      stop("Transmission parameters must be set before running.")
    if (is.null(unlist(object@initial_conditions)))
      stop("Initial conditions must be set before running.")
    if (is.null(unlist(object@intervention_parameters)))
      stop("Intervention parameters must be set before running.")
    
    # set initial state vector
    state <- c(S = initial_conditions(object)$S0,
               E = initial_conditions(object)$E0,
               I = initial_conditions(object)$I0,
               R = initial_conditions(object)$R0,
               V = initial_conditions(object)$V0,
               C = 0,
               D = 0)
    # set transmission parameters vector
    parameters <- c(b = transmission_parameters(object)$beta,
                    k = transmission_parameters(object)$kappa,
                    g = transmission_parameters(object)$gamma,
                    m = transmission_parameters(object)$mu,
                    n = transmission_parameters(object)$nu,
                    d_v = transmission_parameters(object)$delta_V,
                    d_r = transmission_parameters(object)$delta_R)
    
    # set intervention parameters vector
    int_parms <- 
      InterventionParameters(
        start=intervention_parameters(object)$starts,
        stop=intervention_parameters(object)$stops,
        coverage= intervention_parameters(object)$coverages)
    
    sim_parms <- SimulationParameters(start =0, stop = tail(times, n=1),
                                      tstep = 0.1)
    
    inter_prot <- intervention_protocol(int_parms, sim_parms, 1) %>%
      filter(time %in% times) %>%
      .$coverage
    
    intervention <- approxfun(times, inter_prot, rule=2)
    
    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters, input) {
      with(
        as.list(c(state, parameters)), {
          inter <- input(t)
          s <- state[1]
          e <- state[2]
          i <- state[3]
          r <- state[4]
          v <- state[5]
          c <- state[6]
          d <- state[7]
          # rate of change
          ds <- -b * s * i - n * inter * s + d_v * v + d_r * r
          de <- b * s * i - k * e
          di <- k * e - (g + m ) * i
          dr <- g * i - d_r * r
          dv <- n * inter * s - d_v * v
          dc <- b * s * i
          d_death <- m * i
          # return the rate of change
          list(c(ds, de, di, dr, dv, dc, d_death))
        })
    }
    
    # call ode solver
    out <- ode(
      y = state, times = times, func = right_hand_side,
      parms = parameters, method = solve_method, input = intervention)
    
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
    
    # Split output into 2 dataframes: one with S,E,I,R and V and one with C and D
    states <- subset(output, !output$compartment %in% c("Incidence", "Deaths"))
    states <- droplevels(states)
    changes <- subset(output, output$compartment %in% c("Incidence", "Deaths"))
    changes <- droplevels(changes)
    
    list("states" = states, "changes" = changes)
  })
