#' @include generics.R
#'
NULL

#' An S4 object representing the SEIRD_CT.
#'
#' This class represents the SEIRD_CT model, which provides a model of contact tracing.
#'
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "P0", "A0", "I0", "Et0", "Pt0", "At0", "It0", "R0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("beta", "beta_a", "gamma", "mu", "pi", "omega", "eta_a", "psi", "phi").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#'
#' @import deSolve
#' @import glue
#' @import reshape2
#'
#' @export SEIRD_CT
SEIRD_CT <- setClass("SEIRD_CT",
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
           output_names = list("S", "E", "P", "A", "I", "Et", "Pt", "At", "It", "R", "D", "Incidence", "Deaths"),
           initial_condition_names = list("S0", "E0", "P0", "A0", "I0", "Et0", "Pt0", "At0", "It0", "R0"),
           transmission_parameter_names = list("beta", "beta_a", "gamma", "mu", "pi", "omega", "eta_a", "psi", "phi"),
           initial_conditions = vector(mode = "list", length = 10),
           transmission_parameters = vector(mode = "list", length = 9)
         )
)

#' @describeIn SEIRD_CT Retrieves initial conditions of SEIRD_CT model.
#'
#' @param object An object of the class SEIRD_CT.
#' 
#' @export
setMethod("initial_conditions", "SEIRD_CT",
          function(object) object@initial_conditions)


#' @describeIn SEIRD_CT Retrieves transmission parameters of SEIRD_CT model.
#'
#' @param object An object of the class SEIRD_CT.
#' 
#' @export
setMethod("transmission_parameters", "SEIRD_CT",
          function(object) object@transmission_parameters)

# SEIRD_CT class specific functions

#' @describeIn SEIRD_CT Setter method for initial conditions (S0, E0, I0 and R0)
#' of the SEIRD_CT model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIRD_CT
#' @param value (list) list of initial conditions S0, E0, I0, R0.
#'
#' @return object of class SEIRD_CT with initial conditions assigned.
#' 
#' @export
setMethod(
  "initial_conditions<-", "SEIRD_CT",
  function(object, value) {

    if (mean(names(value) %in% object@initial_condition_names) != 1)
      stop(paste0("Initial conditions must contain: ",
                  object@initial_condition_names))
    init_cond <- value

    # raise errors if age category dimensions do not match initial state vectors
    # also raise errors if initial state and parameter values are not doubles
    for (p in list("S0", "E0", "P0", "A0", "I0", "Et0", "Pt0", "At0", "It0", "R0")) {
      if (!is.numeric(init_cond[[p]])) {
        stop(glue("{p} format must be numeric"))
        }
    }

    # check that the initial conditions are properly normalized
    if (init_cond$S0 + init_cond$E0 + init_cond$P0 + init_cond$A0 + init_cond$I0 +
        init_cond$Et0 + + init_cond$Pt0 + init_cond$At0 + init_cond$It0 +
        init_cond$R0 != 1) {
      stop("Invalid initial conditions. Must add up to 1.")
    }

    object@initial_conditions <- init_cond

    object
  })


#' @describeIn SEIRD_CT Set transmission parameters (beta, kappa, gamma and mu)
#' of the SEIRD_CT model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIRD_CT model)
#' @param value (list) list of values for beta, kappa, gamma, mu, respectively.
#'
#' @return object of class SEIRD_CT with transmission parameter values
#' assigned.
#' 
#' @export
setMethod(
  "transmission_parameters<-", "SEIRD_CT",
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


#' @describeIn SEIRD_CT Solves ODEs of the SEIRD_CT specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' \deqn{\frac{dS(t)}{dt} = - (beta (P(t) + I(t)) + beta_a A(t)) S(t)}
#' \deqn{\frac{dE(t)}{dt} = beta (1 - pi) (P(t) + I(t)) S(t) + beta_a A(t) S(t) - omega E(t)}
#' \deqn{\frac{dP(t)}{dt} = (1 - eta_a) omega E(t) - psi P(t)}
#' \deqn{\frac{dA(t)}{dt} = eta_a omega E(t) - gamma A(t)}
#' \deqn{\frac{dI(t)}{dt} = (1 - phi) psi P(t) - (gamma + mu) I(t)}
#' \deqn{\frac{dEt(t)}{dt} = beta pi (P(t) + I(t)) S(t) - omega E(t)}
#' \deqn{\frac{dPt(t)}{dt} = (1 - eta_a) omega Et(t) - psi Pt(t)}
#' \deqn{\frac{dAt(t)}{dt} = eta_a omega Et(t) - gamma At(t)}
#' \deqn{\frac{dIt(t)}{dt} = psi Pt(t) - phi psi P(t) - (gamma + mu) It(t)}
#' \deqn{\frac{dR(t)}{dt} = gamma (I(t) + It(t) + A(t) + At(t))}
#' \deqn{\frac{dD(t)}{dt} = (gamma + mu)(I(t) + It(t))}
#' \deqn{\frac{dC(t)}{dt} = (beta (P(t) + I(t)) + beta_a A(t)) S(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIRD_CT
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is lsoda which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return two dataframes: one with the time steps, age range, time series of S,
#' E, I and R population fractions, and one with the time steps, age range,
#' time series of incidences and deaths population fraction.
#' 
#' @export
setMethod(
  "run", "SEIRD_CT",
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
               P = initial_conditions(object)$P0,
               A = initial_conditions(object)$A0,
               I = initial_conditions(object)$I0,
               Et = initial_conditions(object)$Et0,
               Pt = initial_conditions(object)$Pt0,
               At = initial_conditions(object)$At0,
               It = initial_conditions(object)$It0,
               R = initial_conditions(object)$R0,
               C = 0,
               D = 0)
    # set transmission parameters vector
    parameters <- c(beta_a = transmission_parameters(object)$beta_a,
                    beta = transmission_parameters(object)$beta,
                    gamma = transmission_parameters(object)$gamma,
                    mu = transmission_parameters(object)$mu,
                    pi=transmission_parameters(object)$pi,
                    omega=transmission_parameters(object)$omega,
                    eta_a=transmission_parameters(object)$eta_a,
                    psi=transmission_parameters(object)$psi,
                    phi=transmission_parameters(object)$phi)

    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters) {
      with(
        as.list(c(state, parameters)), {
          s <- state[1]
          e <- state[2]
          p <- state[3]
          a <- state[4]
          i <- state[5]
          e_t <- state[6]
          p_t <- state[7]
          a_t <- state[8]
          i_t <- state[9]
          r <- state[10]
          c <- state[11]
          d <- state[12]

          # rate of change
          # untraced
          ds <- -(beta * (p + i) + beta_a * a) * s
          de <- beta * (1 - pi) * (p + i) * s + beta_a * a * s - omega * e
          dp <- (1 - eta_a) * omega * e - psi * p
          da <- eta_a * omega * e - gamma * a
          di <- (1 - phi) * psi * p - (gamma + mu) * i
          
          # traced
          de_t <- beta * pi * (p + i) * s - omega * e_t
          dp_t <- (1 - eta_a) * omega * e_t - psi * p_t
          da_t <- eta_a * omega * e_t - gamma * a_t
          di_t <- psi * p_t + phi * psi * p - (gamma + mu) * i_t
          
          # other
          dr <- gamma * (i + i_t + a + a_t)
          dc <- (beta * (p + i) + beta_a * a) * s
          d_death <- (gamma + mu) * (i + i_t)

          # return the rate of change
          list(c(ds, de, dp, da, di, de_t, dp_t, da_t, di_t, dr, dc, d_death))
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

    # Split output into 2 dataframes: one with S,E,I, and R and one with C and D
    states <- subset(output, !output$compartment %in% c("Incidence", "Deaths"))
    states <- droplevels(states)
    changes <- subset(output, output$compartment %in% c("Incidence", "Deaths"))
    changes <- droplevels(changes)

    list("states" = states, "changes" = changes)
  })

#' @describeIn SEIRD_CT Calculates basic reproduction number for SEIRD_CT model
#'
#' The R0 parameter is given by:
#' \deqn{R_0 = \beta/(\gamma + \mu)}
#'
#' @param model an SEIRD_CT model
#'
#' @return an R0 value
#' 
#' @export
setMethod("R0", "SEIRD_CT", function(model) {
  beta_a = transmission_parameters(object)$beta_a
  beta = transmission_parameters(object)$beta
  kappa = transmission_parameters(object)$kappa
  gamma = transmission_parameters(object)$gamma
  mu = transmission_parameters(object)$mu
  pi=transmission_parameters(object)$pi
  omega=transmission_parameters(object)$omega
  eta_a=transmission_parameters(object)$eta_a
  psi=transmission_parameters(object)$psi
  phi=transmission_parameters(object)$phi
  
  infections_asymptomatics <- beta_a * eta_a / gamma
  infections_presymptomatics <- beta / psi
  infections_infectious <- (1 - phi) * beta / (gamma + mu)
  
  infections_asymptomatics + (1 - pi) * (1 - eta_a) * (
    infections_presymptomatics + infections_infectious
  )
})
