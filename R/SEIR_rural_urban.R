#' An S4 object representing the SEIR_rural_urban.
#'
#' This class represents the SEIR model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time. The model
#' considers these populations in two types of communities: rural and urban.
#' This class considers the model where people DO NOT move between communities
#' but can infect people from the other community.
#'
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S_U0", "E_U0", "I_U0", "R_U0", "S_Y0",
#'       "E_Y0", "I_Y0", "R_Y0").
#' @slot initial_cases_deaths_names name for initial cases and deaths
#'       (characters). Default is list("C_U0", "D_U0", "C_Y0", "D_Y0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("bu", "by", "buy", "byu", "k", "g", "m").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot initial_cases_deaths list of values for initial cases and deaths.
#'       Both set to 0, not to be changed by user (double).
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#'
#' @import deSolve
#' @import glue
#' @import reshape2

setClass("SEIR_rural_urban",
         # slots
         slots = c(
           output_names = "list",
           initial_condition_names = "list",
           transmission_parameter_names = "list",
           initial_conditions = "list",
           transmission_parameters = "list",
           initial_cases_deaths_names = "list",
           initial_cases_deaths = "list"
         ),
         # prototypes for the slots, automatically set parameter names and
         # its data type
         prototype = list(
           output_names = list("S_U", "E_U", "I_U", "R_U",
                               "Incidences_U", "Deaths_U",
                               "S_Y", "E_Y", "I_Y", "R_Y",
                               "Incidences_Y", "Deaths_Y"),
           initial_condition_names = list("S_U0", "E_U0", "I_U0", "R_U0", "S_Y0",
           "E_Y0", "I_Y0", "R_Y0"),
           initial_cases_deaths_names = list("C_U0", "D_U0", "C_Y0", "D_Y0"),
           transmission_parameter_names = list("bu", "by", "buy", "byu", "k",
                                               "g", "m"),
           initial_conditions = vector(mode = "list", length = 8),
           initial_cases_deaths = vector(mode = "list", length = 4),
           transmission_parameters = vector(mode = "list", length = 7)
         )
)
#' Retrieves initial conditions of SEIR_rural_urban model.
#'
#' @param object An object of the class SEIR_rural_urban.
#' @export

setGeneric("initial_conditions",
           function(object) standardGeneric("initial_conditions"))


#' @describeIn SEIR_rural_urban Retrieves initial conditions of SEIR_rural_urban model.
#'
#' @param object An object of the class SEIR_rural_urban.
#' @aliases initial_conditions,ANY,ANY-method
#' @export

setMethod("initial_conditions", "SEIR_rural_urban",
          function(object) object@initial_conditions)

#' Retrieves initial cases and deaths of SEIR_rural_urban model.
#'
#' @param object An object of the class SEIR_rural_urban.
#' @export

setGeneric("initial_cases_deaths",
           function(object) standardGeneric("initial_cases_deaths"))

#' @describeIn SEIR_rural_urban Retrieves initial cases and deaths of SEIR_rural_urban model.
#'
#' @param object An object of the class SEIR_rural_urban.
#' @aliases initial_cases_deaths,ANY,ANY-method
#' @export

setMethod("initial_cases_deaths", "SEIR_rural_urban",
          function(object) object@initial_cases_deaths)

#' Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIR_rural_urban.
#' @export

setGeneric("transmission_parameters",
           function(object) standardGeneric("transmission_parameters"))

#' @describeIn SEIR_rural_urban Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIR_rural_urban.
#' @aliases transmission_parameters,ANY,ANY-method
#' @export

setMethod("transmission_parameters", "SEIR_rural_urban",
          function(object) object@transmission_parameters)

#' Setter method for initial conditions (S0, E0, I0 and R0) for both communities
#' in the SEIR model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIR_rural_urban
#' @param value (list) list of initial conditions S_U0, E_U0, I_U0, R_U0, S_Y0,
#' E_Y0, I_Y0, R_Y0.
#'
#' @return object of class SEIR_rural_urban with initial conditions assigned.
#'
#' @export

setGeneric(
  "initial_conditions<-",
  function(object, value) {
    standardGeneric("initial_conditions<-")
  })

#' @describeIn SEIR_rural_urban Setter method for initial conditions (S0, E0, I0 and R0)
#' for both communities in the SEIR model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIR_rural_urban
#' @param value (list) list of initial conditions S_U0, E_U0, I_U0, R_U0, S_Y0,
#' E_Y0, I_Y0, R_Y0.
#'
#' @return object of class SEIR_rural_urban with initial conditions assigned.
#'
#' @aliases initial_conditions<-,ANY,ANY-method
#' @export

setMethod(
  "initial_conditions<-", "SEIR_rural_urban",
  function(object, value) {
    
    # create list of parameter values
    init_cond <- value
    
    # add names to each value
    names(init_cond) <- object@initial_condition_names
    
    # raise errors if age category dimensions do not match initial state vectors
    # also raise errors if initial state and parameter values are not doubles
    for (p in list("S_U0", "E_U0", "I_U0", "R_U0",
                   "S_Y0", "E_Y0", "I_Y0", "R_Y0")) {
      if (!is.numeric(init_cond[[p]])) {
        stop(glue("{p} format must be numeric"))
      }
    }
    
    # check that the initial conditions are properly normalized
    if (init_cond$S_U0 + init_cond$E_U0 + init_cond$I_U0 + init_cond$R_U0 +
        init_cond$S_Y0 + init_cond$E_Y0 + init_cond$I_Y0 + init_cond$R_Y0 != 1) {
      stop("Invalid initial conditions. Must add up to 1.")
    }
    
    # if all above tests are passed, assign the init_cond namelist to the object
    # and assign initial cases and deaths
    object@initial_conditions <- init_cond
    init_c0_d0 <- list(init_cond$E_U0 + init_cond$I_U0 + init_cond$R_U0, 0,
                       init_cond$E_Y0 + init_cond$I_Y0 + init_cond$R_Y0, 0)
    names(init_c0_d0) <- object@initial_cases_deaths_names
    
    object@initial_cases_deaths <- init_c0_d0
    
    return(object)
  })

#' Setter method for transmission parameters
#' (bu, by, buy, byu, k, g and m) of the SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIR_rural_urban model)
#' @param value (list) list of values for bu, by, buy, byu, k, g, m,
#' respectively.
#'
#' @return object of class SEIR_rural_urban with transmission parameter values
#' assigned.
#' @export

setGeneric(
  "transmission_parameters<-",
  function(object, value) {
    standardGeneric("transmission_parameters<-")
  })


#' @describeIn SEIR_rural_urban Setter method for transmission parameters
#' (bu, by, buy, byu, k, g and m) of the SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIR_rural_urban model)
#' @param value (list) list of values for  bu, by, buy, byu, k, g, m,
#' respectively.
#'
#' @return object of class SEIR_rural_urban with transmission parameter values
#' assigned.
#' @aliases transmission_parameters<-,ANY,ANY-method
#' @export

setMethod(
  "transmission_parameters<-", "SEIR_rural_urban",
  function(object, value) {
    
    # create list of parameter values
    trans_params <- value
    
    # add names to each value
    names(trans_params) <- object@transmission_parameter_names
    
    # check format of parameters b, k and g
    if (length(trans_params$bu) != 1
        | length(trans_params$by) != 1
        | length(trans_params$buy) != 1
        | length(trans_params$byu) != 1
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

# SEIR_rural_urban class specific functions

#' Solves ODEs of the SEIR_rural_urban specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#' For the urban community:
#' \deqn{\frac{dS_U(t)}{dt} = - b_U S_U(t) I_U(t) - b_{UY} S_U I_U}
#' \deqn{\frac{dE_U(t)}{dt} =  b_U S_U(t) I_U(t) + b_{UY} S_U I_U - k E_U(t)}
#' \deqn{\frac{dI_U(t)}{dt} = k E_U(t) - (g + m) I_U(t)}
#' \deqn{\frac{dR_U(t)}{dt} = g I_U(t)}
#' \deqn{\frac{dC_U(t)}{dt} = b_U S_U(t) I_U(t) + b_{UY} S_U I_U}
#' \deqn{\frac{dD_U(t)}{dt} = m I_U(t)}
#' 
#' For the rural community:
#' \deqn{\frac{dS_Y(t)}{dt} = - b_Y S_Y(t) I_Y(t) - b_{YU} S_Y I_Y}
#' \deqn{\frac{dE_Y(t)}{dt} =  b_Y S_Y(t) I_Y(t) + b_{YU} S_Y I_Y - k E_Y(t)}
#' \deqn{\frac{dI_Y(t)}{dt} = k E_Y(t) - (g + m) I_Y(t)}
#' \deqn{\frac{dR_Y(t)}{dt} = g I_Y(t)}
#' \deqn{\frac{dC_Y(t)}{dt} = b_Y S_Y(t) I_Y(t) + b_{YU} S_Y I_Y}
#' \deqn{\frac{dD_Y(t)}{dt} = m I_Y(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIR_rural_urban
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return a dataframe with the time steps, time series of S, E, I and R
#' population fractions, and incidence numbers and deaths of the SEIR_rural_urban model.
#' @export

setGeneric(name = "simulate_SEIR_rural_urban",
           def = function(object, times = seq(0, 100, by = 1),
                          solve_method = "lsoda") {
             standardGeneric("simulate_SEIR_rural_urban")})

#' @describeIn SEIR_rural_urban Solves ODEs of the SEIR_rural_urban specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' For the urban community:
#' \deqn{\frac{dS_U(t)}{dt} = - b_U S_U(t) I_U(t) - b_{UY} S_U I_U}
#' \deqn{\frac{dE_U(t)}{dt} =  b_U S_U(t) I_U(t) + b_{UY} S_U I_U - k E_U(t)}
#' \deqn{\frac{dI_U(t)}{dt} = k E_U(t) - (g + m) I_U(t)}
#' \deqn{\frac{dR_U(t)}{dt} = g I_U(t)}
#' \deqn{\frac{dC_U(t)}{dt} = b_U S_U(t) I_U(t) + b_{UY} S_U I_U}
#' \deqn{\frac{dD_U(t)}{dt} = m I_U(t)}
#' 
#' For the rural community:
#' \deqn{\frac{dS_Y(t)}{dt} = - b_Y S_Y(t) I_Y(t) - b_{YU} S_Y I_Y}
#' \deqn{\frac{dE_Y(t)}{dt} =  b_Y S_Y(t) I_Y(t) + b_{YU} S_Y I_Y - k E_Y(t)}
#' \deqn{\frac{dI_Y(t)}{dt} = k E_Y(t) - (g + m) I_Y(t)}
#' \deqn{\frac{dR_Y(t)}{dt} = g I_Y(t)}
#' \deqn{\frac{dC_Y(t)}{dt} = b_Y S_Y(t) I_Y(t) + b_{YU} S_Y I_Y}
#' \deqn{\frac{dD_Y(t)}{dt} = m I_Y(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIR_rural_urban
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return a dataframe with the time steps, time series of S, E, I and R
#' population fractions, and incidence numbers and deaths for both communities
#' in the SEIR_rural_urban model.
#' @aliases simulate_SEIR_rural_urban,ANY,ANY-method
#' @export

setMethod(
  "simulate_SEIR_rural_urban", "SEIR_rural_urban",
  function(object, times, solve_method = "lsoda") {
    if (!is.double(times)) {
      stop("Evaluation times of the model storage format must be a vector.")
    }
    
    # set initial state vector
    state <- c(SU = initial_conditions(object)$S_U0,
               EU = initial_conditions(object)$E_U0,
               IU = initial_conditions(object)$I_U0,
               RU = initial_conditions(object)$R_U0,
               CU = initial_cases_deaths(object)$C_U0,
               DU = initial_cases_deaths(object)$D_U0,
               SY = initial_conditions(object)$S_Y0,
               EY = initial_conditions(object)$E_Y0,
               IY = initial_conditions(object)$I_Y0,
               RY = initial_conditions(object)$R_Y0,
               CY = initial_cases_deaths(object)$C_Y0,
               DY = initial_cases_deaths(object)$D_Y0)
    # set transmission parameters vector
    parameters <- c(bu = transmission_parameters(object)$bu,
                    by = transmission_parameters(object)$by,
                    buy = transmission_parameters(object)$buy,
                    byu = transmission_parameters(object)$byu,
                    k = transmission_parameters(object)$k,
                    g = transmission_parameters(object)$g,
                    m = transmission_parameters(object)$m)
    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters) {
      with(
        as.list(c(state, parameters)), {
          su <- state[1]
          eu <- state[2]
          iu <- state[3]
          ru <- state[4]
          cu <- state[5]
          du <- state[6]
          sy <- state[7]
          ey <- state[8]
          iy <- state[9]
          ry <- state[10]
          cy <- state[11]
          dy <- state[12]
          # rate of change: urban
          dsu <- -bu * su * iu - buy * su *iy
          deu <- bu * su * iu + buy * su *iy - k * eu
          diu <- k * eu - (g + m) * iu
          dru <- g * iu
          dcu <- bu * su * iu + buy * su *iy
          d_deathu <- m * iu
          # rate of change: rural
          dsy <- -by * sy * iy - byu * sy *iu
          dey <- by * sy * iy + byu * sy *iu - k * ey
          diy <- k * ey - (g + m) * iy
          dry <- g * iy
          dcy <- by * sy * iy + byu * sy *iu
          d_deathy <- m * iy
          # return the rate of change
          list(c(dsu, deu, diu, dru, dcu, d_deathu,
                 dsy, dey, diy, dry, dcy, d_deathy))
        })
    }
    
    # call ode solver
    out <- ode(
      y = state, times = times, func = right_hand_side,
      parms = parameters, method = solve_method)
    
    output <- as.data.frame.array(out)
    
    # Compute incidences and deaths: urban
    output$CU[2:length(output$CU)] <- output$CU[
      2:length(output$CU)] - output$CU[1:(length(output$CU) - 1)]
    output$CU[1] <- 0
    output$DU[2:length(output$DU)] <- output$DU[
      2:length(output$DU)] - output$DU[1:(length(output$DU) - 1)]
    
    # Compute incidences and deaths: rural
    output$CY[2:length(output$CY)] <- output$CY[
      2:length(output$CY)] - output$CY[1:(length(output$CY) - 1)]
    output$CY[1] <- 0
    output$DY[2:length(output$DY)] <- output$DY[
      2:length(output$DY)] - output$DY[1:(length(output$DY) - 1)]
    
    colnames(output) <- c("time", object@output_names)
    
    # Create long format of output
    output <- melt(output, id.vars = "time")
    names(output) <- c("time", "compartment", "value")
    
    # Added for consistency of output format across models
    output$age_group <- rep("0-150", length(output$time))
    
    return(output)
  })
