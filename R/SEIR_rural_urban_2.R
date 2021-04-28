#' An S4 object representing the SEIR_rural_urban_2.
#'
#' This class represents the SEIR model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time. The model
#' considers these populations in two types of communities: rural and urban.
#' This class considers the model where people DO  move between communities
#' but can only infect people in their own community
#'
#' @slot output_names2 list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names2 list of names of initial conditions
#'       (characters). Default is list("S_U0", "E_U0", "I_U0", "R_U0", "S_Y0",
#'       "E_Y0", "I_Y0", "R_Y0").
#' @slot initial_cases_deaths_names2 name for initial cases and deaths
#'       (characters). Default is list("C_U0", "D_U0", "C_Y0", "D_Y0").
#' @slot transmission_parameter_names2 list of names of transmission parameters
#'       (characters). Default is list("bu", "by", "k", "g", "m", "fsu", "fsy",
#'       "feu", "fey", "fiu", "fiy", "fru", "fry").
#' @slot initial_conditions2 list of values for initial conditions (double).
#' @slot initial_cases_deaths2 list of values for initial cases and deaths.
#'       Both set to 0, not to be changed by user (double).
#' @slot transmission_parameters2 list of values for transmission parameters
#'       (double).
#'
#' @import deSolve
#' @import glue
#' @import reshape2

setClass("SEIR_rural_urban_2",
         # slots
         slots = c(
           output_names2 = "list",
           initial_condition_names2 = "list",
           transmission_parameter_names2 = "list",
           initial_conditions2 = "list",
           transmission_parameters2 = "list",
           initial_cases_deaths_names2 = "list",
           initial_cases_deaths2 = "list"
         ),
         # prototypes for the slots, automatically set parameter names and
         # its data type
         prototype = list(
           output_names2 = list("S_U", "E_U", "I_U", "R_U",
                               "Incidences_U", "Deaths_U",
                               "S_Y", "E_Y", "I_Y", "R_Y",
                               "Incidences_Y", "Deaths_Y"),
           initial_condition_names2 = list("S_U0", "E_U0", "I_U0", "R_U0", "S_Y0",
                                          "E_Y0", "I_Y0", "R_Y0"),
           initial_cases_deaths_names2 = list("C_U0", "D_U0", "C_Y0", "D_Y0"),
           transmission_parameter_names2 = list("bu", "by", "k", "g", "m",
                                               "fsu", "fsy", "feu", "fey", "fiu",
                                               "fiy", "fru", "fry"),
           initial_conditions2 = vector(mode = "list", length = 8),
           initial_cases_deaths2 = vector(mode = "list", length = 4),
           transmission_parameters2 = vector(mode = "list", length = 13)
         )
)
#' Retrieves initial conditions of SEIR_rural_urban_2 model.
#'
#' @param object An object of the class SEIR_rural_urban_2.
#' @export

setGeneric("initial_conditions2",
           function(object) standardGeneric("initial_conditions2"))


#' @describeIn SEIR_rural_urban_2 Retrieves initial conditions of SEIR_rural_urban_2 model.
#'
#' @param object An object of the class SEIR_rural_urban_2.
#' @aliases initial_conditions2,ANY,ANY-method
#' @export

setMethod("initial_conditions2", "SEIR_rural_urban_2",
          function(object) object@initial_conditions2)

#' Retrieves initial cases and deaths of SEIR_rural_urban_2 model.
#'
#' @param object An object of the class SEIR_rural_urban_2.
#' @export

setGeneric("initial_cases_deaths2",
           function(object) standardGeneric("initial_cases_deaths2"))

#' @describeIn SEIR_rural_urban_2 Retrieves initial cases and deaths of SEIR_rural_urban_2 model.
#'
#' @param object An object of the class SEIR_rural_urban_2.
#' @aliases initial_cases_deaths2,ANY,ANY-method
#' @export

setMethod("initial_cases_deaths2", "SEIR_rural_urban_2",
          function(object) object@initial_cases_deaths2)

#' Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIR_rural_urban_2.
#' @export

setGeneric("transmission_parameters2",
           function(object) standardGeneric("transmission_parameters2"))

#' @describeIn SEIR_rural_urban_2 Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIR_rural_urban_2.
#' @aliases transmission_parameters2,ANY,ANY-method
#' @export

setMethod("transmission_parameters2", "SEIR_rural_urban_2",
          function(object) object@transmission_parameters2)

#' Setter method for initial conditions (S0, E0, I0 and R0) for both communities
#' in the SEIR model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIR_rural_urban_2
#' @param value (list) list of initial conditions S_U0, E_U0, I_U0, R_U0, S_Y0,
#' E_Y0, I_Y0, R_Y0.
#'
#' @return object of class SEIR_rural_urban_2 with initial conditions assigned.
#'
#' @export

setGeneric(
  "initial_conditions2<-",
  function(object, value) {
    standardGeneric("initial_conditions2<-")
  })

#' @describeIn SEIR_rural_urban_2 Setter method for initial conditions (S0, E0, I0 and R0)
#' for both communities in the SEIR model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIR_rural_urban_2
#' @param value (list) list of initial conditions S_U0, E_U0, I_U0, R_U0, S_Y0,
#' E_Y0, I_Y0, R_Y0.
#'
#' @return object of class SEIR_rural_urban_2 with initial conditions assigned.
#'
#' @aliases initial_conditions2<-,ANY,ANY-method
#' @export

setMethod(
  "initial_conditions2<-", "SEIR_rural_urban_2",
  function(object, value) {
    
    # create list of parameter values
    init_cond <- value
    
    # add names to each value
    names(init_cond) <- object@initial_condition_names2
    
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
    object@initial_conditions2 <- init_cond
    init_c0_d0 <- list(init_cond$E_U0 + init_cond$I_U0 + init_cond$R_U0, 0,
                       init_cond$E_Y0 + init_cond$I_Y0 + init_cond$R_Y0, 0)
    names(init_c0_d0) <- object@initial_cases_deaths_names2
    
    object@initial_cases_deaths2 <- init_c0_d0
    
    return(object)
  })

#' Setter method for transmission parameters
#' ("bu", "by", "k", "g", "m", "fsu", "fsy", "feu", "fey", "fiu", "fiy", "fru",
#' "fry") of the SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIR_rural_urban_2 model)
#' @param value (list) list of values for SEIR_rural_urban
#' respectively.
#'
#' @return object of class SEIR_rural_urban_2 with transmission parameter values
#' assigned.
#' @export

setGeneric(
  "transmission_parameters2<-",
  function(object, value) {
    standardGeneric("transmission_parameters2<-")
  })


#' @describeIn SEIR_rural_urban_2 Setter method for transmission parameters
#' ("bu", "by", "k", "g", "m", "fsu", "fsy", "feu", "fey", "fiu", "fiy", "fru",
#' "fry") of the SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIR_rural_urban_2 model)
#' @param value (list) list of values for "bu", "by", "k", "g", "m", "fsu", "fsy",
#'       "feu", "fey", "fiu", "fiy", "fru", "fry", respectively.
#'
#' @return object of class SEIR_rural_urban_2 with transmission parameter values
#' assigned.
#' @aliases transmission_parameters2<-,ANY,ANY-method
#' @export

setMethod(
  "transmission_parameters2<-", "SEIR_rural_urban_2",
  function(object, value) {
    
    # create list of parameter values
    trans_params <- value
    
    # add names to each value
    names(trans_params) <- object@transmission_parameter_names2
    
    # check format of parameters b, k and g
    if (length(trans_params$bu) != 1
        | length(trans_params$by) != 1
        | length(trans_params$k) != 1
        | length(trans_params$g) != 1
        | length(trans_params$m) != 1
        | length(trans_params$fsu) != 1
        | length(trans_params$fsy) != 1
        | length(trans_params$feu) != 1
        | length(trans_params$fey) != 1
        | length(trans_params$fiu) != 1
        | length(trans_params$fiy) != 1
        | length(trans_params$fru) != 1
        | length(trans_params$fry) != 1) {
      stop("The parameter values should be 1-dimensional.")
    }
    
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters2 <- trans_params
    
    return(object)
  })

# SEIR_rural_urban_2 class specific functions

#' Solves ODEs of the SEIR_rural_urban_2 specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#' For the urban community:
#' \deqn{\frac{dS_U(t)}{dt} = - b_U S_U(t) I_U(t) + \phi_{SY}S_Y - \phi_{SU}S_U}
#' \deqn{\frac{dE_U(t)}{dt} =  b_U S_U(t) I_U(t) - k E_U(t) + \phi_{EY}E_Y
#' - \phi_{EU}E_U}
#' \deqn{\frac{dI_U(t)}{dt} = k E_U(t) - (g + m) I_U(t) + \phi_{IY}I_Y
#' - \phi_{IU}I_U}
#' \deqn{\frac{dR_U(t)}{dt} = g I_U(t) + \phi_{RY}R_Y - \phi_{RU}R_U}
#' \deqn{\frac{dC_U(t)}{dt} = b_U S_U(t) I_U(t)}
#' \deqn{\frac{dD_U(t)}{dt} = m I_U(t)}
#' 
#' For the rural community:
#' \deqn{\frac{dS_Y(t)}{dt} = - b_Y S_Y(t) I_Y(t) + \phi_{SU}S_U - \phi_{SY}S_Y}
#' \deqn{\frac{dE_Y(t)}{dt} =  b_Y S_Y(t) I_Y(t) - k E_Y(t)  + \phi_{EU}E_U
#' - \phi_{EY}E_Y}
#' \deqn{\frac{dI_Y(t)}{dt} = k E_Y(t) - (g + m) I_Y(t) + \phi_{IU}I_U
#' - \phi_{IY}I_Y}}
#' \deqn{\frac{dR_Y(t)}{dt} = g I_Y(t) + \phi_{RU}R_U - \phi_{RY}R_Y}
#' \deqn{\frac{dC_Y(t)}{dt} = b_Y S_Y(t) I_Y(t)}
#' \deqn{\frac{dD_Y(t)}{dt} = m I_Y(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIR_rural_urban_2
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return a dataframe with the time steps, time series of S, E, I and R
#' population fractions, and incidence numbers and deaths of the SEIR_rural_urban_2 model.
#' @export

setGeneric(name = "simulate_SEIR_rural_urban_2",
           def = function(object, times = seq(0, 100, by = 1),
                          solve_method = "lsoda") {
             standardGeneric("simulate_SEIR_rural_urban_2")})

#' @describeIn SEIR_rural_urban_2 Solves ODEs of the SEIR_rural_urban_2 specified in object
#' for the time points specified in times and integration method specified in
#' solve_method.
#'
#' For the urban community:
#' \deqn{\frac{dS_U(t)}{dt} = - b_U S_U(t) I_U(t) + \phi_{SY}S_Y - \phi_{SU}S_U}
#' \deqn{\frac{dE_U(t)}{dt} =  b_U S_U(t) I_U(t) - k E_U(t) + \phi_{EY}E_Y- \phi_{EU}E_U}
#' \deqn{\frac{dI_U(t)}{dt} = k E_U(t) - (g + m) I_U(t) + \phi_{IY}I_Y - \phi_{IU}I_U}
#' \deqn{\frac{dR_U(t)}{dt} = g I_U(t) + \phi_{RY}R_Y - \phi_{RU}R_U}
#' \deqn{\frac{dC_U(t)}{dt} = b_U S_U(t) I_U(t)}
#' \deqn{\frac{dD_U(t)}{dt} = m I_U(t)}
#' 
#' For the rural community:
#' \deqn{\frac{dS_Y(t)}{dt} = - b_Y S_Y(t) I_Y(t) + \phi_{SU}S_U - \phi_{SY}S_Y}
#' \deqn{\frac{dE_Y(t)}{dt} =  b_Y S_Y(t) I_Y(t) - k E_Y(t)  + \phi_{EU}E_U - \phi_{EY}E_Y}
#' \deqn{\frac{dI_Y(t)}{dt} = k E_Y(t) - (g + m) I_Y(t) + \phi_{IU}I_U - \phi_{IY}I_Y}
#' \deqn{\frac{dR_Y(t)}{dt} = g I_Y(t) + \phi_{RU}R_U - \phi_{RY}R_Y}
#' \deqn{\frac{dC_Y(t)}{dt} = b_Y S_Y(t) I_Y(t)}
#' \deqn{\frac{dD_Y(t)}{dt} = m I_Y(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIR_rural_urban_2
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return a dataframe with the time steps, time series of S, E, I and R
#' population fractions, and incidence numbers and deaths for both communities
#' in the SEIR_rural_urban_2 model.
#' @aliases simulate_SEIR_rural_urban_2,ANY,ANY-method
#' @export

setMethod(
  "simulate_SEIR_rural_urban_2", "SEIR_rural_urban_2",
  function(object, times, solve_method = "lsoda") {
    if (!is.double(times)) {
      stop("Evaluation times of the model storage format must be a vector.")
    }
    
    # set initial state vector
    state <- c(SU = initial_conditions2(object)$S_U0,
               EU = initial_conditions2(object)$E_U0,
               IU = initial_conditions2(object)$I_U0,
               RU = initial_conditions2(object)$R_U0,
               CU = initial_cases_deaths2(object)$C_U0,
               DU = initial_cases_deaths2(object)$D_U0,
               SY = initial_conditions2(object)$S_Y0,
               EY = initial_conditions2(object)$E_Y0,
               IY = initial_conditions2(object)$I_Y0,
               RY = initial_conditions2(object)$R_Y0,
               CY = initial_cases_deaths2(object)$C_Y0,
               DY = initial_cases_deaths2(object)$D_Y0)
    # set transmission parameters vector
    parameters <- c(bu = transmission_parameters2(object)$bu,
                    by = transmission_parameters2(object)$by,
                    k = transmission_parameters2(object)$k,
                    g = transmission_parameters2(object)$g,
                    m = transmission_parameters2(object)$m,
                    fsu = transmission_parameters2(object)$fsu,
                    fsy = transmission_parameters2(object)$fsy,
                    feu = transmission_parameters2(object)$feu,
                    fey = transmission_parameters2(object)$fey,
                    fiu = transmission_parameters2(object)$fiu,
                    fiy = transmission_parameters2(object)$fiy,
                    fru = transmission_parameters2(object)$fru,
                    fry = transmission_parameters2(object)$fry)
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
          dsu <- -bu * su * iu + fsy * sy - fsu * su
          deu <- bu * su * iu - k * eu + fey * ey - feu * eu
          diu <- k * eu - (g + m) * iu + fiy * iy - fiu * iu
          dru <- g * iu + fry * ry - fru * ru
          dcu <- bu * su * iu 
          d_deathu <- m * iu
          # rate of change: rural
          dsy <- -by * sy * iy + fsu * su - fsy * sy
          dey <- by * sy * iy  - k * ey + feu * eu - fey * ey
          diy <- k * ey - (g + m) * iy + fiu * iu - fiy * iy
          dry <- g * iy + fru * ru - fry * ry
          dcy <- by * sy * iy 
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
    
    colnames(output) <- c("time", object@output_names2)
    
    # Create long format of output
    output <- melt(output, id.vars = "time")
    names(output) <- c("time", "compartment", "value")
    
    # Added for consistency of output format across models
    output$age_group <- rep("0-150", length(output$time))
    
    return(output)
  })
