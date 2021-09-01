#' An S4 object representing the SEIRD_RU.
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
#'       (characters). Default is list("b", "k", "g", "m", "C").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot initial_cases_deaths list of values for initial cases and deaths.
#'       Both set to 0, not to be changed by user (double).
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#' @slot contact_matrices list of two contact matrices, one for urban communties
#'       and one for rural communities, normalized to population size (double).
#' @slot contact_matrices_names list of names for two contact matrices,
#'       one for urban communities and one for rural communities
#' @slot country_demog list of vectors with percentage of population in each age
#'       group in urban and rural community.
#' @slot country_demog_names list of names for urban and rural demographic data.
#'
#' @import deSolve
#' @import glue
#' @import reshape2
#' @importFrom methods new
#' @export SEIRD_RU

SEIRD_RU <- setClass("SEIRD_RU",
         # slots
         slots = c(
           output_names = "list",
           initial_condition_names = "list",
           transmission_parameter_names = "list",
           initial_conditions = "list",
           transmission_parameters = "list",
           initial_cases_deaths_names = "list",
           initial_cases_deaths = "list",
           contact_matrices = "list",
           contact_matrices_names = "list",
           country_demog = "list",
           country_demog_names = "list"
         ),
         # prototypes for the slots, automatically set parameter names and
         # its data type
         prototype = list(
           output_names = list("S_U", "E_U", "I_U", "R_U",
                               "Incidences_U", "Deaths_U",
                               "S_Y", "E_Y", "I_Y", "R_Y",
                               "Incidences_Y", "Deaths_Y"),
           initial_condition_names = list("S_U0", "E_U0", "I_U0", "R_U0",
                                          "S_Y0", "E_Y0", "I_Y0", "R_Y0"),
           initial_cases_deaths_names = list("C_U0", "D_U0", "C_Y0", "D_Y0"),
           transmission_parameter_names = list("b", "k", "g", "m", "C"),
           initial_conditions = vector(mode = "list", length = 8),
           initial_cases_deaths = vector(mode = "list", length = 4),
           transmission_parameters = vector(mode = "list", length = 5),
           contact_matrices = vector(mode = "list", length = 2),
           contact_matrices_names = list("urban", "rural"),
           country_demog = list("urban", "rural"),
           country_demog_names = list("urban", "rural")
         )
)

#-----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Retrieves initial conditions of
#'             SEIRD_RU model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("initial_conditions", "SEIRD_RU",
          function(object) object@initial_conditions)

#-----------------------------------------------------------------------------
#' Retrieves initial cases and deaths of SEIRD_RU model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setGeneric("initial_cases_deaths",
           function(object) standardGeneric("initial_cases_deaths"))

#' @describeIn SEIRD_RU Retrieves initial cases and deaths of
#'             SEIRD_RU model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("initial_cases_deaths", "SEIRD_RU",
          function(object) object@initial_cases_deaths)

#-----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("transmission_parameters", "SEIRD_RU",
          function(object) object@transmission_parameters)

#-----------------------------------------------------------------------------
#' Retrieves contact_matrices of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setGeneric("contact_matrices",
           function(object) standardGeneric("contact_matrices"))

#' @describeIn SEIRD_RU Retrieves contact matrices of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("contact_matrices", "SEIRD_RU",
          function(object) object@contact_matrices)

#-----------------------------------------------------------------------------
#' Retrieves demographic data of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setGeneric("country_demog",
           function(object) standardGeneric("country_demog"))

#' @describeIn SEIRD_RU Retrieves demographic data of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("country_demog", "SEIRD_RU",
          function(object) object@country_demog)

#-----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Setter method for initial conditions
#' (S0, E0, I0 and R0) for both communities in the SEIR model.
#'
#' All initial conditions must sum up to 1.
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#'
#' @param object an object of the class SEIRD_RU
#' @param value (list) list of initial conditions S_U0, E_U0, I_U0, R_U0, S_Y0,
#' E_Y0, I_Y0, R_Y0.
#'
#' @return object of class SEIRD_RU with initial conditions assigned.
#'
#' @export

setMethod(
  "initial_conditions<-", "SEIRD_RU",
  function(object, value) {

    # create list of parameter values
    init_cond <- value

    # add names to each value
    names(init_cond) <- object@initial_condition_names

    # raise errors if initial state and parameter values are not doubles
    for (p in list("S_U0", "E_U0", "I_U0", "R_U0",
                   "S_Y0", "E_Y0", "I_Y0", "R_Y0")) {
      if (!is.numeric(init_cond[[p]])) {
        stop(glue("{p} format must be numeric"))
      }
    }

    # raise errors if initial states are not one value each
    for (p in list("S_U0", "E_U0", "I_U0", "R_U0",
                   "S_Y0", "E_Y0", "I_Y0", "R_Y0")) {
      if (length(init_cond[[p]]) != 1) {
        stop(glue("{p} must be one value"))
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

#-----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Setter method for transmission parameters
#' (b, k, g, m and C) of the SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of values for b, k, g, m, C,respectively.
#'
#' @return object of class SEIRD_RU with transmission parameter values
#' assigned.
#' @export

setMethod(
  "transmission_parameters<-", "SEIRD_RU",
  function(object, value) {

    # create list of parameter values
    trans_params <- value

    # add names to each value
    names(trans_params) <- object@transmission_parameter_names

    # check format of parameters b, k, g, m, C
    if (length(trans_params$b) != 1
        | length(trans_params$k) != 1
        | length(trans_params$g) != 1
        | length(trans_params$m) != 1
        | length(trans_params$C) != 1) {
      stop("The parameter values should be 1-dimensional.")
    }

    # check that the connectedness parameter C is between 0 and 1
    if (trans_params$C < 0 | trans_params$C > 1) {
      stop("Connectedness parameter C must be in the range 0-1.")
    }
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params

    return(object)
  })

#-----------------------------------------------------------------------------
#' Setter method for contact matrices for urban and rural communities in the
#' SEIR model. Matrices must be of type double.
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of contact matrices
#'
#' @return object of class SEIRD_RU with contact matrices
#' assigned.
#' @export

setGeneric(
  "contact_matrices<-",
  function(object, value) {
    standardGeneric("contact_matrices<-")
  })

#' @describeIn SEIRD_RU Setter method for contact matrices for
#' urban and rural communities in the SEIR model.
#' Matrices must be of type double
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of contact matrices
#'
#' @return object of class SEIRD_RU with contact matrices
#' assigned.
#' @export

setMethod(
  "contact_matrices<-", "SEIRD_RU",
  function(object, value) {

    # create list of parameter values
    contact_mat <- value

    # add names to each value
    names(contact_mat) <- object@contact_matrices_names

    # check format of contact matrices
    if (typeof(contact_mat$urban) != "double"
        | typeof(contact_mat$rural) != "double") {
      stop("Contact matrices must be of type double.")
    }

    # if all above tests are passed, assign the contact_mat namelist to the
    # object
    object@contact_matrices <- contact_mat

    return(object)
  })

#-----------------------------------------------------------------------------
#' Setter method for demographic data for urban and rural communities in the
#' SEIR model. both vectors together must sum to 1.
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of 2 sets of population fractions by age group
#'
#' @return object of class SEIRD_RU with  contact matrices
#' assigned.
#' @export

setGeneric(
  "country_demog<-",
  function(object, value) {
    standardGeneric("country_demog<-")
  })

#' @describeIn SEIRD_RU Setter method for demographic data for urban
#' and rural communities in the SEIR model. both vectors together must sum to 1.
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of 2 set of population fractions by age group
#'
#' @return object of class SEIRD_RU with demographic data
#' assigned.
#' @export

setMethod(
  "country_demog<-", "SEIRD_RU",
  function(object, value) {

    # create list of parameter values
    demo_data <- value

    # add names to each value
    names(demo_data) <- object@country_demog_names

    # check format of contact matrices
    if (typeof(demo_data$urban) != "double"
        | typeof(demo_data$rural) != "double") {
      stop("Contact matrices must be of type double.")
    }

    # check that vectors are normalized: loosing some precision in division
    # so want to make sure it is accurate to 0.1% of the population
    if (1 - sum(demo_data$urban) - sum(demo_data$rural) > 0.001) {
      stop("Sum over all age groups and both communities must be 1. The
           difference between 1 and the sum over age groups and over both
           communities is more than 0.001.")
    }

    # if all above tests are passed, assign the contact_mat namelist to the
    # object
    object@country_demog <- demo_data

    return(object)
  })

#-----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Solves ODEs of the SEIRD_RU specified
#' in object for the time points specified in times and integration method
#' specified in solve_method.
#'
#' For the urban community:
#' \deqn{\frac{dS_U(t)}{dt} = - b S_U (I_U + I_Y) N_U C
#'                            - b S_U/f_urban I_U/f_urban N_U (1-C)}
#' \deqn{\frac{dE_U(t)}{dt} =  b S_U (I_U + I_Y) N_U C
#'                            + b S_U/f_urban I_U/f_urban N_U (1-C) -k E_U}
#' \deqn{\frac{dI_U(t)}{dt} = k E_U - (g + m) I_U}
#' \deqn{\frac{dR_U(t)}{dt} = g I_U}
#' \deqn{\frac{dC_U(t)}{dt} = b S_U (I_U + I_Y) N_U C
#'                            + b S_U/f_urban I_U/f_urban N_U (1-C)}
#' \deqn{\frac{dD_U(t)}{dt} = m I_U}
#'
#' For the rural community:
#' \deqn{\frac{dS_Y(t)}{dt} = - b S_Y (I_U + I_Y) N_Y C
#'                            - b S_Y/f_rural I_Y/f_rural N_Y (1-C)}
#' \deqn{\frac{dE_Y(t)}{dt} =   b S_Y (I_U + I_Y) N_Y C
#'                            + b S_Y/f_rural I_Y/f_rural N_Y (1-C) - k E_Y}
#' \deqn{\frac{dI_Y(t)}{dt} = k E_Y - (g + m) I_Y}
#' \deqn{\frac{dR_Y(t)}{dt} = g I_Y}
#' \deqn{\frac{dC_Y(t)}{dt} =  b S_Y (I_U + I_Y) N_Y C
#'                            + b S_Y/f_rural I_Y/f_rural N_Y (1-C)}
#' \deqn{\frac{dD_Y(t)}{dt} = m I_Y}
#'
#' This function relies on the package deSolve.
#'
#' @param object an object of the class SEIRD_RU
#' @param times (double) a sequence of time points at which the solution to
#' the system of ODEs should be returned. Must be of the form
#' seq(t_start, t_end, by=t_step). Default time series is seq(0, 100, by = 1).
#' @param solve_method (string) a string of chosen numerical integration method
#' for solving the ode system. Default is "lsoda" which is also the default for
#' the ode function in the deSolve package used in this function.
#'
#' @return a dataframe with the time steps, time series of S, E, I and R
#' population fractions, and incidence numbers and deaths for both communities
#' in the SEIRD_RU model.
#' @export

setMethod(
  "run", "SEIRD_RU",
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
    # compute contacts N_U and N_Y from contact matrices
    # number of urban contacts for an urban individual, normalized over the
    # whole population through the age demographics
    N_U <- sum(rowSums(contact_matrices(object)$urban) *
                (country_demog(object)$urban / sum(country_demog(object)$urban))) / 2
    # number of rural contacts for a rural individual, normalized over the
    # whole population through the age demographics
    N_Y <- sum(rowSums(contact_matrices(object)$rural) *
                (country_demog(object)$rural / sum(country_demog(object)$rural))) / 2

    # fraction of the population that is urban
    f_urban <- sum(country_demog(object)$urban)
    #fraction of the population that is rural
    f_rural <- sum(country_demog(object)$rural)

    # set transmission parameters vector
    parameters <- c(b = transmission_parameters(object)$b,
                    k = transmission_parameters(object)$k,
                    g = transmission_parameters(object)$g,
                    m = transmission_parameters(object)$m,
                    C = transmission_parameters(object)$C,
                    N_U = N_U,
                    N_Y = N_Y,
                    f_urban = f_urban,
                    f_rural = f_rural
                    )
    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters) {
      with(as.list(c(state, parameters)), {
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
          dsu <- - (b * su * (iu + iy) * (f_urban * N_U + f_rural * N_Y) * C +
                    b * su * (iu / f_urban) * N_U * (1 - C))
          deu <- b * su * (iu + iy) * (f_urban * N_U + f_rural * N_Y) * C +
                 b * su * (iu / f_urban) * N_U * (1 - C) - k * eu
          diu <- k * eu - (g + m) * iu
          dru <- g * iu
          dcu <- b * su * (iu + iy) * (f_urban * N_U + f_rural * N_Y) * C +
                 b * su * (iu / f_urban) * N_U * (1 - C)
          d_deathu <- m * iu
          # rate of change: rural
          dsy <- - (b * sy * (iu + iy) * (f_urban * N_U + f_rural * N_Y) * C +
                    b * sy * (iy / f_rural) * N_Y * (1 - C))
          dey <- b * sy * (iu + iy) * (f_urban * N_U + f_rural * N_Y) * C +
                 b * sy * (iy / f_rural) * N_Y * (1 - C) - k * ey
          diy <- k * ey - (g + m) * iy
          dry <- g * iy
          dcy <- b * sy * (iu + iy) * (f_urban * N_U + f_rural * N_Y) * C +
                 b * sy * (iy / f_rural) * N_Y * (1 - C)
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

#----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Returns the value of R0 for the model with the specified
#' parameter values.
#' R0 is computed using the next generation matrix method.
#'
#' @param model a model of the class SEIRD_RU
#'
#' @return the value of R0
#' @export
setMethod(
    "R0", "SEIRD_RU", function(model) {
      # initial susceptible populations
      S0U <- initial_conditions(model)$S_U0
      S0Y <- initial_conditions(model)$S_Y0
      # required parameter values
      b <- transmission_parameters(model)$b
      k <- transmission_parameters(model)$k
      g <- transmission_parameters(model)$g
      m <- transmission_parameters(model)$m
      C <- transmission_parameters(model)$C
      # compute contacts N_U and N_Y from contact matrices
      # number of urban contacts for an urban individual, normalized over the
      # whole population through the age demographics
      NU <- sum(rowSums(contact_matrices(model)$urban) *
                   (country_demog(model)$urban / sum(country_demog(model)$urban))) / 2
      # number of rural contacts for a rural individual, normalized over the
      # whole population through the age demographics
      NY <- sum(rowSums(contact_matrices(model)$rural) *
                   (country_demog(model)$rural / sum(country_demog(model)$rural))) / 2
      # fraction of the population that is urban
      f_urban <- sum(country_demog(model)$urban)
      #fraction of the population that is rural
      f_rural <- sum(country_demog(model)$rural)
      # convenience parameter
      K <- ((1 - f_rural) * NU + f_rural * NY)

      matF <- matrix(data = c(0, 0, b * S0U * (K * C + NU / (1 - f_rural) * (1 - C)), b * S0U * K * C,
                              0, 0,         b * S0Y * K * C,       b * S0Y * (K * C + NY / f_rural * (1 - C)),   
                              0, 0,                 0,                                   0,
                              0, 0,                 0,                                   0),
                     nrow = 4, ncol = 4, byrow = TRUE)
      matV <- matrix(data = c(k, 0, 0, 0,
                              0, k, 0, 0,
                              -k, 0, (g + m), 0,
                              0, -k, 0, (g + m)),
                     nrow = 4, ncol = 4, byrow = TRUE)

      R0 <- max(abs(eigen(matF %*% inv(matV))$values))
      return(R0)
    })
