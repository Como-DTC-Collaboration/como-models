#' @include generics.R
#'
NULL

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
#' @slot contact_rates list of two contact rates, one for urban communities
#'       and one for rural communities, with the number of contacts as a
#'       fraction of the total population size (double).
#' @slot contact_rates_names list of names for two contact rates,
#'       one for urban communities and one for rural communities
#' @slot country_demog list of two vectors, each describing the age breakdown
#'       of the population in the urban and rural communities.
#' @slot country_demog_names list of names for two demographic vectors,
#'       one for urban communities and one for rural communities
#' @slot fraction_rural fraction of the population that lives in a rural
#'      environment.
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
           contact_rates = "list",
           contact_rates_names = "list",
           country_demog = "list",
           country_demog_names = "list",
           fraction_rural = "numeric"
         ),
         # prototypes for the slots, automatically set parameter names and
         # its data type
         prototype = list(
           output_names = list("S_U", "E_U", "I_U", "R_U", "D_U",
                               "S_Y", "E_Y", "I_Y", "R_Y", "D_Y",
                               "Incidence_U", "Deaths_U",
                               "Incidence_Y", "Deaths_Y"),
           initial_condition_names = list("S_U0", "E_U0", "I_U0", "R_U0",
                                          "S_Y0", "E_Y0", "I_Y0", "R_Y0"),
           initial_cases_deaths_names = list("C_U0", "D_U0", "C_Y0", "D_Y0"),
           transmission_parameter_names = list("b", "k", "g", "m", "C"),
           initial_conditions = vector(mode = "list", length = 8),
           initial_cases_deaths = vector(mode = "list", length = 4),
           transmission_parameters = vector(mode = "list", length = 5),
           contact_rates = vector(mode = "list", length = 2),
           contact_rates_names = list("urban", "rural"),
           country_demog = vector(mode = "list", length = 2),
           country_demog_names = list("urban", "rural"),
           fraction_rural = 0.0
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
#' Retrieves contact rates of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setGeneric("contact_rates",
           function(object) standardGeneric("contact_rates"))

#' @describeIn SEIRD_RU Retrieves contact rates of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("contact_rates", "SEIRD_RU",
          function(object) object@contact_rates)
#-----------------------------------------------------------------------------
#' Retrieves age demographics, if provided, of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setGeneric("country_demog",
           function(object) standardGeneric("country_demog"))

#' @describeIn SEIRD_RU Retrieves age demographics, if provided, of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("country_demog", "SEIRD_RU",
          function(object) object@country_demog)

#-----------------------------------------------------------------------------
#' Retrieves fraction of the population that lives in a rural environment
#' of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setGeneric("fraction_rural",
           function(object) standardGeneric("fraction_rural"))

#' @describeIn SEIRD_RU Retrieves fraction of the population that lives in a
#' rural environment of SEIR model.
#'
#' @param object An object of the class SEIRD_RU.
#' @export

setMethod("fraction_rural", "SEIRD_RU",
          function(object) object@fraction_rural)

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
#' Setter method for contact matrices, or contact rates directly, for urban and
#' rural communities in the SEIRD_RU model. Matrices must be of type double.
#' NOTE: if inputting matrices, you MUST assign fraction_rural first, and you 
#' must input full age demographics, i.e. you cannot only provide the fraction
#' of the population that is rural!
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of contact matrices or contact rates.
#'
#' @return object of class SEIRD_RU with contact rates assigned.
#' @export

setGeneric(
  "contact_rates<-",
  function(object, value) {
    standardGeneric("contact_rates<-")
  })

#' @describeIn SEIRD_RU Setter method for contact matrices for
#' urban and rural communities in the SEIR model.
#' Matrices must be of type double.
#' NOTE: if inputting matrices, you MUST assign fraction_rural first, and you 
#' must input full age demographics, i.e. you cannot only provide the fraction
#' of the population that is rural!
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of contact matrices, with urban first, then rural.
#'
#' @return object of class SEIRD_RU with contact matrices
#' assigned.
#' @export

setMethod(
  "contact_rates<-", "SEIRD_RU",
  function(object, value) {

    # create list of parameter values
    contact_mat <- value

    # add names to each value
    names(contact_mat) <- object@contact_rates_names

    # check format of contact matrices
    if (typeof(contact_mat$urban) != "double"
        | typeof(contact_mat$rural) != "double") {
      stop("Contact matrices must be of type double.")
    }
    
    #check if input is matrices or numbers, process accordingly
    if (length(contact_mat$urban) > 1) {
      # check if age demographics have been provided
      if(length(country_demog(object)$urban) == 0 || length(country_demog(object)$rural) == 0) {
        stop("If you wish to use contact matrices for the model, you must provide
             vectors of the age demographics of the rural and urban communities
             BEFORE assigning the contact matrices.")
      }
    # compute contacts N_U and N_Y from contact matrices
    # number of urban contacts for an urban individual. The number of contacts
    # has been divided through by the size of the population.
    N_U <- sum(rowSums(contact_mat$urban) *
                 (country_demog(object)$urban / sum(country_demog(object)$urban))) / 2
    # number of rural contacts for a rural individual, where the number of
    # contacts has been divided by the size of the population.
    N_Y <- sum(rowSums(contact_mat$rural) *
                 (country_demog(object)$rural / sum(country_demog(object)$rural))) / 2
    contact_mat <- list(N_U, N_Y)
    names(contact_mat) <- object@contact_rates_names}
    # if all above tests are passed, assign the contact_mat namelist to the
    # object
    object@contact_rates <- contact_mat

    return(object)
  })

#-----------------------------------------------------------------------------
#' Setter method for demographic data for urban and rural communities in the
#' SEIR model. If inputting demographic vectors, both vectors together must sum
#' to 1.
#'
#' @param object (SEIRD_RU model)
#' @param value (list) list of 2 sets of population fractions by age group, or
#' a single value noting the fraction of the population that is rural.
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
#' @param value (list) list of 2 sets of population fractions by age group, or
#' a single value noting the fraction of the population that is rural.
#'
#' @return object of class SEIRD_RU with demographic data
#' assigned.
#' @export

setMethod(
  "country_demog<-", "SEIRD_RU",
  function(object, value) {

    # create list of parameter values
    demo_data <- value
    
    if (length(demo_data) == 2 && length(demo_data[[1]]) == 16 && length(demo_data[[2]]) == 16){
      # add names to each value
      names(demo_data) <- object@country_demog_names
  
      # check format of contact matrices
      if (typeof(demo_data$urban) != "double"
          | typeof(demo_data$rural) != "double") {
        stop("Contact matrices must be of type double.")
      }

      # check that vectors sum to 1: losing some precision in division
      # so want to make sure it is accurate to 0.1% of the population
      if (1 - sum(demo_data$urban) - sum(demo_data$rural) > 0.001) {
        stop("Sum over all age groups and both communities must be 1. The
             difference between 1 and the sum over age groups and over both
             communities is more than 0.001.")
      }
      
      #fraction of the population that is rural
      f_rural <- sum(demo_data$rural)
      # if all above tests are passed, assign the contact_mat namelist to the
      # object
      object@country_demog <- demo_data
      object@fraction_rural <- f_rural
    }
    else {
      if (length(demo_data) != 1) {
        stop("The entered demographic data must be either two vectors, or one 
             value.")
      }
      if (typeof(demo_data) != "double") {
        stop("Contact matrices or fraction must be of type double.")
      }
      if (demo_data >= 1 || demo_data <= 0) {
        stop("Frastion of the population that is rural must be more than 0 and
             less than 1.")
      }
      object@fraction_rural <- demo_data
    }
    
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
    state <- c(S_U = initial_conditions(object)$S_U0,
               E_U = initial_conditions(object)$E_U0,
               I_U = initial_conditions(object)$I_U0,
               R_U = initial_conditions(object)$R_U0,
               C_U = initial_cases_deaths(object)$C_U0,
               D_U = initial_cases_deaths(object)$D_U0,
               S_Y = initial_conditions(object)$S_Y0,
               E_Y = initial_conditions(object)$E_Y0,
               I_Y = initial_conditions(object)$I_Y0,
               R_Y = initial_conditions(object)$R_Y0,
               C_Y = initial_cases_deaths(object)$C_Y0,
               D_Y = initial_cases_deaths(object)$D_Y0)

    # set transmission parameters vector
    parameters <- c(b = transmission_parameters(object)$b,
                    k = transmission_parameters(object)$k,
                    g = transmission_parameters(object)$g,
                    m = transmission_parameters(object)$m,
                    C = transmission_parameters(object)$C,
                    N_U = contact_rates(object)$urban,
                    N_Y = contact_rates(object)$rural,
                    f_urban = (1-fraction_rural(object)),
                    f_rural = fraction_rural(object)
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

    # Compute incidences and deaths
    cases <- c(0, diff(output$C_U))
    deaths <- c(0, diff(output$D_U))
    output$Incidence_U <- cases
    output$Deaths_U <- deaths
    cases <- c(0, diff(output$C_Y))
    deaths <- c(0, diff(output$D_Y))
    output$Incidence_Y <- cases
    output$Deaths_Y <- deaths
    output <- output[, c("time", unlist(object@output_names))]

    # Create long format of output
    output <- melt(output, id.vars = "time")
    names(output) <- c("time", "compartment", "value")

    # Added for consistency of output format across models
    output$age_group <- rep("0-150", length(output$time))
    
    # Split output into 2 dataframes: one with S,E,I, and R and one with C and D
    states <- subset(output, !stringr::str_detect(output$compartment, "Incidence|Deaths"))
    states <- droplevels(states)
    changes <- subset(output, stringr::str_detect(output$compartment, "Incidence|Deaths"))
    changes <- droplevels(changes)

    list("states" = states, "changes" = changes)
  })

#----------------------------------------------------------------------------
#' @describeIn SEIRD_RU Returns the value of R0, the basic reproduction number,
#' for the model with the specified parameter values.
#' R0 is computed using the next generation matrix method as set out in 
#' van den Driessche (2017):
#' van  den  Driessche, P. Reproduction numbers of infectious disease models.
#' Infectious Disease Modelling, 2:288–303, 8 2017.
#' 
#' In this method, the variables are split into two groups: those that represent
#' infected individuals (in our case the exposed and infectious compartments in
#' both communities), and those that do not (the susceptible, recovered and dead).
#' We only further consider the system of equations consisting of the ODEs 
#' describing the variables in the first group,
#' in this case (E_U, E_Y, I_U and I_Y).
#' The differential equations for the variables in the first group are written
#' in the form
#' \deqn{\frac{dx_i}{dt} = F_i - V_i}
#' where F_i are the terms describing new cases of infection and V_i the terms
#' describing how individuals move between infected and other compartments.
#' We compute the matrices matF and matV as
#' \deqn{matF = \partial F_i(x_0)/\partial x_j}
#' \deqn{matV = \partial V_i(x_0)/\partial x_j}
#' where x_j are all the variables representing infected individuals.
#' Finally, R0 is the spectral radius of the matrix FV^(-1). For a more
#' detailed description of this method, see van den Driessche (2017).
#' 
#' @param model a model of the class SEIRD_RU
#'
#' @return the value of R0
#' @export
setMethod(
    "R0", "SEIRD_RU", function(model) {
      # initial susceptible populations: This method applies to the disease-free 
      # equilibrium. At the disease-free equilibrium that we are concerned about
      # everyone is susceptible, but it still matters which fraction of the
      # population lives in which community
      S0U <- 1 - fraction_rural(model)
      S0Y <- fraction_rural(model)
      # required parameter values
      b <- transmission_parameters(model)$b
      k <- transmission_parameters(model)$k
      g <- transmission_parameters(model)$g
      m <- transmission_parameters(model)$m
      C <- transmission_parameters(model)$C
      # compute contacts N_U and N_Y from contact matrices
      # number of urban contacts for an urban individual, as a fraction of the
      # total population
      NU <- contact_rates(model)$urban
      # number of rural contacts for a rural individual,  as a fraction of the
      # total population
      NY <- contact_rates(model)$rural
      # fraction of the population that is urban
      f_urban <- 1 - fraction_rural(model)
      #fraction of the population that is rural
      f_rural <- fraction_rural(model)
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

      R0 <- max(abs(eigen(matF %*% solve(matV))$values))
      return(R0)
    })
