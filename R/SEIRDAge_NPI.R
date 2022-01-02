#' @include generics.R
#'
NULL

#' Defines an age-structured SEIR model with non-pharmaceutical interventions (NPIs)
#' which influence social contact. Methods solve the set of
#' ordinary differential equations of the model with a chosen method of
#' numerical integration.
#'
#' @slot output_names names of the compartments which are used by the
#'     model.
#' @slot initial_condition_names names of the initial conditions used by the
#'     model.
#' @slot transmission_parameter_names names of the transmission parameters used
#'     by the model.
#' @slot npi_parameter_names list of names of NPI parameters
#'       (characters). Default is list ("starts", "stops").
#' @slot initial_conditions named list containing the initial conditions of the
#'     model. Initial values for each compartment, S0, E0, I0, R0.
#' @slot transmission_parameters named list containing the transmission
#'     parameters of the model. Transmission parameters b, k, g represent the
#'     rates of changes between the compartments.
#' @slot npi_parameters list of values for NPI parameters
#'       (double).
#' @slot interventions list of NPI periods
#' @slot contact_matrix A square matrices, each with dimension
#'     equal to n_age_categories x n_age_categories. This matrix represents the
#'     contact between different age groups (rows) with age groups of
#'     people they come in contact with (columns) in the ist interval of the
#'     simulation (normally with no restriction on social contact.
#' @slot contact_matrix_npi A list of square matrices, each with dimension
#'     equal to n_age_categories x n_age_categories. This matrix represents the
#'     contact between different age groups (rows) with age groups of
#'     people they come in contact with (columns) in the ist interval of the
#'     simulation (normally with no restriction on social contact.
#' @slot n_age_categories number of age categories.
#' @slot n_npi number of NPI periods.
#' @slot age_ranges list of string characters representing the range of ages of
#'     people in each age category. This object must have length
#'     \code{n_age_categories} (otherwise an error is returned) and each element
#'     must be formatted as 'age1-age2'.
#'
#' @import deSolve
#' @import glue
#' @import tidyverse
#' @import reshape2
#' @importFrom methods new
#' 
#' @export SEIRDNPIAge
#' 
SEIRDNPIAge <- setClass('SEIRDNPIAge',
                                   # slots
                                   slots = c(
                                     output_names = 'list',
                                     initial_condition_names = 'list',
                                     transmission_parameter_names = 'list',
                                     npi_parameter_names = 'list',
                                     initial_conditions = 'list',
                                     transmission_parameters = 'list',
                                     interventions = 'list',
                                     age_ranges = 'list',
                                     n_age_categories = 'numeric',
                                     n_npi = 'numeric',
                                     contact_matrix= 'matrix',
                                     contact_matrix_npi = 'list'
                                   ),                 
                                   # prototypes for the slots, automatically set output and param
                                   # names
                                   prototype = list(
                                     output_names = list('S', 'E', 'I', 'R', 'D' ,'Incidence'),
                                     initial_condition_names = list('S0', 'E0', 'I0', 'R0', 'D0'),
                                     transmission_parameter_names = list('beta_npi', 'beta', 
                                                                         'kappa', 'gamma', 'mu'),
                                     npi_parameter_names = list("starts", "stops", "coverages"), 
                                     initial_conditions = vector(mode = "list", length = 5),
                                     transmission_parameters = vector(mode = "list", length = 5),
                                     interventions = vector(mode = "list"),
                                     age_ranges = vector(mode = 'list'),
                                     n_age_categories = NA_integer_,
                                     n_npi = NA_integer_,
                                     contact_matrix = matrix(NA),
                                     contact_matrix_npi = vector(mode = 'list')
                                   )
)

# Setter and getter methods for initial_conditions of an age-structured
# SEIRD model with NPIs.

#' @describeIn SEIRDNPIAge Retrieves initial_conditions for an
#' age-structured SEIRD model.
#'
#' @param object An object of the class SEIRDNPIAge.
#'
#' @return Initial conditions of SEIRDNPIAge model.
#'
#' @export
setMethod('initial_conditions', 'SEIRDNPIAge',
          function(object) object@initial_conditions)


#' @describeIn SEIRDNPIAge Sets initial_conditions of an age-structured
#' SEIRD model with NPIs.
#' If the initial conditions provided to do not sum to 1 or of different
#' sizes compared to the number of age groups, an error is thrown.
#'
#' @param object An object of the class SEIRDNPIAge.
#' @param value a named list of (S0, E0, I0, R0) where each element can be a list
#' of vector of doubles, with each element corresponding to the fraction for a
#' single age group.
#'
#' @return Updated version of the age-structured SEIR model with NPIs
#'
#' @export
setMethod(
  'initial_conditions<-', 'SEIRDNPIAge',
  function(object, value) {
    S0 = value$S0
    E0 = value$E0
    I0 = value$I0
    R0 = value$R0
    D0 = value$D0
    # check that ICs are valid
    if (sum(S0, E0, I0, R0, D0) != 1) {
      stop('Invalid initial conditions. Must sum to 1.')
    }
    
    # create list of parameter values
    ic <- list(S0, E0, I0, R0, D0)
    
    # add names to each value
    names(ic) = object@initial_condition_names
    
    # raise errors if age category dimensions do not match initial state vectors
    # also raise errors if initial state and parameter values are not doubles
    for (p in list('S0', 'E0', 'I0', 'R0', 'D0')){
      if(length(ic[[p]]) != object@n_age_categories){
        stop(glue('Wrong number of age groups for {p}
              compartments.'))}
      if(!is.numeric(ic[[p]])){
        stop(glue('{p} format must be numeric'))}
    }
    if(sum(S0, E0, I0, R0, D0) != 1){
      stop('All compartments need to sum up to 1.')
    }
    
    # if all above tests are passed, assign the ic namelist to the object
    object@initial_conditions <- ic
    
    return(object)
  })


# Setter and getter methods for transmission_parameters of an age-structured
# SEIRD model with NPIs

#' @describeIn SEIRDNPIAge Retrieves transmission_parameters for an
#' age-structured SEIR model with NPIs.
#'
#' @param object An object of the class SEIRDNPIAge.
#'
#' @return Transmission parameters of SEIRDNPIAge model.
#'
#' @export
setMethod('transmission_parameters', 'SEIRDNPIAge',
          function(object) object@transmission_parameters)


#' @describeIn SEIRDNPIAge Sets the transmission_parameters for an
#' age-structured SEIRD model with NPIs.
#' 
#' Sets transmission_parameters of an
#' age-structured SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param value a named list of form list(beta=, kappa=, gamma=, mu=)
#'
#' All rates of change between compartments are equal regardless of
#' age group.
#'
#' @return Updated version of the age-structured SEIR model with NPIs.
#'
#' @export
#'
setMethod(
  'transmission_parameters<-', 'SEIRDNPIAge',
  function(object, value) {
    
    # create list of parameter values
    #intervention_frac <- value$intervention_frac
    beta_npi <- value$beta_npi
    beta <- value$beta
    kappa <- value$kappa
    gamma <- value$gamma
    mu <- value$mu
    
    # set transmission parameters
    trans_params <- list(beta_npi, beta, kappa, gamma, mu)
    # add names to each value
    names(trans_params) = object@transmission_parameter_names
    
    # check format of parameters: should be either a single 
    # positive real value or a list of positive real values with
    # the length equal to the number of age groups
    for(i in seq_along(trans_params)){
      param_vals = trans_params[[i]]
      param_name = names(trans_params[i])
      if((length(param_vals) != 1) & (length(param_vals)!=object@n_age_categories)){
        stop(paste0("The parameter value(s) of ", param_name, " should be of length 1 or
            the number of age classes (i.e. object@n_age_categories)"))
      }
    }
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params
    
    
    # Set the row and column names of each of the instance's contact matrix
    rownames(object@contact_matrix) <- object@age_ranges
    colnames(object@contact_matrix) <- object@age_ranges
    
    for(i in seq_along(object@contact_matrix_npi)){
      rownames(object@contact_matrix_npi[[i]]) <- object@age_ranges
      colnames(object@contact_matrix_npi[[i]]) <- object@age_ranges
    }
    return(object)
  })


#' @describeIn SEIRDNPIAge Retrieves NPI parameters of SEIRDNPIAge model.
#'
#' @param object An object of the class SEIRDNPIAge.
#'
#' @export
setMethod("interventions", "SEIRDNPIAge",
          function(object) object@interventions)


#' @describeIn SEIRDNPIAge Setter method for NPI parameters of the SEIRDNPIAge model.
#'
#' @param object an object of the class SEIRDNPIAge
#' @param value (list) list of NPI parameters: starts, stops, coverages
#'
#' @return object of class SEIRDNPIAge with NPI parameters assigned.
#'
#' @export
setMethod(
  "interventions<-", "SEIRDNPIAge",
  function(object, value) {
    if (length(value) != 1 &
        length(value) != object@n_age_categories &
        length(value) != object@n_npi){
      stop("Need one intervention must be for all age groups or one per age group.")
    }
    
    for (i in seq_along(value)){
      if (mean(names(value[[i]]) %in% object@npi_parameter_names) != 1)
        stop(paste0("Intervention parameters must contain: ",
                    object@npi_parameter_names))
      
      # raise errors if NPIparameters are not doubles
      for (p in list("starts", "stops", "coverages")) {
        if (!is.numeric(value[[i]][[p]])) {
          stop(glue("{p} format must be numeric"))
        }
      }
      
      # check that the NPI parameters are all of the same size
      if (length(value[[i]]$starts) != length(value[[i]]$stops)|
          length(value[[i]]$starts) != length(value[[i]]$coverages)|
          length(value[[i]]$coverages) != length(value[[i]]$stops)) {
        stop("Invalid intervention parameters. Must have same size.")
      }
    }
    
    object@interventions <- value
    
    object
  })



#' @describeIn SEIRDNPIAge Method to simulate output using from model.
#'
#' Solves a system to ODEs which form an
#' age-structured simple SEIR model. The system of equations for the time
#' evolution of population fractions in Susceptible (S), Exposed (E), Infected
#' (I), Recovered (R) and Dead (D) groups in a given age group indexed by i is
#' given by
#'
#' \deqn{\frac{dS_i(t)}{dt} = -\beta_i(t) S_i \Sigma_{j}C_{ij}(t) I_j}
#' \deqn{\frac{dE_i(t)}{dt} = \beta_i(t) S_i \Sigma_{j}C{ij}(t) I_j-\kappa E_i}
#' \deqn{\frac{dI_i(t)}{dt} = \kappa E_i - (\gamma_i + \mu_i) I_i}
#' \deqn{\frac{dR_i(t)}{dt} = \frac{\text{d}R_i}{\text{d}t} = \gamma_i I_i}
#' \deqn{\frac{dC(t)}{dt} = \beta_i(t) S_i \Sigma_{j}C_{ij}(t) I_j}
#' \deqn{\frac{dD_i(t)}{dt} = \mu_i I_i(t)}
#'
#' where C is a contact matrix whose elements represents the
#' contact between different age groups (rows) with age groups of
#' people they come in contact with (columns). Inter(t) is the value at time t
#' of the NPI protocol defined by the NPI (intervention) parameters.
#' The model encorperates the influence of NPIs
#' which act to reduce the degree of social contact and/or change the transmission
#' parameters beta between individuals. This is
#' done by splitting any simulation of the model into several intervals and utilising
#' a different contact matrix in each interval to simulate the introduction of NPIs through
#' time. This function relies on the package deSolve to numerically integrate 
#' the set of equations above.
#'
#' @param object An object of the class SEIRDNPIAge.
#' @param times (vector) time sequence over which to solve the model.
#'        Must be of the form seq(t_start,t_end,by=t_step). Default time series
#'        is seq(0, 100, by = 1).
#'
#' @param solve_method A string indicating the chosen numerical integration
#' method for solving the ode system. Default is `lsoda` which is also the
#' default for the ode function in the deSolve package used in this function.
#'
#' @return a data frame containing the time vector and time series of S, R, I and
#' D population fractions for each age group outputs with incidence numbers
#' for each age group.
#'
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
setMethod(
  "run", 'SEIRDNPIAge',
  function(object, times, solve_method = 'lsoda') {
    
    # error if times is not a vector or list of doubles
    if(!is.double(times)){
      stop('Evaluation times of the model storage format must be a vector.')
    }
    if (is.null(unlist(object@transmission_parameters)))
      stop("Transmission parameters must be set before running.")
    if (is.null(unlist(object@initial_conditions)))
      stop("Initial conditions must be set before running.")
    if (is.null(unlist(object@interventions)))
      stop("Intervention parameters must be set before running.")
    if (is.null(unlist(object@contact_matrix)))
      stop("Contact matrix must be set before running.")
    if (is.null(unlist(object@contact_matrix_npi)))
      stop("Contact matrix at each intervention period must be set before running.")
    
    # fetch number of age catagories
    age <- object@n_age_categories
    
    # set initial state vector
    state <- c(S = object@initial_conditions$S0,
               E = object@initial_conditions$E0,
               I = object@initial_conditions$I0,
               R = object@initial_conditions$R0,
               D = object@initial_conditions$D0,
               cc = rep(0, age))
    
    # set parameters vector
    parameters <- c(
                    b_npi = object@transmission_parameters$beta_npi,
                    b = object@transmission_parameters$beta,
                    k = object@transmission_parameters$kappa,
                    g = object@transmission_parameters$gamma,
                    mu = object@transmission_parameters$mu)
    
    C = object@contact_matrix_npi
    C[[object@n_npi+1]] = object@contact_matrix
    b_finals = c(object@transmission_parameters$beta_npi,
                    object@transmission_parameters$beta)
    
    # set intervention parameters vector
    #  function for the ode to give the correct beta_npi and contact matrices at corresponding time point
    #  currently, we consider applying same beta_npi for all age groups. SHOULD MODIFY!
    idx_interv <- function(t){
      idx_interv <- -1
      i = 1
      while ((idx_interv < 0) & (i <= object@n_npi)){
        #print(paste(idx_interv, i))
        if ((t>=interventions(object)[[i]]$starts) & (t<=interventions(object)[[i]]$stops)) idx_interv = i
        i = i+1
      } 
      # assign to the idx referring to the non-intervention parameter
      if(idx_interv < 0) idx_interv = object@n_npi + 1
      return(idx_interv)
    }
    idx_interv_list <- lapply(times, function(t)idx_interv(t))
    inter <- approxfun(times, idx_interv_list, rule=2) # avoid NA
    
    # In each intervention, the contact matrix 
    right_hand_side <- function(t, state, parameters, input) {
      with(
        as.list(c(state, parameters)),
        {
          inter <- input(t)
          C_final <- C[[inter]]
          b_final <- b_finals[inter]
          
          S <- state[1:age]
          E <- state[(age + 1):(2 * age)]
          I <- state[(2 * age + 1):(3 * age)]
          R <- state[(3 * age + 1):(4 * age)]
          D <- state[(4 * age + 1):(5 * age)]
          cc <- state[(5 * age + 1):(6 * age)]
          
          g <- object@transmission_parameters$gamma
          mu <- object@transmission_parameters$mu

          # rate of change
          dS <- - S*(b_final * C_final %*% I)
          dE <- S*(b_final * C_final %*% I) - k * E
          dI <- k * E - g * I - mu * I
          dR <- g* I
          dD <- mu * I
          dcc <- S*(b_final * C_final %*% I)
          # return the rate of change
          list(c(dS, dE, dI, dR, dD, dcc))
        })
    }
    
    # call ode solver
    out <- ode(
      y = state, times = times, func = right_hand_side,
      parms = parameters, method = solve_method, input = inter)
    
    # melt dataframe to wide format
    output_all <- as.data.frame.array(out)
    
    out_temp = melt(output_all, 'time')
    
    
    
    # add compartment and age range columns
    n_compartment_measurements <- length(times) * age
    out_temp$compartment = c(replicate(n_compartment_measurements, "S"),
                             replicate(n_compartment_measurements, "E"),
                             replicate(n_compartment_measurements, "I"),
                             replicate(n_compartment_measurements, "R"),
                             replicate(n_compartment_measurements, "D"),
                             replicate(n_compartment_measurements, "cc"))
    
    out_temp$age_range = unlist(rep(object@age_ranges, each=length(times)))
    
    # drop the old variable column
    out_temp = out_temp %>%
      dplyr::select(-.data$variable) %>%
      dplyr::mutate(compartment=as.factor(.data$compartment)) %>%
      dplyr::mutate(compartment=forcats::fct_relevel(.data$compartment, "S", "E", "I", "R", "D", "cc")) %>%
      dplyr::mutate(age_range=as.factor(.data$age_range)) %>%
      dplyr::mutate(age_range=forcats::fct_relevel(.data$age_range, object@age_ranges))
    
    # compute incidence and deaths
    changes <- out_temp %>%
      dplyr::filter(.data$compartment %in% c("cc", "D")) %>%
      dplyr::group_by(.data$compartment, .data$age_range) %>%
      dplyr::mutate(value = c(0, diff(.data$value))) %>%
      dplyr::mutate(compartment = dplyr::if_else(.data$compartment == "cc", "Incidence",
                                                 "Deaths")) %>%
      dplyr::ungroup() %>%
      as.data.frame()
    
    # remove cumulative cases column from state vector
    states = out_temp %>%
      dplyr::filter(.data$compartment != "cc") %>%
      droplevels() %>%
      dplyr::ungroup()
    
    return(list("states" = states, "changes" = changes))
  })

#' @describeIn SEIRDNPIAge Calculates the basic reproduction number for SEIRDNPIAge model
#'
#' To calculate this parameter, we first calculate the next generation matrix
#' G, where G_ij gives the expected number of secondary infections of type i
#' caused by a single infectious individual of type j, assuming that all
#' of type i are susceptible. In the SEIRDNPIAge model, the number of contacts
#' resulting in infection per unit time in age group i is beta N_i C_ij, where
#' N_i corresponds to the proportion of the population in that age group and
#' C_ij is the contact matrix element. The average duration of infection is
#' 1 / (mu_j + gamma_j) for an individual in age group j. This means the average number of
#' secondary infections of type i caused by an infectious individual of type j is 
#' g_ij = beta N_i C_ij / (mu_i + gamma_i). R0 is then given by the dominant
#' eigenvalue of the G matrix.
#'
#' @param model an SEIRDNPIAge model
#'
#' @return an R0 value
#' 
#' @export
setMethod("R0", "SEIRDNPIAge", function(model) {
  
  beta <- model@transmission_parameters$beta
  gamma <- model@transmission_parameters$gamma
  mu <- model@transmission_parameters$mu
  
  # we apply the original contact matrix without any intervention
  C <- model@contact_matrix
  
  # sum over all population groups for each age group
  population_sums <- matrix(unlist(initial_conditions(model)), nrow = model@n_age_categories) %>% rowSums
  # calculate population fractions
  population_fractions <- population_sums / sum(population_sums)
  # calculate next generation matrix
  C_times_N <- sweep(C, 1, population_fractions, "*")
  death_plus_recovery <- mu + gamma
  G <- sweep(beta * C_times_N, 1, death_plus_recovery, "/")
  
  # return dominant eigenvalue of it
  lambda_dominant <- eigen(G)$values[1]
  Re(lambda_dominant)
})



