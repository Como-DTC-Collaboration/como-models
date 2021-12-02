#' @include generics.R
#'
NULL

#' Defines an age-structured SEIR model with non pharmaceutical interventions
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
#' @slot intervention_parameter_names list of names of intervention parameters
#'       (characters). Default is list ("starts", "stops").
#' @slot initial_conditions named list containing the initial conditions of the
#'     model. Initial values for each compartment, S0, E0, I0, R0.
#' @slot transmission_parameters named list containing the transmission
#'     parameters of the model. Transmission parameters b, k, g represent the
#'     rates of changes between the compartments.
#' @slot intervention_parameters list of values for intervention parameters
#'       (double).
#' @slot contact_matrix A list of square matrices, each with dimension
#'     equal to n_age_categories x n_age_categories. This matrix represents the
#'     contact between different age groups (rows) with age groups of
#'     people they come in contact with (columns) in the ist interval of the
#'     simulation (normally with no restriction on social contact.
#' @slot n_age_categories number of age categories.
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
#' @export SEIRDAge_interventions
#' 
SEIRDAge_interventions <- setClass('SEIRDAge_interventions',
                                   # slots
                                   slots = c(
                                     output_names = 'list',
                                     initial_condition_names = 'list',
                                     transmission_parameter_names = 'list',
                                     intervention_parameter_names = 'list',
                                     initial_conditions = 'list',
                                     transmission_parameters = 'list',
                                     interventions = 'list',
                                     age_ranges = 'list',
                                     n_age_categories = 'numeric',
                                     contact_matrix= 'list'
                                   ),                 
                                   # prototypes for the slots, automatically set output and param
                                   # names
                                   prototype = list(
                                     output_names = list('S', 'E', 'I', 'R', 'D' ,'Incidence'),
                                     initial_condition_names = list('S0', 'E0', 'I0', 'R0', 'D0'),
                                     transmission_parameter_names = list('isolated_frac', 'beta_isolated', 'beta_not_isolated', 
                                                                         'kappa', 'gamma', 'mu'),
                                     intervention_parameter_names = list("starts", "stops", "coverages"), 
                                     initial_conditions = vector(mode = "list", length = 5),
                                     transmission_parameters = vector(mode = "list", length = 6),
                                     interventions = vector(mode = "list", length = 3),
                                     age_ranges = vector(mode = 'list'),
                                     n_age_categories = NA_real_,
                                     contact_matrix= vector(mode = 'list')
                                   )
)

# Setter and getter methods for initial_conditions of an age-structured
# SEIR model with interventions.

#' @describeIn SEIRDAge_interventions Retrieves initial_conditions for an
#' age-structured SEIR model.
#'
#' @param object An object of the class SEIRDAge_interventions.
#'
#' @return Initial conditions of SEIRDAge_interventions model.
#'
#' @export
setMethod('initial_conditions', 'SEIRDAge_interventions',
          function(object) object@initial_conditions)


# Sets initial_conditions of an age-structured
# SEIR model with interventions.

#' @describeIn SEIRDAge_interventions Sets initial_conditions of an age-structured
#' SEIR model with interventions.
#' If the initial conditions provided to do not sum to 1 or of different
#' sizes compared to the number of age groups, an error is thrown.
#'
#' @param object An object of the class SEIRDAge_interventions.
#' @param value a named list of (S0, E0, I0, R0) where each element can be a list
#' of vector of doubles, with each element corresponding to the fraction for a
#' single age group.
#'
#' @return Updated version of the age-structured SEIR model with interventions
#'
#' @export
setMethod(
  'initial_conditions<-', 'SEIRDAge_interventions',
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
# SEIRD model with interventions interventions

#' @describeIn SEIRDAge_interventions Retrieves transmission_parameters for an
#' age-structured SEIR model with interventions.
#'
#' @param object An object of the class SEIRDAge_interventions.
#'
#' @return Transmission parameters of SEIRDAge_interventions model.
#'
#' @export
setMethod('transmission_parameters', 'SEIRDAge_interventions',
          function(object) object@transmission_parameters)


#' @describeIn SEIRDAge_interventions Sets the transmission_parameters for an
#' age-structured SEIRD model with interventions.
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
#' @return Updated version of the age-structured SEIR model with interventions.
#'
#' @export
#'
setMethod(
  'transmission_parameters<-', 'SEIRDAge_interventions',
  function(object, value) {
    
    # create list of parameter values
    isolated_frac <- value$isolated_frac
    beta_isolated <- value$beta_isolated
    beta_not_isolated <- value$beta_not_isolated
    kappa <- value$kappa
    gamma <- value$gamma
    mu <- value$mu
    
    trans_params <- list(isolated_frac, beta_isolated, beta_not_isolated, kappa, gamma, mu)
    
    # add names to each value
    names(trans_params) = object@transmission_parameter_names
    
    # check format of parameters b, k and g
    if(length(beta_isolated) != 1 | length(beta_not_isolated) != 1 | length(kappa) != 1 | length(gamma) != 1){
      stop('The parameter values should be 1-dimensional.')
    }
    
    # Set the row and column names of the instance's contact matrix
    for(i in seq(length(object@contact_matrix))){
      rownames(object@contact_matrix[[i]]) <- object@age_ranges
      colnames(object@contact_matrix[[i]]) <- object@age_ranges
      
    }
    
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params
    
    return(object)
  })


#' @describeIn SEIRDAge_interventions Retrieves intervention parameters of SEIRDAge_interventions model.
#'
#' @param object An object of the class SEIRDAge_interventions.
#'
#' @export
setMethod("interventions", "SEIRDAge_interventions",
          function(object) object@interventions)


#' @describeIn SEIRDAge_interventions Setter method for intervention parameters of the SEIRDAge_interventions model.
#'
#' Intervention parameters have same size. A tanh function is used to smooth interventions during simulation. This class is designed for interventions
#' which last several days at least and have several days between them; interventions involving rapid fluctuations may be distorted.
#'
#' @param object an object of the class SEIRDAge_interventions
#' @param value (list) list of intervention parameters: starts & stops
#'
#' @return object of class SEIRDAge_interventions with intervention parameters assigned.
#'
#' @export
setMethod(
  "interventions<-", "SEIRDAge_interventions",
  function(object, value) {
    if (length(value) != 1 &
        length(value) != object@n_age_categories){
      stop("Need one intervention must be for all age groups or one per age group.")
    }
    
    for (i in seq_along(value)){
      if (mean(names(value[[i]]) %in% object@intervention_parameter_names) != 1)
        stop(paste0("Intervention parameters must contain: ",
                    object@intervention_parameter_names))
      
      # raise errors if intervention parameters are not doubles
      for (p in list("starts", "stops", "coverages")) {
        if (!is.numeric(value[[i]][[p]])) {
          stop(glue("{p} format must be numeric"))
        }
      }
      
      # check that the intervention parameters are all of the same size
      if (length(value[[i]]$starts) != length(value[[i]]$stops)|
          length(value[[i]]$starts) != length(value[[i]]$coverages)|
          length(value[[i]]$coverages) != length(value[[i]]$stops)) {
        stop("Invalid intervention parameters. Must have same size.")
      }
    }
    
    object@interventions <- value
    
    object
  })



#' @describeIn SEIRDAge_interventions Method to simulate output using from model.
#'
#' Solves a system to ODEs which form an
#' age-structured simple SEIR model. The system of equations for the time
#' evolution of population fractions in Susceptible (S), Exposed (E), Infected
#' (I), Recovered (R) and Dead (D) groups in a given age group indexed by i is
#' given by
#'
#' \deqn{\frac{dS_i(t)}{dt} = - \beta_{isolated} S_i(t) \Sigma_{j}C_{ij}{isolated\_frac * I_j(t)} - \beta_{not_isolated} S_i(t) \Sigma_{j}C_{ij}{(1-isolated\_frac) * I_j(t)}
#' \deqn{\frac{dE_i(t)}{dt} = \beta_{isolated} S_i(t) \Sigma_{j}C_{ij}{isolated\_frac * I_j(t)} - \beta_{not_isolated} S_i(t) \Sigma_{j}C_{ij}{(1-isolated\_frac) * I_j(t)} - \kappa E_i(t)}
#' \deqn{\frac{dI_i(t)}{dt} = \kappa E_i(t) - \gamma I_i(t) - \mu I_i(t)}
#' \deqn{\frac{dR_i(t)}{dt} = \gamma I_i(t)
#' \deqn{\frac{dC(t)}{dt} = \beta_{isolated} S_i(t) \Sigma_{j}C_{ij}{isolated\_frac * I_j(t)} - \beta_{not_isolated} S_i(t) \Sigma_{j}C_{ij}{(1-isolated\_frac) * I_j(t)}
#' \deqn{\frac{dD_i(t)}{dt} = \mu I_i(t)}
#'
#' where C is a contact matrix whose elements represents the
#' contact between different age groups (rows) with age groups of
#' people they come in contact with (columns). Inter(t) is the value at time t
#' of the intervention protocol defined by
#' the intervention parameters. This function relies on the package deSolve.
#' The model encorperates the influence of non-pharmeceutical interventions (NPIs)
#' which act to reduce the degree of social contact between individuals. This is is
#' done by splitting any simulation of the model into several intervals and utilising
#' a different contact matrix in each interval to simulate the introduction of NPIs through
#' time. This function relies on the package deSolve to numerically integrate 
#' the set of equations above.
#'
#' @param object An object of the class SEIRDAge_interventions.
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
  "run", 'SEIRDAge_interventions',
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
    
    # fetch number of age catagories
    age <- object@n_age_categories
    
    # set initial state vector
    state <- c(S = object@initial_conditions$S0,
               E = object@initial_conditions$E0,
               I = object@initial_conditions$I0,
               R = object@initial_conditions$R0,
               D = object@initial_conditions$D0,
               cc = rep(0, age))
    
    # dt = times[2]-times[1]
    # set parameters vector
    parameters <- c(isolated_frac = object@transmission_parameters$isolated_frac,
                    beta_isolated = object@transmission_parameters$beta_isolated,
                    beta_not_isolated = object@transmission_parameters$beta_not_isolated,
                    kappa = object@transmission_parameters$kappa,
                    gamma = object@transmission_parameters$gamma,
                    mu = object@transmission_parameters$mu)
    
    C = object@contact_matrix[[1]]
    
    # set intervention parameters vector
    
    # use tstep=0.1 and tanh_slope=1 for good nice step-function-like shape of
    # the intervention wave
    sim_parms <- SimulationParameters(start = 0, stop = tail(times, n=1),
                                      tstep = 0.1)
    
    
    inter_prot <- vector("list", length = length(interventions(object)))
    for(i in 1:length(interventions(object))){
      int_parms <- 
        InterventionParameters(
          start=interventions(object)[[i]]$starts,
          stop=interventions(object)[[i]]$stops,
          coverage= interventions(object)[[i]]$coverages)
      prot <- intervention_protocol(int_parms, sim_parms, 1)
      inter_prot[[i]] <- approxfun(prot$time,
                                   prot$coverage, rule=2)
    }
    
    inter <- function(t){
      interven <- vector("numeric", length = length(interventions(object)))
      for(i in 1:length(interventions(object))){
        interven[i] <- inter_prot[[i]](t)
      }
      return(interven)
    }
    #print(interven)
    
    # function for RHS of ode system
    #Intervention = GetIdxIntervention(object, times)
    
    right_hand_side <- function(t, state, parameters, input) {
      with(
        as.list(c(state, parameters)),
        {
          inter <- input(t)
          #intv <- Intervention[which(as.integer(t)==times)]
          #isIntervention <- 1 * (intv>0)
          #C <- object@contact_matrix[[intv+1]]
          
          S <- state[1:age]
          E <- state[(age + 1):(2 * age)]
          I <- state[(2 * age + 1):(3 * age)]
          R <- state[(3 * age + 1):(4 * age)]
          D <- state[(4 * age + 1):(5 * age)]
          cc <- state[(5 * age + 1):(6 * age)]
          
          
          # rate of change
          dS <- - S*(beta_isolated * C %*% I * inter * isolated_frac  + beta_not_isolated * C %*% (I * (1-isolated_frac)))
          dE <- S*(beta_isolated * C %*% I * inter * isolated_frac  + beta_not_isolated * C %*% (I * (1-isolated_frac))) - kappa * E
          dI <- kappa * E - gamma * I - mu * I
          dR <- gamma * I
          dD <- mu * I
          dcc <- S*(beta_isolated * C %*% I * inter * isolated_frac  + beta_not_isolated * C %*% (I * (1-isolated_frac)))
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


GetIdxIntervention <- function(object, times){
  #calculate dt (timestep)
  dt = times[2] - times[1]
  Intervention <- rep(0, length(times))
  n_interventions <- length(object@interventions[["starts"]])
  
  for(intv in seq(n_interventions)){
    Intervention[seq(object@interventions[["starts"]][intv],
                     object@interventions[["stops"]][intv],
                     dt)] = intv
  }
  return(Intervention)
  
  
}

