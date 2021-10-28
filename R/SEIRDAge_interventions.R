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
#' @slot initial_conditions named list containing the initial conditions of the
#'     model. Initial values for each compartment, S0, E0, I0, R0.
#' @slot transmission_parameters named list containing the transmission
#'     parameters of the model. Transmission parameters b, k, g represent the
#'     rates of changes between the compartments.
#' @slot contact_matrix_1 A square matrix with dimension
#'     equal to n_age_categories x n_age_categories. This matrix represents the
#'     contact between different age groups (rows) with age groups of
#'     people they come in contact with (columns) in the 1st interval of the
#'     simulation (normally with no restriction on social contact.
#' @slot contact_matrix_2 A square matrix with dimension
#'     equal to n_age_categories x n_age_categories. This matrix represents the
#'     contact between different age groups (rows) with age groups of
#'     people they come in contact with (columns) in the 2nd interval of the
#'     simulation (normally with measures in place to limit social contact).
#' @slot contact_matrix_3 A square matrix with dimension
#'     equal to n_age_categories x n_age_categories. This matrix represents the
#'     contact between different age groups (rows) with age groups of
#'     people they come in contact with (columns) in the 3rd interval of the
#'     simulation (normally with some relaxation of measures compared to the
#'     2nd interval).
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
                       initial_conditions = 'list',
                       transmission_parameters = 'list',
                       age_ranges = 'list',
                       n_age_categories = 'numeric',
                       contact_matrix_1 = 'matrix',
                       contact_matrix_2 = 'matrix',
                       contact_matrix_3 = 'matrix'
                       
                     ),                 
                     # prototypes for the slots, automatically set output and param
                     # names
                     prototype = list(
                       output_names = list('S', 'E', 'I', 'R', 'D' ,'Incidence'),
                       initial_condition_names = list('S0', 'E0', 'I0', 'R0', 'D0'),
                       transmission_parameter_names = list('isolated_frac', 'beta_isolated', 'beta_not_isolated', 
                                                           'kappa', 'gamma', 'mu'),
                       initial_conditions = vector(mode = "list", length = 5),
                       transmission_parameters = vector(mode = "list", length = 6),
                       age_ranges = vector(mode = 'list'),
                       n_age_categories = NA_real_,
                       contact_matrix_1 = matrix(NA),
                       contact_matrix_2 = matrix(NA),
                       contact_matrix_3 = matrix(NA)
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
#' @return Updated version of the age-structured SEIR model.
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
    rownames(object@contact_matrix_1) <- object@age_ranges
    colnames(object@contact_matrix_1) <- object@age_ranges
    
    rownames(object@contact_matrix_2) <- object@age_ranges
    colnames(object@contact_matrix_2) <- object@age_ranges
    
    rownames(object@contact_matrix_3) <- object@age_ranges
    colnames(object@contact_matrix_3) <- object@age_ranges
    
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params
    
    return(object)
  })


#' @describeIn SEIRDAge_interventions Method to simulate output using from model.
#' 
#' Solves a system to ODEs which form an
#' age-structured simple SEIR model. The system of equations for the time
#' evolution of population fractions in Susceptible (S), Exposed (E), Infected
#' (I), Recovered (R) and Dead (D) groups in a given age group indexed by i is 
#' given by

#' where C is a contact matrix whose elements represents the
#' contact between different age groups (rows) with age groups of
#' people they come in contact with (columns). The model encorperates the
#' influence of non-pharmeceutical interventions (NPIs) which act to reduce the 
#' degree of social contact between individuals. This is is done by splitting 
#' any simulation of the model into 3 intervals and utilising a different 
#' contact matrix in each interval to simulate the introduction of NPIs through
#' time. This function relies on the 
#' package deSolve to numerically integrate the set of equations above.
#' 
#' @param object An object of the class SEIRDAge_interventions.
#' @param times (vector) time sequence over which to solve the model.
#'        Must be of the form seq(t_start,t_end,by=t_step). Default time series
#'        is seq(0, 100, by = 1).
#' @param t_intervention_1_2 time at which the first intervention takes place and
#' the model switches to utilising contact_matrix_2 to solve the ode system
#' 
#' @param t_intervention_2_3 time at which the second intervention takes place and
#' the model switches to utilising contact_matrix_3 to solve the ode system
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
  function(object, times, t_intervention_1_2, t_intervention_2_3, solve_method = 'lsoda') {
    
    # error if times is not a vector or list of doubles
    if(!is.double(times)){
      stop('Evaluation times of the model storage format must be a vector.')
    }
    
    # fetch number of age catagories
    age <- object@n_age_categories
    
    #calculate dt (timestep)
    dt = times[2] - times[1]
    
    # set initial state vector
    state <- c(S = object@initial_conditions$S0,
               E = object@initial_conditions$E0,
               I = object@initial_conditions$I0,
               R = object@initial_conditions$R0,
               D = object@initial_conditions$D0,
               cc = rep(0, age))
    
    # set parameters vector
    parameters <- c(isolated_frac = object@transmission_parameters$isolated_frac,
                    beta_isolated = object@transmission_parameters$beta_isolated,
                    beta_not_isolated = object@transmission_parameters$beta_not_isolated,
                    kappa = object@transmission_parameters$kappa,
                    gamma = object@transmission_parameters$gamma,
                    mu = object@transmission_parameters$mu)

    # set C as the 1st interval's contact matrix
    C = object@contact_matrix_1

    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters) {
      with(
        as.list(c(state, parameters)),
        {
          S <- state[1:age]
          E <- state[(age + 1):(2 * age)]
          I <- state[(2 * age + 1):(3 * age)]
          R <- state[(3 * age + 1):(4 * age)]
          D <- state[(4 * age + 1):(5 * age)]
          cc <- state[(5 * age + 1):(6 * age)]
          
          # split infectious compartment into isolating and non-isolating
          I_isolated = isolated_frac * I
          I_not_isolated = (1 - isolated_frac) * I
          
          # rate of change
          dS <- - S*(beta_isolated * C %*% I * isolated_frac + beta_not_isolated * C %*% I * (1 - isolated_frac))
          # dS <- - beta_isolated * S * C %*% I 
          dE <- S*(beta_isolated * C %*% I * isolated_frac + beta_not_isolated * C %*% I * (1 - isolated_frac)) - kappa*E
          # dE <- beta_isolated * S * C %*% I - kappa * E
          dI <- kappa * E - gamma * I - mu * I
          dR <- gamma * I
          dD <- mu * I
          # dcc <- beta_isolated * S * C %*% I 
          dcc <- S*(beta_isolated * C %*% I * isolated_frac + beta_not_isolated * C %*% I * (1 - isolated_frac))

          # return the rate of change
          list(c(dS, dE, dI, dR, dD, dcc))
        })
    }
    
    
    # call ode solver for the first interval
    out <- ode(y = state, times = seq(times[1], t_intervention_1_2, by = dt),
              func = right_hand_side, parms = parameters, method = solve_method)
    
    # output as a new dataframe 
    output_all <- as.data.frame.array(out)

    # construct list of contact matrices for the 2 next intervals of NPIs
    contact_matrices <- list(object@contact_matrix_2,
                          object@contact_matrix_3
                          )
    
    #extract the final state from the 1st interval
    #state = filter(output_all, time == t_intervention_1_2)
    state = output_all[output_all$time == t_intervention_1_2,!(names(output_all) %in% c("time"))] %>% unlist

    # construct list of time lists for next 2 intervals
    interval_times = list(seq(t_intervention_1_2+dt, t_intervention_2_3, by = dt),
                       seq(t_intervention_2_3+dt, utils::tail(times, n=1), by = dt)
                       )
      
    # loop over intervals, solve ode with each contact matrix, initialise next
    # interval with last state from current interval.
    for (i in 1:2){
      
      # set C to the current interval's contact matrix  
      C = contact_matrices[[i]]
        
      # call ode solver for the current interval
      out <- ode(y = state, times = interval_times[[i]], func = right_hand_side,
      parms = parameters, method = solve_method)
      
      # output as a dataframe
      output <- as.data.frame.array(out)
      
      
      # append the output from the current interval to the overall output
      output_all = rbind(output_all, output)
      state = output_all[output_all$time == utils::tail(interval_times[[i]], n=1),!(names(output_all) %in% c("time"))] %>% unlist
    
    }

    # melt dataframe to wide format
    out_temp = melt(output_all, 'time')
    
    # add compartment and age range columns
    # out_temp$compartment = lapply(out_temp$variable,function(x) substr(x,1,1)) %>% unlist

    
    # out_temp$age_range = rep((rep(unlist(object@age_ranges), each=length(times))),length(object@initial_conditions))
    
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

