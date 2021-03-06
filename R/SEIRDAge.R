#' @include generics.R
#'
NULL

#' An S4 object representing the SEIRDAge.
#' 
#' Defines an age-structured SEIRD model and solves the set of
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
#'     model. Initial values for each compartment, S0, E0, I0, R0, D0.
#' @slot transmission_parameters named list containing the transmission
#'     parameters of the model. Transmission parameters b, k, g represent the
#'     rates of changes between the compartments.
#' @slot contact_matrix A square matrix with dimension
#'     equal to n_age_categories x n_age_categories. This matrix represents the
#'     contact between different age groups (rows) with age groups of
#'     people they come in contact with (columns)
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
#' @export SEIRDAge
#' 
SEIRDAge <- setClass('SEIRDAge',
                     # slots
                     slots = c(
                       output_names = 'list',
                       initial_condition_names = 'list',
                       transmission_parameter_names = 'list',
                       initial_conditions = 'list',
                       transmission_parameters = 'list',
                       age_ranges = 'list',
                       n_age_categories = 'numeric',
                       contact_matrix = 'matrix'
                     ),
                     
                     # prototypes for the slots, automatically set output and param
                     # names
                     prototype = list(
                       output_names = list('S', 'E', 'I', 'R', 'D' ,'Incidence'),
                       initial_condition_names = list('S0', 'E0', 'I0', 'R0', 'D0'),
                       transmission_parameter_names = list('b', 'k', 'g', 'mu'),
                       initial_conditions = vector(mode = "list", length = 5),
                       transmission_parameters = vector(mode = "list", length = 4),
                       age_ranges = vector(mode = 'list'),
                       n_age_categories = NA_real_,
                       contact_matrix = matrix(NA)
                       
                     )
)

# Setter and getter methods for initial_conditions of an age-structured
# SEIRD model.

#' @describeIn SEIRDAge Retrieves initial_conditions for an
#' age-structured SEIRD model.
#'
#' @param object An object of the class SEIRDAge.
#'
#' @return Initial conditions of SEIRDAge model.
#' @export
#' 
setMethod('initial_conditions', 'SEIRDAge',
          function(object) object@initial_conditions)

#' @describeIn SEIRDAge Sets initial_conditions of an age-structured
#' SEIRD model.
#'
#' If the initial conditions provided to do not sum to 1 or of different
#' sizes compared to the number of age groups, an error is thrown.
#'
#' @param object An object of the class SEIRDAge.
#' @param value a named list of (S0, E0, I0, R0) where each element can be a list
#' of vector of doubles, with each element corresponding to the fraction for a
#' single age group.
#'
#' @return Updated version of the age-structured SEIRD model.
#' @export
#' 
setMethod(
  'initial_conditions<-', 'SEIRDAge',
  function(object, value) {
    S0 = value$S0
    E0 = value$E0
    I0 = value$I0
    R0 = value$R0
    D0 = value$D0
    # check that ICs are valid
    if (abs(sum(S0, E0, I0, R0, D0)-1)>=10^(-3)) {
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
    if(abs(sum(S0, E0, I0, R0, D0)-1)>=10^(-3)){
      stop('All compartments need to sum up to 1.')
    }
    
    # if all above tests are passed, assign the ic namelist to the object
    object@initial_conditions <- ic
    
    return(object)
  })

# Setter and getter methods for transmission_parameters of an age-structured
# SEIRD model.

#' @describeIn SEIRDAge Retrieves transmission_parameters for an
#' age-structured SEIRD model.
#'
#' @param object An object of the class SEIRDAge.
#'
#' @return Transmission parameters of SEIRDAge model.
#' @export
#' 
setMethod('transmission_parameters', 'SEIRDAge',
          function(object) object@transmission_parameters)

#' @describeIn SEIRDAge Sets transmission_parameters of an
#' age-structured SEIRD model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param value a named list of form list(b=, k=, g=, mu=)
#'
#' Here b is the rate of infection (S->I); k is the rate of transitioning from the
#' infected to infectious compartment (E->I); g is the rate of recovery (I->R). The
#' parameters b and k are numbers. The death rate
#' mu and recovery rate g can either be a single number, in which case all ages
#' are assumed to
#' have the same rate; or it can be a vector of length equal to the number of
#' age classes. 
#'
#' @return Updated version of the age-structured SEIRD model.
#' @export
#' 
setMethod(
  'transmission_parameters<-', 'SEIRDAge',
  function(object, value) {
    
    # create list of parameter values
    b <- value$b
    k <- value$k
    g <- value$g
    mu <- value$mu
    
    trans_params <- list(b, k, g, mu)
    
    # add names to each value
    names(trans_params) = object@transmission_parameter_names
    
    # check format of parameters b, k and g
    if(length(b) != 1 | length(k) != 1){
      stop('The parameter values should be 1-dimensional.')
    }
    
    if(length(mu) != 1 & length(mu) != object@n_age_categories){
      stop('The mortality parameter values should be of length 1 or
            number of age classes.')
    }
    
    if(length(g) != 1 & length(g) != object@n_age_categories){
      stop('The recovery rate parameter values should be of length 1 or
            number of age classes.')
    }
    
    # Set the row and column names of the instance's contact matrix
    rownames(object@contact_matrix) <- object@age_ranges
    colnames(object@contact_matrix) <- object@age_ranges
    
    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params
    
    return(object)
  })

#' @describeIn SEIRDAge Method to simulate output using from model.
#' 
#' Solves a system of ODEs which form an
#' age-structured SEIRD model. The system of equations for the time
#' evolution of population fractions in Susceptible (S), Exposed (E), Infected
#' (I), Recovered (R) and Dead (D) groups in a given age group indexed by i is 
#' given by
#'
#' \deqn{\frac{dS_i(t)}{dt} = - \beta S_i(t) \Sigma_{j}C_{ij} I_j(t)}
#' \deqn{\frac{dE_i(t)}{dt} = \beta S_i(t) \Sigma_{j}C_{ij} I_j(t) - \kappa E_i(t)}
#' \deqn{\frac{dI_i(t)}{dt} = \kappa E_i(t) - \gamma I_i(t) - \mu_i I_i(t)}
#' \deqn{\frac{dR_i(t)}{dt} = \gamma I_i(t)}
#' \deqn{\frac{dD_i(t)}{dt} = \mu_i I_i(t)}

#' where C is a contact matrix whose elements represents the
#' contact between different age groups (rows) with age groups of
#' people they come in contact with (columns). This function relies on the 
#' package deSolve to numerically integrate the set of equations above.
#' 
#'
#' @param object An object of the class SEIRDAge.
#' @param times (vector) time sequence over which to solve the model.
#'        Must be of the form seq(t_start,t_end,by=t_step). Default time series
#'        is seq(0, 100, by = 1).
#' @param solve_method A string indicating the chosen numerical integration
#' method for solving the ode system. Default is `lsoda` which is also the
#' default for the ode function in the deSolve package used in this function.
#'
#' @return data frame containing the time vector and time series of S, E, I, R and
#' D population fractions for each age group outputs with incidence numbers
#' for each age group.
#' 
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
setMethod(
  "run", 'SEIRDAge',
  function(object, times, solve_method = 'lsoda') {
    
    # error if times is not a vector or list of doubles
    if(!is.double(times)){
      stop('Evaluation times of the model storage format must be a vector.')
    }
    if(!is.numeric(times)){
      stop('Evaluation times of the model storage format must numeric.')
    }
    if(!(all(diff(times) > 0))){
      stop('Evaluation times of the model storage format must be increasing')
    }
    
    #fetch number of age categories
    n_age <- object@n_age_categories
    
    # set initial state vector
    state <- c(S = initial_conditions(object)$S0,
               E = initial_conditions(object)$E0,
               I = initial_conditions(object)$I0,
               R = initial_conditions(object)$R0,
               D = initial_conditions(object)$D0,
               cc = rep(0, n_age))
    
    # set parameters vector
    parameters <- list(b = transmission_parameters(object)$b,
                    k = transmission_parameters(object)$k,
                    g = transmission_parameters(object)$g,
                    mu = transmission_parameters(object)$mu)
    
    # fetch contact matrix of the instance
    C = object@contact_matrix
    
    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters) {
      with(
        as.list(c(state, parameters)),
        {
          S <- state[1:n_age]
          E <- state[(n_age + 1):(2 * n_age)]
          I <- state[(2 * n_age + 1):(3 * n_age)]
          R <- state[(3 * n_age + 1):(4 * n_age)]
          D <- state[(4 * n_age + 1):(5 * n_age)]
          cc <- state[(5 * n_age + 1):(6 * n_age)]
          
          
          # rate of change
          dS <- -b * S * C %*% I
          dE <- b * S * C %*% I - k * E
          dI <- k * E - g * I  - mu * I
          dR <- g * I
          dD <- mu * I
          dcc <- b * S * C %*% I
          # return the rate of change
          list(c(dS, dE, dI, dR, dD, dcc))
        })
    }
    
    # call ode solver
    out <- ode(
      y = state, times = times, func = right_hand_side,
      parms = parameters, method = solve_method)
    
    #output as a dataframe
    output <- as.data.frame.array(out)
    
    # melt dataframe to wide format
    out_temp = melt(output, 'time')
    
    # add compartment and age range columns
    n_compartment_measurements <- length(times) * n_age
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

#' @describeIn SEIRDAge Calculates basic reproduction number for SEIRDAge model
#'
#' To calculate this parameter, we first calculate the next generation matrix
#' G, where G_ij gives the expected number of infections of type i
#' caused by a single infectious individual of type j, assuming that all
#' of type i are susceptible. In the SEIRDAge model, the number of contacts
#' resulting in infection per unit time in age group i is beta N_i C_ij, where
#' N_i corresponds to the proportion of the population in that age group and
#' C_ij is the contact matrix element. The average duration of infection is
#' 1 / (mu_j + gamma_j) for an individual in age group j. This means the average number of
#' secondary infections of type i caused by an infectious individual of type j is 
#' g_ij = beta N_i C_ij / (mu_i + gamma_i). R0 is then given by the dominant
#' eigenvalue of the G matrix.
#'
#' @param model an SEIRDAge model
#' @param population_fractions the fraction of the population in each age group
#'
#' @return an R0 value
#' 
#' @export
setMethod("R0", "SEIRDAge", function(model, population_fractions) {
  beta <- model@transmission_parameters$b
  gamma <- model@transmission_parameters$g
  mu <- model@transmission_parameters$mu
  C <- model@contact_matrix
  
  # calculate next generation matrix
  C_times_N <- sweep(C, 1, population_fractions, "*")
  death_plus_recovery <- mu + gamma
  G <- sweep(beta * C_times_N, 2, death_plus_recovery, "/")
  
  # return dominant eigenvalue of it
  lambda_dominant <- eigen(G)$values[1]
  Re(lambda_dominant)
})

#' @describeIn SEIRDAge Prints a compartmental diagram for the SEIRDAge model
#'
#' This plot supposes the model comprises two age classes only.
#'
#' @param model an SEIRDAge model
#'
#' @return An ODE-compartmental structure diagram object of class html
#' 
#' @export
setMethod("ode_structure_diagram", "SEIRDAge", function(model) {
  htmltools::HTML(comomodels:::seirdage_structure)
})