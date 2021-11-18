#' @include generics.R
#'
NULL

#' An S4 object representing the SEIRDVAge.
#' 
#' Defines an age-structured SEIRDV model and solves the set of
#' ordinary differential equations of the model with a chosen method of
#' numerical integration. Vaccinated individuals are considered in their own
#' compartment.
#' 
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I0", "V0", "R0", "VR0", "D0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("beta", "kappa", "gamma", "mu",  "nu",
#'       "delta_V", "delta_R", "delta_VR").
#' @slot intervention_parameter_names list of names of intervention parameters
#'       (characters). Default is list ("starts", "stops", "coverages")
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters
#'       (double).
#' @slot intervention_parameters list of values for intervention parameters
#'       (double).
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
#' @export SEIRDVAge
#' 
SEIRDVAge <- setClass('SEIRDVAge',
                     # slots
                     slots = c(
                       output_names = 'list',
                       initial_condition_names = 'list',
                       transmission_parameter_names = 'list',
                       intervention_parameter_names = 'list',
                       initial_conditions = 'list',
                       transmission_parameters = 'list',
                       intervention_parameters = 'list',
                       age_ranges = 'list',
                       n_age_categories = 'numeric',
                       contact_matrix = 'matrix'
                     ),
                     
                     # prototypes for the slots, automatically set output and param
                     # names
                     prototype = list(
                       output_names = list('S', 'E', 'I', 'R', 'V', 'VR', 'D' ,'Incidence'),
                       initial_condition_names = list('S0', 'E0', 'I0', 'R0', 'V0', 'VR0', 'D0'),
                       transmission_parameter_names = list('beta', 'kappa', 'gamma',
                                                           'mu', 'nu', 'delta_V',
                                                           'delta_R', 'delta_VR'),
                       intervention_parameter_names = list('starts', 'stops', 'coverages'),
                       initial_conditions = vector(mode = 'list', length = 7),
                       transmission_parameters = vector(mode = 'list', length = 8),
                       intervention_parameters = vector(mode = 'list', length = 3),
                       age_ranges = vector(mode = 'list'),
                       n_age_categories = NA_real_,
                       contact_matrix = matrix(NA)
                     )
)

# Setter and getter methods for initial_conditions of an age-structured
# SEIRDV model.

#' @describeIn SEIRDVAge Retrieves initial_conditions for an
#' age-structured SEIRDV model.
#'
#' @param object An object of the class SEIRDVAge.
#'
#' @return Initial conditions of SEIRDVAge model.
#' @export
#' 
setMethod('initial_conditions', 'SEIRDVAge',
          function(object) object@initial_conditions)

#' @describeIn SEIRDVAge Sets initial_conditions of an age-structured
#' SEIRDV model.
#'
#' If the initial conditions provided to do not sum to 1 or of different
#' sizes compared to the number of age groups, an error is thrown.
#'
#' @param object An object of the class SEIRDVAGE.
#' @param value a named list of (S0, E0, I0, R0, V0, VR) where each element can be a list
#' of vector of doubles, with each element corresponding to the fraction for a
#' single age group.
#'
#' @return Updated version of the age-structured SEIRDV model.
#' @export
#' 
setMethod(
  'initial_conditions<-', 'SEIRDVAge',
  function(object, value) {
    S0 = value$S0
    E0 = value$E0
    I0 = value$I0
    R0 = value$R0
    V0 = value$V0
    VR0 = value$VR0
    D0 = value$D0
    # check that ICs are valid
    if (abs(sum(S0, E0, I0, R0, V0, VR0, D0)-1)>=10^(-3)) {
      stop('Invalid initial conditions. Must sum to 1.')
    }
    
    # create list of parameter values
    ic <- list(S0, E0, I0, R0, V0, VR0, D0)
    
    # add names to each value
    names(ic) = object@initial_condition_names
    
    # raise errors if age category dimensions do not match initial state vectors
    # also raise errors if initial state and parameter values are not doubles
    for (p in list('S0', 'E0', 'I0', 'R0', 'V0', 'VR0', 'D0')){
      if(length(ic[[p]]) != object@n_age_categories){
        stop(glue('Wrong number of age groups for {p}
              compartments.'))}
      if(!is.numeric(ic[[p]])){
        stop(glue('{p} format must be numeric'))}
    }
    if(abs(sum(S0, E0, I0, R0, V0, VR0, D0)-1)>=10^(-3)){
      stop('All compartments need to sum up to 1.')
    }
    
    # if all above tests are passed, assign the ic namelist to the object
    object@initial_conditions <- ic
    
    return(object)
  })

# Setter and getter methods for transmission_parameters of an age-structured
# SEIRDV model.

#' @describeIn SEIRDVAge Retrieves transmission_parameters for an
#' age-structured SEIRDV model.
#'
#' @param object An object of the class SEIRDVAge.
#'
#' @return Transmission parameters of SEIRDVAge model.
#' @export
#' 
setMethod('transmission_parameters', 'SEIRDVAge',
          function(object) object@transmission_parameters)

#' @describeIn SEIRDVAge Sets transmission_parameters of an
#' age-structured SEIRDV model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param value a named list of form list(beta=, kappa=, gamma=, mu=, nu=,
#' delta_V=, delta_R=, delta_VR=)
#'
#' All rates of change between compartments are equal regardless of
#' age group.
#'
#' @return Updated version of the age-structured SEIRDV model.
#' @export
#' 
setMethod(
  'transmission_parameters<-', 'SEIRDVAge',
  function(object, value) {
    
    # create list of parameter values
    beta <- value$beta
    kappa <- value$kappa
    gamma <- value$gamma
    mu <- value$mu
    nu <- value$nu
    delta_V <- value$delta_V
    delta_R <- value$delta_R
    delta_VR <- value$delta_VR
    
    trans_params <- list(beta, kappa, gamma, mu, nu, delta_V, delta_R, delta_VR)
    
    # add names to each value
    names(trans_params) = object@transmission_parameter_names
    
    # check format of parameters beta, kappa and gamma
    if(length(beta) != 1 |
       length(kappa) != 1 |
       length(gamma) != 1 |
       length(delta_V) != 1 |
       length(delta_R) != 1 |
       length(delta_VR) != 1){
      stop('The parameter values should be 1-dimensional.')
    }
    
    if(length(mu) != 1 & length(mu) != object@n_age_categories){
      stop('The mortality parameter values should be of length 1 or
            number of age classes.')
    }
    
    if(length(nu) != 1 & length(nu) != object@n_age_categories){
      stop('The vaccination rate parameter values should be of length 1 or
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

# SEIRDV class specific functions

#' @describeIn SEIRDVAge Retrieves intervention parameters of age-structured SEIRDV model.
#'
#' @param object An object of the class SEIRDVAge.
#' 
#' @export
setMethod("intervention_parameters", "SEIRDVAge",
          function(object) object@intervention_parameters)

#' @describeIn SEIRDVAge Setter method for intervention parameters of the 
#' age-structured SEIRV model.
#'
#' Intervention parameters have same size. A tanh function is used to smooth interventions during simulation. This class is designed for interventions
#' which last several days at least and have several days between them; interventions involving rapid fluctuations may be distorted.
#'
#' @param object an object of the class SEIRDVAge
#' @param value (list) list of intervention parameters: starts, stops and
#'              coverages. Coverages can be either a vector, each value representing
#'              the level of intervention for the corresponding pair of start-stop
#'              time points, or a list of vectors, each element of the list representing
#'              the same timeline corresponding to each age-group
#'
#' @return object of class SEIRDVAge with intervention parameters assigned.
#' 
#' @export
setMethod(
  "intervention_parameters<-", "SEIRDVAge",
  function(object, value) {
    
    if (mean(names(value) %in% object@intervention_parameter_names) != 1)
      stop(paste0("Intervention parameters must contain: ",
                  object@intervention_parameter_names))
    interv_par <- value
    
    # raise errors if intervention parameters are not doubles
    for (p in list("starts", "stops")) {
      if (!is.numeric(interv_par[[p]])) {
        stop(glue("{p} format must be numeric."))
      }
    }
    if(!is.numeric(interv_par$coverages) &
       !(is.list(interv_par$coverages) & is.numeric(unlist(interv_par$coverages)))) {
      stop("coverages format must be numeric or list of numerics.")
    }
    
    # check that the intervention parameters are all of the same size
    if (length(interv_par$starts) != length(interv_par$stops)) {
      stop("Invalid intervention parameters. Must have same size.")
    }
    
    if(is.numeric(interv_par$coverages)){
      if(length(interv_par$starts) != length(interv_par$coverages)){
        stop("Invalid intervention parameters. Must have same size.")
      }
    }
    else{
      if(length(interv_par$coverages) != object@n_age_categories){
        stop("Wrong number of age groups for coverages for intervention parameters.")
      }
      if(all(lengths(interv_par$coverages) != rep(length(interv_par$starts),
                                                 object@n_age_categories))){
        stop("Invalid intervention parameters. Must have same size for each coverage.")
      }
    }
    
    object@intervention_parameters <- interv_par
    
    object
  })

#' @describeIn SEIRDVAge Method to simulate output using from model.
#' 
#' Solves a system of ODEs which form an
#' age-structured SEIRDV model. The system of equations for the time
#' evolution of population fractions in Susceptible (S), Exposed (E), Infected
#' (I), Recovered (R), Vaccinated (V) and Dead (D) groups in a given age group indexed by i is 
#' given by
#'
#' \deqn{\frac{dS_i(t)}{dt} = - \beta S_i(t) \Sigma_{j}C_{ij} I_j(t) - \nu Inter(t) S_i(t) + \delta_V V_i(t) + \delta_R R_i(t) + \delta_VR VR_i(t)}
#' \deqn{\frac{dE_i(t)}{dt} = \beta S_i(t) \Sigma_{j}C_{ij} I_j(t) - \kappa E_i(t)}
#' \deqn{\frac{dI_i(t)}{dt} = \kappa E_i(t) - \gamma I_i(t) - \mu I_i(t)}
#' \deqn{\frac{dR_i(t)}{dt} = \gamma I_i(t) -  \delta_R R_i(t) - \nu Inter(t) R_i(t)}
#' \deqn{\frac{dV(t)}{dt} = \nu Inter(t) S_i(t) - \delta_V V_i(t)}
#' \deqn{\frac{dVR(t)}{dt} = \nu Inter(t) R_i(t) - \delta_VR VR_i(t)}
#' \deqn{\frac{dC(t)}{dt} = \beta S_i(t) \Sigma_{j}C_{ij} I_j(t)}
#' \deqn{\frac{dD_i(t)}{dt} = \mu I_i(t)}
#' 
#' where C is a contact matrix whose elements represents the
#' contact between different age groups (rows) with age groups of
#' people they come in contact with (columns). This function relies on the 
#' package deSolve to numerically integrate the set of equations above.
#' 
#'
#' @param object An object of the class SEIRDVAge.
#' @param times (vector) time sequence over which to solve the model.
#'        Must be of the form seq(t_start,t_end,by=t_step). Default time series
#'        is seq(0, 100, by = 1).
#' @param solve_method A string indicating the chosen numerical integration
#' method for solving the ode system. Default is `lsoda` which is also the
#' default for the ode function in the deSolve package used in this function.
#'
#' @return data frame containing the time vector and time series of S, E, I, R, V, VR and
#' D population fractions for each age group outputs with incidence numbers
#' for each age group.
#' 
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
setMethod(
  "run", 'SEIRDVAge',
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
    if (is.null(unlist(object@transmission_parameters)))
      stop("Transmission parameters must be set before running.")
    if (is.null(unlist(object@initial_conditions)))
      stop("Initial conditions must be set before running.")
    if (is.null(unlist(object@intervention_parameters)))
      stop("Intervention parameters must be set before running.")
    
    #fetch number of age categories
    n_age <- object@n_age_categories
    
    # set initial state vector
    state <- c(S = initial_conditions(object)$S0,
               E = initial_conditions(object)$E0,
               I = initial_conditions(object)$I0,
               R = initial_conditions(object)$R0,
               V = initial_conditions(object)$V0,
               VR = initial_conditions(object)$VR0,
               D = initial_conditions(object)$D0,
               cc = rep(0, n_age))
    
    # set parameters vector
    parameters <- list(b = transmission_parameters(object)$beta,
                    k = transmission_parameters(object)$kappa,
                    g = transmission_parameters(object)$gamma,
                    m = transmission_parameters(object)$mu,
                    n = transmission_parameters(object)$nu,
                    d_v = transmission_parameters(object)$delta_V,
                    d_r = transmission_parameters(object)$delta_R,
                    d_vr = transmission_parameters(object)$delta_VR)
    
    # fetch contact matrix of the instance
    C = object@contact_matrix
    
    # set intervention parameters vector
    
    # use tstep=0.1 and tanh_slope=1 for good nice step-function-like shape of
    # the intervention wave
    sim_parms <- SimulationParameters(start =0, stop = tail(times, n=1),
                                      tstep = 0.1)
    
    if(is.numeric(intervention_parameters(object)$coverages)){
      int_parms <- 
        InterventionParameters(
          start=intervention_parameters(object)$starts,
          stop=intervention_parameters(object)$stops,
          coverage= intervention_parameters(object)$coverages)
      inter_prot <- intervention_protocol(int_parms, sim_parms, 1) %>%
        filter(time %in% times) %>%
        .$coverage
    }
    else{
      i = 1
      inter_prot <- vector(mode = "list", length = n_age)
      for(age_cov in intervention_parameters(object)$coverages){
        int_parms <- 
          InterventionParameters(
            start=intervention_parameters(object)$starts,
            stop=intervention_parameters(object)$stops,
            coverage= age_cov)
        inter_prot[[i]] <- intervention_protocol(int_parms, sim_parms, 1) %>%
          filter(time %in% times) %>%
          .$coverage
        i <- i+1
      }
    }
    
    intervention <- function(t){
      if(is.numeric(inter_prot)){
        interv_func <- approxfun(times, inter_prot, rule=2)(t)
      }
      else{
        interv_func <- numeric(n_age)
        for(i in 1:n_age){
          interv_func[i] <- approxfun(times, inter_prot[[i]], rule=2)(t)
        }
      }
      
      interv_func
    }
    
    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters, input) {
      with(
        as.list(c(state, parameters)),
        {
          inter <- input(t)
          S <- state[1:n_age]
          E <- state[(n_age + 1):(2 * n_age)]
          I <- state[(2 * n_age + 1):(3 * n_age)]
          R <- state[(3 * n_age + 1):(4 * n_age)]
          V <- state[(4 * n_age + 1):(5 * n_age)]
          VR <- state[(5 * n_age + 1):(6 * n_age)]
          D <- state[(6 * n_age + 1):(7 * n_age)]
          cc <- state[(7 * n_age + 1):(8 * n_age)]
          
          
          # rate of change
          dS <- -b * S * C %*% I - n * inter * S + d_v * V + d_r * R + d_vr * VR
          dE <- b * S * C %*% I - k * E
          dI <- k * E - g * I  - m * I
          dR <- g * I - d_r * R - n * inter * R
          dV <- n * inter * S - d_v * V
          dVR <- n * inter * R - d_vr * VR
          dD <- m * I
          dcc <- b * S * C %*% I
          # return the rate of change
          list(c(dS, dE, dI, dR, dV, dVR, dD, dcc))
        })
    }
    
    # call ode solver
    out <- ode(
      y = state, times = times, func = right_hand_side,
      parms = parameters, method = solve_method, input = intervention)
    
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
                             replicate(n_compartment_measurements, "V"),
                             replicate(n_compartment_measurements, "VR"),
                             replicate(n_compartment_measurements, "D"),
                             replicate(n_compartment_measurements, "cc"))
    
    out_temp$age_range = unlist(rep(object@age_ranges, each=length(times)))
    
    # drop the old variable column
    out_temp = out_temp %>% 
      dplyr::select(-.data$variable) %>% 
      dplyr::mutate(compartment=as.factor(.data$compartment)) %>% 
      dplyr::mutate(compartment=forcats::fct_relevel(.data$compartment, "S", "E", "I", "R", "V", "VR", "D", "cc")) %>% 
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