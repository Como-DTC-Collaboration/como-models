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

#' Retrieves initial_conditions for an
#' age-structured SEIRD model.
#'
#' @param object An object of the class SEIRDAge.
#'
#' @return Initial conditions of SEIRDAge model.
#' @aliases initial_conditions,ANY,ANY-method
#' @rdname SEIRDAge-class
#' @export
setMethod('initial_conditions', 'SEIRDAge',
          function(object) object@initial_conditions)

#' Sets initial_conditions of an age-structured
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
#' @aliases initial_conditions<-,ANY,ANY-method
#' @rdname SEIRDAge-class
#' @export
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

#' Retrieves transmission_parameters for an
#' age-structured SEIRD model.
#'
#' @param object An object of the class SEIRDAge.
#'
#' @return Transmission parameters of SEIRDAge model.
#' @aliases transmission_parameters,ANY,ANY-method
#' @rdname SEIRDAge-class
#' @export
setMethod('transmission_parameters', 'SEIRDAge',
          function(object) object@transmission_parameters)
#' Sets transmission_parameters of an
#' age-structured SEIRD model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param value a named list of form list(b=, k=, g=, mu=)
#'
#' All rates of change between compartments are equal regardless of
#' age group.
#'
#' @return Updated version of the age-structured SEIRD model.
#' @aliases transmission_parameters<-,ANY,ANY-method
#' @rdname SEIRDAge-class
#' @export
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
    if(length(b) != 1 | length(k) != 1 | length(g) != 1){
      stop('The parameter values should be 1-dimensional.')
    }
    
    # Set the row and column names of the instance's contact matrix
    rownames(object@contact_matrix) <- object@age_ranges
    colnames(object@contact_matrix) <- object@age_ranges

    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params

    return(object)
  })

#' Method to simulate output using from model.
#' 
#' Solves a system of ODEs which form an
#' age-structured SEIRD model. The system of equations for the time
#' evolution of population fractions in Susceptible (S), Exposed (E), Infected
#' (I), Recovered (R) and Dead (D) groups in a given age group indexed by i is 
#' given by
#'
#' \deqn{\frac{dS_i(t)}{dt} = - \beta S_i(t) \Sigma_{j}C_{ij} I_j(t)}
#' \deqn{\frac{dE_i(t)}{dt} = \beta S_i(t) \Sigma_{j}C_{ij} I_j(t)} - \kappa E_i(t)}
#' \deqn{\frac{dI_i(t)}{dt} = \kappa E_i(t) - \gamma I_i(t)} - \mu I_i(t)}
#' \deqn{\frac{dR_i(t)}{dt} = \gamma I_i(t)}
#' \deqn{\frac{dD_i(t)}{dt} = \mu I_i(t)}

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
#' @return data frame containing the time vector and time series of S, R, I and
#' D population fractions for each age group outputs with incidence numbers
#' for each age group.
#' @rdname SEIRDAge-class
#' @aliases run,ANY,ANY-method
#' @importFrom magrittr "%>%"
#' @importFrom rlang .data
#' @export
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
    
    #fetch number of age catagories
    age <- object@n_age_categories

    # set initial state vector
    state <- c(S = initial_conditions(object)$S0,
               E = initial_conditions(object)$E0,
               I = initial_conditions(object)$I0,
               R = initial_conditions(object)$R0,
               D = initial_conditions(object)$D0)

    # set parameters vector
    parameters <- c(b = transmission_parameters(object)$b,
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
          S <- state[1:age]
          E <- state[(age + 1):(2 * age)]
          I <- state[(2 * age + 1):(3 * age)]
          R <- state[(3 * age + 1):(4 * age)]
          D <- state[(4 * age + 1):(5 * age)]
          
          
          # rate of change
          dS <- -b * S * C %*% I
          dE <- b * S * C %*% I - k * E
          dI <- k * E - g * I  - mu * I
          dR <- g * I
          dD <- mu * I
          # return the rate of change
          list(c(dS, dE, dI, dR, dD))
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
    out_temp$compartment = c(replicate(length(times)*age, "S"),
                             replicate(length(times)*age, "E"),
                             replicate(length(times)*age, "I"),
                             replicate(length(times)*age, "R"),
                             replicate(length(times)*age, "D"))

    out_temp$age_range = unlist(rep(object@age_ranges, each=length(times)))

    #drop the old variable column
    out_temp = out_temp %>% 
      dplyr::select(-.data$variable) %>% 
      dplyr::mutate(compartment=as.factor(compartment)) %>% 
      dplyr::mutate(compartment=forcats::fct_relevel(compartment, "S", "E", "I", "R","D")) %>% 
      dplyr::mutate(age_range=as.factor(age_range)) %>% 
      dplyr::mutate(age_range=forcats::fct_relevel(age_range, object@age_ranges))

    # compute incidence number
    total_inf <- output[, (2*age+2):(3*age+1)] + output[, (3*age+2):(4*age+1)]
    n_inc <- rbind(rep(0, age),
                   total_inf[2:nrow(total_inf),]-total_inf[1:nrow(total_inf)-1,]
                   )

    # melt the incidence dataframe to long format
    incidence_temp = melt(n_inc, id.vars=NULL)
    
    # add time, compartment and age_range columns as above
    incidence_temp$time = rep(times, age)
    incidence_temp$compartment = rep('Incidence', age*length(times))
    incidence_temp$age_range = unlist(rep(object@age_ranges, each=length(times)))

    # drop the old variable column
    incidence_temp = incidence_temp %>% 
      dplyr::select(-.data$variable)

    # bind SEIRD and incidence dataframes
    output = rbind(out_temp, incidence_temp)

    return(output)
  })

