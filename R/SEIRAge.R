#' Defines an age-structured SEIR model and solves the set of
#' ordinary differential equations of the model with a chosen method of
#' numerical integration.
#'
#' @slot name character representing name of model
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
#' @slot contact_matrix 
#' @slot n_age_categories number of age categories.
#' @slot age_ranges list of string characters representing the range of ages of
#' people in each age category. This object must have length
#' \code{n_age_categories} (otherwise an error is returned) and each element
#' must be formatted as 'age1-age2'.
#'
#' @import deSolve
#' @import glue
#' @import tidyverse
#' @import reshape2


setClass('SEIRAge',
         # slots
         slots = c(
           name = 'character',
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
           name = NA_character_,
           output_names = list('S', 'E', 'I', 'R', 'Incidence'),
           initial_condition_names = list('S0', 'E0', 'I0', 'R0'),
           transmission_parameter_names = list('b', 'k', 'g'),
           initial_conditions = vector(mode = "list", length = 4),
           transmission_parameters = vector(mode = "list", length = 3),
           age_ranges = vector(mode = 'list'),
           n_age_categories = NA_real_,
           contact_matrix = matrix(NA)

         )
)

# Setter and getter methods for transmission_parameters of an age-structured
# SEIR model.

#' @describeIn SEIRAge Retrieves initial_conditions for an
#' age-structured SEIR model.
#'
#' @param object An object of the class SEIRAge.
#'
#' @return Initial conditions of SEIRAge model.
setGeneric('initial_conditions',
           function(object) standardGeneric('initial_conditions'))

setMethod('initial_conditions', 'SEIRAge',
          function(object) object@initial_conditions)

#' @describeIn SEIRAge Sets initial_conditions of an age-structured
#' SEIR model.
#'
#' If the initial conditions provided to do not sum to 1 or of different
#' sizes compared to the number of age groups, an error is thrown.
#'
#' @param object An object of the class SEIRAge.
#' @param S0 initial fraction of the population that is susceptible
#'           by age group. Data can be provided as a list or vector of doubles
#'           each element corresponding to the fraction for a single age group.
#' @param E0 initial fraction of the population that has been exposed
#'           by age group.  Data can be provided as a list or vector of doubles
#'           each element corresponding to the fraction for a single age group.
#' @param I0 initial fraction of the population that is infectious
#'           by age group.  Data can be provided as a list or vector of doubles
#'           each element corresponding to the fraction for a single age group.
#' @param R0 initial fraction of the population that has recovered
#'           by age group. Data can be provided as a list or vector of doubles.
#'
#' All initial conditions must sum up to 1.
#'
#' @return Updated version of the age-structured SEIR model.
setGeneric(
  'initial_conditions<-',
  function(object, S0, E0, I0, R0){
    standardGeneric('initial_conditions<-')
  })

setMethod(
  'initial_conditions<-', 'SEIRAge',
  function(object, S0, E0, I0, R0) {

    # check that ICs are valid
    if (sum(S0, E0, I0, R0) != 1) {
      stop('Invalid initial conditions. Must add up to 1.')
    }

    # create list of parameter values
    ic <- list(S0, E0, I0, R0)

    # add names to each value
    names(ic) = object@initial_condition_names

    # raise errors if age category dimensions do not match initial state vectors
    # also raise errors if initial state and parameter values are not doubles
    for (p in list('S0', 'E0', 'I0', 'R0')){
      if(length(ic[[p]]) != object@n_age_categories){
        stop(glue('Wrong number of age groups for {p}
              compartments.'))}
      if(!is.numeric(ic[[p]])){
        stop(glue('{p} format must be numeric'))}
    }
    if(sum(S0, E0, I0, R0) != 1){
      stop('All compartments need to sum up to 1.')
    }

    # if all above tests are passed, assign the ic namelist to the object
    object@initial_conditions <- ic

    return(object)
  })

# Setter and getter methods for transmission_parameters of an age-structured
# SEIR model.

#' @describeIn SEIRAge Retrieves transmission_parameters for an
#' age-structured SEIR model.
#'
#' @param object An object of the class SEIRAge.
#'
#' @return Transmission parameters of SEIRAge model.
setGeneric('transmission_parameters',
           function(object) standardGeneric('transmission_parameters'))

setMethod('transmission_parameters', 'SEIRAge',
          function(object) object@transmission_parameters)

#' @describeIn  SEIRAge
#' @description Sets transmission_parameters of an
#' age-structured SEIR model.
#'
#' If the transmission parameters provided to are not 1-dimensional an error is
#' thrown.
#'
#' @param b rate at which an infected individual exposes susceptible.
#' @param k rate at which exposed individuals become infected.
#' @param g rate at which infected individuals recover.
#'
#' All rates of change between compartments are equal regardless of
#' age group.
#'
#' @return Updated version of the age-structured SEIR model.
setGeneric(
  'transmission_parameters<-',
  function(object, b, k, g){
    standardGeneric('transmission_parameters<-')
  })

setMethod(
  'transmission_parameters<-', 'SEIRAge',
  function(object, b, k, g) {

    # create list of parameter values
    trans_params <- list(b, k, g)

    # add names to each value
    names(trans_params) = object@transmission_parameter_names

    # check format of parameters b, k and g
    if(length(b) != 1 | length(k) != 1 | length(g) != 1){
      stop('The parameter values should be 1-dimensional.')
    }

    # if all above tests are passed, assign the trans_params namelist to the
    # object
    object@transmission_parameters <- trans_params

    return(object)
  })

# Method to simulate output using from SEIRAge model.

#' @describeIn SEIRAge Solves a system to ODEs which form an
#' age-structured simple SEIR model. The system of equations for the time
#' evolution of population fractions in Susceptible (S), Exposed (E), Infected
#' (I) and Recovered (R) groups in a given age group indexed by i is given by
#'
#' \deqn{\frac{dS_i(t)}{dt} = - b S_i(t) I_i(t)}
#' \deqn{\frac{dE_i(t)}{dt} =  b S_i(t) I_i(t) - k E_i(t)}
#' \deqn{\frac{dI_i(t)}{dt} = k E_i(t) - g I_i(t)}
#' \deqn{\frac{dR_i(t)}{dt} = g I_i(t)}
#'
#' This function relies on the package deSolve.
#'
#' @param object An object of the class SEIRAge.
#' @param times (vector) time sequence over which to solve the model.
#'        Must be of the form seq(t_start,t_end,by=t_step). Default time series
#'        is seq(0, 100, by = 1).
#' @param solve_method A string indicating the chosen numerical integration
#' method for solving the ode system. Default is `lsoda` which is also the
#' default for the ode function in the deSolve package used in this function.
#'
#' @return data frame containing the time vector and time series of S, R and I
#' population fractions for each age group outputs with incidence numbers
#' for each age group.
setGeneric(name = 'simulate_SEIRAge',
           def = function(object, times = seq(0, 100, by = 1),
                          solve_method = 'lsoda'){
             standardGeneric('simulate_SEIRAge')
           }
)

setMethod(
  'simulate_SEIRAge', 'SEIRAge',
  function(object, times, solve_method = 'lsoda') {

    # error if times is not a vector or list of doubles
    if(!is.double(times)){
      stop('Evaluation times of the model storage format must be a vector.')
    }
    
    #fetch number of age catagories
    age <- object@n_age_categories

    # set initial state vector
    state <- c(S = initial_conditions(object)$S0,
               E = initial_conditions(object)$E0,
               I = initial_conditions(object)$I0,
               R = initial_conditions(object)$R0)

    # set parameters vector
    parameters <- c(b = transmission_parameters(object)$b,
                    k = transmission_parameters(object)$k,
                    g = transmission_parameters(object)$g)
    
    # fetch contact matrix of the instance
    C = object@contact_matrix

    # function for RHS of ode system
    right_hand_side <- function(t, state, parameters) {
      with(
        as.list(c(state, parameters)),
        {
          S <- state[1:age]
          E <- state[(age+1):(2*age)]
          I <- state[(2*age+1):(3*age)]
          R <- state[(3*age+1):(4*age)]
          
          # rate of change
          dS <- -b*S*C%*%I
          dE <- b*S*C%*%I - k*E
          dI <- k*E - g*I
          dR <- g*I
          # return the rate of change
          list(c(dS, dE, dI, dR))
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
                             replicate(length(times)*age, "R"))
    out_temp$age_range = rep(object@age_ranges, each=length(times))

    #drop the old variable column
    out_temp = subset(out_temp, select = -c(variable) )

    # compute incidence number
    total_inf <- output[, (2*age+2):(3*age+1)] + output[, (3*age+2):(4*age+1)]
    n_inc <- rbind(rep(0, age),
                   total_inf[2:nrow(total_inf),]-total_inf[1:nrow(total_inf)-1,]
                   )

    # melt the incidence dataframe to long format
    incidence_temp = melt(n_inc)

    # add time, compartment and age_range columns as above
    incidence_temp$time = rep(times, age)
    incidence_temp$compartment = rep('Incidence', age*length(times))
    incidence_temp$age_range = rep(object@age_ranges, each=length(times))

    # drop the old variable column
    incidence_temp = subset(incidence_temp, select = -c(variable))

    # bind SEIR and incidence dataframes
    output = rbind(out_temp, incidence_temp)

    return(output)
  })

