#' Defines an age-structured SEIR model and solves the set of ordinary
#' differential equations of the model with a chosen method of numerical
#' integration.
#'
#' @slot name character representing name of model
#' @slot output_names names of the compartments which are used by the
#'     model.
#' @slot parameter_names names of the initial conditions and parameters
#'     which are used by the model.
#' @slot parameters named list containing parameters of the model.
#'     Initial values for each compartment, S0, E0, I0, R0.
#'     Other parameters: b, k, g which represent the rates of changes
#'     between the compartments.
#' @slot n_age_categories number of age categories.
#' @import deSolve
#' @import glue

setClass('SEIRAge',
         # slots
         slots = c(
           name = 'character',
           output_names = 'list',
           parameter_names = 'list',
           parameters = 'list',
           n_age_categories = 'numeric'
         ),

         # prototypes for the slots, automatically set output and param
         # names
         prototype = list(
           name = NA_character_,
           output_names = list('S', 'E', 'I', 'R', 'Incidence'),
           parameter_names = list('S0', 'E0', 'I0', 'R0', 'b', 'k', 'g'),
           parameters = vector(mode = "list", length = 7),
           n_age_categories = NA_real_

         )
)

#' @describeIn Retrieves parameters for an age-structured simple SEIR model.
#'
#' @param object An object of the class SEIRAge.
setGeneric('get_parameters', function(object) standardGeneric('get_parameters'))
setMethod('get_parameters', 'SEIRAge', function(object) object@parameters)

#' @describeIn Setter and getter methods for parameters of
#'  an age-structured simple SEIR model.
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
#' @param b rate at which an infected individual exposes susceptible.
#' @param k rate at which exposed individuals become infected.
#' @param g rate at which infected individuals recover.
#'
#' All rates of change between compartments are equal regardless of
#' age group.
#'
#' @return updated version of the age-structured SEIR model.
setGeneric(
  'set_parameters',
  function(object, S0, E0, I0, R0, b, k, g){
    standardGeneric('set_parameters')
  })

setMethod(
  'set_parameters', 'SEIRAge',
  function(object, S0, E0, I0, R0, b, k, g) {

    # check that ICs are valid
    if (sum(S0, E0, I0, R0) != 1) {
      stop('Invalid initial conditions. Must add up to 1.')
    }

    # create list of parameter values
    params <- list(S0, E0, I0, R0, b, k, g)

    # add names to each value
    names(params) = object@parameter_names

    # raise errors if age category dimensions do not match initial state vectors
    # also raise errors if initial state and parameter values are not doubles
    for (p in list('S0', 'E0', 'I0', 'R0')){
      if(length(params[[p]]) != object@n_age_categories){
        stop(glue('Wrong number of age groups for {p}
              compartments.'))}
      if(!is.numeric(params[[p]])){
        stop(glue('{p} format must be numeric'))}
    }
    if(sum(S0, E0, I0, R0) != 1){
      stop('All compartments need to sum up to 1.')
    }

    # check format of parameters a and b
    if(any(length(b) != 1 | length(k) != 1 | length(g) != 1)){
      stop('The parameter values should be 1-dimensional.')
    }

    # if all above tests are passed, assign the params namelist to the object
    object@parameters <- params

    return(object)
  })


setGeneric(name = 'simulate_SEIRAge',
           def = function(object, times = seq(0, 100, by = 1),
                          solve_method = 'lsoda'){
             standardGeneric('simulate_SEIRAge')
           }
)

#' @describeIn Solves a system to ODEs which form an
#' age-structured simple SEIR model. The system of equations for the time
#' evolution of population fractions in susceptable (S), Exposed (E), Infected
#' (I) and Recovered (R) groups in a given age group indexed by i
#' is given by
#'
#' \deqn{\frac{dS_i(t)}{dt} = - b S_i(t) I_i(t)
#' \deqn{\frac{dE_i(t)}{dt} =  b S_i(t) I_i(t) - k E_i(t)
#' \deqn{\frac{dI_i(t)}{dt} = k E_i(t) - g I_i(t)
#' \deqn{\frac{dR_i(t)}{dt} = g I_i(t)
#'
#' This function relies on the packages deSolve and ggplot2.
#'
#' @param object An object of the class SEIRAge. Default time series
#' is seq(0, 100, by = 1).
#' @param times (vector) time sequence over which to solve the model.
#'        Must be of the form seq(t_start,t_end,by=t_step).
#' @param solve_method A string indicating the chosen numerical integration
#' method for solving the ode system. Default is 'lsoda' which is also the
#' default for the ode function in the desolve package used in this function.
#'
#' @return data frame containing the time vector and time series of S, R and I
#'  population fractions for each age group outputs with incidence numbers
#'  for each age group.
setMethod(
  'simulate_SEIRAge', 'SEIRAge',
  function(object, times, solve_method = 'lsoda') {

    # error if times is not a vector or list of doubles
    if(!is.double(times)){
      stop('Evaluation times of the model storage format must be a vector.')
    }

    age <- object@n_age_categories

    # set initial state vector
    state <- c(S = get_parameters(object)$S0,
               E = get_parameters(object)$E0,
               I = get_parameters(object)$I0,
               R = get_parameters(object)$R0)

    # set parameters vector
    parameters <- c(b = get_parameters(object)$b,
                    k = get_parameters(object)$k,
                    g = get_parameters(object)$g)

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
          dS <- -b*S*I
          dE <- b*S*I - k*E
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

    # output as a dataframe
    output <- as.data.frame.array(out)

    # compute incidence number
    total_inf <- output[, (2*age+2):(3*age+1)] + output[, (3*age+2):(4*age+1)]
    n_inc <- rbind(rep(0, age),
                   total_inf[2:nrow(total_inf),]-total_inf[1:nrow(total_inf)-1,]
                   )
    output$Incidence <- unname(as.matrix(n_inc))

    # Return only at integer times
    return(output)
  })

