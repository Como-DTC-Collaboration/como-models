#' Defines an age-structured simple SEIR model.
#' Slots of this class are as follows
#'
#' @slot name character representing name of model
#' @slot output_names names of the compartments which are used by the
#'     model.
#' @slot parameter_names names of the initial conditions and parameters
#'     which are used by the model.
#' @slot parameters named list containing parameters of the model.
#'     Initial values for each compartment, S0, E0, I0, R0.
#'     Other parameters: a, b, c which represent the rates of changes
#'     between the compartments.
#' @slot n_age_categories number of age categories.
#'
#' @import deSolve
#' @import glue
#' @import ggplot2

setClass('age_model',
         #slots
         slots = c(
           name = 'character',
           output_names = 'list',
           parameter_names = 'list',
           parameters = 'list',
           n_age_categories = 'numeric'
         ),

         #prototypes for the slots, automatically set output and param
         #names
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
#' @param object An object of the class age_model.
setGeneric('get_parameters', function(object) standardGeneric('get_parameters'))

setMethod(
  'get_parameters', 'age_model',
  function(object) object@parameters
)

#' @describeIn Sets parameters for an age-structured simple SEIR model.
#'
#' Default parameters b = 1, k = 1 and g = 1.
#' If the initial conditions provided to do not sum to 1 or of different
#' sizes compared to the number of age groups, an error is thrown.
#'
#' @param object An object of the class age_model.
#' @param S0 initial fraction of the population that is susceptible
#'           by age group. Data can be provided as a list or vector of doubles
#'           each element corresponding to the fraction for a single age group.
#' @param E0 initial fraction of the population that has been exposed
#'           by age group.  Data can be provided as a list or vector of doubles
#'           each element corresponding to the fraction for a single age group.
#' @param I0 initial fraction of the population that is infected
#'           by age group.  Data can be provided as a list or vector of doubles
#'           each element corresponding to the fraction for a single age group.
#' @param R0 initial fraction of the population that has recovered
#'           by age group. Data can be provided as a list or vector of doubles.
#'
#' All initial conditions must sum up to 1.
#'
#' @param b rate at which an infected individual exposes susceptible
#' @param k rate at which exposed individuals become infected
#' @param g rate at which infected individuals recover
#'
#' All rates of change between compartments are equal regardless of
#' age group.
#'
#' @return updated version of the age-structured SEIR model.
setGeneric(
  'set_parameters',
  function(object, S0, E0, I0, R0, a = 1, b = 1, c = 1){
    standardGeneric('set_parameters')
  })

setMethod(
  'set_parameters', 'age_model',
  function(object, S0, E0, I0, R0, b, k, g) {

    # check that ICs are valid
    if (sum(S0, E0, I0, R0) != 1) {
      stop('Invalid initial conditions. Must add up to 1.')
    }

    #create list of parameter values
    params <- list(S0, E0, I0, R0, b, k, g)

    #add names to each value
    names(params) = object@parameter_names

    #raise errors if age category dimensions do not match initial state vectors
    #also raise errors if initial state and parameter values are not doubles
    for (p in list('S0', 'E0', 'I0', 'R0')){
      if(length(params[[p]]) != object@n_age_categories){
        stop(glue('Wrong number of age groups for {p}
              compartments.'))}
      if(!is.double(params[[p]])){
        stop(glue('{p} storage format must be a vector.'))}
    }
    if(sum(S0, E0, I0, R0) != 1){
      stop('All compartments need to sum up to 1.')
    }

    #check format of parameters a and b
    if(any(length(b) != 1 | length(k) != 1 | length(g) != 1)){
      stop('The rates of change between compartments are 1-dimensional.')
    }

    #if all above tests are passed, assign the params namelist to the object
    object@parameters <- params

    return(object)
  })

setGeneric(name = 'simulate',
           def = function(object, times = seq(0, 100, by = 1), is_plot=TRUE){
             standardGeneric('simulate')
           }
)

#' @describeIn Solves an age-structured simple SEIR model.
#'
#' Default time series is seq(0, 100, by = 1).
#' This function relies on the packages deSolve and ggplot2.
#' This function creates a plot of the variables over time and returns a
#' vectors of the incidence numbers for each age group.
#'
#' @param object An object of the class age_model.
#' @param times (vector) time sequence over which to solve the model.
#'        Must be of the form seq(t_start,t_end,by=t_step).
#' @param is_plot (logical) whether plots for the compartments for the
#'        different age groups are drawn.
#'
#' @return data frame of time and outputs with incidence numbers for each
#'         age group.
setMethod(
  'simulate', 'age_model',
  function(object, times, is_plot) {
    if(!is.double(times)){
      stop('Evaluation times of the model storage format must be a vector.')
    }
    if(!is.logical(is_plot)){
      stop('Trigger for plotting must be logical.')
    }
    age <- object@n_age_categories

    #set initial state vector
    state <- c(S = get_parameters(object)$S0,
               E = get_parameters(object)$E0,
               I = get_parameters(object)$I0,
               R = get_parameters(object)$R0)

    #set parameters vector
    parameters <- c(b = get_parameters(object)$b,
                    k = get_parameters(object)$k,
                    g = get_parameters(object)$g)

    #function for RHS of ode system
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

    #call ode solver
    out <- ode(
      y = state, times = times, func = right_hand_side, parms = parameters)

    output <- as.data.frame.array(out)

    if(is_plot == TRUE){
      col <- c('cS' = 'blue', 'cE' = 'green', 'cI' = 'yellow', 'cR' = 'red')
      for(a in 1:age){
        out_df <- output[,c(1, a+1, age+a+1, 2*age+a+1, 3*age+a+1)]
        names(out_df) <- c('time', 'S', 'E', 'I', 'R')
        SEIRplot <- ggplot(out_df, aes(x=time)) +
          geom_line(aes(y=S, color = 'cS'), size = 1.5) +
          geom_line(aes(y=E, color = 'cE'), size = 1.5) +
          geom_line(aes(y=I, color = 'cI'), size = 1.5) +
          geom_line(aes(y=R, color = 'cR'), size = 1.5) +
          labs(x = 'time', y = 'fraction of the population',
               title = glue('SEIR model for age group {a}')) +
          theme(legend.position = 'right') +
          scale_color_manual(values = col)
        #show plot
        print(SEIRplot)

        #printtype of the plotting object to the command line for testing
        print(typeof(SEIRplot))
      }
    }

    # compute incidence number
    total_inf <- output[, (2*age+2):(3*age+1)] + output[, (3*age+2):(4*age+1)]
    n_inc <- rbind(rep(0, age),
                   total_inf[2:nrow(total_inf),]-total_inf[1:nrow(total_inf)-1,]
                   )
    output$Incidence <- unname(as.matrix(n_inc))

    # Return only at integer times
    return(output)
  })

