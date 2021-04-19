#' An SEIR model for compartments of different symptoms, 
#'

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Class definitions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' @noRd
setClass("gg")

#' An S4 class of a basic SEIR model with infected population of different symptome compartments: asymptomatic, mild and severe.
#'
#' @slot name A string gives the name of the model
#' @slot initial_population A named list of the initial population size of each group with names: "S", "E", "I_asymptomatic", "I_mild", "I_severe", "R", "D_cumulative" 
#' @slot parameters A named list of the model parameters with names: "lam", "gamma", "omega", "e2i", "i2r", "pdeath"
#' @slot output A dataframe holding the ode simulation output, with columns as population groups and rows as simulation time points
#' @slot plot_output A ggplot holding the plot of the ode simulation output
#'
#' @import deSolve
#' @import ggplot2
#' @import reshape2
#' @import plyr
#' @import magrittr
#' 
#' @note
#' 1. Total initial population size is normalised to 1
#' 2. The current model does not include natural death or birth.

setClass(Class = "SEIaImIsR",
         slots = c(
           name = "character",
           initial_population = "list",
           parameters = "list",
           output = "list",
           plot_output = "gg"
         ),
         prototype = list(
           name = NA_character_,
           initial_population = vector(mode = "list", length = 7) %>%
             setNames(list("S", "E", "I_asymptomatic", "I_mild", "I_severe", "R", "D_cumulative")),
           parameters = vector(mode = "list", length = 6) %>%
             setNames(list("lam", "gamma", "omega", "e2i", "i2r", "pdeath")),
           output = data.frame(),
           plot_output = ggplot2::ggplot()
         ))


              
#' @describeIn SEIaImIsR set initial population sizes (in fraction) and parameters
#'
#' @param object An object of class SEIaImIsR
#' @param S Numeric, initial population of the susceptible
#' @param E Numeric, initial population of the exposed
#' @param I_asymptomatic Numeric, initial population of the infected with no symptom
#' @param I_mild Numeric, initial population of the infected with mild symptoms
#' @param I_severe Numeric, initial population of the infected with severe symptoms
#' that need further hospitalization
#' @param D_cumulative Numeric, initial cumulative mortality
#' @param R Numeric, initial population of the recovered
#' @param lam Numeric, rate at which an infected individual exposes susceptible
#' @param gamma Numeric, rate at which exposed individuals become infected
#' @param omega Numeric, rate at which recovered individuals become susceptible
#' @param e2i Numeric vector, fractions of different symptome compartments (I_asymptomic, I_mild, I_severe)
#' @param i2r Numeric vector, rate at which each infected group recover
#' @param pdeath Numeric vector, rate of disease-caused mortality of each infected group
#' @return An object of class SEIaImIsR with initial population and parameters
#'
setGeneric(name = "set_init",
           def = function(object, S=NA_real_, E=NA_real_,
                          I_asymptomatic=NA_real_, I_mild=NA_real_, I_severe=NA_real_,
                          R=NA_real_, D_cumulative=NA_real_,
                          lam=NA_real_, gamma=NA_real_, omega=NA_real_,
                          e2i=list(), i2r=list(), pdeath=list()) standardGeneric("set_init"))
setMethod("set_init", signature(object = "SEIaImIsR"),
          function(object, S, E, I_asymptomatic, I_mild, I_severe, R, D_cumulative, lam, gamma, omega, e2i, i2r, pdeath) {
            param_list <- list(lam, gamma, omega, e2i, i2r, pdeath)
            init_pop_list <- list(S, E, I_asymptomatic, I_mild, I_severe, R, D_cumulative)
            names(param_list) <- names(object@parameters)
            names(init_pop_list) <- names(object@initial_population)
            object@initial_population <- init_pop_list
            object@parameters <- param_list

            # check if initial settings are valid
            check <- check_init(object)
            if (check == TRUE) object
            else stop(paste(check, ", please check and rerun set_init.\n"))
          })

#' @describeIn SEIaImIsR to check validity of the settings of the parameters and initial conditions
#'
#' @param object An object of class SEIaImIsR
#' @return TRUE if passing all checks, or an error message if any check fails
#'
setGeneric(name = "check_init", def = function(object) standardGeneric("check_init"))
setMethod("check_init", signature(object = "SEIaImIsR"),
          function(object) {
            errors <- character()
            # check whether all required parameters are set
            is_na_params <- is.na(object@parameters)
            if (sum(is_na_params) != 0) {
              msg <- paste("Missing parameters:",
                           paste(names(object@parameters)[is_na_params], collapse = ", ")
              )
              errors <- c(errors, msg)
            }
            # check whether all required initial population groups are set
            is_na_pop <- is.na(object@initial_population)
            if (sum(is_na_pop) != 0) {
              msg <- paste("Missing initial setting for population group:",
                           paste(names(object@initial_population)[is_na_pop], collapse = ", ")
              )
              errors <- c(errors, msg)
            }else{
              # check whether the sum of initial population is normalized to 1
              sum_init_pop <- sum(unlist(object@initial_population))
              if (sum_init_pop != 1) {
                msg <- "Sum of initial population is not 1, please normalize"
                errors <- c(errors, msg)
              }
            }
            # check whether the lengths of i2r and pdeath correspond to the i2rmber of infected groups
            n_i2r <- length(object@parameters$i2r)
            if (n_i2r != 3) {
              msg <- paste0(
                "Length of parameter i2r,", n_i2r, ", is not equal to the setting ", 3)
              errors <- c(errors, msg)
            }
            n_pdeath <- length(object@parameters$pdeath)
            if (n_pdeath != 3) {
              msg <- paste0(
                "Length of parameter pdeath,", n_pdeath, ", is not equal to the setting ", 3)
              errors <- c(errors, msg)
            }
            if (length(errors) == 0) TRUE else errors
          })


#' @describeIn SEIaImIsR to solve the ode system.
#'
#' Model:
#' \deqn{\frac{dS}{dt} = -\lambda S(I_asymptomatic + I_mild + I_severe) + \omega R}
#' \deqn{\frac{dE}/{dt} = \lambda S(I_asymptomatic + I_mild + I_severe) - \gamma E}
#' \deqn{\frac{dI_asymptomatic}{dt} = e2i.i_asymptomic \gamma E - i2r.i_asymptomatic I_asymptomatic - pdeath.i_asymptomatic I_asymptomatic}
#' \deqn{\frac{dI_mild}{dt} = e2i.i_mild\gamma E - i2r.i_mild I_mild - pdeath.i_mild I_mild}
#' \deqn{\frac{dI_severe}{dt} = e2i.i_severe\gamma E - i2r.i_severe I_severe  - pdeath.i_severe I_severe}
#' \deqn{\frac{dR}{dt} = -\omega R + i2r.i_mild I_mild + i2r.i_severe I_severe}
#' \deqn{\frac{dD_cumulative}{dt} =  pdeath.i_mild  I_mild + pdeath.i_severe I_severe}
#'
#' @param object An object of class SEIaImIsR
#' @param times A list of time points of the simulation period
#' @param method A string indicating which ode integrator to use. Default is set to 'lsoda'
#' @return An object of class SEIaImIsR with a dataframe of simulation output
#'
setGeneric(name = "ode_simulate",
           def = function(object, times, method = "lsoda") standardGeneric("ode_simulate"))

setMethod("ode_simulate", signature(object = "SEIaImIsR"),
  function(object, times, method) {
    # initial population groups
    pop_groups <- c(S = object@initial_population$S,
                    E = object@initial_population$E,
                    I_asymptomatic = object@initial_population$I_asymptomatic,
                    I_mild = object@initial_population$I_mild,
                    I_severe = object@initial_population$I_severe,
                    R = object@initial_population$R,
                    D_cumulative = object@initial_population$D_cumulative)
    # parameters
    params <- c(lam = object@parameters$lam,
                gamma = object@parameters$gamma,
                omega = object@parameters$omega,
                e2i = object@parameters$e2i,
                i2r = object@parameters$i2r,
                pdeath = object@parameters$pdeath)

    # ODE system RHS
    ode_symptome_rhs <- function(t, pop_groups, parameters) {
      with(
        as.list(c(pop_groups, params)), {
          dS <- -lam * S * (I_asymptomatic + I_mild + I_severe) + omega * R
          dE <- lam * S * (I_asymptomatic + I_mild + I_severe) - gamma * E
          dI_asymptomatic <- (1.0 - e2i.i_mild - e2i.i_severe) * gamma * E - i2r.i_asymptomatic * I_asymptomatic  - pdeath.i_asymptomatic * I_asymptomatic
          dI_mild <- e2i.i_mild * gamma * E - i2r.i_mild * I_mild  - pdeath.i_mild * I_mild
          dI_severe <- e2i.i_severe * gamma * E - i2r.i_severe * I_severe  - pdeath.i_severe * I_severe
          dR <- -omega * R + i2r.i_asymptomatic * I_asymptomatic + i2r.i_mild * I_mild + i2r.i_severe * I_severe
          dD_cumulative <-  pdeath.i_asymptomatic * I_asymptomatic + pdeath.i_mild * I_mild + pdeath.i_severe * I_severe
          # return the rate of cI_severenge
          list(c(dS, dE, dI_asymptomatic, dI_mild, dI_severe, dR, dD_cumulative))
        })
    }
    # solving ode
    output <- ode(y = pop_groups,
                  times = times,
                  func = ode_symptome_rhs,
                  parms = params,
                  method = method)
    output <- as.data.frame(output)
    object@output <- output
    return(object)
    })

#' @describeIn SEIaImIsR to plot the outcome of the ode similuation
#'
#' @param object An object of class SEIaImIsR
#' @return An object of class SEIaImIsR with a plot showing the ode simulation outcome
#'
setGeneric(name = "plot_ode_output",
           def = function(object) standardGeneric("plot_ode_output"))
setMethod("plot_ode_output", signature(object = "SEIaImIsR"),
          function(object) {
            output <- object@output
            if (empty(output)) {
            error <- "Empty output. \
            Please first run the ode simulation by calling ode_simulate."
            return(error)
            }else{
              # reshape data frame
              output.melt <- melt(output, id.vars = "time")
              names(output.melt) <- c("time", "population_group", "fraction")
              # plot the each group
              p <- ggplot(data = output.melt, aes(x = time, y = fraction)) +
                geom_line(aes(colour = population_group)) +
                theme_classic()
              object@plot_output <- p
              return(object)
            }
          })
