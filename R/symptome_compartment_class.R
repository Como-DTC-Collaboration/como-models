#' An SEIR model for compartments of different symptoms
#'
#' Note:
#' 1) Sum of poulation equals to 1 at any time point.
#' An error will be reported the sum of initial population is not equal to 1.
#' 2) At this stage, we only consider disease-caused death cases
#' calculating mortality.
#'
#' model structure:
#'    ---------------------------(omega)---------------------------------
#'    |                                                                 |
#'    |                                                                 |
#'    --> S --(lam)--> E --(gamma) ------(1-ihr)-------> I ---(nu.i)--> R <--
#'                                 |                     |                  |
#'                                 |                 (pdeath.i)             |
#'                                 |                     |                  |
#'                                 |            --> CM <--                  |
#'                                 |            |                           |
#'                                 |      (pdeath.hsa)                      |
#'                                 |            |                           |
#'                                 -- (ihr)---> HA --(nu.hsa)----------------

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Class definitions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' @describeIn model_symptome to set up dependent classes for model_symptome
setClass("ggplot")
setClass("gg")

#' An S4 class of an basic SEIR model with compartments of different symptomes
#'
#' @slot name A string gives the name of the model
#' @slot initial_population A list of the initial population size of each group
#' @slot parameters A list of the model parameters
#' @slot output A dataframe holding the ode simulation output, with columns as population groups and rows as simulation time points
#' @slot plot_output A ggplot holding the plot of the ode simulation output
#'
#' @import deSolve
#' @import ggplot2
#' @import reshape2
#' @import plyr
#' @import magrittr

setClass(Class = "model_symptome",
         slots = c(
           name = "character",
           initial_population = "list",
           parameters = "list",
           output = "list",
           plot_output = "gg"
         ),
         prototype = list(
           name = NA_character_,
           initial_population = vector(mode = "list", length = 6) %>%
             setNames(list("S", "E", "I", "HA", "R", "CM")),
           parameters = vector(mode = "list", length = 6) %>%
             setNames(list("lam", "gamma", "omega", "ihr", "nu", "pdeath")),
           output = data.frame(),
           plot_output = ggplot()
         ))
              
#' @describeIn model_symptome set initial population sizes (in fraction) and parameters
#'
#' @param object An object of class model_symptome
#' @param S Numeric, initial population of the susceptible
#' @param E Numeric, initial population of the exposed
#' @param I Numeric, initial population of the infected with mild/no symptom
#' @param HA Numeric, initial population of the infected with severe symptoms
#' that need further hospitalization
#' @param CM Numeric, initial cumulative mortality
#' @param R Numeric, initial population of the recovered
#' @param lam Numeric, rate at which an infected individual exposes susceptible
#' @param gamma Numeric, rate at which exposed individuals become infected
#' @param omega Numeric, rate at which recovered individuals become susceptible
#' @param ihr Numeric, fraction of infected individuals develop severe symptoms
#' @param nu Numeric vector, rate at which each infected group recover
#' @param pdeath Numeric vector, rate of disease-caused mortality of each infected group
#' @return An object of class model_symptome with initial population and parameters
#'
setGeneric(name = "set_init",
           def = function(object, S=NA_real_, E=NA_real_, I=NA_real_,
                          HA=NA_real_, R=NA_real_, CM=NA_real_,
                          lam=NA_real_, gamma=NA_real_, omega=NA_real_,
                          ihr=NA_real_, nu=list(), pdeath=list()) standardGeneric("set_init"))
setMethod("set_init", signature(object = "model_symptome"),
          function(object, S, E, I, HA, R, CM, lam, gamma, omega, ihr, nu, pdeath) {
            param_list <- list(lam, gamma, omega, ihr, nu, pdeath)
            init_pop_list <- list(S, E, I, HA, R, CM)
            names(param_list) <- names(object@parameters)
            names(init_pop_list) <- names(object@initial_population)
            object@initial_population <- init_pop_list
            object@parameters <- param_list

            # check if initial settings are valid
            check <- check_init(object)
            if (check == TRUE) object
            else stop(paste(check, ", please check and rerun set_init.\n"))
          })

#' @describeIn model_symptome to check validity of the settings of the parameters and initial conditions
#'
#' @param object An object of class model_symptome
#' @return TRUE if passing all checks, or an error message if any check fails
#'
setGeneric(name = "check_init", def = function(object) standardGeneric("check_init"))
setMethod("check_init", signature(object = "model_symptome"),
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
            # check whether the lengths of nu and pdeath correspond to the number of infected groups
            n_nu <- length(object@parameters$nu)
            if (n_nu != 2) {
              msg <- paste0(
                "Length of parameter nu,", n_nu, ", is not equal to the setting ", 2)
              errors <- c(errors, msg)
            }
            n_pdeath <- length(object@parameters$pdeath)
            if (n_pdeath != 2) {
              msg <- paste0(
                "Length of parameter pdeath,", n_pdeath, ", is not equal to the setting ", 2)
              errors <- c(errors, msg)
            }
            if (length(errors) == 0) TRUE else errors
          })


#' @describeIn model_symptome to solve the ode system.
#'
#' @param object An object of class model_symptome
#' @param times A list of time points of the simulation period
#' @param method A string indicating which ode integrator to use. Default is set to 'lsoda'
#' @return An object of class model_symptome with a dataframe of simulation output
#'
setGeneric(name = "ode_simulate",
           def = function(object, times, method = "lsoda") standardGeneric("ode_simulate"))

setMethod("ode_simulate", signature(object = "model_symptome"),
  function(object, times, method) {
    # initial population groups
    pop_groups <- c(S = object@initial_population$S,
                    E = object@initial_population$E,
                    I = object@initial_population$I,
                    HA = object@initial_population$HA,
                    R = object@initial_population$R,
                    CM = object@initial_population$CM)
    # parameters
    params <- c(lam = object@parameters$lam,
                gamma = object@parameters$gamma,
                omega = object@parameters$omega,
                ihr = object@parameters$ihr,
                nu = object@parameters$nu,
                pdeath = object@parameters$pdeath)

    # ODE system RHS
    ode_symptome_rhs <- function(t, pop_groups, parameters) {
      with(
        as.list(c(pop_groups, params)), {
          dS <- -lam * S * (I + HA) + omega * R
          dE <- lam * S * (I + HA) - gamma * E
          dI <- (1.0 - ihr) * gamma * E - nu.i * I  - pdeath.i * I
          dHA <- ihr * gamma * E - nu.hsa * HA  - pdeath.hsa * HA
          dR <- -omega * R + nu.i * I + nu.hsa * HA
          dCM <-  pdeath.i * I + pdeath.hsa * HA
          # return the rate of change
          list(c(dS, dE, dI, dHA, dR, dCM))
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

#' @describeIn model_symptome to plot the outcome of the ode similuation
#'
#' @param object An object of class model_symptome
#' @return An object of class model_symptome with a plot showing the ode simulation outcome
#'
setGeneric(name = "plot_ode_output",
           def = function(object) standardGeneric("plot_ode_output"))
setMethod("plot_ode_output", signature(object = "model_symptome"),
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
