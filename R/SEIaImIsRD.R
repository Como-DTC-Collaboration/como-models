#' An SEIRD model for compartments of different symptoms (asymptomatic, mild and severe) with different transmission rates (beta).
#'

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Class definitions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' @noRd
setClass("gg")

#' An S4 object representing the SEIRD model with infected population of different symptome compartments: asymptomatic, mild and severe.
#'
#' This class represents an extension of the SEIRD model, showing how populations of susceptible,
#' exposed, infectious and recovered individuals evolve over time, in which the infectious individuals
#' are subgrouped into compartments according to different severity of symptoms, i.e. asymptomatic, mild and severe.
#'
#' 1. Total initial population size is normalised to 1.
#' 2. The current model does not include natural death or birth.
#' 3. The current model defines different infection.
#'
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I_asymptomatic0", "I_mild0", "I_severe0", "R0", "D0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("beta", "kappa", "kappa", "mu").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters (double).
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#' @slot output dataframe of output in long format
#'
#' @import deSolve
#' @import ggplot2
#' @import reshape2
#' @import plyr
#' @import magrittr
#'
#' @concept objects
#' @export SEIaImIsRD
SEIaImIsRD <- setClass(Class = "SEIaImIsRD",
         slots = c(
           initial_condition_names = "list",
           transmission_parameter_names = "list",
           initial_conditions = "list",
           transmission_parameters = "list",
           output_names = "list",
           output = "list"
         ),
         prototype = list(
           initial_condition_names = list("S0", "E0", "I_asymptomatic0",
                                          "I_mild0", "I_severe0", "R0", "D0"),
           transmission_parameter_names = list("beta", "kappa", "omega",
                                              "p_symptom", "gamma", "mu"),
           initial_conditions = vector(mode = "list", length = 6),
           transmission_parameters = vector(mode = "list", length = 6),
           output_names = list("S", "E", "I_asymptomatic",
                               "I_mild", "I_severe", "R", "D"),
           output = data.frame()
         ))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Functions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#' set initial population sizes (in fraction) and parameters
#'
#' @param object An object of class SEIaImIsRD
#' @param S Numeric, initial fraction of the population that is susceptible
#' @param E Numeric, initial fraction of the population that is exposed
#' @param I_asymptomatic Numeric, initial fraction of the population that is infected with no symptom
#' @param I_mild Numeric, initial fraction of the population that is infected with mild symptoms
#' @param I_severe Numeric, initial fraction of the population that is infected severe symptoms
#' that need further hospitalization
#' @param D Numeric, initial fraction of the population that is dead due to the infection
#' @param R Numeric, initial fraction of the population that is recovered
#' @param beta Numeric vector, effective contact rate (i.e. rate at which an infected individual exposes susceptible) from different symptome compartments
#' @param kappa Numeric, rate of progression from exposed to infectious (the reciprocal is the incubation period).
#' @param omega Numeric, rate at which recovered individuals become susceptible
#' @param p_symptom Numeric vector, fractions of different symptome compartments
#' @param gamma Numeric vector, rate of removal (i.e. rate at which each infected group recover).
#' @param mu Numeric vector, rate of disease-caused mortality of each infected group
#' @return An object of class SEIaImIsRD with initial population and parameters
#' @rdname SEIaImIsRD-class
#' @export
#'
set_init <- function(
  object,
  S=NA_real_,
  E=NA_real_,
  I_asymptomatic=NA_real_, I_mild=NA_real_, I_severe=NA_real_,
  R=NA_real_,
  D=NA_real_,
  beta=list(), kappa=NA_real_, omega=NA_real_,
  p_symptom=list(), gamma=list(), mu=list()
) {
  param_list <- list(beta, kappa, omega, p_symptom, gamma, mu)
  init_pop_list <- list(S, E, I_asymptomatic, I_mild, I_severe, R, D)
  names(param_list) <- object@transmission_parameter_names
  names(init_pop_list) <- object@initial_condition_names
  object@initial_conditions <- init_pop_list
  object@transmission_parameters <- param_list
  # check if initial settings are valid
  check <- check_init(object)
  if (check == TRUE) object
  else stop(paste(check, ", please check and rerun set_init.\n"))
}

#' to check validity of the settings of the parameters and initial conditions
#'
#' @param object An object of class SEIaImIsRD
#' @return TRUE if passing all checks, or an error message if any check fails
#' @rdname SEIaImIsRD-class
#' @export
#'
check_init <- function(object) {
  errors <- character()
  # check whether all required parameters are set
  is_na_params <- is.na(object@transmission_parameters)
  if (sum(is_na_params) != 0) {
    msg <- paste("Missing parameters:",
                 paste(names(object@transmission_parameters)[is_na_params], collapse = ", ")
    )
    errors <- c(errors, msg)
  }
  # check whether all required initial population groups are set
  is_na_pop <- is.na(object@initial_conditions)
  if (sum(is_na_pop) != 0) {
    msg <- paste("Missing initial setting for population group:",
                 paste(names(object@initial_conditions)[is_na_pop], collapse = ", ")
    )
    errors <- c(errors, msg)
  }else{
    # check whether the sum of initial population is normalized to 1
    sum_init_pop <- sum(unlist(object@initial_conditions))
    if (sum_init_pop != 1) {
      msg <- "Sum of initial population is not 1, please normalize"
      errors <- c(errors, msg)
    }
  }
  # check whether the lengths of beta, gamma and mu correspond to the number of infected groups
  n_beta <- length(object@transmission_parameters$beta)
  if (n_beta != 3) {
    msg <- paste0(
      "Length of parameter beta,", n_beta, ", is not equal to the setting ", 3)
    errors <- c(errors, msg)
  }
  n_gamma <- length(object@transmission_parameters$gamma)
  if (n_gamma != 3) {
    msg <- paste0(
      "Length of parameter gamma,", n_gamma, ", is not equal to the setting ", 3)
    errors <- c(errors, msg)
  }
  n_mu <- length(object@transmission_parameters$mu)
  if (n_mu != 3) {
    msg <- paste0(
      "Length of parameter mu,", n_mu, ", is not equal to the setting ", 3)
    errors <- c(errors, msg)
  }
  if (length(errors) == 0) TRUE else errors
}



#' Solves the ode system.
#'
#' \deqn{\frac{dS(t)}{dt} = -S(t)(\beta.i_{asymptomatic} I_{asymptomatic}(t) + \beta.i_{mild} I_{mild}(t) + \beta.i_{severe} I_{severe}(t)) + \omega R(t)}
#' \deqn{\frac{dE(t)}{dt} = S(t)(\beta.i_{asymptomatic} I_{asymptomatic}(t) + \beta.i_{mild} I_{mild}(t) + \beta.i_{severe} I_{severe}(t)) - \kappa E(t)}
#' \deqn{\frac{dI_{asymptomatic}(t)}{dt} = p_symptom.i_{asymptomatic} \kappa E(t) - (\gamma.i_{asymptomatic} + \mu.i_{asymptomatic}) I_{asymptomatic}(t)}
#' \deqn{\frac{dI_{mild}(t)}{dt} = p_symptom.i_{mild}\kappa E(t) - (\gamma.i_{mild} + \mu.i_{mild}) I_{mild}(t)}
#' \deqn{\frac{dI_{severe}(t)}{dt} = p_symptom.i_{severe}\kappa E(t) - (\gamma.i_{severe} + \mu.i_{severe}) I_{severe}(t)}
#' \deqn{\frac{dR(t)}{dt} = -\omega R(t) + \gamma.i_{mild} I_{mild}(t) + \gamma.i_{severe} I_{severe}(t)}
#' \deqn{\frac{dD(t)}{dt} =  \mu.i_{mild} I_{mild}(t) + \mu.i_{severe} I_{severe}(t)}
#'
#' @param object An object of class SEIaImIsRD
#' @param times A list of time points of the simulation period
#' @param method A string indicating which ode integrator to use. Default is set to 'lsoda'
#' @return An object of class SEIaImIsRD with a dataframe of simulation output
#' @rdname SEIaImIsRD-class
#' @export
#'
ode_simulate <- function(
  object,
  times,
  method = "lsoda") {
  # initial population groups
  pop_groups <- c(S = object@initial_conditions$S,
                  E = object@initial_conditions$E,
                  I_asymptomatic = object@initial_conditions$I_asymptomatic,
                  I_mild = object@initial_conditions$I_mild,
                  I_severe = object@initial_conditions$I_severe,
                  R = object@initial_conditions$R,
                  D = object@initial_conditions$D)
  # parameters
  params <- c(beta = object@transmission_parameters$beta,
              kappa = object@transmission_parameters$kappa,
              omega = object@transmission_parameters$omega,
              p_symptom = object@transmission_parameters$p_symptom,
              gamma = object@transmission_parameters$gamma,
              mu = object@transmission_parameters$mu)

  # ODE system RHS
  ode_symptome_rhs <- function(t, pop_groups, parameters) {
    with(
      as.list(c(pop_groups, params)), {
        dS <- -S * (beta.i_asymptomatic * I_asymptomatic + beta.i_mild * I_mild + beta.i_severe * I_severe) + omega * R
        dE <- S * (beta.i_asymptomatic * I_asymptomatic + beta.i_mild * I_mild + beta.i_severe * I_severe) - kappa * E
        dI_a <- (1.0 - p_symptom.i_mild - p_symptom.i_severe) * kappa * E - (gamma.i_asymptomatic + mu.i_asymptomatic) * I_asymptomatic
        dI_m <- p_symptom.i_mild * kappa * E - (gamma.i_mild + mu.i_mild) * I_mild
        dI_s <- p_symptom.i_severe * kappa * E - (gamma.i_severe + mu.i_severe) * I_severe
        dR <- -omega * R + gamma.i_asymptomatic * I_asymptomatic + gamma.i_mild * I_mild + gamma.i_severe * I_severe
        dD <-  mu.i_asymptomatic * I_asymptomatic + mu.i_mild * I_mild + mu.i_severe * I_severe
        # return the rate of cI_severenge
        list(c(dS, dE, dI_a, dI_m, dI_s, dR, dD))
      })
  }
  # solving ode
  output <- ode(y = pop_groups,
                times = times,
                func = ode_symptome_rhs,
                parms = params,
                method = method)
  output <- as.data.frame(output)
  # reshape data frame into long format
  output.melt <- melt(output, id.vars = "time")
  names(output.melt) <- c("time", "population_group", "fraction")
  object@output <- output.melt
  return(object)
}


#' # describeIn SEIaImIsRD Plot the outcome of the ode similuation (for any dataframe)
#'
#' @param dataframe A dataframe in long format
#' @param x The variable in the dataframe to be mapped to the x axis in ggploy aesthetics. Default is set to "time".
#' @param y The variable in the dataframe to be mapped to the y axis in ggploy aesthetics. Default is set to "fraction".
#' @param c The variable in the dataframe to be plotted in different lines with different colours. Default is set to "population_group".
#'
#' @return A ggplot
#' @rdname plot_dataframe
#' @export
#' @aliases plot_dataframe,ANY,ANY-method
plot_dataframe <- function(data, x = "time", y = "fraction", c = "population_group") {
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_line(aes_string(colour = c)) +
    theme_classic()
  return(p)
}
