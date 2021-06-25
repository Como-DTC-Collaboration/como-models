#' An SEIRD model for compartments of different symptoms (asymptomatic, mild and severe) with different transmission rates (beta).
#'

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Class definitions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
#'       (characters). Default is list("beta", "kappa", "p_symptom", "gamma", "mu").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters (double).
#' @slot R0 basic reproduction number (double).
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
           R0 = "numeric",
           output_names = "list",
           output = "list"
         ),
         prototype = list(
           initial_condition_names = list("S0", "E0", "I_asymptomatic0",
                                          "I_mild0", "I_severe0", "R0", "D0"),
           transmission_parameter_names = list("beta", "kappa", "omega",
                                              "p_symptom", "gamma", "mu"),
           initial_conditions = vector(mode = "list", length = 7),
           transmission_parameters = vector(mode = "list", length = 6),
           R0 = NA_real_,
           output_names = list("S", "E", "I_asymptomatic",
                               "I_mild", "I_severe", "R", "D"),
           output = data.frame()
         ))



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Functions
# Generics defined in SEIRD will only be reset methods here to avoid ambiguity.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#' @describeIn SEIaImIsRD Retrieves initial conditions of SEIaImIsRD model.
#'
#' @param object An object of the class SEIaImIsRD.
#' @aliases initial_conditions,SEIaImIsRD-method
#' @export
setMethod("initial_conditions", "SEIaImIsRD",
          function(object) object@initial_conditions)


#' @describeIn SEIaImIsRD Retrieves transmission parameters of SEIR model.
#'
#' @param object An object of the class SEIaImIsRD.
#' @aliases initial_conditions,SEIaImIsRD-method
#' @export
setMethod("transmission_parameters", "SEIaImIsRD",
          function(object) object@transmission_parameters)



#' @describeIn SEIaImIsRD Setter method for initial population sizes (in fraction)
#' of the SEIaImIsRD model.
#'
#' @param object An object of class SEIaImIsRD
#' @param value A numeric named list containing the initial fraction of population groups:
#' S - susceptible,
#' E - exposed,
#' I_asymptomatic - infected with no symptom,
#' I_mild - infected with mild symptoms,
#' I_severe - infected severe symptoms that need further hospitalization,
#' D - dead due to the infection,
#' R - recovered.
#'
#' @return An object of class SEIaImIsRD with initial population and parameters
#' @aliases initial_conditions<-, SEIaImIsRD-method
#' @export
#'
setMethod(
  "initial_conditions<-",
  "SEIaImIsRD",
  function(object,
           value = list(S = NA_real_, E = NA_real_,
           I_asymptomatic = NA_real_, I_mild = NA_real_, I_severe = NA_real_,
           R = NA_real_,
           D = NA_real_)) {
    init_pop_list <- value
    names(init_pop_list) <- object@initial_condition_names
    object@initial_conditions <- init_pop_list
    # check if values are valid
    errors <- character()
    # check whether all required initial population groups are set
    is_na_pop <- is.na(init_pop_list)
    if (sum(is_na_pop) != 0) {
      msg <- paste("Missing initial setting for population group:",
                   paste(names(init_pop_list)[is_na_pop], collapse = ", ")
      )
      errors <- c(errors, msg)
    }else{
      # check whether the sum of initial population is normalized to 1
      sum_init_pop <- sum(unlist(init_pop_list))
      if (sum_init_pop != 1) {
        msg <- "Sum of initial population is not 1, please normalize"
        errors <- c(errors, msg)
      }
    }
    if (length(errors) == 0) {
      object@initial_conditions <- init_pop_list
      object
      }
    else stop(paste(errors, ", please check and rerun transmission_parameters<-.\n"))
  })


#' @describeIn SEIaImIsRD Setter method for transmission parameters
#' of the SEIaImIsRD model.
#'
#' @param object An object of class SEIaImIsRD
#' @param value A numeric named list of values for transmission parameters:
#' beta - a list of the effective contact rate from each infected group (i.e. rate at which an infected individual exposes susceptible),
#' kappa - rate of progression from exposed to infectious (the reciprocal is the incubation period),
#' omega - rate at which recovered individuals become susceptible,
#' p_symptom - a list of fraction of different infected groups,
#' gamma - a list of the rate of removal of each infected group (i.e. recovery rate of an infected individual),
#' mu - a list of the rate of disease-caused mortality of each infected group
#' @return An object of class SEIaImIsRD with initial population and parameters
#' @aliases transmission_parameters<-, SEIaImIsRD-method
#' @export
#'
setMethod(
  "transmission_parameters<-",
  "SEIaImIsRD",
  function(
    object,
    value = list(beta = list(), kappa = NA_real_, omega = NA_real_,
    p_symptom = list(), gamma = list(), mu = list())) {
    param_list <- value
    names(param_list) <- object@transmission_parameter_names
    object@transmission_parameters <- param_list
    # check if values are valid
    errors <- character()
    # check whether all required parameters are set
    is_na_params <- is.na(param_list)
    if (sum(is_na_params) != 0) {
      msg <- paste("Missing parameters:",
                   paste(names(param_list)[is_na_params], collapse = ", ")
      )
      errors <- c(errors, msg)
    }
    # check whether the lengths of beta, gamma and mu correspond to the number of infected groups
    n_beta <- length(param_list$beta)
    if (n_beta != 3) {
      msg <- paste0(
        "Length of parameter beta,", n_beta, ", is not equal to the setting ", 3)
      errors <- c(errors, msg)
    }
    n_gamma <- length(param_list$gamma)
    if (n_gamma != 3) {
      msg <- paste0(
        "Length of parameter gamma,", n_gamma, ", is not equal to the setting ", 3)
      errors <- c(errors, msg)
    }
    n_mu <- length(param_list$mu)
    if (n_mu != 3) {
      msg <- paste0(
        "Length of parameter mu,", n_mu, ", is not equal to the setting ", 3)
      errors <- c(errors, msg)
    }
    if (length(errors) == 0) {
      object@transmission_parameters <- param_list
      object
      }
    else stop(paste(errors, ", please check and rerun transmission_parameters<-.\n"))
    })

#' Solves the ode system.
#'
#' \deqn{\frac{dS(t)}{dt} = -S(t)(\beta.i_{asymptomatic} I_{asymptomatic}(t) + \beta.i_{mild} I_{mild}(t) + \beta.i_{severe} I_{severe}(t)) + \omega R(t)}
#' \deqn{\frac{dE(t)}{dt} = S(t)(\beta.i_{asymptomatic} I_{asymptomatic}(t) + \beta.i_{mild} I_{mild}(t) + \beta.i_{severe} I_{severe}(t)) - \kappa E(t)}
#' \deqn{\frac{dI_{asymptomatic}(t)}{dt} = p_symptom.i_{asymptomatic} \kappa E(t) - (\gamma.i_{asymptomatic} + \mu.i_{asymptomatic}) I_{asymptomatic}(t)}
#' \deqn{\frac{dI_{mild}(t)}{dt} = p_symptom.i_{mild}\kappa E(t) - (\gamma.i_{mild} + \mu.i_{mild}) I_{mild}(t)}
#' \deqn{\frac{dI_{severe}(t)}{dt} = p_symptom.i_{severe}\kappa E(t) - (\gamma.i_{severe} + \mu.i_{severe}) I_{severe}(t)}
#' \deqn{\frac{dR(t)}{dt} = -\omega R(t) + \gamma.i_{mild} I_{mild}(t) + \gamma.i_{severe} I_{severe}(t)}
#' \deqn{\frac{dD(t)}{dt} =  \mu.i_{asymptomatic} I_{asymptomatic}(t) + \mu.i_{mild} I_{mild}(t) + \mu.i_{severe} I_{severe}(t)}
#'
#' @param object An object of class SEIaImIsRD
#' @param times A list of time points of the simulation period
#' @param solved_method A string indicating which ode integrator to use. Default is set to 'lsoda'
#' @return An object of class SEIaImIsRD with a dataframe of simulation output
#' @rdname SEIaImIsRD-class
#' @aliases run,SEIaImIsRD-method
#' @export
#'
setMethod("run",
          "SEIaImIsRD",
          def = function(
            object,
            times,
            solve_method = "lsoda") {
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
                          method = solve_method)
            output <- as.data.frame(output)
            # reshape data frame into long format
            output.melt <- melt(output, id.vars = "time")
            # names(output.melt) <- c("time", "population_group", "fraction")
            names(output.melt) <- c("time", "compartment", "value")
            object@output <- output.melt
            return(object)
          })
 

#' Calculate the basic reproduction number (\code{R_0}) of the system using the next generation matrix approach.
#' @seealso \url{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6002118/pdf/main.pdf} mathematical
#' details of the next generation matrix approach.
#'
#' The R0 parameter is given by:
#' \deqn{R_0 = \rho(FV^{-1})},
#' where \deqn{F=\frac{\partial F_i{(x_0)}}{\partial x_j}} and \deqn{V=\frac{\partial V_i{(x_0)}}{\partial x_j}}
#' and \code{rho} represents the spectral radius of a matrix (i.e. the largest absolute of eigenvalue).
#' The \code{F_i} are the new infections, while the \code{V_i} transfers of infections from one compartment to
#' another. \code{x_0} is the disease-free equilibrium state.
#'
#' @param model A model of an SEIaImIsRD class object with initial_conditions and transmission_parameters set.
#' @return A SEIaImIsRD class object with R0 value stored
#' @rdname SEIaImIsRD-class
#' @aliases R0,SEIaImIsRD-method
#' @export
#'
setMethod("R0", "SEIaImIsRD", function(model) {
  # get required parameters:
  S <- model@initial_conditions$S
  beta <- model@transmission_parameters$beta
  kappa <- model@transmission_parameters$kappa
  p_symptom <- model@transmission_parameters$p_symptom
  gamma <- model@transmission_parameters$gamma
  mu <- model@transmission_parameters$mu
  # define matrices F and V:
  F <- matrix(0, 4, 4)
  V <- matrix(0, 4, 4)
  F[1, 2:4] <-  c(S * beta$i_asymptomatic, S * beta$i_mild, S * beta$i_severe)
  V[1, 1] <- kappa
  V[2, 1] <- -kappa * (1 - p_symptom$i_mild - p_symptom$i_severe)
  V[2, 2] <- gamma$i_asymptomatic + mu$i_asymptomatic
  V[3, 1] <- -kappa * p_symptom$i_mild
  V[3, 3] <- gamma$i_mild + mu$i_mild
  V[4, 1] <- -kappa * p_symptom$i_severe
  V[4, 4] <- gamma$i_severe + mu$i_severe
  # calculate R0 as the spectral radius for the matrix F x V^(-1):
  eigVals <- eigen(F %*% (solve(V)))$values
  model@R0 <- max(abs(eigVals))
  return(model)
  })

#' # describeIn SEIaImIsRD Plot the outcome of the ode similuation (for any dataframe)
#'
#' @param dataframe A dataframe in long format
#' @param x The variable in the dataframe to be mapped to the x axis in ggploy aesthetics. Default is set to "time".
#' @param y The variable in the dataframe to be mapped to the y axis in ggploy aesthetics. Default is set to "value".
#' @param c The variable in the dataframe to be plotted in different lines with different colours. Default is set to "compartment".
#'
#' @return A ggplot
#' @rdname plot_dataframe
#' @export
#' @aliases plot_dataframe,ANY,ANY-method
#'
plot_dataframe <- function(data, x = "time", y = "value", c = "compartment") {
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_line(aes_string(colour = c)) +
    theme_classic()
  return(p)
}
