#' @include generics.R
#'
NULL

#' An S4 object representing the SEIaImIsRD.
#' 
#' This class defines the SEIaImIsRD model, an extension of the SEIRD model.
#' The model shows how populations of susceptible, exposed, infectious and 
#' recovered individuals evolve over time, in which the infectious individuals
#' are subgrouped into compartments according to different severity of symptoms, i.e. asymptomatic, mild and severe.
#'
#' Notes:
#' 1. Total initial population size is normalised to 1.
#' 2. The current model does not include natural death or birth.
#'
#' @slot initial_condition_names list of names of initial conditions
#'       (characters). Default is list("S0", "E0", "I_asymptomatic0", "I_mild0", "I_severe0", "R0", "D0").
#' @slot transmission_parameter_names list of names of transmission parameters
#'       (characters). Default is list("beta", "kappa", "p_symptom", "gamma", "mu").
#' @slot initial_conditions list of values for initial conditions (double).
#' @slot transmission_parameters list of values for transmission parameters (double).
#' @slot output_names list of compartments name which are used by the model and
#'       incidence.
#'
#' @import deSolve
#' @import ggplot2
#' @import reshape2
#' @importFrom methods new
#' @export SEIaImIsRD
SEIaImIsRD <- setClass(Class = "SEIaImIsRD",
         slots = c(
           initial_condition_names = "list",
           transmission_parameter_names = "list",
           initial_conditions = "list",
           transmission_parameters = "list",
           output_names = "list"
         ),
         prototype = list(
           initial_condition_names = list("S0", "E0", "I_asymptomatic0",
                                          "I_mild0", "I_severe0", "R0", "D0"),
           transmission_parameter_names = list("beta", "kappa",
                                              "p_symptom", "gamma", "mu"),
           initial_conditions = vector(mode = "list", length = 7),
           transmission_parameters = vector(mode = "list", length = 5),
           output_names = list("S", "E", "I_asymptomatic",
                               "I_mild", "I_severe", "R", "D",
                               "Incidence", "Deaths")
         ))


#' @describeIn SEIaImIsRD Retrieves initial conditions of SEIaImIsRD model.
#'
#' @param object An object of the class SEIaImIsRD.
#' @export
setMethod("initial_conditions", "SEIaImIsRD",
          function(object) object@initial_conditions)


#' @describeIn SEIaImIsRD Retrieves transmission parameters of SEIaImIsRD model.
#'
#' @param object An object of the class SEIaImIsRD.
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
#' beta - a named list of the effective contact rate from each infected group (i.e. rate at which an infected individual exposes susceptible), each element with value in \code{[0,1]}
#' kappa - rate of progression from exposed to infectious (the reciprocal is the incubation period),
#' p_symptom - a named list of the probability of exposed individuals moving into each of the different infected groups, 
#' Here, only probabilities for the mild (p_symptom.mild) and severe (p_symptom.severe) groups need be specified and 
#' the asymptomatic probability is the remainder (1 - p_symptom.mild - p_symptom.severe). Thus we require 
#' p_symptom.mild + p_symptom.severe <= 1.
#' gamma - a list of the rate of removal of each infected group (i.e. recovery rate of an infected individual), each element with value in \code{[0,1]}
#' mu - a list of the rate of disease-caused mortality of each infected group, each element with value in \code{[0,1]}
#' @return An object of class SEIaImIsRD with initial population and parameters
#' @export
#'
setMethod(
  "transmission_parameters<-",
  "SEIaImIsRD",
  function(
    object,
    value = list(beta = list(asymptomatic = NA_real_, mild = NA_real_, severe = NA_real_), 
    kappa = NA_real_, 
    p_symptom = list(mild = NA_real_, severe = NA_real_), 
    gamma = list(asymptomatic = NA_real_, mild = NA_real_, severe = NA_real_), 
    mu = list(asymptomatic = NA_real_, mild = NA_real_, severe = NA_real_))) {
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
    n_p_symptom <- length(param_list$p_symptom)
    if (n_p_symptom != 2) {
      msg <- paste0(
        "Length of parameter p_symptom,", n_mu, ", is not equal to the setting ", 2)
      errors <- c(errors, msg)
    }
    else if (param_list$p_symptom %>% unlist %>% sum > 1){
      msg <- paste0("Sum of p_symptom, ", param_list$p_symptom %>% unlist %>% sum, ", is greater than ", 1)
      errors <- c(errors, msg)
    }
    if (length(errors) == 0) {
      object@transmission_parameters <- param_list
      object
      }
    else stop(paste(errors, ", please check and rerun transmission_parameters<-.\n"))
    })

#' @describeIn SEIaImIsRD Solves the ode system.
#'
#' \deqn{\frac{dS(t)}{dt} = -S(t)(\beta.asymptomatic I_{asymptomatic}(t) + \beta.mild I_{mild}(t) + \beta.severe I_{severe}(t))}
#' \deqn{\frac{dE(t)}{dt} = S(t)(\beta.asymptomatic I_{asymptomatic}(t) + \beta.mild I_{mild}(t) + \beta.severe I_{severe}(t)) - \kappa E(t)}
#' \deqn{\frac{dI_{asymptomatic}(t)}{dt} = p_symptom.asymptomatic \kappa E(t) - (\gamma.asymptomatic + \mu.asymptomatic) I_{asymptomatic}(t)}
#' \deqn{\frac{dI_{mild}(t)}{dt} = p_symptom.mild\kappa E(t) - (\gamma.mild + \mu.mild) I_{mild}(t)}
#' \deqn{\frac{dI_{severe}(t)}{dt} = p_symptom.severe\kappa E(t) - (\gamma.severe + \mu.severe) I_{severe}(t)}
#' \deqn{\frac{dR(t)}{dt} =  \gamma.asymptomatic I_{asymptomatic}(t) + \gamma.mild I_{mild}(t) + \gamma.severe I_{severe}(t)}
#' \deqn{\frac{dD(t)}{dt} =  \mu.asymptomatic I_{asymptomatic}(t) + \mu.mild I_{mild}(t) + \mu.severe I_{severe}(t)}
#'
#' @param object An object of class SEIaImIsRD
#' @param times A list of time points of the simulation period
#' @param solve_method A string indicating which ode integrator to use. Default is set to 'lsoda'
#' @return A list of two dataframes: one with the time steps, time series of S,
#' E, I_asymptomatic, I_mild, I_severe and R population fractions, and one with the time steps,
#' time series of incidences and deaths population fraction
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
                            D = object@initial_conditions$D,
                            C = 0)
            # parameters
            params <- c(beta = object@transmission_parameters$beta,
                        kappa = object@transmission_parameters$kappa,
                        p_symptom = object@transmission_parameters$p_symptom,
                        gamma = object@transmission_parameters$gamma,
                        mu = object@transmission_parameters$mu)

            # ODE system RHS
            ode_symptome_rhs <- function(t, pop_groups, parameters) {
              with(
                as.list(c(pop_groups, params)), {
                  dS <- -S * (beta.asymptomatic * I_asymptomatic + beta.mild * I_mild + beta.severe * I_severe)
                  dE <- S * (beta.asymptomatic * I_asymptomatic + beta.mild * I_mild + beta.severe * I_severe) - kappa * E
                  dI_a <- (1.0 - p_symptom.mild - p_symptom.severe) * kappa * E - (gamma.asymptomatic + mu.asymptomatic) * I_asymptomatic
                  dI_m <- p_symptom.mild * kappa * E - (gamma.mild + mu.mild) * I_mild
                  dI_s <- p_symptom.severe * kappa * E - (gamma.severe + mu.severe) * I_severe
                  dR <-  gamma.asymptomatic * I_asymptomatic + gamma.mild * I_mild + gamma.severe * I_severe
                  dD <-  mu.asymptomatic * I_asymptomatic + mu.mild * I_mild + mu.severe * I_severe
                  dC <- S * (beta.asymptomatic * I_asymptomatic + beta.mild * I_mild + beta.severe * I_severe)
                  # return the rate of cI_severenge
                  list(c(dS, dE, dI_a, dI_m, dI_s, dR, dD, dC))
                })
            }
            # solving ode
            output <- ode(y = pop_groups,
                          times = times,
                          func = ode_symptome_rhs,
                          parms = params,
                          method = solve_method)
            output <- as.data.frame(output)
            
            
            # Compute incidences and deaths
            cases <- c(0, diff(output$C))
            deaths <- c(0, diff(output$D))
            output$Incidence <- cases
            output$Deaths <- deaths
            output <- output[, c("time", unlist(object@output_names))]
            
            # Create long format of output
            output <- melt(output, id.vars = "time")
            output <- output[, c("time", "value", "variable")]
            names(output) <- c("time", "value", "compartment")

            # Added for consistency of output format across models
            output$age_range <- rep("0-150", length(output$time))

            # Split output into 2 dataframes: one with S,E,I, and R and one with C and D
            states <- subset(output, !output$compartment %in% c("Incidence", "Deaths"))
            states <- droplevels(states)
            changes <- subset(output, output$compartment %in% c("Incidence", "Deaths"))
            changes <- droplevels(changes)
            list("states" = states, "changes" = changes)
          })
 

#' @describeIn SEIaImIsRD Calculate the basic reproduction number (\code{R_0}) of the system using the next generation matrix approach.
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
#' @return An R0 value
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
  Fmat <- matrix(0, 4, 4)
  Vmat <- matrix(0, 4, 4)
  Fmat[1, 2:4] <-  c(S * beta$asymptomatic, S * beta$mild, S * beta$severe)
  Vmat[1, 1] <- kappa
  Fmat[2, 1] <- kappa * (1 - p_symptom$mild - p_symptom$severe)
  Vmat[2, 2] <- gamma$asymptomatic + mu$asymptomatic
  Fmat[3, 1] <- kappa * p_symptom$mild
  Vmat[3, 3] <- gamma$mild + mu$mild
  Fmat[4, 1] <- kappa * p_symptom$severe
  Vmat[4, 4] <- gamma$severe + mu$severe
  # calculate R0 as the spectral radius for the matrix F x V^(-1):
  eigVals <- eigen(Fmat %*% (solve(Vmat)))$values
  R0 = max(abs(eigVals))
  return(R0)
  })

