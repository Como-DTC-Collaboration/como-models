#' SEIR basic model for compartments of symptome
#'
#' model diagram:
#'  --> S --(lam)--> E --(gamma), (ihr),((phs))--> (I, HA, VentA, ICUA) --((nu))--> R
#'  |                                                                               |
#'  |_____________________________________(omega)___________________________________|
#'
#'
#' Population groups:
#'
#' S - suspeptable,
#' E - effective,
#' R - recovered
#' I - infectious with mild/no symptome
#' HA - infectious with severe symptomes that need normal hospitalization (no need for icu/vent, H+HC)
#' VentA - infectious with severe symptomes that need venticulartor (Vent+VentC+HCV+ICUCV)
#' ICUA - infectious with severe symptomes that need icu (ICU+ICUC+HCICU)
#'
#' Notice:
#' 1) I, HA, VentA, ICUA - compartments of infectious population groups based on symptomes
#' 2) Sum of the population at each time point are normalized to 1. 
#' Error will be reported if the sum of initial population is not equal to 1.
#'
#' Parameters:
#' lam - Numeric(range(0,1)), rate from S to E
#' gamma - Numeric(range(0,1)),rate from E to become infectious
#' omega - Numeric(range(0,1)),rate from R to S
#' ihr - Numeric(range(0,1)),rate from E to infectious groups that have severe symptome (HA+VentA+ICUA)
#' phs$phs_icua - Numeric(range(0,1)),rate from E to ICUA
#' phs$phs_venta - Numeric(range(0,1)),rate from severe symptomes to VentA
#' nu - Numeric vector (range(0,1)) , rate of recovery for each infectious group.

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Class definitions
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#' @describeIn model_symptome of dependent classes
setClass("ggplot")
setClass("gg")
#' An S4 class of an basic SEIR model with compartments of different symptomes
#'
#' @slot name A string gives the name of the model
#' @slot initial_population A list of the initial population size of each group
#' @slot parameters A list of the model parameters
#' @slot output A dataframe holding the ode simulation output, with columns as population groups and rows as simulation time points
#' @slot plot_output A list holding the plot of the ode simulation output
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
           initial_population = vector(mode = "list", length = 7) %>%
             setNames(list("S", "E", "I", "HA", "ICUA", "VentA", "R")),
           parameters = vector(mode = "list", length = 6) %>%
             setNames(list("lam", "gamma", "omega", "ihr", "phs", "nu")),
           output = data.frame(),
           plot_output = ggplot()
         ))

#' @describeIn model_symptome to check validity of the settings of the parameters and initial conditions
#'
#' @param object An object of class model_symptome
#' @return TRUE or a string explaining the error message
#'
check_init <- function(object) {
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

  # check whether the lengths of phs and nu are correct
  n_phs <- length(object@parameters$phs)
  if (n_phs != 2) {
    msg <- paste("Length of parameter phs,", n_phs, ", is not equal to the setting ", 2)
    errors <- c(errors, msg)
  }
  
  n_nu <- length(object@parameters$nu)
  if (n_nu != 4) {
    msg <- paste0(
      "Length of parameter nu,", n_nu, ", is not equal to the setting ", 4)
    errors <- c(errors, msg)
  }

  if (length(errors) == 0) TRUE else errors
}

#' @describeIn model_symptome to set initial population sizes (in fraction) and parameters
#'
#' @param object An object of class model_symptome
#' @param S Numeric, initial population of the susceptible
#' @param E Numeric, initial population of the exposed
#' @param I Numeric, initial population of the infectious with mild/no symptome
#' @param HA Numeric, initial population of the infectious with severe symptomes 
#' that need normal hospitalization
#' @param ICUA Numeric, initial population of the infectious with severe symptomes that need icu
#' @param VentA Numeric, initial population of the infectious with severe symptomes 
#' that need venticulartor
#' @param R Numeric, initial population of the recovered
#' @param lam Numeric
#' @param gamma Numeric
#' @param omega Numeric
#' @param ihr Numeric
#' @param phs Numeric list
#' @param nu Numeric list
#' 
#' @return An object of class model_symptome
#'
setGeneric(name = "set_init", 
           def = function(object, S=NA_real_, E=NA_real_, I=NA_real_,
                          HA=NA_real_, ICUA=NA_real_, VentA=NA_real_, R=NA_real_,
                          lam=NA_real_, gamma=NA_real_, omega=NA_real_,
                          ihr=NA_real_, phs=list(), nu=list()) standardGeneric("set_init"))

setMethod("set_init", signature(object = "model_symptome"),
          function(object, S, E, I, HA, ICUA, VentA, R, lam, gamma, omega, ihr, phs, nu) {
            param_list <- list(lam, gamma, omega, ihr, phs, nu)
            init_pop_list <- list(S, E, I, HA, ICUA, VentA, R)
            names(param_list) <- names(object@parameters)
            names(init_pop_list) <- names(object@initial_population)
            object@initial_population <- init_pop_list
            object@parameters <- param_list

            # check if initial settings are valid
            check <- check_init(object)
            if (check == TRUE) object
            else stop(paste(check, ", please check and rerun set_init.\n"))
          })

#' @describeIn model_symptome to solve the ode system.
#' Notice that the population is normalized to 1 for each time point.
#' 
#' @param object An object of class model_symptome
#' @param times A list of time points of the simulation period
#' @param method A string indicating which ode integrator to use. Default is set to 'lsoda'.
#' @return An object of class model_symptome
#'
setGeneric(name = "ode_simulate", def = function(object, times, method = "lsoda") standardGeneric("ode_simulate"))

setMethod("ode_simulate", signature(object = "model_symptome"),
  function(object, times, method) {
    # initial population groups
    pop_groups <- c(S = object@initial_population$S,
                    E = object@initial_population$E,
                    I = object@initial_population$I,
                    HA = object@initial_population$HA,
                    ICUA = object@initial_population$ICUA,
                    VentA = object@initial_population$VentA,
                    R = object@initial_population$R)
    # parameters
    params <- c(lam = object@parameters$lam,
                gamma = object@parameters$gamma,
                omega = object@parameters$omega,
                ihr = object@parameters$ihr,
                phs = object@parameters$phs,
                nu = object@parameters$nu)
  
    # ODE system RHS
    ode_symptome_rhs <- function(t, pop_groups, parameters) {
      with(
        as.list(c(pop_groups, params)),
        {
          dSdt <- -lam * S * (I + HA + ICUA + VentA) + omega * R
          dEdt <- lam * S * (I + HA + ICUA + VentA) - gamma * E
          dIdt <- -nu.nui * I + (1.0 - ihr) * gamma * E # - ratetestI*I
          dHAdt <- gamma * ihr * (1 - phs.phs_icua - phs.phs_venta) * E - nu.nuhsa * HA
          dICUAdt <- gamma * ihr * phs.phs_icua * E - nu.nu_icua * ICUA
          dVentAdt <- gamma * ihr * phs.phs_venta * E - nu.nu_venta * VentA
          dRdt <- -omega * R + nu.nui * I + nu.nuhsa * HA + nu.nu_icua * ICUA + nu.nu_venta * VentA
          # return the rate of change
          list(c(dSdt, dEdt, dIdt, dHAdt, dICUAdt, dVentAdt, dRdt))
        })
    }
    # solving ode
    output <- ode(y = pop_groups, times = times, func = ode_symptome_rhs, parms = params, method = method)
    output <- as.data.frame(output)

    # normalize: keep the output population as fraction (range(0,1)) for each time step
    output.pop <- output[, -which(names(output) == "time")]
    output.pop <- sweep(output.pop, 1, apply(output.pop, 1, min))
    output[, -which(names(output) == "time")] <- output.pop / rowSums(output.pop)
    object@output <- output
    return(object)
    })

#' @describeIn model_symptome to plot the outcome of the ode similuation
#' - the population change of each group by time.
#'
#' @param object An object of class model_symptome
#' @return An object of class model_symptome or a string of error message
#'
setGeneric(name = "plot_ode_output", def = function(object) standardGeneric("plot_ode_output"))

setMethod("plot_ode_output", signature(object = "model_symptome"),
          function(object) {
            # if(is_analytical){
            #   output <- object@output_analytical
            # }else{
            #   output <- object@output
            # }
            output <- object@output
            if (empty(output)) {
            error <- "Empty output. \
            Please run the ode solver by calling ode_simulate."
            return(error)
            }else{
              # reshape data frame
              output.melt <- melt(output, id.vars = "time")
              names(output.melt) <- c("time", "population_group", "fraction")
              # plot the each group
              p <- ggplot2::ggplot(data = output.melt, aes(x = time, y = fraction)) +
                geom_line(aes(colour = population_group)) +
                theme_classic()
              object@plot_output <- p
              return(object)
            }
          })
