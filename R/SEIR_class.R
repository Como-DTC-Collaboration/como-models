#' An S4 object representing the SEIR_model.
#'
#' This class represents the SEIR model, showing how populations of susceptible,
#' exposed, infected and recovered individuals evolve over time.
#'
#' @slot name character representing name of model
#' @slot parameter_names list of names of parameters (characters). Default is
#'       list("S0", "E0", "I0", "R0", "b", "k", "g")
#' @slot parameters list of parameter values (double). Default is a list of 1s.
setClass("SEIR_model",
         # slots
         slots = c(
           name = "character",
           parameter_names = "list",
           parameters = "list"
         ),
         # prototypes for the slots, automatically set parameter names and
         # values
         prototype = list(
           name = NA_character_,
           parameter_names = list("S0", "E0", "I0", "R0", "b", "k", "g"),
           parameters = as.list(rep(1, 7))
         )
)

#' @describeIn SEIR_model prints the parameters of the SEIR model in object.
#'
#' @param object An object of the class SEIR_model.
setGeneric("get_parameters", function(object) standardGeneric("get_parameters"))
setMethod("get_parameters", "SEIR_model", function(object) object@parameters)


#' @describeIn SEIR_model sets the parameter values (initial conditions S0, E0,
#' I0, R0, and parameters b, k, g) of the SEIR model contained in object.
#'
#' @param object an object of the class SEIR_model
#' @param S0 initial fraction of the population that is susceptible
#' @param E0 initial fraction of the population that has been exposed
#' @param I0 initial fraction of the population that is infected
#' @param R0 initial fraction of the population that has recovered
#' @param b rate at which an infected individual exposes susceptible
#' @param k rate at which exposed individuals become infected
#' @param g rate at which infected individuals recover
#'
#' @return object of class SEIR_model with initial conditions and parameter
#' values assigned.
setGeneric(
  "set_parameters",
  function(object, s0, e0, i0, r0, b, k, g) {
    standardGeneric("set_parameters")
  })
setMethod(
  "set_parameters", "SEIR_model",
  function(object, s0, e0, i0, r0, b, k, g) {

    # create list of parameter values
    params <- list(s0, e0, i0, r0, b, k, g)

    # add names to each value
    names(params) <- object@parameter_names

    # raise errors if initial state and parameter values are not doubles
     for (p in list("S0", "E0", "I0", "R0")) {
      if (!is.double(params[[p]])) {
        stop(glue("{p} storage format must be a vector."))
        }
    }

    # check format of parameters a and b
    if (any(length(b) != 1 | length(k) != 1 | length(g) != 1)) {
      stop("The rates of change between compartments are 1-dimensional.")
    }

    # check that the initial conditions are properly normalized
    if (s0 + e0 + i0 + r0 != 1) {
      stop("Initial conditions do not add up to 1.")
    }

    #if all above tests are passed, assign the params namelist to the object
    object@parameters <- params

    return(object)
  })

# SEIR_model class specific functions

#' @describeIn SEIR_model solves the ODEs of the SEIR_model specified in object
#' for the time points specified in times.
#'
#' @param object an object of the class SEIR_model
#' @param times a sequence of time points at which the solution to the system
#' of ODEs should be returned.
#'
#' @return a dataframe with the time steps, and solutions for S, E, I and R.
setGeneric(name = "simulate",
           def = function(object, times) {
             standardGeneric("simulate")
           }
)
setMethod(
  "simulate", "SEIR_model",
  function(object, times) {
    if (!is.double(times)) {
      stop("Evaluation times of the model storage format must be a vector.")
    }

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
        as.list(c(state, parameters)), {
          s <- state[1]
          e <- state[2]
          i <- state[3]
          r <- state[4]
          # rate of change
          ds <- -b * s * i
          de <- b * s * i - k * e
          di <- k * e - g * i
          dr <- g * i
          # return the rate of change
          list(c(ds, de, di, dr))
        })
    }

    # call ode solver
    output <- ode(
      y = state, times = times, func = right_hand_side, parms = parameters)

    return(as.data.frame(output))
  })

# test case for creating an instance of the SEIR_model class
my_model <- new("SEIR_model", name = "my_model")
my_model <- set_parameters(my_model, 0.9, 0, 0.1, 0, 1, 1, 1)
get_parameters(my_model)
out_df <- simulate(my_model, seq(0, 10, by = 0.1))

# plot results
col <- c("cS" = "blue", "cE" = "green", "cI" = "yellow", "cR" = "red")
ggplot(out_df, aes(x = time)) +
  geom_line(aes(y = S, color = "cS"), size = 1.5) +
  geom_line(aes(y = E, color = "cE"), size = 1.5) +
  geom_line(aes(y = I, color = "cI"), size = 1.5) +
  geom_line(aes(y = R, color = "cR"), size = 1.5) +
  labs(x = "time", y = "fraction of the population", title = "SEIR model") +
  theme(legend.position = "right") +
  scale_color_manual(values = col)
