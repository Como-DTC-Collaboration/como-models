#' Solves a simple SEIR model with default parameters b = 1, k = 1 and g = 1.
#' Default initial conditions are S = 0.9, E = 0, I = 0.1, and R = 0.
#' Default time series is seq(0,100,by=0.1).
#' If the initial conditions provided to do not sum to 1, an error is thrown.
#' This function relies on the packages deSolve and ggplot2. 
#' This function creates a plot of the variables over time and returns a
#' vector of the incidence number.
#'
#' @param params parameters of the model: b, k, and g.
#' @param ICs initial conditions of the model, must add to 1.
#' @param times time sequence over which to solve the model. Must be of the
#'        form seq(t_start,t_end,by=t_step)
#'
#' @return vector of time and incidence numbers.

SEIR <- function(params = c(1,1,1),ICs = c(0.9, 0, 0.1, 0),times = seq(0, 100, by = 0.1)){

# check that ICs are valid
if (sum(ICs) != 1) {
  stop("Invalif initial conditions. Must add up to 1.")
}

#
y0 <- c("S" = ICs[1], "E" = ICs[2],"I" = ICs[3],"R" = ICs[4])
parameters <- c("b" = params[1], "k" = params[2], g = params[3])

# define ODEs
model_odes <- function(t,y0,parameters) {
  with(as.list(c(y0,parameters)),{
    dS <- -b*S*I
    dE <- b*S*I - k*E
    dI <- k*E - g*I
    dR <- g*I
      
    list(c(dS,dE,dI,dR))
  })
}

# solve ODEs
out <- ode(y0, times, model_odes, parameters)
out_df <- as.data.frame.array(out)

# plot results
col <- c("cS" = "blue", "cE" = "green", "cI" = "yellow", "cR" = "red")
SEIRplot <- ggplot(out_df, aes(x=time)) + 
  geom_line(aes(y=S, color = "cS"), size = 1.5) +
  geom_line(aes(y=E, color = "cE"), size = 1.5) +
  geom_line(aes(y=I, color = "cI"), size = 1.5) +
  geom_line(aes(y=R, color = "cR"), size = 1.5) +
  labs(x = "time", y = "fraction of the population", title = "SEIR model") +
  theme(legend.position = "right") +
  scale_color_manual(values = col)
print(SEIRplot)

# compute incidence number
total_inf <- out_df$I + out_df$R
n_inc <- c(0,total_inf[2:length(total_inf)]-total_inf[1:length(total_inf)-1])

return(c("t" = out_df$time, "inc" = n_inc))
}

