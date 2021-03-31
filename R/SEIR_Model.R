# SEIR Model
rm(list = ls(all = TRUE))
library(deSolve)
library(ggplot2)

# parameter values
parameters <-c(b = 1, k = 1, g = 1)

# initial conditions, normalized
y0 <- c(S = 0.75, E = 0, I = 0.25, R = 0)

# time specification
times <- seq(0, 100, by = 0.1)

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
ggplot(out_df, aes(x=time)) + 
  geom_line(aes(y=S, color = "cS"), size = 1.5) +
  geom_line(aes(y=E, color = "cE"), size = 1.5) +
  geom_line(aes(y=I, color = "cI"), size = 1.5) +
  geom_line(aes(y=R, color = "cR"), size = 1.5) +
  labs(x = "time", y = "fraction of the population", title = "SEIR model") +
  theme(legend.position = "right") +
  scale_color_manual(values = col)

# compute incidence number
total_inf <- out_df$I + out_df$R
n_inc <- c(0,total_inf[2:length(total_inf)]-total_inf[1:length(total_inf)-1])
