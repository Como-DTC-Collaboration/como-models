# script to compare the two versions of the urban vs. rural SEIRD models
# set wd to folder with this file in it
source("SEIR_rural_urban.R")
source("SEIR_rural_urban_2.R")
library(deSolve)
library(glue)
library(reshape2)
library(ggplot2)
library(gridExtra)

# create model with NO migration, but with cross-community infection
model_nomig <- new("SEIR_rural_urban")
# create model with migration, but NO cross-community infection
model_mig <- new("SEIR_rural_urban_2")

# set parameters for both models, as equal as possible
# parameters they have in common
bu = 1 # infection rate in urban community
by = 0.1 # infection rate in rural community
k = 0.2 # 1/(incubation period in days)
g = 0.1# 1/(days between infection and recovery)
m = 0.05 # 1/(days between infection and death)
# no migration model: bu, by, buy, byu, k, g, m
buy = 0.05 # rate at which rural individuals infect urban individuals
byu = 0.05 # rate at which urban individuals infect rural individuals
transmission_parameters(model_nomig) <- list(bu, by, buy, byu, k, g, m)
# migration model: bu, by, k, g, m, fsu, fsy, feu, fey, fiu, fiy, fru, fry
fsu = 0.05 # migration rate of susceptible urban people to rural communities
fsy = 0.05 # migration rate of susceptible rural people to urban communities
feu = 0.05 # migration rate of exposed urban people to rural communities
fey = 0.05 # migration rate of exposed rural people to urban communities
fiu = 0.01 # migration rate of infectious urban people to rural communities
fiy = 0.01 # migration rate of infectious rural people to urban communities
fru = 0.05 # migration rate of recovered urban people to rural communities
fry = 0.05 # migration rate of recovered rural people to urban communities
transmission_parameters2(model_mig) <- list(bu, by, k, g, m, fsu, fsy,
                                            feu, fey, fiu, fiy, fru, fry)

# set initial conditions, same in both models
initial_conditions(model_nomig) <- list(0.59, 0, 0.01, 0, 0.4, 0, 0, 0)
initial_conditions2(model_mig) <- list(0.59, 0, 0.01, 0, 0.4, 0, 0, 0)

# simulate both models
out_nomig <- simulate_SEIR_rural_urban(model_nomig, seq(0, 100, by = 0.1))
out_mig <- simulate_SEIR_rural_urban_2(model_mig, seq(0, 100, by = 0.1))

# visualize results
# model without migration, but with intercommunity infection
i <- sapply(out_nomig, is.factor)
out_nomig[i] <- lapply(out_nomig[i], as.character)
SEIR_df <- subset(out_nomig, compartment != "Incidences_U" & compartment != "Deaths_U" & compartment != "Incidences_Y" & compartment != "Deaths_Y")
SEIR_df$compartment <- factor(SEIR_df$compartment, levels = c("S_U", "E_U", "I_U", "R_U", "S_Y", "E_Y", "I_Y", "R_Y"))
col <- c("S_U" = "blue", "E_U" = "green", "I_U" = "yellow", "R_U" = "red", "S_Y" = "lightblue1", "E_Y" = "lightgreen", "I_Y" = "lightyellow", "R_Y" = "indianred")
SEIRplot <- ggplot(SEIR_df, aes(x = time, y = value)) +
  geom_line(aes(color = compartment), size = 1.5) +
  labs(x = "time", y = "fraction of the population",
       title = glue("SEIR model with urban and rural communities: no migration")) +
  theme(legend.position = "right") +
  scale_color_manual(values = col)
#print(SEIRplot)

case_df <- subset(out_nomig, compartment == "Incidences_U" | compartment == "Deaths_U" | compartment == "Incidences_Y" | compartment == "Deaths_Y")
case_df$compartment <- factor(case_df$compartment, levels = c("Incidences_U", "Deaths_U", "Incidences_Y", "Deaths_Y"))
col <- c("Incidences_U" = "grey28", "Deaths_U" = "black", "Incidences_Y" = "lightgray", "Deaths_Y" = "darkgray")
inc_plot <- ggplot(case_df, aes(x = time, y = value)) +
  geom_line(aes(color = compartment), size = 1.5) +
  labs(x = "time", y = "fraction of the population",
       title = glue("Incidences and deaths: no migration")) +
  theme(legend.position = "right") +
  scale_color_manual(values = col)
#print(inc_plot)


i <- sapply(out_mig, is.factor)
out_mig[i] <- lapply(out_mig[i], as.character)
SEIR_df2 <- subset(out_mig, compartment != "Incidences_U" & compartment != "Deaths_U" & compartment != "Incidences_Y" & compartment != "Deaths_Y")
SEIR_df2$compartment <- factor(SEIR_df2$compartment, levels = c("S_U", "E_U", "I_U", "R_U", "S_Y", "E_Y", "I_Y", "R_Y"))
col <- c("S_U" = "blue", "E_U" = "green", "I_U" = "yellow", "R_U" = "red", "S_Y" = "lightblue1", "E_Y" = "lightgreen", "I_Y" = "lightyellow", "R_Y" = "indianred")
SEIRplot2 <- ggplot(SEIR_df2, aes(x = time, y = value)) +
  geom_line(aes(color = compartment), size = 1.5) +
  labs(x = "time", y = "fraction of the population",
       title = glue("SEIR model with urban and rural communities: migration")) +
  theme(legend.position = "right") +
  scale_color_manual(values = col)
#print(SEIRplot2)

case_df2 <- subset(out_mig, compartment == "Incidences_U" | compartment == "Deaths_U" | compartment == "Incidences_Y" | compartment == "Deaths_Y")
case_df2$compartment <- factor(case_df2$compartment, levels = c("Incidences_U", "Deaths_U", "Incidences_Y", "Deaths_Y"))
col <- c("Incidences_U" = "grey28", "Deaths_U" = "black", "Incidences_Y" = "lightgray", "Deaths_Y" = "darkgray")
inc_plot2 <- ggplot(case_df2, aes(x = time, y = value)) +
  geom_line(aes(color = compartment), size = 1.5) +
  labs(x = "time", y = "fraction of the population",
       title = glue("Incidences and deaths: migration")) +
  theme(legend.position = "right") +
  scale_color_manual(values = col)
#print(inc_plot2)

grid.arrange(SEIRplot,inc_plot,SEIRplot2,inc_plot2,nrow = 2)
