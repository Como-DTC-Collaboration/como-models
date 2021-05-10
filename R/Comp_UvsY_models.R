# script to compare the two versions of the urban vs. rural SEIRD models
# set wd to folder with this file in it
setwd("~/Documents/GitHub/DTC/como-models/R")
source("SEIR_rural_urban.R")
source("SEIR_rural_urban_2.R")
library(deSolve)
library(glue)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(readxl)

# load contact matrices
load("../Data/contact_all_urban.rdata")
contact_all_urban <- contact_all
names_urban <- names(contact_all_urban)
load("../Data/contact_all_rural.rdata")
contact_all_rural <- contact_all
names_rural <- names(contact_all_rural)
names_common <- intersect(names_urban,names_rural)

# specify country and obtain population data
# NOTE: World bank data and contact matrix data use the same three-letter country codes!
country <- "ETH"
if (!(country %in% names_common)) {
  stop(paste(country," is not a valid three-letter country code."))
}
country_data <- read_excel("../Data/Country_ruralpop_data.xlsx", sheet = "Data", skip = 3)
country_frac_rural <- country_data$`2019`[country_data$`Country Code` == country]
country_frac_rural <- country_frac_rural/100 #turn percentage into fraction

# create model with NO migration, but with cross-community infection
model_nomig <- new("SEIR_rural_urban")
# create model with migration, but NO cross-community infection
# model_mig <- new("SEIR_rural_urban_2")

# compute average contact rate for urban an rural communities
# units: number of contacts/day
contact_rate_urban = mean(contact_all_urban[[country]])
contact_rate_rural = mean(contact_all_rural[[country]])
scaling_urban_to_rural = contact_rate_rural/contact_rate_urban
res_rural = matrix(0,174,2)
for (i in 1:174) {
  n = names_common[i]
  rate_urban = mean(contact_all_urban[[n]])
  rate_rural = mean(contact_all_rural[[n]])
  res_rural[i,] = c(rate_urban,rate_rural)
}

res_rural <- data.frame("urban" = res_rural[,1], "rural" = res_rural[,2])
row.names(res_rural) <- names_common

ggplot(res_rural, aes(x=urban, y=rural)) + geom_point() +
  coord_cartesian(xlim = c(0.45, 1.1), ylim = c(0.45, 1.1)) +
  geom_segment(aes(x = 0, y = 0, xend = 1.5, yend = 1.5)) +
  geom_text(label=names_common, hjust = -0.1, vjust = -0.1)

# set parameters for both models, as equal as possible
# parameters they have in common
b = 0.3 # baseline infection rate, assuming urban community
bu = b # infection rate in urban community
by = b * scaling_urban_to_rural # infection rate in rural community
k = 0.2 # 1/(incubation period in days)
g = 0.1# 1/(days between infection and recovery)
m = 0.03 # probability of death, cases-fatality ratio.
# no migration model: bu, by, buy, byu, k, g, m
c_uy = 0.1 # number of urban contacts per day for a rural individual, relative to urban-urban contact
c_yu = 0.1 # number of rural contacts per day for an urban individual, relative to urban-urban contact
buy = b * c_uy # rate at which rural individuals infect urban individuals
byu = b * c_yu # rate at which urban individuals infect rural individuals
transmission_parameters(model_nomig) <- list(bu, by, buy, byu, k, g, m)
# migration model: bu, by, k, g, m, fsu, fsy, feu, fey, fiu, fiy, fru, fry
#fsu = 0.05 # migration rate of susceptible urban people to rural communities
#fsy = 0.05 # migration rate of susceptible rural people to urban communities
#feu = 0.05 # migration rate of exposed urban people to rural communities
#fey = 0.05 # migration rate of exposed rural people to urban communities
#fiu = 0.01 # migration rate of infectious urban people to rural communities
#fiy = 0.01 # migration rate of infectious rural people to urban communities
#fru = 0.05 # migration rate of recovered urban people to rural communities
#fry = 0.05 # migration rate of recovered rural people to urban communities
#transmission_parameters2(model_mig) <- list(bu, by, k, g, m, fsu, fsy,
#                                            feu, fey, fiu, fiy, fru, fry)

# set initial conditions, same in both models
start_infected_urban = 0.05
initial_conditions(model_nomig) <- list(1-country_frac_rural - start_infected_urban, 0, start_infected_urban, 0, country_frac_rural, 0, 0, 0)
#initial_conditions2(model_mig) <- list(0.59, 0, 0.01, 0, 0.4, 0, 0, 0)

# simulate both models
out_nomig <- simulate_SEIR_rural_urban(model_nomig, seq(0, 200, by = 0.1))
#out_mig <- simulate_SEIR_rural_urban_2(model_mig, seq(0, 100, by = 0.1))

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
       title = glue("SEIR model with urban and rural communities")) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values = col)
#print(SEIRplot)

case_df <- subset(out_nomig, compartment == "Incidences_U" | compartment == "Deaths_U" | compartment == "Incidences_Y" | compartment == "Deaths_Y")
case_df$compartment <- factor(case_df$compartment, levels = c("Incidences_U", "Deaths_U", "Incidences_Y", "Deaths_Y"))
col <- c("Incidences_U" = "grey28", "Deaths_U" = "black", "Incidences_Y" = "lightgray", "Deaths_Y" = "darkgray")
inc_plot <- ggplot(case_df, aes(x = time, y = value)) +
  geom_line(aes(color = compartment), size = 1.5) +
  labs(x = "time", y = "fraction of the population",
       title = glue("Incidences and deaths")) +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values = col)
#print(inc_plot)

grid.arrange(SEIRplot,inc_plot,nrow = 1)

#i <- sapply(out_mig, is.factor)
#out_mig[i] <- lapply(out_mig[i], as.character)
#SEIR_df2 <- subset(out_mig, compartment != "Incidences_U" & compartment != "Deaths_U" & compartment != "Incidences_Y" & compartment != "Deaths_Y")
#SEIR_df2$compartment <- factor(SEIR_df2$compartment, levels = c("S_U", "E_U", "I_U", "R_U", "S_Y", "E_Y", "I_Y", "R_Y"))
#col <- c("S_U" = "blue", "E_U" = "green", "I_U" = "yellow", "R_U" = "red", "S_Y" = "lightblue1", "E_Y" = "lightgreen", "I_Y" = "lightyellow", "R_Y" = "indianred")
#SEIRplot2 <- ggplot(SEIR_df2, aes(x = time, y = value)) +
#  geom_line(aes(color = compartment), size = 1.5) +
#  labs(x = "time", y = "fraction of the population",
#       title = glue("SEIR model with urban and rural communities: migration")) +
#  theme(legend.position = "right") +
#  scale_color_manual(values = col)
#print(SEIRplot2)

#case_df2 <- subset(out_mig, compartment == "Incidences_U" | compartment == "Deaths_U" | compartment == "Incidences_Y" | compartment == "Deaths_Y")
#case_df2$compartment <- factor(case_df2$compartment, levels = c("Incidences_U", "Deaths_U", "Incidences_Y", "Deaths_Y"))
#col <- c("Incidences_U" = "grey28", "Deaths_U" = "black", "Incidences_Y" = "lightgray", "Deaths_Y" = "darkgray")
#inc_plot2 <- ggplot(case_df2, aes(x = time, y = value)) +
#  geom_line(aes(color = compartment), size = 1.5) +
#  labs(x = "time", y = "fraction of the population",
#       title = glue("Incidences and deaths: migration")) +
#  theme(legend.position = "right") +
#  scale_color_manual(values = col)
#print(inc_plot2)


