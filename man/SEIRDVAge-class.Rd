% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SEIRDVAge.R
\docType{class}
\name{SEIRDVAge-class}
\alias{SEIRDVAge-class}
\alias{SEIRDVAge}
\alias{initial_conditions,SEIRDVAge-method}
\alias{initial_conditions<-,SEIRDVAge-method}
\alias{transmission_parameters,SEIRDVAge-method}
\alias{transmission_parameters<-,SEIRDVAge-method}
\alias{interventions,SEIRDVAge-method}
\alias{interventions<-,SEIRDVAge-method}
\alias{run,SEIRDVAge-method}
\title{An S4 object representing the SEIRDVAge.}
\usage{
\S4method{initial_conditions}{SEIRDVAge}(object)

\S4method{initial_conditions}{SEIRDVAge}(object) <- value

\S4method{transmission_parameters}{SEIRDVAge}(object)

\S4method{transmission_parameters}{SEIRDVAge}(object) <- value

\S4method{interventions}{SEIRDVAge}(object)

\S4method{interventions}{SEIRDVAge}(object) <- value

\S4method{run}{SEIRDVAge}(object, times, solve_method = "lsoda")
}
\arguments{
\item{object}{An object of the class SEIRDVAge.}

\item{value}{(list) list of interventions. Each intervention has the following
parameters: starts, stops and coverages.}

\item{times}{(vector) time sequence over which to solve the model.
Must be of the form seq(t_start,t_end,by=t_step). Default time series
is seq(0, 100, by = 1).}

\item{solve_method}{A string indicating the chosen numerical integration
method for solving the ode system. Default is \code{lsoda} which is also the
default for the ode function in the deSolve package used in this function.}
}
\value{
Initial conditions of SEIRDVAge model.

Updated version of the age-structured SEIRDV model.

Transmission parameters of SEIRDVAge model.

Updated version of the age-structured SEIRDV model.

object of class SEIRDVAge with intervention parameters assigned.

data frame containing the time vector and time series of S, E, I, R, V, VR and
D population fractions for each age group outputs with incidence numbers
for each age group.
}
\description{
Defines an age-structured SEIRDV model and solves the set of
ordinary differential equations of the model with a chosen method of
numerical integration. Vaccinated individuals are considered in their own
compartment.
}
\section{Methods (by generic)}{
\itemize{
\item \code{initial_conditions}: Retrieves initial_conditions for an
age-structured SEIRDV model.

\item \code{initial_conditions<-}: Sets initial_conditions of an age-structured
SEIRDV model.

If the initial conditions provided to do not sum to 1 or of different
sizes compared to the number of age groups, an error is thrown.

\item \code{transmission_parameters}: Retrieves transmission_parameters for an
age-structured SEIRDV model.

\item \code{transmission_parameters<-}: Sets transmission_parameters of an
age-structured SEIRDV model.

If the transmission parameters provided to are not 1-dimensional an error is
thrown.

\item \code{interventions}: Retrieves interventions of age-structured SEIRDV model.

\item \code{interventions<-}: Setter method for intervention of the
age-structured SEIRV model.

Intervention parameters have same size. A tanh function is used to smooth interventions during simulation. This class is designed for interventions
which last several days at least and have several days between them; interventions involving rapid fluctuations may be distorted.

\item \code{run}: Method to simulate output using from model.

Solves a system of ODEs which form an
age-structured SEIRDV model. The system of equations for the time
evolution of population fractions in Susceptible (S), Exposed (E), Infected
(I), Recovered (R), Vaccinated (V) and Dead (D) groups in a given age group indexed by i is
given by

\deqn{\frac{dS_i(t)}{dt} = - \beta S_i(t) \Sigma_{j}C_{ij} I_j(t) - \nu Inter(t) S_i(t) + \delta_V V_i(t) + \delta_R R_i(t) + \delta_VR VR_i(t)}
\deqn{\frac{dE_i(t)}{dt} = \beta S_i(t) \Sigma_{j}C_{ij} I_j(t) - \kappa E_i(t)}
\deqn{\frac{dI_i(t)}{dt} = \kappa E_i(t) - \gamma I_i(t) - \mu I_i(t)}
\deqn{\frac{dR_i(t)}{dt} = \gamma I_i(t) -  \delta_R R_i(t) - \nu Inter(t) R_i(t)}
\deqn{\frac{dV(t)}{dt} = \nu Inter(t) S_i(t) - \delta_V V_i(t)}
\deqn{\frac{dVR(t)}{dt} = \nu Inter(t) R_i(t) - \delta_VR VR_i(t)}
\deqn{\frac{dC(t)}{dt} = \beta S_i(t) \Sigma_{j}C_{ij} I_j(t)}
\deqn{\frac{dD_i(t)}{dt} = \mu I_i(t)}

where C is a contact matrix whose elements represents the
contact between different age groups (rows) with age groups of
people they come in contact with (columns). This function relies on the
package deSolve to numerically integrate the set of equations above.
}}

\section{Slots}{

\describe{
\item{\code{output_names}}{list of compartments name which are used by the model and
incidence.}

\item{\code{initial_condition_names}}{list of names of initial conditions
(characters). Default is list("S0", "E0", "I0", "V0", "R0", "VR0", "D0").}

\item{\code{transmission_parameter_names}}{list of names of transmission parameters
(characters). Default is list("beta", "kappa", "gamma", "mu",  "nu",
"delta_V", "delta_R", "delta_VR").}

\item{\code{intervention_parameter_names}}{list of names of parameters for each
intervention (characters). Default is list ("starts", "stops", "coverages").}

\item{\code{initial_conditions}}{list of values for initial conditions (double).}

\item{\code{transmission_parameters}}{list of values for transmission parameters
(double).}

\item{\code{interventions}}{list interventions. Each intervention has the parameters
in the same format (list of double).}

\item{\code{contact_matrix}}{A square matrix with dimension
equal to n_age_categories x n_age_categories. This matrix represents the
contact between different age groups (rows) with age groups of
people they come in contact with (columns).}

\item{\code{n_age_categories}}{number of age categories.}

\item{\code{age_ranges}}{list of string characters representing the range of ages of
people in each age category. This object must have length
\code{n_age_categories} (otherwise an error is returned) and each element
must be formatted as 'age1-age2'.}
}}

