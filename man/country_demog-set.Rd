% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SEIRD_RU.R
\name{country_demog<-}
\alias{country_demog<-}
\title{Setter method for demographic data for urban and rural communities in the
SEIR model. If inputting demographic vectors, both vectors together must sum
to 1.}
\usage{
country_demog(object) <- value
}
\arguments{
\item{object}{(SEIRD_RU model)}

\item{value}{(list) list of 2 sets of population fractions by age group, or
a single value noting the fraction of the population that is rural.}
}
\value{
object of class SEIRD_RU with  contact matrices
assigned.
}
\description{
Setter method for demographic data for urban and rural communities in the
SEIR model. If inputting demographic vectors, both vectors together must sum
to 1.
}
