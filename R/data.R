#' contact_home
#'
#' A list of 152 16x16 asymmetrical contact matrices, one for each country,
#' describing which age groups have contact with which other age groups AT HOME.
#' The units of entries are the average number of contacts with a person in that
#' age group in a day. The columns are the age groups 0-5 (X1), 5-10 (X2),
#' 10-15 (X3), and continuing with 5 year gaps up to 80 years old.
#' The rows are the age group of the contact, where row 1 is 0-5 year olds,
#' row 2 is 5-10 year olds, etc. up to 80 year olds.
#'
#' @format A Large list of 152 tibbles, each a 16x16 list of doubles.
#'
#' @source \url{https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697#sec020}
"contact_home"

#' contact_school
#'
#' A list of 152 16x16 asymmetrical contact matrices, one for each country,
#' describing which age groups have contact with which other age groups AT
#' SCHOOL. The units of entries are the average number of contacts with a person
#' in that age group in a day. The columns are the age groups 0-5 (X1),
#' 5-10 (X2), 10-15 (X3), and continuing with 5 year gaps up to 80 years old.
#' The rows are the age group of the contact, where row 1 is 0-5 year olds,
#' row 2 is 5-10 year olds, etc. up to 80 year olds.
#'
#' @format A Large list of 152 tibbles, each a 16x16 list of doubles.
#'
#' @source \url{https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697#sec020}
"contact_school"

#' contact_work
#'
#' A list of 152 16x16 asymmetrical contact matrices, one for each country,
#' describing which age groups have contact with which other age groups AT WORK.
#' The units of entries are the average number of contacts with a person in that
#' age group in a day. The columns are the age groups 0-5 (X1), 5-10 (X2),
#' 10-15 (X3), and continuing with 5 year gaps up to 80 years old.
#' The rows are the age group of the contact, where row 1 is 0-5 year olds,
#' row 2 is 5-10 year olds, etc. up to 80 year olds.
#'
#' @format A Large list of 152 tibbles, each a 16x16 list of doubles.
#'
#' @source \url{https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697#sec020}
"contact_work"

#' contact_other
#'
#' A list of 152 16x16 asymmetrical contact matrices, one for each country,
#' describing which age groups have contact with which other age groups AT OTHER
#' PLACES (not covered by the other contact matrices). The units of entries are
#' the average number of contacts with a person in that age group in a day. The
#' columns are the age groups 0-5 (X1), 5-10 (X2), 10-15 (X3), and continuing
#' with 5 year gaps up to 80 years old. The rows are the age group of the
#' contact, where row 1 is 0-5 year olds, row 2 is 5-10 year olds, etc. up to
#' 80 year olds.
#'
#' @format A Large list of 152 tibbles, each a 16x16 list of doubles.
#'
#' @source \url{https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697#sec020}
"contact_other"

#' population
#'
#' A tibble containing populations, birth rates and death rates for 152
#' countries split into 5 year age groups. These age groups range from 0-4 y.o.
#' to 100+ y.o.
#'
#' @format A tibble with dimensions 4221x5.
#'
#' @source \url{https://population.un.org/wpp/Download/Standard/Population/}
"population"

#' contact_all_rural
#'
#' A list of 177 16x16 asymmetrical contact matrices, one for each country,
#' describing which age groups have contact with which other age groups in
#' RURAL communities.
#' The units of entries are the average number of contacts with a person in that
#' age group in a day. The columns are the age groups 0-5 (X1), 5-10 (X2),
#' 10-15 (X3), and continuing with 5 year gaps up to 80 years old.
#' The rows are the age group of the contact, where row 1 is 0-5 year olds,
#' row 2 is 5-10 year olds, etc. up to 80 year olds.
#'
#' @format A Large list of 177 tibbles, each a 16x16 list of doubles.
#' @docType data
#' @name contact_all_rural
#' @usage data(contact_all_rural)
#' @source \url{https://github.com/kieshaprem/synthetic-contact-matrices}
"contact_all_rural"

#' contact_all_urban
#'
#' A list of 177 16x16 asymmetrical contact matrices, one for each country,
#' describing which age groups have contact with which other age groups in
#' URBAN communities.
#' The units of entries are the average number of contacts with a person
#' in that age group in a day. The columns are the age groups 0-5 (X1),
#' 5-10 (X2), 10-15 (X3), and continuing with 5 year gaps up to 80 years old.
#' The rows are the age group of the contact, where row 1 is 0-5 year olds,
#' row 2 is 5-10 year olds, etc. up to 80 year olds.
#'
#' @format A Large list of 177 tibbles, each a 16x16 list of doubles.
#' @docType data
#' @name contact_all_urban
#' @usage data(contact_all_urban)
#' @source \url{https://github.com/kieshaprem/synthetic-contact-matrices}
"contact_all_urban"

#' agedemographics_by_country
#'
#' A data frame describing the number of individuals (in thousands) per age
#' group. Age groups span five years, starting with 0-4 years, 5-9 years, etc.
#' with the oldest age group being 100+ years.
#' Columns of the data frame are
#' \itemize{
#'   \item Index number
#'   \item Variant: type of data in the table row. Mostly estimates
#'   \item Region, subregion, country or area: lists region to which data in the
#'   row applies.
#'   \item Notes
#'   \item Country code: three-letter code referring to country or region.
#'   \item Type
#'   \item Parent code
#'   \item Reference data: year of count or estimate
#'   \item 0-4: count (in thousands) of individuals in the age group 0-4.
#'   \item 5-9: count (in thousands) of individuals in the age group 5-9.
#'   \item 10-14: count (in thousands) of individuals in the age group 10-14.
#'   \item 15-19: count (in thousands) of individuals in the age group 15-19.
#'   \item 20-24: count (in thousands) of individuals in the age group 20-24.
#'   \item 25-29: count (in thousands) of individuals in the age group 25-29.
#'   \item 30-34: count (in thousands) of individuals in the age group 30-34.
#'   \item 35-39: count (in thousands) of individuals in the age group 35-39.
#'   \item 40-44: count (in thousands) of individuals in the age group 40-44.
#'   \item 45-49: count (in thousands) of individuals in the age group 45-49.
#'   \item 50-54: count (in thousands) of individuals in the age group 50-54.
#'   \item 55-59: count (in thousands) of individuals in the age group 55-59.
#'   \item 60-64: count (in thousands) of individuals in the age group 60-64.
#'   \item 65-69: count (in thousands) of individuals in the age group 65-69.
#'   \item 70-74: count (in thousands) of individuals in the age group 70-74.
#'   \item 75-79: count (in thousands) of individuals in the age group 75-79.
#'   \item 80-84: count (in thousands) of individuals in the age group 80-84.
#'   \item 85-89: count (in thousands) of individuals in the age group 85-89.
#'   \item 90-94: count (in thousands) of individuals in the age group 90-94.
#'   \item 95-99: count (in thousands) of individuals in the age group 95-99.
#'   \item 100+: count (in thousands) of individuals in the age group 100+.
#' }
#' @format A dataframe with 29 variables and 18105 observations.
#' @docType data
#' @name agedemographics_by_country
#' @usage data(agedemographics_by_country)
#' @source UN Department of Economic and Social Affairs: Populations dynamics. World Population Prospects 2019
"agedemographics_by_country"

#' country_codetoname
#'
#' A dataframe with two variables: CountryCode, which is the three-letter code,
#' and CountryName, which is the full name of the country.
#'
#' @format A data frame with 2 variables and 263 observations.
#' @docType data
#' @name country_codetoname
#' @usage data(country_codetoname)
#' @source UN Department of Economic and Social Affairs: Populations dynamics. World Population Prospects 2019
"country_codetoname"

#' percentrural_by_country
#'
#' A dataframe showing the percentage of the population that lived in a rural
#' environment for 264 countries in the years 1960-2020. The columns are
#'
#' \itemize{
#'   \item Country Name: full name of country
#'   \item Country Code: three-letter country code
#'   \item Indicator Name: Rural population (% of total population)
#'   \item Indicator Code: shorthand for Indicator Name
#'   \item 61 columns, one for each year in the range 1960-2020
#' }
#'
#' @format A dataframe with 65 variables and 264 observations.
#' @docType data
#' @name percentrural_by_country
#' @usage data(percentrural_by_country)
#' @source \url{https://data.worldbank.org/indicator/SP.RUR.TOTL.ZS?view=chart}
"percentrural_by_country"

#' infection_fatality_ratio
#'
#' A list of objects containg mortality rates due to infection with SARS-CoV-2.
#' 
#' There are two regimes for considering moratlity:
#' \itemize{
#'  \item age_structured: A tibble containing mortality rates split into 5 year age groups.
#' These age groups range from 0-4 y.o. to 100+ y.o.
#'  \item overall: probability of mortality across all ages.
#' }
#'
#' @format A list containing a tibble with dimensions 21x2 and a numeric.
#' @docType data
#' @name infection_fatality_ratio
#' @usage data(infection_fatality_ratio)
#' @source Verity, Robert, et al. "Estimates of the severity of coronavirus disease 2019: a model-based analysis." The Lancet infectious diseases 20.6 (2020): 669-677. \url{https://www.thelancet.com/journals/laninf/article/PIIS1473-3099(20)30243-7/fulltext}
"infection_fatality_ratio"