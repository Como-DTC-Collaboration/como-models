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