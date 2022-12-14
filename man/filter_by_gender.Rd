% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{filter_by_gender}
\alias{filter_by_gender}
\title{Filter the data by the gender of the patient}
\usage{
filter_by_gender(data, gender)
}
\arguments{
\item{data}{A data frame containing the qualitative diagnosis elements of several patients.}

\item{gender}{The gender the user wants to filter the data on. It can be either 'male' or 'female'.}
}
\value{
A data frame
}
\description{
Filter the data by the gender of the patient
}
\details{
This function allows the user to filter a data frame containing the medical
information on the gender of the patient. See the examples below for a concrete
use case.
}
\examples{
# Filter on men
filter_by_gender(patients, 'male')

# Filter on women
filter_by_gender(patients, 'female')
}
