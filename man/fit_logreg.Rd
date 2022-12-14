% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{fit_logreg}
\alias{fit_logreg}
\title{Fit a logistic regression model on the patients data set}
\usage{
fit_logreg(data)
}
\arguments{
\item{data}{A data frame containing the qualitative diagnosis elements
of several patients. Typically, it can be a training dataset.}
}
\value{
A logistic regression model, along with the formula used in the model,
The coefficient values, the degrees of freedom, the null and residual deviance
and the AIC.
}
\description{
Fit a logistic regression model on the patients data set
}
\details{
The model used in the function has been selected using a backward selection
on the AIC criterion.
}
