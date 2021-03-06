% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/physio-arrangeInputs.R
\name{arrange_inputs.physio}
\alias{arrange_inputs.physio}
\title{arrange_inputs(data, inputorder)}
\usage{
arrange_inputs.physio(data, inputorder, subject = 1)
}
\arguments{
\item{inputorder}{a named character vector, names needs to be Input1 to Input8, values only "EKG, "BVP", or "EDA"}

\item{subject, }{a integer; specify for which subject input order shall be adjusted.}

\item{list}{of class physio}
}
\value{
list of class phasio
}
\description{
Function order the inputs as they are needed for further physio processings. Additional, all Inputs
which are not part of the parameter inputorder will be removed.
}
\examples{
 
inputorder <-c(EKG = "Input1", BVP = "Input2", EDA ="Input3")
}

