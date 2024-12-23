% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Synaptic.R
\name{getMeaData}
\alias{getMeaData}
\title{Obtain MEA data}
\usage{
getMeaData(object, mea_name = activeMea(object), img_transf = TRUE)
}
\arguments{
\item{object}{An object of class \code{\link[SPATA2]{SPATA2}} or, in case of S4 generics,
objects of classes for which a method has been defined.}

\item{mea_name}{Character value. The name of the \code{\link{MeaData}} object of interest.}
}
\value{
An object of class \code{\link{MeaData}}.
}
\description{
Extracts an object of class \code{\link{MeaData}}.
}
