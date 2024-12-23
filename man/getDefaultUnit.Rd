% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Synaptic.R
\name{getDefaultUnit}
\alias{getDefaultUnit}
\alias{getDefaultUnit,SPATA2-method}
\alias{getDefaultUnit,MeaData-method}
\alias{getDefaultUnit,MeaDesign-method}
\title{Obtain default units for MEA objects}
\usage{
getDefaultUnit(object, ...)

\S4method{getDefaultUnit}{SPATA2}(object, mea_name = activeMea(object), which = "dist")

\S4method{getDefaultUnit}{MeaData}(object, which = "dist")

\S4method{getDefaultUnit}{MeaDesign}(object, which = "dist")
}
\arguments{
\item{object}{An object of class \code{\link{MeaData}}, an object that contains MEA data
or, in case of S4 generics, objects of classes for which a methods has been defined.}

\item{...}{Additional arguments passed to specific methods (not used in current implementation).}

\item{mea_name}{Character value. The name of the \code{\link{MeaData}} object of interest.}

\item{which}{\emph{character} (optional). Specifies the type of unit to retrieve. Common values include:
\itemize{
\item \code{"dist"}: Units for distances.
}
Default is \code{"dist"}.}
}
\value{
The default unit as a \emph{character} value, corresponding to the specified type (\code{which}).
}
\description{
This function retrieves the default unit for specific measurements or properties
(e.g., distances) associated with the given MEA object, based on the object's design or spatial configuration.
}
\details{
\itemize{
\item \strong{SPATA2 Method}: Retrieves the unit from the object's spatial method for distances or delegates
to the MEA design if another unit type is specified.
\item \strong{MeaData Method}: Delegates to the associated MEA design to retrieve the unit.
\item \strong{MeaDesign Method}: Retrieves the unit directly from the \verb{@units} slot of the \code{MeaDesign} object.
}
}
\seealso{
\code{\link[=getSpatialMethod]{getSpatialMethod()}}, \code{\link[=getMeaDesign]{getMeaDesign()}}
}
