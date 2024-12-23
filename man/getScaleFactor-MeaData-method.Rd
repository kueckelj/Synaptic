% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Synaptic.R
\name{getScaleFactor,MeaData-method}
\alias{getScaleFactor,MeaData-method}
\title{Obtain scale factors}
\usage{
\S4method{getScaleFactor}{MeaData}(object, img_name = activeImage(object), fct_name)
}
\arguments{
\item{object}{An object of class \code{\link[SPATA2]{SPATA2}} or, in case of S4 generics,
objects of classes for which a method has been defined.}

\item{img_name}{Character value. The name of the image of interest.
If \code{NULL}, the \link[SPATA2:concept_active]{active} image is chosen by default. Either way, must
be one of \code{\link[SPATA2:getImageNames]{getImageNames()}}.}

\item{fct_name}{Character value. Name of the scale factor.}
}
\value{
Single value whose properties depend on \code{fct_name}.
}
\description{
Extracts scale factors. See details for more.
}
\details{
This function gives access to slot @scale_factors of each registered \code{\link[SPATA2]{HistoImage}}.
As it is a list it can be flexibly expanded. The following scale factor slots are
reserved:

\itemize{
\item{\emph{image}:}{ The image scale factor used to create variables \emph{x} and \emph{y} from
variables \emph{x_orig} and \emph{y_orig} in the coordinates data.frame and the outline data.frames
of the spatial annotations and the tissue. The scale factor depends on the deviation in
resolution from the original image - based on which the coordinates data.frame
was created - and the image picked in \code{img_name}.}
\item{\emph{pixel}:}{ The pixel scale factor is used to convert pixel values into SI units.
It should have an attribute called "unit" conforming to the format "SI-unit/px}
}

Find more information \code{\link[SPATA2:concept_scale_factors]{here}}.
}
