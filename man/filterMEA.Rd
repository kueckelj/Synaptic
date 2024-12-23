% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Synaptic.R
\name{filterMEA}
\alias{filterMEA}
\title{Subset MEA object with logical expressions}
\usage{
filterMEA(object, ..., .img_name = activeImage(object))
}
\arguments{
\item{object}{An object of class \code{\link[SPATA2]{SPATA2}} or, in case of S4 generics,
objects of classes for which a method has been defined.}

\item{...}{<\code{\link[rlang:args_data_masking]{data-masking}}> Expressions that
return a logical value, and are defined in terms of the variables in
\code{.data}. If multiple expressions are included, they are combined with the
\code{&} operator. Only rows for which all conditions evaluate to \code{TRUE} are
kept.}

\item{.img_name}{Character value. The name of the image of interest.
If \code{NULL}, the \link[SPATA2:concept_active]{active} image is chosen by default. Either way, must
be one of \code{\link[SPATA2:getImageNames]{getImageNames()}}.}
}
\value{
The updated input object, containing the added, removed or computed results.
}
\description{
This function filters a \link{MeaData} object based on specified logical expressions,
retaining only the \link[=concept_mea_nodes]{nodes} and \link[=concept_mea_edges]{edges} that meet the criteria.
It has the same effect as the function \code{\link[=subsetMEA]{subsetMEA()}} has, but it provides more convenient input options.

Note the \code{.} prefix before the arguments.
}
\details{
The function filters the input \code{MeaData} object based on the logical expressions provided in \code{...}.
If no expressions are provided, the function returns the input object with a warning.
The observations that meet the criteria specified by the logical expressions are retained.
}
