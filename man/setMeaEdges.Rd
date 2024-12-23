% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Synaptic.R
\name{setMeaEdges}
\alias{setMeaEdges}
\alias{setMeaEdges,SPATA2-method}
\alias{setMeaEdges,MeaData-method}
\alias{setMeaNodes}
\alias{setMeaNodes,SPATA2-method}
\alias{setMeaNodes,MeaData-method}
\title{Set MEA edges or nodes}
\usage{
setMeaEdges(object, ...)

\S4method{setMeaEdges}{SPATA2}(object, edges)

\S4method{setMeaEdges}{MeaData}(object, edges)

setMeaNodes(object, ...)

\S4method{setMeaNodes}{SPATA2}(object, nodes)

\S4method{setMeaNodes}{MeaData}(object, nodes)
}
\arguments{
\item{object}{An object of class \code{\link{SPATA2}} or \code{\link{MeaData}}.}

\item{...}{Additional arguments (not used in the current implementation).}

\item{edges}{A \code{data.frame} containing edge data to be stored in the object. Must include the
columns \code{from}, \code{to}, and optionally spatial coordinates (\code{x_orig}, \code{y_orig}, \code{xend_orig}, \code{yend_orig}).}

\item{nodes}{A \code{data.frame} containing node data to be stored in the object. Must include the
column \code{id}, representing the unique identifiers for the nodes.}
}
\value{
The updated input object, containing the added, removed or computed results.
}
\description{
These functions set edge or node data for a \code{SPATA2} or \code{MeaData} object. The data
is validated and stored in the corresponding slots of the \code{MeaData} object or within a \code{SPATA2} object.
}
\details{
\itemize{
\item \strong{Edges}:
\itemize{
\item \code{setMeaEdges} stores the provided edge data in the \code{edges} slot of a \code{MeaData} object.
\item The edge data is validated to ensure it includes required columns and matches the expected data structure.
}
\item \strong{Nodes}:
\itemize{
\item \code{setMeaNodes} stores the provided node data in the \code{nodes} slot of a \code{MeaData} object.
\item The node data is validated to ensure it includes the required column (\code{id}) and matches the expected structure.
}
\item When used with a \code{SPATA2} object, the methods delegate the operation to the corresponding \code{MeaData} object.
}
}
\seealso{
\code{\link[=getMeaData]{getMeaData()}}, \code{\link[=setMeaData]{setMeaData()}}
}
