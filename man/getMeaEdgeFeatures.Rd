% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Synaptic.R
\name{getMeaEdgeFeatures}
\alias{getMeaEdgeFeatures}
\alias{getMeaNodeFeatures}
\title{Obtain feature names for MEA edges and nodes}
\usage{
getMeaEdgeFeatures(object, mea_name = activeMea(object), .p = NULL)

getMeaNodeFeatures(object, mea_name = activeMea(object), .p = NULL)
}
\arguments{
\item{object}{An object containing MEA data and metadata.}

\item{mea_name}{The name of the MEA data to use. Defaults to the
active MEA in the object (\code{activeMea(object)}).}

\item{.p}{\emph{function} or \emph{formula} (optional). A predicate function or formula used to filter
the features. Only features satisfying the predicate will be included in the output.}
}
\value{
A character vector containing the names of the features for:
\itemize{
\item \code{getMeaEdgeFeatures()}: MEA edges.
\item \code{getMeaNodeFeatures()}: MEA nodes.
}
}
\description{
These functions extract the feature names (column names) from the data associated
with microelectrode array (MEA) edges or nodes, excluding identifiers and protected variables.
Optional filtering can be applied to include only specific features.
}
\examples{
# Retrieve all edge features
edge_features <- getMeaEdgeFeatures(object)

# Retrieve all node features
node_features <- getMeaNodeFeatures(object)

# Retrieve only numeric edge features
numeric_edge_features <- getMeaEdgeFeatures(object, .p = is.numeric)

}
\seealso{
\code{\link[=getMeaEdges]{getMeaEdges()}}, \code{\link[=getMeaNodes]{getMeaNodes()}}
}
