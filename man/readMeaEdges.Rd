% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Synaptic.R
\name{readMeaEdges}
\alias{readMeaEdges}
\title{Read and set MEA edges}
\usage{
readMeaEdges(object, dir, var_from, var_to, var_weight, ...)
}
\arguments{
\item{object}{An object of class \code{\link{MeaData}}.}

\item{dir}{\emph{character}. The directory of the file containing the edge data.}

\item{var_from}{\emph{character}. The name of the variable in the file that corresponds to the
starting node (\code{from}) of each edge.}

\item{var_to}{\emph{character}. The name of the variable in the file that corresponds to the
ending node (\code{to}) of each edge.}

\item{var_weight}{\emph{character}. The name of the variable in the file that specifies the weight
of each edge.}

\item{...}{Additional arguments passed to \code{\link[=read_mea_file]{read_mea_file()}}.}
}
\value{
A modified \code{MeaData} object with the updated edge data.
}
\description{
This function reads edge data from an external file, validates the data, and sets
it as the edge information in a \code{MeaData} object. The function ensures that the edge connections
overlap with the nodes in the MEA design and associates spatial coordinates with the edges.
}
\details{
\itemize{
\item The function validates the presence of the specified variables (\code{var_from}, \code{var_to}, \code{var_weight})
in the file and checks for overlap between the edges and nodes in the MEA design.
\item Spatial coordinates for edges are joined based on the associated node positions.
}
}
\examples{
# Read edges from a file and set them in the MeaData object
mea_data <- readMeaEdges(
  object = mea_data,
  dir = "path/to/edges_file.csv",
  var_from = "start_node",
  var_to = "end_node",
  var_weight = "connection_strength"
)

}
