% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/local_densities.R
\name{local_densities}
\alias{local_densities}
\title{Calculate Local Densities}
\usage{
local_densities(location_data, x = 1, y = 2, radius)
}
\arguments{
\item{location_data}{expects a data.frame with three columns:
\describe{
\item{x}{Easting / x-coordinate of location}
\item{y}{Northing / y-coordinate of location}
\item{type}{categorical variable for type of evidence at location}
}}

\item{x}{Index of column containing the x-coordinate}

\item{y}{Index of column containing the y-coordinate}

\item{radius}{\emph{numeric}. the radius of what constitues the
"neighborhood" of each point}
}
\value{
a data.frame that includes the point location and type of
each point, as well as the density of points of each type within
the specified radius of each point.
}
\description{
\code{local_densities()} is a function that calls the \code{\link[=local_counts]{local_counts()}} function,
then calculates the area of the neighborhood defined by the specified
radius. The \emph{local density} is the count divided by the area of the
neighborhood.
}
\seealso{
\code{\link[=local_counts]{local_counts()}}
}
