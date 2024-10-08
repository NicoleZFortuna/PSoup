% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Simulations.R
\name{tidyScreen}
\alias{tidyScreen}
\title{A function to make sure that data.frames containing conditions to screen
are organised correctly. The wild-type condition is made to come first,
and any duplicate rows are removed.}
\usage{
tidyScreen(frame, name, exogenous = FALSE)
}
\arguments{
\item{frame}{a data.frame}

\item{name}{a string giving the name of the data frame in case a warning is generated.}

\item{exogenous}{logical. Indicates if the wild type row should be 1s or 0s.
if checking modifier or node screens, should be set to F.
If checking exogenous screens, should be T.}
}
\description{
A function to make sure that data.frames containing conditions to screen
are organised correctly. The wild-type condition is made to come first,
and any duplicate rows are removed.
}
