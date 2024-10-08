% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ObjManipulate.R
\docType{class}
\name{Genotype-class}
\alias{Genotype-class}
\alias{Genotype}
\title{A function to generate objects of class 'Genotype'}
\description{
A function to generate objects of class 'Genotype'
}
\section{Slots}{

\describe{
\item{\code{name}}{the name of the gene. Will be used as a tag to place influence within the network.}

\item{\code{expression}}{numeric vector with names "scion", and "rootstock".
Numerical values set between 0 (no expression),
and 1 (full expression). The default expression is 1
(the wildtype genotype).}

\item{\code{coregulator}}{of class character. The names of other genotypes that coregulate together.}

\item{\code{influence}}{a data.frame with the column names Node and Influence. Influences can one
of either "production", "degradation", "inhibition", "perception".}
}}

\examples{
NA
}
