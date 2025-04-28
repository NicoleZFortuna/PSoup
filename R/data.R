#' Definition for the peaNetwork object.
#'
#' An object of class Network
#'
#' @format A list containing 4 objects:
#' \describe{
#'   \item{name}{name of object}
#'   \item{expression}{location of action}
#'   \item{coregulator}{coregulating genotypes}
#'   \item{influence}{influenced nodes}
#' }
"peaNetwork"

#' A data.frame containing the language conventions and their
#' equivalence of the PSoup package, and the SBGN languages
#' 'activity flow' and 'entity relationship'.
#'
#' An object of class data.frame
#'
#' @format a data.frame containing three rows:
#' \describe{
#'   \item{PSoup}{The relationships supported by PSoup}
#'   \item{AF}{The equivalent relationship for the activity flow notation}
#'   \item{ER}{The equivalent relationship for the entitiy relationship notation}
#' }
"langConversion"
