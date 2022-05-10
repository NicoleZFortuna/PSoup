#' A function to generate objects of class 'Hormone'
#'
#' @slot name the name of the hormone. Will be used as a tag to place node within the network.
#' @slot container the location of action for the hormone. Either "scion", or "rootstock".
#' @slot inputs data.frame with column names Node, and Influence. Influences can one of either
#'              "up regulate", "down regulate", "necessary stimulation", "necessary inhibition",
#'              "sufficient stimulation", "sufficient inhibition".
#' @slot outputs data.frame with column names Node, and Influence. Influences can one of either
#'              "up regulate", "down regulate", "necessary stimulation", "necessary inhibition",
#'              "sufficient stimulation", "sufficient inhibition".
#' @slot travel specifies if the hormone travels between compartments. Can be either
#'              "Up", "Down", or "None".
#' @slot genotype data.frame with column names Gene, and Influence.

Hormone <- setClass("Hormone", slots = c(name = "character",
                                         container = "character",
                                         inputs = "data.frame",
                                         outputs = "data.frame",
                                         travel = "character",
                                         genotypes = "character"))


#' A function to generate objects of class 'Hormone'
#'
#' @slot name the name of the gene. Will be used as a tag to place influence within the network.
#' @slot expression data.frame with column names Compartment, and Expression.
#'                 The compartment column can include "scion", or "rootstock".
#'                 The expression column is a numerical value set between 0 (no expression),
#'                 and 1 (full expression). All expression levels are set to the default 1
#'                 (the wildtype genotype).

Genotype <- setClass("Genotype", slots = c(name = "character",
                                           expression = "data.frame"))

