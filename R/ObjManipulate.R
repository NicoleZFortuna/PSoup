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
#' @slot travel specifies if the hormone travels between compartments. Hormones travel from
#'              their current container, to the other. Is a numeric value specifying the rate of
#'              travel. Is 0 if there is not travel between compartments. Default for hormones
#'              traveling down is 1, and up is 0.8 to reflect that signals moving up take a
#'              longer time than signals moving down.
#' @slot genotype data.frame with column names Gene, and Influence.
#' @export

Hormone <- setClass("Hormone", slots = c(name = "character",
                                         container = "character",
                                         inputs = "data.frame",
                                         outputs = "data.frame",
                                         travel = "numeric",
                                         genotypes = "character"))


#' A function to generate objects of class 'Hormone'
#'
#' @slot name the name of the gene. Will be used as a tag to place influence within the network.
#' @slot expression data.frame with column names Compartment, and Expression.
#'                 The compartment column can include "scion", or "rootstock".
#'                 The expression column is a numerical value set between 0 (no expression),
#'                 and 1 (full expression). All expression levels are set to the default 1
#'                 (the wildtype genotype).
#' @export

Genotype <- setClass("Genotype", slots = c(name = "character",
                                           expression = "data.frame"))

#' A function for building a network object
#'
#' This function accepts a list of nodes and a list of genotypes. It will check
#' if the objects are appropriate, and organise them into a single object of
#' class network. Will check the network to make sure that all objects and
#' genotypes have been pointed to, and will return a list of objects that have
#' not been.
#'
#' @param hormones a list containing objects of class hormone.
#' @param genotypes a list containing objects of class hormone.

network <- function(hormones, genotypes) {
  if (class(hormones) != "list" | class(genotypes) != "list") {
    stop("Make sure that you have provided you hormones and genotypes in list form")
  }


}

#' A function to list all objects of class hormone in the current environment
#'
#' @export

listNodes <- function() {
  Filter(function(x) inherits(get(x), "Hormone"), ls(envir=parent.env(environment())))
}

#' A function to list all objects of class genotype in the current environment
#'
#' @export

listGenotypes <- function() {
  Filter(function(x) inherits(get(x), "Genotype"), ls(envir=parent.env(environment())))
}

#' A function to restore the base model.
#'
#' All hormone and genotype objects will be purged from the environment,
#' and the standard objects reloaded.
#'
#' @export

restoreBaseModel <- function() {
  nodes = listNodes()
  rm(nodes)

  geno = listGenotypes()
  rm(geno)

  obj = data(package="peaSoup", verbose = T)$results[,3]
  for (i in obj) data(i)
}

#' A function to restore a constructed model.
#'
#' All hormone and genotype objects will be purged from the environment,
#' and the objects of the desired model will be loaded.
#'
#' @export
#'
loadMyModel <- function(wd = ".") {

}
