#' A function to generate objects of class 'Hormone'
#'
#' @slot name the name of the hormone. Will be used as a tag to place node within the network.
#' @slot container the location of action for the hormone. Either "scion", or "rootstock".
#' @slot inputs data.frame with column names Node, Coregulator, Influence, and Delay. Influences can
#'              one of either "stimulation", "inhibition", "necessary stimulation",
#'              "necessary inhibition", "altSource" (in the case that some hormone is produced
#'              somewhere else and travels to the location), and "Unknown". Delay gives the
#'              interval of time by which the effect is delayed.
#' @slot outputs data.frame with column names Node, Coregulator, and Influence. Influences can
#'              one of either "stimulation", "inhibition", "necessary stimulation",
#'              "necessary inhibition", "altSource" (in the case that some hormone is produced
#'              somewhere else and travels to the location), and "Unknown". Delay gives the
#'              interval of time by which the effect is delayed.
#' @slot travel specifies if the hormone travels between compartments. Hormones travel from
#'              their current container, to the other. Is a numeric value specifying the rate of
#'              travel. Is 0 if there is no travel between compartments. Default for hormones
#'              traveling down is 1, and up is 0.8 to reflect that signals moving up take a
#'              longer time than signals moving down.
#' @slot degradation the rate at which the hormone degrades.
#' @slot genotypes a vector with objects of class character. Lists the genotypes that
#'              are important to this node.
#' @export
#' @examples
#' NA

Hormone <- setClass("Hormone", slots = c(name = "character",
                                         container = "character",
                                         inputs = "data.frame",
                                         outputs = "data.frame",
                                         travel = "numeric",
                                         degradation = "numeric",
                                         genotypes = "character"))


#' A function to generate objects of class 'Genotype'
#'
#' @slot name the name of the gene. Will be used as a tag to place influence within the network.
#' @slot expression numeric vector with names "scion", and "rootstock".
#'                 Numerical values set between 0 (no expression),
#'                 and 1 (full expression). The default expression is 1
#'                 (the wildtype genotype).
#' @slot coregulator of class character. The names of other genotypes that coregulate together.
#' @slot influence a data.frame with the column names Node and Influence. Influences can one
#'                 of either "production", "degradation", "inhibition", "perception".
#' @export
#' @examples
#' NA

Genotype <- setClass("Genotype", slots = c(name = "character",
                                           expression = "numeric",
                                           coregulator = "character",
                                           influence = "data.frame"))

#' A function to generate objects of class 'Network'
#'
#' @slot name the name of the network.
#' @slot model a list of length 2 with the names Hormones and Genotypes

Network <- setClass("Network", slots = c(name = "character",
                                         objects = "list"))

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
#' @param name the name to be given to the network
#' @importFrom methods new
#' @importFrom methods is
#' @export
#' @examples
#' NA

buildNetwork <- function(hormones, genotypes, name) {
  # Making sure that data will be of the correct format
  if (!is(hormones, "list") | !is(genotypes, "list")) {
    stop("Make sure that you have provided you hormones and genotypes in list form")
  }

  h = NULL
  for (i in 1:length(hormones)) {
    if (!is(hormones[[i]], "Hormone")) {
      h = c(h, i)
    }
  }

  g = NULL
  for (i in 1:length(genotypes)) {
    if (!is(genotypes[[i]], "Genotype")) {
      g = c(g, i)
    }
  }

  if (!is.null(h)) {
    print(paste("Hormone object:", h, "of incorrect data type."))
  }

  if (!is.null(g)) {
    print(paste("Hormone object:", g, "of incorrect data type."))
  }

  if (!is.null(h) | !is.null(g)) {
    stop("Please provide objects of correct data type!")
  }

  # Making sure that data is correctly self referential
    # stop if points to something that doesn't exist
    # stop if objects are not mutually referential
  Hnames <- rep(NA, length(hormones))
  for (i in 1:length(hormones)) {
    Hnames[i] <- hormones[[i]]@name
  }
  names(hormones) <- Hnames

  Gnames <- rep(NA, length(genotypes))
  for (i in 1:length(genotypes)) {
    Gnames[i] <- genotypes[[i]]@name
  }
  names(genotypes) <- Gnames

  # Build object of class network
  network <- methods::new("Network",
                    name = name,
                    objects = list(Hormones = hormones,
                                   Genotypes = genotypes))

  return(network)
}

#' This is a function which specifies the print method for objects
#' of class Network.
#'
#' This method is applied automatically with out needing to call
#' the print function.
#' @export
#' @examples
#' NA

print.Network <- setMethod(f = "show",
  signature = "Network",
  definition = function(object){
  writeLines("This is an object of class Network. It contains:")
  writeLines(paste(length(object@objects$Hormones), "hormones"))

  writeLines(paste0("\t", names(object@objects$Hormones)[order(names(object@objects$Hormones))]))

  writeLines(paste(length(object@objects$Genotypes), "genotypes"))

  writeLines(paste0("\t", names(object@objects$Genotypes)[order(names(object@objects$Genotypes))]))
  # list out what the relevant hormones and genotypes are?
})

## print method for hormone class?

## print method for genotype class?

#' A function to list all objects of class hormone in the current environment
#'
#' @param base logical. Indicates whether the user wants to return a
#'             list of nodes that were contained in the original
#'             base package. Default is set to true. If the user
#'             wants to return nodes listed in the main environment
#'             (nodes that they have built themselves), they should
#'             set this parameter to FALSE.
#' @export
#' @examples
#' listNodes()

listNodes <- function(base = T) {
  #browser()
  if (base == T) {
    Filter(function(x) inherits(get(x), "Hormone"),
           ls(envir=parent.env(parent.env(environment()))))
  } else {
    Filter(function(x) inherits(get(x), "Hormone"),
         ls(envir=parent.env(environment())))
  }
}

#' A function to list all objects of class genotype in the current environment
#'
#' @param base logical. Indicates whether the user wants to return a
#'             list of genotypes that were contained in the original
#'             base package. Default is set to true. If the user
#'             wants to return genotypes listed in the main environment
#'             (genotypes that they have built themselves), they should
#'             set this parameter to FALSE.
#' @export
#' @examples
#' NA

listGenotypes <- function(base = T) {
  if (base == T) {
    Filter(function(x) inherits(get(x), "Genotype"),
           ls(envir=parent.env(parent.env(environment()))))
  } else {
    Filter(function(x) inherits(get(x), "Genotype"),
         ls(envir=parent.env(environment())))
  }
}

#' A function to restore the base model.
#'
#' All hormone and genotype objects will be purged from the environment,
#' and the standard objects reloaded.
#'
#' @export
#' @examples
#' NA

restoreBaseModel <- function() {

  nodes = listNodes()
  rm(nodes)

  geno = listGenotypes()
  rm(geno)

  files <- paste0(getwd(), "/", list.files("Data", full.names = T))

  #obj = data(package="PSoup", verbose = T)$results[,3]
  for (i in files) load(i, envir = parent.env(environment()))
}

#' A function to generate a data frame which lists all of
#' the edges of a network.
#'
#' @param network an object of class Network.
#' @param keepAltSource default set to FALSE. Specify if
#'        you want to recognise if the incoming node
#'        is an alternate source of the same 'hormone' type.
#'        If not, these alternative sources will be reported
#'        as stimulants.
#' @param keepNecessaryStimulant default set to FALSE.
#'        Specify if you want to distinguish necessary
#'        stimulants from stimulants.

generateEdgeList <- function(network, keepAltSource = F,
                             keepNecessaryStimulant = F) {
  # function to collect a data.frame of wanted info for
  # a particular node
  get <- function(x) {
    got <- data.frame(From = x@inputs$Node,
                      To = rep(x@name, nrow(x@inputs)),
                      Influence = x@inputs$Influence)
    got
  }

  # collect wanted info all at once
  frames <- lapply(network@objects$Hormones,
                   FUN = function(x) get(x))

  # merge info into single data frame
  dat <- do.call("rbind", frames)

  # remove unwanted rownames
  rownames(dat) = NULL

  # change altSource influences to stimulation if needed
  if (keepAltSource == F) {
    dat$Influence[dat$Influence == "altSource"] <- "stimulation"
  }

  # change altSource influences to stimulation if needed
  if (keepNecessaryStimulant == F) {
    dat$Influence[dat$Influence == "necessary stimulation"] <- "stimulation"
  }

  dat
}

#' a function to move the example .sbgn file into a folder of the users choice.
#'
#' @param folder a folder on the users computer. If the folder exists already
#'        on the users computer, the file will be moved there, otherwise the
#'        folder will be generated before moving the file to the specified
#'        location.
#' @details The function will return TRUE if the file has been placed in the
#'        requested folder. If the file already exists in the specified
#'        location, FALSE will be returned.
#' @export

getExampleDiagram <- function(folder, ...) {
  location <- paste0(system.file(package = "PSoup"))
  # check if folder exists, if not, create it
  if (!dir.exists(folder)) {
    dir.create(folder)
  }

  file.copy(from = paste0(location, "/DunAFgenotype.sbgn"),
            to = paste0(folder, "/DunAFgenotype.sbgn"),
            overwrite = FALSE, recursive = FALSE)
}
