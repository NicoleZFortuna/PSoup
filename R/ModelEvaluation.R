#' A function to check the internal consistency of a Network object.
#'
#' This function checks the inputs and outputs of all nodes in the
#' network to make sure that all inputs and outputs for nodes are
#' in agreement with each other. In addition, checks that the
#' genotypes are consistent in their coregulators, and that they
#' agree with the nodes in terms of regulation.
#'
#' @param network an object of class network.
#' @export

consistencyCheck <- function(network) {
  Hnames <- names(network@objects$Hormones)
  Gnames <- names(network@objects$Genotypes)

  errorList = list()
  eL=1

  # Do not need to make sure that the R object name matches the internal
  # object name. The buildNetwork function names all the objects based
  # on the internal name.

  # Checking that the stated relationships between hormones are consistent
  # between all hormone objects in the network
  for (h in 1:length(Hnames)) {
    key = network@objects$Hormones[[h]]@name

    # Check that the node has an input
    if (nrow(network@objects$Hormones[[key]]@inputs) > 0) {
      # Cycle through inputs
      for (i in 1:nrow(network@objects$Hormones[[key]]@inputs)) {
        inKey = network@objects$Hormones[[key]]@inputs$Node[i]

        #Checking that the input key refers to a node in the network
        if (!inKey %in% Hnames) {
          errorList[[eL]] <- paste("The input node", inKey, "of the", key,
                                   "node does not exist.")
          eL = eL + 1
        } else {
          # Check that output node lists key as an input
          if (!key %in% network@objects$Hormones[[inKey]]@outputs$Node) {
            errorList[[eL]] <- paste("The nodes", key, "and", inKey,
                                     "disagree on whether they have a relationship.")
            eL = eL + 1
          } else {
            #Checking that the input and output of associated nodes are matching
            if (network@objects$Hormones[[key]]@inputs$Influence[i] !=
                network@objects$Hormones[[inKey]]@outputs$Influence[network@objects$Hormones[[inKey]]@outputs$Node==key]) {
              errorList[[eL]] <- paste("The nodes", key, "and", inKey,
                                       "disagree on the nature of their relationship.")
              eL = eL + 1
            }
          }
        }

        #Check if the input has a coregulator
        if (!is.na(network@objects$Hormones[[key]]@inputs$Coregulator[i])) {
          coKey = network@objects$Hormones[[key]]@inputs$Coregulator[i]
          #Check that the output coregulator disagrees
          if (coKey !=
              network@objects$Hormones[[coKey]]@outputs$Coregulator[network@objects$Hormones[[coKey]]@outputs$Node==key])
              errorList[[eL]] <- paste("The nodes", key, "and", coKey,
                                       "disagree on whether they corregulate.")
              eL = eL + 1
        }

      }
    }

    # Check that the node has an output
    if (nrow(network@objects$Hormones[[key]]@outputs) > 0) {
      # Cycle through outputs
      for (o in 1:nrow(network@objects$Hormones[[key]]@outputs)) {
        outKey = network@objects$Hormones[[key]]@outputs$Node[o]

        #Checking that the output key refers to a node in the network
        if (!outKey %in% Hnames) {
          errorList[[eL]] <- paste("The output node", outKey, "of the", key,
                                   "node does not exist.")
          eL = eL + 1
        } else {
          # Checking that the input node lists the key as an output node
          if (!key %in% network@objects$Hormones[[outKey]]@inputs$Node) {
            errorList[[eL]] <- paste("The nodes", key, "and", outKey,
                                     "disagree on whether they have a relationship.")
            eL = eL + 1
          } else {
            # Checking that the stated relationship between the two nodes is consistent
            if (network@objects$Hormones[[key]]@outputs$Influence[o] !=
                network@objects$Hormones[[outKey]]@inputs$Influence[network@objects$Hormones[[outKey]]@inputs$Node==key]) {
              errorList[[eL]] <- paste("The nodes", key, "and", outKey,
                                       "disagree on the nature of their relationship.")
              eL = eL + 1
            }
          }
        }

        #Check if the output has a coregulator
        if (!is.na(network@objects$Hormones[[key]]@outputs$Coregulator[o])) {
          coKey = network@objects$Hormones[[key]]@outputs$Coregulator[o]
          #Check that the input coregulator disagrees
          if (coKey !=
              network@objects$Hormones[[coKey]]@inputs$Coregulator[network@objects$Hormones[[coKey]]@inputs$Node==key])
            errorList[[eL]] <- paste("The nodes", key, "and", coKey,
                                     "disagree on whether they corregulate.")
          eL = eL + 1
        }
      }
    }

    # Check that any listed genotype, lists the hormone as being under
    # its influence

    # Check that the hormone object has any genotype
    if (length(network@objects$Hormones[[h]]@genotypes) > 0) {
      for (g in length(network@objects$Hormones[[h]]@genotypes)) {
        gKey = network@objects$Hormones[[h]]@genotypes[g]

        # Check if the genotype key refers to a genotype object
        if (!gKey %in% Gnames) {
          errorList[[eL]] <- paste("The node", key, "lists the genotype", gKey,
                                   "which cannot be found in the network.")
          eL = eL + 1
        } else {
          # Check if hormone is listed in the genotypes influencers
          if (!key %in% network@objects$Genotypes[[gKey]]@influence$Node) {
            errorList[[eL]] <- paste("The node", key, "and the genotype", gKey,
                                     "do not agree on having a relationship.")
            eL = eL + 1
          }
        }
      }
    }
  }

  # Checking that the information contained within genotype objects is
  # consistent with other genotypes, as well as hormones

  # Cycle through all genotype objects
  for (g in 1:length(Gnames)) {
    gKey = network@objects$Genotypes[[g]]@name
    # Check if there is a coregulator
    if (length(network@objects$Genotypes[[g]]@coregulator) > 0) {
      for (cr in 1:length(network@objects$Genotypes[[g]]@coregulator)) {
        crKey = network@objects$Genotypes[[g]]@coregulator[cr]

        # Check if the coregulator exists in the network
        if (!crKey %in% Gnames) {
          errorList[[eL]] <- paste("The", gKey, "coregulator", crKey,
                                   "cannot be found in the network.")
          eL = eL + 1
        } else {
          # Check that the coregulator refers back
          if (!gKey %in% network@objects$Genotypes[[crKey]]@coregulator) {
            errorList[[eL]] <- paste("The", gKey, "and", crKey,
                                     "genotypes disagree about being coregulators.")
            eL = eL + 1
          }
        }
      }
    }

    # Cycle through the hormones that the genotype has an influence on
    for (i in 1:nrow(network@objects$Genotypes[[g]]@influence)) {
      iKey = network@objects$Genotypes[[g]]@influence$Node[i]

      # Check that the influenced hormone exists
      if (!iKey %in% Hnames) {
        errorList[[eL]] <- paste("The", gKey, "influenced hormone", iKey,
                                 "cannot be found.")
        eL = eL + 1
      } else {
        # Check that the influenced hormone refers back to the genotype
        if (!gKey %in% network@objects$Hormones[[iKey]]@genotypes) {
          errorList[[eL]] <- paste("The", gKey, "and", iKey,
                                   "disagree on having a relationship.")
        }
      }
    }
  }

  # travel?

  if (length(errorList) > 0) {
    stop(writeLines(paste(errorList, sep = "/n")))
  } else {
    print("Network is internally consistant.")
  }
}

# validityCheck



