# A function to check the internal consistency of a Network object.
#
# This function checks the inputs and outputs of all nodes in the
# network to make sure that all inputs and outputs for nodes are
# in agreement with each other. In addition, checks that the
# genotypes are consistent in their coregulators, and that they
# agree with the nodes in terms of regulation.
#
# @param network an object of class network.
consistencyCheck <- function(network) {
  Hnames <- names(network@objects$Hormones)
  Gnames <- names(network@objects$Genotypes)

  for (i in Hnames) {
    for (j in 1:nrow(network@objects$Hormones[[i]]@inputs)) {
      network@objects$Hormones[[i]]@inputs
    }

  }

  # travel
}

# validityCheck

