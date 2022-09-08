#' A function to simulate a network
#'
#' Takes the output file of the buildNetwork function and uses it to run a
#' network simulation.
#' @param file a file of the type produced by the buildModel function. Contains
#'             the starting values for nodes, defines the genotypes of the
#'             model, and defines the nextStep function for simulating the
#'             change at each timestep.
#' @param delay the amount of delay to apply for delayed action of nodes. The
#'             default is set to 2.
#' @param tmax the maximum number of steps that you want to simulate for. If
#'             set to NA (the default), will simulate until stability is reached.

simulateNetwork <- function(file, delay = 2, tmax = NA) {
  if (delay == 1) {
    warning("You have selected a delay of 1 which is functionaly equivalent to
            no delay at all.")
  } else if (delay < 1) {
    warning("You have chosen an invalid delay. The delay has been reset to 2.")
    delay = 2
  }

  source(file)

  simDat <- dat
  simDat[1:(tmax + delay - 1), ] <- NA
  simDat[1:delay, ] <- dat

  for (t in 2:tmax) {
    row = t + delay - 1
    simDat[row, ] <- nextStep(simDat[c((row-delay),(row-1)), ], gen, delay)
  }

  simDat = simDat[-c(1:(delay-1)), ]
  simDat
}
