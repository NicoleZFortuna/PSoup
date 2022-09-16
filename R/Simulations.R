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
#' @param genotype default set to NA. Allows the user to provide a different gen
#'             data.frame from the one specified in the provided file.
#' @param startingValues default set to NA. Allows the user to provide a different
#'             dat data.frame from the one specified in the provided file.

simulateNetwork <- function(file, delay = 2, tmax = NA, genotype = NA, startingValues = NA) {
  if (delay == 1) {
    warning("You have selected a delay of 1 which is functionaly equivalent to
            no delay at all.")
  } else if (delay < 1) {
    warning("You have chosen an invalid delay. The delay has been reset to 2.")
    delay = 2
  }

  source(file, local = T)

  if (!is.na(genotype)) {
    gen <- genotype
  }
  if (!is.na(startingValues)) {
    dat <- startingValues
  }

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

#' A wrapper function that allows you to run simulations of a network under
#' many differet conditions.
#'
#' This function allows you to set up a variety of genotype conditions to be
#' simulated. It also allows you to test the outcome of simulations under
#' randomly assigned starting values. The function will make sure to include
#' one wild type condition (all genotypes set to 1) regardless of if specified
#' by the user.
#' @param file a file of the type produced by the buildModel function. Contains
#'             the starting values for nodes, defines the genotypes of the
#'             model, and defines the nextStep function for simulating the
#'             change at each timestep.
#' @param randomStart the number of random starting point reassignments that
#'        you want to test. Starting points will be generated for each node,
#'        pulled from a uniform distribution ranging from 0 to 2. The default
#'        is set to NA, in which case only the provided start values (as stated
#'        in the provided file) will be run.
#' @param delay the amount of delay for any delayed transport. Default set to 2.
#' @param tmax the maximum number of timesteps for which to simulate.
setupSims <- function(file, randomStart = NA, delay = 2, tmax = NA) {
  source(file, local = T)

  if (!is.na(randomStart)) {
    dat[(nrow(dat) + 1):(nrow(dat) + randomStart), ] <- runif(randomStart*ncol(dat), 0, 2)
  }

  if (any(!apply(gen, 1, function(x) any(x != rep(1, ncol(gen)))))) {
    # if there are any rows in gen that contain a WT condition
    WT = which(!apply(gen, 1, function(x) any(x != rep(1, ncol(dat)))))
    if (WT != 1)
      # make sure that the first row is the WT condition
      gen <- gen[c(WT, c(1:nrow(gen))[-WT]), ]
  } else {
    # adding a row at the top with WT conditions
    gen[1:nrow(gen)+1, ] <- gen
    gen[1, ] <- 1
  }

  sims <- list()
  i = 1
  for (d in 1:nrow(dat)) {
    for (g in 1:nrow(gen)) {
      sims[[i]] <- list(scenario = list(genotype = gen[g, ],
                                        startingValues = dat[d, ]),
                        simulation = simulateNetwork(file = file,
                                                     delay = delay,
                                                     tmax = tmax,
                                                     genotype = gen[g, ],
                                                     startingValues = dat[d, ]))
      i = i + 1
    }
  }

  sims
}
