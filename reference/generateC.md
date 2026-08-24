# A function to generate a C script which will execute a simulation of the network given a starting condition.

A function to generate a C script which will execute a simulation of the
network given a starting condition.

## Usage

``` r
generateC(
  network,
  maxStep = 100,
  steadyThreshold = 4,
  folder = "./Model",
  forceOverwrite = FALSE,
  ruleStyle = "Dun",
  necStimFunc = NULL,
  sharp = FALSE,
  exogenous = TRUE
)
```

## Arguments

- network:

  an object of class network.

- maxStep:

  the maximum value that the simulation will be allowed ton proceed. If
  the midpoint is reached, a warning will be returned. The default value
  is set to 0.

- steadyThreshold:

  the number of decimal places to which node values must be equivalent
  to be considered a steady state. This threshold must be passed for all
  nodes.

- folder:

  the name of the folder that you want the components of the model to be
  saved to. The default is to create a directory called Model in the
  current working directory. The user can provide their own directory
  and folder name. In this folder will be generated three R scripts: one
  to define genotypes (genotypeDef.R), one to define the starting node
  values (nodestartDef.R), and one to define the function that gives the
  difference equations that are used to simulate the network
  (nextStep.R).

- forceOverwrite:

  default set to FALSE. Will stop the function if the folder already
  exits. Can set to true if you want to replace the existing folder.

- ruleStyle:

  either "Dun", or "Mike". The Dun style resembles the original Dun
  equations normalised such that WT conditions are always 1. The Mike
  style creates mirrored stimulatory and inhibitory effects.

- necStimFunc:

  the name of the function to be applied to necessary stimulants. The
  default is NULL, in which case no function will be applied, ant
  therefore the form will be linear.

- sharp:

  logical. Indicates if the code is being generated is C# rather than C.

- exogenous:

  default set to TRUE. This argument allows you to tailor the
  construction of a model in either C or C#. If TRUE, the generated code
  will allow for an exogenous supply to be added to the model.
