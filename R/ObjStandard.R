StrigolactoneR <- new("Hormone",
                      name = "StrigolactoneR",
                      container = "rootstock",
                      inputs = data.frame(Node = c("Feedback", "Inhibitor"),
                                          Influence = c("up regulate", "down regulate")),
                      outputs = data.frame(Node = "StrigolactoneS", Influence = "up regulate"),
                      travel = "Up",
                      genotypes = c("RMS1", "RMS5"))

RMS1 <- new("Genotype",
            name = "RMS1",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)))
RMS5 <- new("Genotype",
            name = "RMS5",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)))
