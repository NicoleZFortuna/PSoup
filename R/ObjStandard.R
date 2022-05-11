StrigolactoneR <- new("Hormone",
                      name = "StrigolactoneR",
                      container = "rootstock",
                      inputs = data.frame(Node = c("FeedbackR", "Inhibitor"),
                                          Influence = c("up regulate", "down regulate")),
                      outputs = data.frame(Node = "StrigolactoneS", Influence = "up regulate"),
                      travel = 0.8,
                      genotypes = c("RMS1", "RMS5", "RMS3", "RMS4"))

StrigolactoneS <- new("Hormone",
                      name = "StrigolactoneR",
                      container = "scion",
                      inputs = data.frame(Node = c("FeedbackS", "StrigolactoneR"),
                                          Influence = c("up regulate", "up regulate")),
                      outputs = data.frame(Node = "BranchInhibitor", Influence = "up regulate"),
                      travel = 0,
                      genotypes = c("RMS1", "RMS5"))

FeedbackR <- new("Hormone",
                 name = "FeedbackR",
                 container = "rootstock",
                 inputs = data.frame(Node = "FeedbackS",
                                     Influence = "up regulate"),
                 outputs = data.frame(Node = c("Cytokinin", "StrigolactoneR"),
                                      Influence = c("down regulate", "up regulate")),
                 travel = 0,
                 genotypes = "RMS2")

FeedbackS <- new("Hormone",
                 name = "FeedbackS",
                 container = "scion",
                 inputs = data.frame(Node = c("BranchInhibitor"),
                                     Influence = c("down regulate")),
                 outputs = data.frame(Node = c("FeedbackR", "StrigolactoneS"),
                                      Influence = c("up regulate", "up regulate")),
                 travel = 1,
                 genotypes = "RMS2")

Cytokinin <- new("Hormone",
                 name = "Cytokinin",
                 container = "rootstock",
                 inputs = data.frame(Node = "FeedbackR",
                                     Influence = "down regulate"),
                 outputs = data.frame(Node = "Bud Outgrowth",
                                     Influence = "up regulate"),
                 travel = 0.8)

BranchInhibitor = new("Hormone",
                      name = "BranchInhibitor",
                      container = "scion",
                      inputs = data.frame(Node = "StrigolactoneS",
                                          Influence = "up regulate"),
                      outputs = data.frame(Node = c("BudRelease", "FeedbackS"),
                                           Influence = c("down regulation", "down regulation")),
                      travel = 1,
                      genotypes = c("RMS3", "RMS4"))

BudRelease <- new("Hormone",
                  name = "BudRelease",
                  container = "scion",
                  inputs = data.frame(Node = "StrigolactoneS",
                                      Influence = "up regulation"),
                  outputs = data.frame(Node = c("BranchInhibitor",
                                                "FeedbackS"),
                                       Influence = c("down regulation",
                                                     "down regulation")),
                  travel = 0,
                  genotypes = c("RMS3", "RMS4"))

BranchOutgrowth <- new("Hormone",
                       name = "BranchOutgrowth",
                       container = "scion",
                       inputs = data.frame(Node = c("BudRelease",
                                                    "Cytokinin"),
                                           Influence = c("necessary stimulation",
                                                         "upregulation")),
                       outputs = data.frame(Node = "ShootSignal",
                                            Influence = "necessary stimulation"),
                       travel = 0)

ShootSignal <- new("Hormone",
                   name = "ShootSignal",
                   container = "scion",
                   inputs = data.frame(Node = "BranchOutgrowth",
                                       Influence = "necessary stimulation"),
                   outputs = data.frame(Node = c("StrigolactoneS",
                                                 "StrigolactoneR"),
                                        Influence = c("up regulation",
                                                      "up regulation")),
                   travel = 1)

RMS1 <- new("Genotype",
            name = "RMS1",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)))
RMS5 <- new("Genotype",
            name = "RMS5",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)))

RMS2 <- new("Genotype",
            name = "RMS2",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)))

RMS3 <- new("Genotype",
            name = "RMS3",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)))

RMS4 <- new("Genotype",
            name = "RMS4",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)))
