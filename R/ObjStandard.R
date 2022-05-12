StrigolactoneR <- new("Hormone",
                      name = "StrigolactoneR",
                      container = "rootstock",
                      inputs = data.frame(Node = c("FeedbackR", "Inhibitor"),
                                          Influence = c("up regulate", "down regulate")),
                      outputs = data.frame(Node = "StrigolactoneS", Influence = "up regulate"),
                      travel = 0.8,
                      genotypes = c("RMS1", "RMS5", "RMS3", "RMS4"))
save(StrigolactoneR, file = "./Data/StrigolactoneR.RData")


StrigolactoneS <- new("Hormone",
                      name = "StrigolactoneR",
                      container = "scion",
                      inputs = data.frame(Node = c("FeedbackS", "StrigolactoneR"),
                                          Influence = c("up regulate", "up regulate")),
                      outputs = data.frame(Node = "BranchInhibitor", Influence = "up regulate"),
                      travel = 0,
                      genotypes = c("RMS1", "RMS5"))
save(StrigolactoneS, file = "./Data/StrigolactoneS.RData")

FeedbackR <- new("Hormone",
                 name = "FeedbackR",
                 container = "rootstock",
                 inputs = data.frame(Node = "FeedbackS",
                                     Influence = "up regulate"),
                 outputs = data.frame(Node = c("Cytokinin", "StrigolactoneR"),
                                      Influence = c("down regulate", "up regulate")),
                 travel = 0,
                 genotypes = "RMS2")
save(FeedbackR, file = "./Data/FeedbackR.RData")

FeedbackS <- new("Hormone",
                 name = "FeedbackS",
                 container = "scion",
                 inputs = data.frame(Node = c("BranchInhibitor"),
                                     Influence = c("down regulate")),
                 outputs = data.frame(Node = c("FeedbackR", "StrigolactoneS"),
                                      Influence = c("up regulate", "up regulate")),
                 travel = 1,
                 genotypes = "RMS2")
save(FeedbackS, file = "./Data/FeedbackS.RData")

Cytokinin <- new("Hormone",
                 name = "Cytokinin",
                 container = "rootstock",
                 inputs = data.frame(Node = "FeedbackR",
                                     Influence = "down regulate"),
                 outputs = data.frame(Node = "Bud Outgrowth",
                                     Influence = "up regulate"),
                 travel = 0.8)
save(Cytokinin, file = "./Data/Cytokinin.RData")

BranchInhibitor = new("Hormone",
                      name = "BranchInhibitor",
                      container = "scion",
                      inputs = data.frame(Node = "StrigolactoneS",
                                          Influence = "up regulate"),
                      outputs = data.frame(Node = c("BudRelease", "FeedbackS"),
                                           Influence = c("down regulation", "down regulation")),
                      travel = 1,
                      genotypes = c("RMS3", "RMS4"))
save(BranchInhibitor, file = "./Data/BranchInhibitor.RData")

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
save(BudRelease, file = "./Data/BudRelease.RData")

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
save(BranchOutgrowth, file = "./Data/BranchOutgrowth.RData")

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
save(ShootSignal, file = "./Data/ShootSignal.RData")

RMS1 <- new("Genotype",
            name = "RMS1",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)))
save(RMS1, file = "./Data/RMS1.RData")

RMS5 <- new("Genotype",
            name = "RMS5",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)))
save(RMS5, file = "./Data/RMS5.RData")

RMS2 <- new("Genotype",
            name = "RMS2",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)))
save(RMS2, file = "./Data/RMS2.RData")

RMS3 <- new("Genotype",
            name = "RMS3",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)))
save(RMS3, file = "./Data/RMS3.RData")

RMS4 <- new("Genotype",
            name = "RMS4",
            expression = data.frame(Container = c("scion", "rootstock"),
                                    Expression = c(1, 1)))
save(RMS4, file = "./Data/RMS4.RData")




