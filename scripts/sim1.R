 
library("methods")
library("EpiModelHPC")
library("mardham2")

args <- commandArgs(trailingOnly = TRUE)
simno <- args[1]
jobno <- args[2]
fsimno <- paste(simno, jobno, sep = ".")
print(fsimno)

load("est/nwstats.rda")

param <- param.adol(nwstats = st, prep.elig.model = "none", 
                    prep.start = 1040,
                    prep.coverage.b=0 , prep.coverage.w=0 ,
                    prep.class.prob.b = c(.0, .0, .0, .0),
                    prep.class.prob.w = c(.0, .0, .0, .0),
                    heat.multiplier.b = 10,
                    heat.multiplier.w = 1,
                    ai.scale.b = 2.63,
                    ai.scale.w = 2.63,
                    prepSpell = TRUE)

init <- init.adol(nwstats = st)
control <- control.adol(simno = fsimno, nsteps = 52 * 40,
                        nsims = 16, ncores = 16, resim.int = 4,
                        save.int = 100, verbose.int = 10,
                        save.network = FALSE, save.other = NULL)

netsim_hpc("est/fit.rda", param, init, control, compress = "gzip")
