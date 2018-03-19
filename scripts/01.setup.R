
setwd("~/Camp")
## Race difference scenario setup file

rm(list = ls())
suppressPackageStartupMessages(library(mardham2))


# Time unit for simulation, relative to 1 day
time.unit <- 7

# Population size by race
num.B <- 5000
num.W <- 5000

# mean degree distributions matrices.
#In adol 60% is stable debut and .27 is extant ties among active.
# 0.6*0.27=.
deg.mp.B <- matrix(c(0.838, 0, 0,
                     0.162, 0, 0),
                   byrow = TRUE, nrow = 2)
deg.mp.W <- matrix(c(0.838, 0, 0,
                     0.162, 0, 0),
                   byrow = TRUE, nrow = 2)

# Revised rates
mdeg.inst.B <- matrix(c(0.0, 0.0, 0.0,
                        0.0, 0.0, 0.0),
                      byrow = TRUE, nrow = 2)
mdeg.inst.W <- matrix(c(0.0, 0.0, 0.0,
                        0.0, 0.0, 0.0),
                      byrow = TRUE, nrow = 2)

# Proportion in same-race partnerships (main, casl, inst)
prop.hom.mpi.B <- c(0.9, 0, 0)
prop.hom.mpi.W <- c(0.9, 0, 0)


#make smaller for small adolecent dist (not using adiff).
sqrt.adiff.BB <- c(0.417, 0.498, 0.456)/4
sqrt.adiff.BW <- c(0.454, 0.629, 0.585)/4
sqrt.adiff.WW <- c(0.520, 0.632, 0.590)/4



# New mean durations
#rates.main <- c(.049505, .049505, .049505)
rates.main <- .049505
rates.pers <- c(0, 0, 0)

durs.main <- 1/rates.main
durs.pers <- c(0, 0, 0)

#  Minimum age at entry .

birth.age.min<-13
birth.age.prob<-c(0.44, 0.13, 0.12, 0.11, 0.10, 0.10)
debut.age.prob = diag(length(birth.age.prob))

debut.entry.prob.b<-.5062467131
debut.entry.prob.w<-.5062467131
debut.prob.b<- 1-(1-.0004864819)^7
debut.prob.w<- 1-(1-.0004864819)^7


#ages<-18:40
ages <- birth.age.min:(birth.age.min+(length(birth.age.prob)-1))


# Quintile distribution weights for risk groups for partnership formation 
riskg.W <- c(.039682539, 0.079365079, 0.119047619 , 0.182539682, 0.579365079)
riskg.B <- c(.039682539, 0.079365079, 0.119047619 , 0.182539682, 0.579365079)


## Death reates need to be made to fit with a generalized age distribution.

#Temporary death rates (for testing age dist)
asmr.B <- c(rep(0, (birth.age.min-1)),
            1- (1 - c(rep(0.00225, (length(birth.age.prob)))))^ (1/(365/time.unit)),
            1)


            
asmr.W <- c(rep(0, (birth.age.min-1)),
            1 - (1 - c(rep(0.00133, (length(birth.age.prob)))))^ (1/(365/time.unit)),
            1)


# I, R, V role frequencies
role.B.prob <- c(0.145, 0.282, 0.573)
role.W.prob <- c(0.145, 0.282, 0.573)


# Create meanstats
st <- calc_nwstats.adol(
  method = 2,
  time.unit = time.unit,
  num.B = num.B,
  num.W = num.W,
  deg.mp.B = deg.mp.B,
  deg.mp.W = deg.mp.W,
 # mdeg.inst.B = mdeg.inst.B,
 # mdeg.inst.W = mdeg.inst.W,
  prop.hom.mpi.B = prop.hom.mpi.B,
  prop.hom.mpi.W = prop.hom.mpi.W,
  balance = "mean",
 # sqrt.adiff.BB = sqrt.adiff.BB,
 # sqrt.adiff.WW = sqrt.adiff.WW,
 # sqrt.adiff.BW = sqrt.adiff.BW,
  diss.main = ~offset(edges),
# diss.pers = ~offset(edges),
  durs.main = durs.main,
# durs.pers = durs.pers,
  ages = ages,
  asmr.B = asmr.B,
  asmr.W = asmr.W,
  birth.age.prob=birth.age.prob,
  debut.entry.prob.b=debut.entry.prob.b,
  debut.entry.prob.w=debut.entry.prob.w,
  debut.prob.b=debut.prob.b,
  debut.prob.w=debut.prob.w,
  role.B.prob = role.B.prob,
  role.W.prob = role.W.prob,
  riskg.B = riskg.B,
  riskg.W = riskg.W)

save(st, file = "scenarios/adol2race/est/nwstats.rda")
rm(list = ls())
