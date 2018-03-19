
suppressPackageStartupMessages(library(mardham2))
rm(list = ls())

load("scenarios/adol2race/est/nwstats.rda")


# 1. Main Model -----------------------------------------------------------

# Initialize network
nw.main <- base_nw.adol(st)

# Assign degree
nw.main <- assign_degree(nw.main, deg.type = "pers", nwstats = st)

# Formulas
formation.m <- ~edges +
                nodemix("race",base=3)+ nodefactor(c("race", "riskg"), base = c(3,8))  +
                offset(nodematch("role.class", diff = TRUE, keep = 1:2)) +
                offset(nodefactor("has.debuted",base=2))


# Fit model
fit.m <- netest(nw.main,
                formation = formation.m,
                coef.form = c(-Inf, -Inf,-Inf),
                target.stats = st$stats.m,
                coef.diss = st$coef.diss.m,
                verbose=TRUE,
                set.control.ergm = control.ergm(MCMC.interval=2048,
                                                MCMC.samplesize=2048,
                                                MPLE.max.dyad.types = 1e10,
                                                init.method = "MPLE",
                                                MCMLE.maxit = 200,
                                                SAN.maxit=100,
                                                SAN.burnin.times = 500))




######################### testing  ##########################.
# 2. Casual Model ---------------------------------------------------------

# Initialize network
nw.pers <- nw.main


# Formulas
formation.p <- ~edges

# Fit model
fit.p <- netest(nw.pers,
                formation = formation.p,
                target.stats = 0,
                coef.diss = st$coef.diss.p,
                set.control.ergm = control.ergm(MPLE.max.dyad.types = 1e9,
                                                init.method = "zeros",
                                                MCMLE.maxit = 100,
                                                SAN.maxit=50,
                                                SAN.burnin.times = 400))


# Fit inst model ----------------------------------------------------------

# Initialize network
nw.inst <- nw.main

# Formulas
formation.i <- ~edges 

# Fit model
fit.i <- netest(nw.inst,
                formation = formation.i,
                target.stats = 0,
                coef.diss = st$coef.diss.i,
                set.control.ergm = control.ergm(MPLE.max.dyad.types = 1e9,
                                                init.method = "zeros",
                                                MCMLE.maxit = 200,
                                                SAN.maxit=100,
                                                SAN.burnin.times = 100))

# Save data
est <- list(fit.m, fit.p, fit.i)
save(est, file = "scenarios/adol2race/est/fit.rda")


# Diagnostics -------------------------------------------------------------

#dx <- netdx(fit.i, nsims = 10000, ncores = 1, dynamic = FALSE,
#            nwstats.formula = ~ edges + nodefactor(c("race", "riskg"), base = 0))
#dx
