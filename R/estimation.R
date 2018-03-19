
#' @title Calculate Target Statistics for Network Model Estimation
#'
#' @description Calculates the target statistics for the formation and dissolution
#'              components of the network model to be estimated with \code{netest}.
#'
#' @param time.unit Time unit relative to 1 for daily.
#' @param method Method for calculating target statistics by race, with options of
#'        \code{2} for preserving race-specific statistics and \code{1} for
#'        averaging over the statistics and dropping the race-specific terms.
#' @param num.B Population size of black MSM.
#' @param num.W Population size of white MSM.
#' @param deg.mp.B Degree distribution matrix for main and casual partners for
#'        black MSM, as a 2 by 3 matrix.
#' @param deg.mp.W Degree distribution matrix for main and causal partners for
#'        white MSM, as a 2 by 3 matrix.
#' @param mdeg.inst.B Mean degree, or rate, of one-off partnerships per day
#'        for black MSM.
#' @param mdeg.inst.W Mean degree, or rate, of one-off partnerships per day
#'        for white MSM.
#' @param prop.hom.mpi.B A vector of length 3 for the proportion of main, casual,
#'        and one-off partnerships in same race for black MSM.
#' @param prop.hom.mpi.W A vector of length 3 for the proportion of main, casual,
#'        and one-off partnerships in same race for white MSM.
#' @param balance Method for balancing of edges by race for number of mixed-race
#'        partnerships, with options of \code{"black"} to apply black MSM counts,
#'        \code{"white"} to apply white MSM counts, and \code{"mean"} to take
#'        the average of the two expectations.
#' @param sqrt.adiff.BB Vector of length 3 with the mean absolute differences
#'        in the square root of ages in main, casual, and one-off black-black
#'        partnerships.
#' @param sqrt.adiff.WW Vector of length 3 with the mean absolute differences
#'        in the square root of ages in main, casual, and one-off white-white
#'        partnerships.
#' @param sqrt.adiff.BW Vector of length 3 with the mean absolute differences
#'        in the square root of ages in main, casual, and one-off black-white
#'        partnerships.
#' @param diss.main Dissolution model formula for main partnerships.
#' @param diss.pers Dissolution model formula for casual partnerships.
#' @param durs.main Vector of length 3 with the duration of BB, BW, and WW main
#'        partnerships in days.
#' @param durs.pers Vector of length 3 with the duration of BB, BW, and WW
#'        casual partnerships in days.
#' @param ages Integer vector of ages in years that defines range of possible
#'        initial ages in the population.
#' @param birth.age.prob vector of probabilities that defines the age of entry into the population 
#'        and the initial age distribution of the population if all new entries are not the min(age).        
#' @param asmr.B Vector of length 40 defining the age-specific
#'        mortality rate for persons within that age slot, for black MSM.
#' @param asmr.W Vector of length 40 defining the age-specific
#'        mortality rate for persons within that age slot, for white MSM.
#' @param role.B.prob Vector of length 3 for the probability of sexual role as
#'        insertive, receptive, and versatile, for black MSM.
#' @param role.W.prob Vector of length 3 for the probability of sexual role as
#'        insertive, receptive, and versatile, for white MSM.
#' @param riskg.B Vector of length 5 with the proportion of ties for each risk quintile.
#' @param riskg.W Vector of length 5 with the proportion of ties for each risk quintile.
#'
#' @details
#' This function performs basic calculations to determine the components of the
#' formationa and dissolution models for the network model estimation to be
#' conducted with \code{\link{netest}}. The inputs inputs for this function are
#' calculated externally to the package in a setup scenario file.
#'
#' @seealso
#' Network statistics calculated here are entered into \code{\link{base_nw.adol}}
#' to construct the base network, and then into the parameters in
#' \code{\link{param.adol}}.
#'
#' @export
#'
calc_nwstats.adol <- function(time.unit = 7, method,
                              num.B, num.W, deg.mp.B, deg.mp.W,
                              prop.hom.mpi.B, prop.hom.mpi.W, balance = "mean",
                              diss.main, durs.main, ages, asmr.B, asmr.W, 
                              birth.age.prob, debut.entry.prob.b, debut.entry.prob.w,
                              debut.prob.b,debut.prob.w,
                              role.B.prob, role.W.prob, riskg.B, riskg.W) {
  
 
  
  if (sum(deg.mp.B) != 1) {
    stop("deg.mp.B must sum to 1.")
  }
  if (sum(deg.mp.W) != 1) {
    stop("deg.mp.W must sum to 1.")
  }
  if (!(method %in% 1:2)) {
    stop("method must either be 1 for one-race models or 2 for two-race models", call. = FALSE)
  }

  num <- num.B + num.W


  # deg main nodal attribute
  if (method == 2) {
    deg.main.B <- apportion_lr(num.B, c("B0", "B1"), rowSums(deg.mp.B))
    deg.main.W <- apportion_lr(num.W, c("W0", "W1"), rowSums(deg.mp.W))
  }
  if (method == 1) {
    deg.main <- apportion_lr(num, 0:1, rowSums(deg.mp.W))
  }

    # deg.pers nodal attribute
 
  if (method == 2) {
   
    deg.pers.B <- apportion_lr(num.B, c("B0", "B1", "B2"), colSums(deg.mp.B))
    deg.pers.W <- apportion_lr(num.W, c("W0", "W1", "W2"), colSums(deg.mp.W))
    
  }
 
  if (method == 1) {
   deg.pers <- apportion_lr(num, 0:2, colSums(deg.mp.W))
  }
  
  
  # Main partnerships -------------------------------------------------------

  # Persons in partnerships by casual degree
  if (method == 2) {
    totdeg.m.by.dp <- c(num.B * deg.mp.B[2, ], num.W * deg.mp.W[2, ])
  }
  if (method == 1) {
    totdeg.m.by.dp <- c(num * deg.mp.B[2, ])
  }

  # Persons in partnerships by race
  if (method == 2) {
    totdeg.m.by.race <- c(sum(totdeg.m.by.dp[1:3]), sum(totdeg.m.by.dp[4:6]))
  }

  # Number of partnerships
  edges.m <- (sum(totdeg.m.by.dp)) / 2

  # Mixing
  if (method == 2) {
    # Number of mixed-race partnerships, with balancing to decide
    edges.m.B2W <- totdeg.m.by.race[1] * (1 - prop.hom.mpi.B[1])
    edges.m.W2B <- totdeg.m.by.race[2] * (1 - prop.hom.mpi.W[1])
    edges.het.m <- switch(balance,
                          black = edges.m.B2W,
                          white = edges.m.W2B,
                          mean = (edges.m.B2W + edges.m.W2B) / 2)

    # Number of same-race partnerships
    edges.hom.m <- (totdeg.m.by.race - edges.het.m) / 2

    # Nodemix target stat: numer of BB, BW, WW partnerships
    edges.nodemix.m <- c(edges.hom.m[1], edges.het.m, edges.hom.m[2])
  }

  # Sqrt absdiff term for age
 # if (method == 2) {
  
  #  sqrt.adiff.m <- edges.nodemix.m * c(sqrt.adiff.BB[1], sqrt.adiff.BW[1], sqrt.adiff.WW[1])
  #}
  #if (method == 1) {
  #  sqrt.adiff.m <- edges.m * mean(c(sqrt.adiff.BB[1], sqrt.adiff.BW[1], sqrt.adiff.WW[1]))
  #}
  
  # Risk quantiles

    if (method == 2) {
      num.riskg.B <- (edges.m*(num.B/(num.B+num.W))*riskg.B)*2
      num.riskg.W <- (edges.m*(num.W/(num.B+num.W))*riskg.W)*2
    }
    if (method == 1) {
      num.riskg <- (edges.m*riskg.W)*2
    }

  # Compile target stats
  if (method == 2) {
    stats.m <- c(edges.m, edges.nodemix.m[-3], num.riskg.B[-3], num.riskg.W[-3])
  }
  if (method == 1) {
    stats.m <- c(edges.m, num.riskg[-3])
  }

  # Dissolution model
  exp.mort <- (mean(asmr.B[ages]) + mean(asmr.W[ages])) / 2

  coef.diss.m <- dissolution_coefs(dissolution = diss.main,
                                   duration = durs.main / time.unit,
                                   d.rate = exp.mort)
  
  coef.diss.p <- dissolution_coefs(dissolution = diss.main,
                                   duration = 7,
                                   d.rate = 0)
  
  coef.diss.i <- dissolution_coefs(dissolution = diss.main,
                                   duration = 7,
                                   d.rate = 0)

   ##Age at entry: birth.age.prob-a vector summing to one for different probabilities for ages of entry or set to 1 for everyone enters at same age.
  
  if (is.null(birth.age.prob)) birth.age.prob<-1
  
  if ((sum(birth.age.prob)) != 1) {
    stop("probabilities in birth.age.prob must sum to 1")
  }
  
  ##Debut at enry:  debut.entry.prob - a probability of being sexaully active the day you enter the similation.
  
    if (is.null(debut.entry.prob.b)) debut.entry.prob.b<-1
    if (is.null(debut.entry.prob.w)) debut.entry.prob.w<-1
  ##Age at debut: debut.prob - a probability per timestep of sexual debut if entry into the simulation does not assume sexaul debut.
  
    if (is.null(debut.prob.b)) debut.prob.b<-1
    if (is.null(debut.prob.w)) debut.prob.w<-1


  # Compile results ---------------------------------------------------------
  out <- list()
  out$method <- method
  if (method == 2) {
    out$deg.main <- c(deg.main.B, deg.main.W)
  }
  if (method == 1) {
    out$deg.main <- deg.main
  }

  out$stats.m <- stats.m

  out$coef.diss.m <- coef.diss.m
  out$coef.diss.p <- coef.diss.p
  out$coef.diss.i <- coef.diss.i
  
  out$ages <- ages
  out$asmr.B <- asmr.B
  out$asmr.W <- asmr.W
  
  out$birth.age.prob<-birth.age.prob
  out$birth.age.min<-min(ages)
  out$debut.entry.prob.b<-debut.entry.prob.b
  out$debut.entry.prob.w<-debut.entry.prob.w
  out$debut.prob.b<-debut.prob.b
  out$debut.prob.w<-debut.prob.w
  
  out$time.unit <- time.unit
  out$num.B <- num.B
  out$num.W <- num.W

  out$deg.mp.B <- deg.mp.B
  out$deg.mp.W <- deg.mp.W

  out$role.B.prob <- role.B.prob
  out$role.W.prob <- role.W.prob
  
  out$riskg.B <- riskg.B
  out$riskg.W <- riskg.W

  class(out) <- "nwstats"
  return(out)
}


#' @title Construct Base Network for Model Estimation and Simulation
#'
#' @description Initializes the base network for model estimation within
#'              \code{netest}.
#'
#' @param nwstats An object of class \code{nwstats}, as output from
#'        \code{\link{calc_nwstats.adol}}.
#'
#' @details
#' This function takes the output of \code{\link{calc_nwstats.adol}} and constructs
#' an empty network with the necessary attributes for race, square root of age based on entry probabilities is supplied,
#' sexual role class, risk catagory and a starting distribution for sexual debut if debut probabilities are supplied. 
#' This base network is used for all three network
#' estimations.
#'
#' @seealso
#' The final vertex attributes on the network for cross-network degree are
#' calculated and set on the network with \code{\link{assign_degree}}.
#'
#' @export
#'
base_nw.adol <- function(nwstats) {

  num.B <- nwstats$num.B
  num.W <- nwstats$num.W

  # Initialize network
  n <- num.B + num.W
  nw <- network::network.initialize(n, directed = FALSE)

  # Calculate attributes
  race <- c(rep("B", num.B), rep("W", num.W))
  race <- sample(race)

  if(nwstats$birth.age.prob[1]==1){
  ager <- nwstats$ages
  ages <- seq(min(ager), max(ager) + 1, 1 / (365 / nwstats$time.unit))
  age <- sample(ages, n, TRUE)
  sqrt.age <- sqrt(age)}
  
  # if using a age specific birth probabilities set initial network to have an age distribution based on those probabilities.
  birth.age.prob<- nwstats$birth.age.prob
  
  if(birth.age.prob[1]<1){
    ager <- nwstats$ages
    
    agedist<-birth.age.prob*(n)
    agedist.new<-(agedist)
    
    for (j in 1:(2*length(agedist))){
      agedist.new[1]<-agedist[length(agedist)]*birth.age.prob[1]
      
      for (k in 2:(length(agedist))){
        agedist.new[k]<-agedist[k-1]+(agedist[length(agedist)]*birth.age.prob[k])
      }
      
      agedist<-agedist.new
      agedist<-round(agedist)
     
      
      if(sum(agedist) < n ) {
        x<-n-sum(agedist)
        agedist[1]<-agedist[1]+x}
     
      
      if(sum(agedist) > n ) {
        x<-sum(agedist)-n
        agedist[1]<-agedist[1]-x}

      sum(agedist)
    }
    
        agetc<-NULL
    for (i in 1:length(birth.age.prob)){
      aget<- seq(ager[i],ager[i]+1, 1 / (365 / nwstats$time.unit))
      aget<-sample(aget,agedist[i],TRUE)
      agetc<-c(agetc,aget)}
    
    age <- sample(agetc, n, TRUE)
    sqrt.age <- sqrt(age)}
  

  role.B <- sample(apportion_lr(num.B, c("I", "R", "V"), nwstats$role.B.prob))
  role.W <- sample(apportion_lr(num.W, c("I", "R", "V"), nwstats$role.W.prob))
  role <- rep(NA, n)
  role[race == "B"] <- role.B
  role[race == "W"] <- role.W

  riskg.B <- sample(apportion_lr(num.B,1:5, rep(0.2, 5)))
  riskg.W <- sample(apportion_lr(num.W,1:5, rep(0.2, 5)))
  riskg <- rep(NA, n)
  riskg[race == "B"] <- riskg.B
  riskg[race == "W"] <- riskg.W

#Set debut (if not specified everyone has debuted, if not the debut.age.prob vector is used).
#Age at debut needs to be an attribute and set here so that the starting pop has debut ages.      
#define by probablity vector.
#That probability will then be used to assign debut.
  
  has.debuted <- nwstats$has.debuted
  debut.entry.prob.b<- nwstats$debut.entry.prob.b
  debut.entry.prob.w<- nwstats$debut.entry.prob.w
  
  if(debut.entry.prob.b<1) {has.debuted[race == "B"] <- rbinom(num.B,1,debut.entry.prob.b)}
  if(debut.entry.prob.w<1) {has.debuted[race == "W"] <- rbinom(num.W,1,debut.entry.prob.w)}
  
  if(debut.entry.prob.b==1) {has.debuted[race == "B"] <- rep('1',num.B)}
  if(debut.entry.prob.w==1) {has.debuted[race == "W"] <- rep('1',num.W)}


  attr.names <- c("race", "riskg", "sqrt.age", "role.class","has.debuted")
  attr.values <- list(race, riskg, sqrt.age, role, has.debuted)
  nw <- network::set.vertex.attribute(nw, attr.names, attr.values)

  return(nw)
}

  
  
