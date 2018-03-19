
#' @title PrEP Module
#'
#' @description Module function for implementation and uptake of pre-exposure
#'              prophylaxis (PrEP) to prevent HIV infection.
#'
#' @inheritParams aging.mard
#'
#' @export
#'
prep.adol <- function(dat, at) {

  if (at < dat$param$prep.start) {
    return(dat)
  }
  

  ## Variables
  ##Attributes
  active <- dat$attr$active
  status <- dat$attr$status
  race<- dat$attr$race


  diag.status <- dat$attr$diag.status
  debuted<-dat$attr$has.debuted
  debuted.time<-dat$attr$has.debuted.time
  active.time<-dat$attr$active.time
  AI.time<-dat$attr$AI.time
  lnt <- dat$attr$last.neg.test
  everAI<-dat$attr$everAI
  of.age<-dat$attr$of.age
  uaicount<-dat$attr$uaicount
 
  prepElig <- dat$attr$prepElig
  prepStat <- dat$attr$prepStat
  prepEver <- dat$attr$prepEver
  prepClass <- dat$attr$prepClass
  prepStart.time<-dat$attr$prepStart.time
  

##PrEP params.
  #Uniform.
  prep.elig.model <- dat$param$prep.elig.model
  prep.cov.method <- dat$param$prep.cov.method
  prep.risk.reassess <- dat$param$prep.risk.reassess
  prepSpell<- dat$param$prepSpell
  
  ##Race specific.
  prep.coverage.b <- dat$param$prep.coverage.b
  prep.cov.rate.b <- dat$param$prep.cov.rate.b
  prep.class.prob.b <- dat$param$prep.class.prob.b
  prepDrop.b<- dat$param$prepDrop.b
  prep.delay.b<-dat$param$prep.delay.b
  prep.uaicount.thresh.b<-dat$param$prep.uaicount.thresh.b
  
  prep.coverage.w <- dat$param$prep.coverage.w
  prep.cov.rate.w <- dat$param$prep.cov.rate.w
  prep.class.prob.w <- dat$param$prep.class.prob.w
  prepDrop.w<- dat$param$prepDrop.w
  prep.delay.w<-dat$param$prep.delay.w
  prep.uaicount.thresh.w<-dat$param$prep.uaicount.thresh.w
  

  ## Eligibility ---------------------------------------------------------------

  # Base eligibility
  idsEligStart.b <- which(active == 1 & status == 0 & prepStat == 0 & race=="B")
  idsEligStart.w <- which(active == 1 & status == 0 & prepStat == 0 & race=="W")
  
  idsEligStop.b <- NULL
  if (prep.risk.reassess == TRUE) {
    idsEligStop.b <- which(active == 1 & prepStat == 1 & race=="B")
  }
  
  idsEligStop.w <- NULL
  if (prep.risk.reassess == TRUE) {
    idsEligStop.w <- which(active == 1 & prepStat == 1 & race=="W")
  }

  if (prep.elig.model == "none")  prepElig[idsEligStart.b] <-0
  if (prep.elig.model == "none")  prepElig[idsEligStart.w] <-0
  
  # Core eligiblity scenarios
  if (prep.elig.model != "none") {
    if (substr(prep.elig.model, 1, 4) == "adol") {
      if (prep.elig.model == "adol.entry") {
        c1.b <- active
        c2.b <- active
        c3.b <- active
        c1.w <- active
        c2.w <- active
        c3.w <- active
      } else if (prep.elig.model == "adol.debuted") {
        c1.b <- active
        c2.b <- debuted
        c3.b <- debuted
        c1.w <- active
        c2.w <- debuted
        c3.w <- debuted
      } else if (prep.elig.model == "adol.AI") {
        c1.b <- active
        c2.b <- debuted
        c3.b <- everAI
        c1.w <- active
        c2.w <- debuted
        c3.w <- everAI
      } else if (prep.elig.model == "adol.entry.older") {
        c1.b <- active
        c2.b <- of.age
        c3.b <- of.age
        c1.w <- active
        c2.w <- of.age
        c3.w <- of.age
      } else if (prep.elig.model == "adol.debuted.older") {
        c1.b <- active
        c2.b <- debuted
        c3.b <- of.age
        c1.w <- active
        c2.w <- debuted
        c3.w <- of.age
      } else if (prep.elig.model == "adol.AI.older") {
        c1.b <- active
        c2.b <- everAI
        c3.b <- of.age
        c1.w <- active
        c2.w <- everAI
        c3.w <- of.age
      } else if (prep.elig.model == "adol.entry.time") {
        c1.b <- active
        c2.b <- ifelse (at - active.time > prep.delay.b,1,0) 
        c3.b <- ifelse (at - active.time > prep.delay.b,1,0)
        c1.w <- active
        c2.w <- ifelse (at - active.time > prep.delay.w,1,0) 
        c3.w <- ifelse (at - active.time > prep.delay.w,1,0)
      } else if (prep.elig.model == "adol.debuted.time") {
        c1.b <- debuted
        c2.b <- ifelse (at - debuted.time > prep.delay.b,1,0) 
        c3.b <- ifelse (at - debuted.time > prep.delay.b,1,0)
        c1.w <- debuted
        c2.w <- ifelse (at - debuted.time > prep.delay.w,1,0) 
        c3.w <- ifelse (at - debuted.time > prep.delay.w,1,0) 
      } else if (prep.elig.model == "adol.AI.time") {
        c1.b <- everAI
        c2.b <- ifelse (at - AI.time > prep.delay.b,1,0) 
        c3.b <- ifelse (at - AI.time > prep.delay.b,1,0)
        c1.w <- everAI
        c2.w <- ifelse (at - AI.time > prep.delay.w,1,0) 
        c3.w <- ifelse (at - AI.time > prep.delay.w,1,0)
      } else if (prep.elig.model == "adol.entry.older.time") {
        c1.b <- active
        c2.b <- of.age
        c3.b <- ifelse (at - active.time > prep.delay.b,1,0)
        c1.w <- active
        c2.w <- of.age
        c3.w <- ifelse (at - active.time > prep.delay.w,1,0)
      } else if (prep.elig.model == "adol.debuted.older.time") {
        c1.b <- debuted
        c2.b <- of.age
        c3.b <- ifelse (at - debuted.time > prep.delay.b,1,0)
        c1.w <- debuted
        c2.w <- of.age
        c3.w <- ifelse (at - debuted.time > prep.delay.w,1,0)
      } else if (prep.elig.model == "adol.AI.older.time") {
        c1.b <- everAI
        c2.b <- of.age
        c3.b <- ifelse (at - AI.time > prep.delay.b,1,0)
        c1.w <- everAI
        c2.w <- of.age
        c3.w <- ifelse (at - AI.time > prep.delay.w,1,0)
      } else if (prep.elig.model == "adol.riskhist") {
        c1.b <- ifelse (uaicount > prep.uaicount.thresh.b,1,0)
        c2.b <- ifelse (uaicount > prep.uaicount.thresh.b,1,0)
        c3.b <- ifelse (uaicount > prep.uaicount.thresh.b,1,0)
        c1.w <- ifelse (uaicount > prep.uaicount.thresh.w,1,0)
        c2.w <- ifelse (uaicount > prep.uaicount.thresh.w,1,0)
        c3.w <- ifelse (uaicount > prep.uaicount.thresh.w,1,0)
      } else if (prep.elig.model == "adol.riskhist.older") {
        c1.b <- ifelse (uaicount > prep.uaicount.thresh.b,1,0)
        c2.b <- of.age
        c3.b <- of.age
        c1.w <- ifelse (uaicount > prep.uaicount.thresh.w,1,0)
        c2.w <- of.age
        c3.w <- of.age
      } else if (prep.elig.model == "adol.riskhist.time") {
        c1.b <- ifelse (uaicount > prep.uaicount.thresh.b,1,0)
        c2.b <- ifelse (at - AI.time > prep.delay.b,1,0)
        c3.b <- ifelse (at - AI.time > prep.delay.b,1,0)
        c1.w <- ifelse (uaicount > prep.uaicount.thresh.w,1,0)
        c2.w <- ifelse (at - AI.time > prep.delay.w,1,0)
        c3.w <- ifelse (at - AI.time > prep.delay.w,1,0)
      } else if (prep.elig.model == "adol.riskhist.older.time") {
        c1.b <- ifelse (uaicount > prep.uaicount.thresh.b,1,0)
        c2.b <- of.age
        c3.b <- ifelse (at - AI.time > prep.delay.b,1,0)
        c1.w <- ifelse (uaicount > prep.uaicount.thresh.w,1,0)
        c2.w <- of.age
        c3.w <- ifelse (at - AI.time > prep.delay.w,1,0)
      }
      
     #Check length of c1.b and idsEligStart.b 
      idsEligStart.b <- intersect(idsEligStart.b,(which(c1.b > 0 & c2.b > 0 & c3.b > 0)))
      idsEligStop.b <- intersect(idsEligStop.b,(which(c1.b == 0 | c2.b == 0 | c3.b == 0)))
      
      idsEligStart.w <- intersect(idsEligStart.w,(which(c1.w > 0 & c2.w > 0 & c3.w > 0)))
      idsEligStop.w <- intersect(idsEligStop.w,(which(c1.w == 0 | c2.w == 0 | c3.w == 0)))
    } 
    
    prepElig[idsEligStart.b] <- 1
    prepElig[idsEligStop.b] <- 0  
    
    prepElig[idsEligStart.w] <- 1
    prepElig[idsEligStop.w] <- 0  
  }

  

  ## Stoppage ------------------------------------------------------------------

  # Diagnosis
  idsStpDx <- which(active == 1 & prepStat == 1 & diag.status == 1)


  # Death
  idsStpDth <- which(active == 0 & prepStat == 1)

  
  #Drop out
  idsStpDrop.b<-integer(0)
  idsStpDrop.w<-integer(0)
  if (prepSpell == TRUE){
  idsStpDrop.b <-which(active==1 & prepStat == 1 & race=="B")
  idsStpDrop.w <-which(active==1 & prepStat == 1 & race=="W")
  
  if (length(idsStpDrop.b>=1)){
          idsStpDrop.b <-ssample(idsStpDrop.b, prepDrop.b*length(idsStpDrop.b), replace=FALSE)
  }
  if (length(idsStpDrop.w>=1)){
          idsStpDrop.w <-ssample(idsStpDrop.w, prepDrop.w*length(idsStpDrop.w), replace=FALSE)
  }
  }
  

  
  # Transition to ineligibility
  idsStpInelig.b <- idsEligStop.b
  idsStpInelig.w <- idsEligStop.w

  # Reset PrEP status
  idsStp <- c(idsStpDx, idsStpDth, idsStpDrop.b, idsStpDrop.w, idsStpInelig.b, idsStpInelig.w)
  prepStat[idsStp] <- 0
  prepClass[idsStp] <-NA
  

  #Drops are added back to eligible list.
  prepElig[idsStpDrop.b] <- 1
  prepElig[idsStpDrop.w] <- 1

  ## Initiation ----------------------------------------------------------------
  blk<-which(race=="B")
  wht<-which(race=="W")
  

  if (prep.cov.method == "curr") {
    prepCov.b <- sum(prepStat[blk] == 1, na.rm = TRUE)/sum(prepElig[blk] == 1, na.rm = TRUE)
    prepCov.w <- sum(prepStat[wht] == 1, na.rm = TRUE)/sum(prepElig[wht] == 1, na.rm = TRUE)
    prepCov <- sum(prepStat == 1, na.rm = TRUE)/sum(prepElig == 1, na.rm = TRUE)
  }
  if (prep.cov.method == "ever") {
    prepCov.b <- sum(prepEver[blk] == 1, na.rm = TRUE)/sum(prepElig[blk] == 1, na.rm = TRUE)
    prepCov.w <- sum(prepEver[wht] == 1, na.rm = TRUE)/sum(prepElig[wht] == 1, na.rm = TRUE)
    prepCov <- sum(prepEver == 1, na.rm = TRUE)/sum(prepElig == 1, na.rm = TRUE)
  }
  prepCov.b <- ifelse(is.nan(prepCov.b), 0, prepCov.b)
  prepCov.w <- ifelse(is.nan(prepCov.w), 0, prepCov.w)
  prepCov <- ifelse(is.nan(prepCov), 0, prepCov)
  
  idsEligSt.b <- which(prepElig == 1 & race == "B")
  nEligSt.b <- length(idsEligSt.b)
  
  idsEligSt.w <- which(prepElig == 1 & race== "W")
  nEligSt.w <- length(idsEligSt.w)

  nStart.b <- max(0, min(nEligSt.b, round((prep.coverage.b - prepCov.b) *
                                             sum(prepElig[blk] == 1, na.rm = TRUE))))
  
  nStart.w <- max(0, min(nEligSt.w, round((prep.coverage.w - prepCov.w) *
                                        sum(prepElig[wht] == 1, na.rm = TRUE))))
  
  idsStart.b <- NULL
  if (nStart.b > 0) {
    if (prep.cov.rate.b >= 1) {
      idsStart.b <- ssample(idsEligSt.b, nStart.b)
    } else {
      idsStart.b <- idsEligSt.b[rbinom(nStart.b, 1, prep.cov.rate.b) == 1]
    }
  }
  
  idsStart.w <- NULL
  if (nStart.w > 0) {
    if (prep.cov.rate.w >= 1) {
      idsStart.w <- ssample(idsEligSt.w, nStart.w)
    } else {
      idsStart.w <- idsEligSt.w[rbinom(nStart.w, 1, prep.cov.rate.w) == 1]
    }
  } 
  
  
  
  

  # Attributes
  if (length(idsStart.b) > 0) {
    prepStat[idsStart.b] <- 1
    prepEver[idsStart.b] <- 1
    prepStart.time[idsStart.b]<-at

    # PrEP class is fixed over PrEP cycles
    needPC.b <- intersect(idsStart.b,which(is.na(prepClass)==TRUE))
    prepClass[needPC.b] <- sample(x = c("n", "l", "m", "h"), size = length(needPC.b),
                                          replace = TRUE, prob = prep.class.prob.b)
  }

  if (length(idsStart.w) > 0) {
    prepStat[idsStart.w] <- 1
    prepEver[idsStart.w] <- 1
    prepStart.time[idsStart.w]<-at
    
    # PrEP class is fixed over PrEP cycles
    needPC.w <- intersect(idsStart.w,which(is.na(prepClass)==TRUE))
    prepClass[needPC.w] <- sample(x = c("n", "l", "m", "h"), size = length(needPC.w),
                                          replace = TRUE, prob = prep.class.prob.w)
  }

  ## Output --------------------------------------------------------------------
  # Attributes

  dat$attr$prepElig <- prepElig
  dat$attr$prepStat <- prepStat
  dat$attr$prepEver <- prepEver
  dat$attr$prepClass <- prepClass
  dat$attr$prepStart.time<-prepStart.time
  
  # Summary Statistics
  dat$epi$prepCov.B[at] <- prepCov.b
  dat$epi$prepCov.W[at] <- prepCov.w
  dat$epi$prepStart.B[at] <- length(idsStart.b)
  dat$epi$prepStart.W[at] <- length(idsStart.w)

  dat$epi$prepCov[at] <- prepCov
  dat$epi$prepStart[at] <- length(idsStart.b)+length(idsStart.w)
 

  return(dat)
}
