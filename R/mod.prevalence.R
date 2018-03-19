
#' @title Prevalence Calculations within Time Steps
#'
#' @description This module calculates demographic, transmission, and clinical
#'              statistics at each time step within the simulation.
#'
#' @inheritParams aging.mard
#'
#' @details
#' Summary statistic calculations are of two broad forms: prevalence and
#' incidence. This function establishes the summary statistic vectors for both
#' prevalence and incidence at time 1, and then calculates the prevalence
#' statistics for times 2 onward. Incidence statistics (e.g., number of new
#' infections or deaths) are calculated within the modules as they depend on
#' vectors that are not stored external to the module.
#'
#' @return
#' This function returns the \code{dat} object with an updated summary of current
#' attributes stored in \code{dat$epi}.
#'
#' @keywords module
#' @export
#'
prevalence.adol <- function(dat, at) {

  # Variables
  active <- dat$attr$active
  race <- dat$attr$race
  status <- dat$attr$status
  diag.status <- dat$attr$diag.status
  tx.status <- dat$attr$tx.status
  time.on.tx <- dat$attr$cum.time.on.tx
  vl.full.supp <- dat$param$vl.full.supp
  vl.part.supp <- dat$param$vl.part.supp
  vl <- dat$attr$vl
  tt.traj <- dat$attr$tt.traj
  stage <- dat$attr$stage
  prepStat <- dat$attr$prepStat
  riskg <- dat$attr$riskg
  debuted<-dat$attr$has.debuted
  evertested<-dat$attr$evertested
  everAI<-dat$attr$everAI
  age<-floor(dat$attr$age)
  age.c<-dat$attr$age
  infector<-dat$attr$infector
  prevfull <- dat$control$prevfull
  nsteps <- dat$control$nsteps
  rNA <- rep(NA, nsteps)

  if (at == 1) {
    dat$epi$num <- rNA
    dat$epi$num.B <- rNA
    dat$epi$num.W <- rNA
    dat$epi$s.num <- rNA
    dat$epi$i.num <- rNA
    dat$epi$i.num.B <- rNA
    dat$epi$i.num.W <- rNA
    dat$epi$i.prev <- rNA
    dat$epi$i.prev.B <- rNA
    dat$epi$i.prev.W <- rNA
    dat$epi$nBirths <- rNA
    dat$epi$nBirths.B <- rNA
    dat$epi$nBirths.W <- rNA
    dat$epi$dth.gen <- rNA
    dat$epi$dth.dis <- rNA
    dat$epi$dth.gen.B <- rNA
    dat$epi$dth.gen.W <- rNA
    dat$epi$dth.dis.B <- rNA
    dat$epi$dth.dis.W <- rNA
    dat$epi$incid <- rNA
    dat$epi$incid.B <- rNA
    dat$epi$incid.W <- rNA
    dat$epi$heat.prev<-rNA
    dat$epi$incid.acte <- rNA
    dat$epi$incid.chrn <- rNA
    dat$epi$incid.aids <- rNA
    dat$epi$incid.main <- rNA
    dat$epi$incid.casl <- rNA
    dat$epi$incid.inst <- rNA
    dat$epi$tx.naive <- rNA
    dat$epi$tx.naive.B <- rNA
    dat$epi$tx.naive.W <- rNA
    dat$epi$tx.full.supp <- rNA
    dat$epi$tx.full.supp.B <- rNA
    dat$epi$tx.full.supp.W <- rNA
    dat$epi$tx.full.supp.prev < -rNA
    dat$epi$acute <- rNA
    dat$epi$acute.B <- rNA
    dat$epi$acute.W <- rNA
    dat$epi$chronic <- rNA
    dat$epi$chronic.B <- rNA
    dat$epi$chronic.W <- rNA
    dat$epi$aids <- rNA
    dat$epi$aids.B <- rNA
    dat$epi$aids.W <- rNA
    dat$epi$i.prev.rg1 <- rNA
    dat$epi$i.prev.rg2 <- rNA
    dat$epi$i.prev.rg3 <- rNA
    dat$epi$i.prev.rg4 <- rNA
    dat$epi$i.prev.rg5 <- rNA
    dat$epi$i.prev.age1 <- rNA
    dat$epi$i.prev.age2 <- rNA
    dat$epi$i.prev.age3 <- rNA
    dat$epi$i.prev.age4 <- rNA
    dat$epi$i.prev.age5 <- rNA
    dat$epi$i.prev.age6 <- rNA
    dat$epi$i.prev.age6.B <- rNA
    dat$epi$i.prev.age6.W <- rNA
    dat$epi$acts <- rNA
    dat$epi$acts.B <- rNA
    dat$epi$acts.W <- rNA
    dat$epi$patp <- rNA
    dat$epi$patp.B <- rNA
    dat$epi$patp.W <- rNA
    dat$epi$prob.uai <- rNA
    dat$epi$prob.uai.B <- rNA
    dat$epi$prob.uai.W <- rNA
    dat$epi$evertested <- rNA
    dat$epi$everAI <- rNA
    dat$epi$everAI.B <- rNA
    dat$epi$everAI.W <- rNA
    dat$epi$evertested.exit <- rNA
    dat$epi$debuted <- rNA
    dat$epi$debuted.B <- rNA
    dat$epi$debuted.W <- rNA
    
    dat$epi$AI.adult.count.13.1<-rNA
    dat$epi$AI.adult.count.13.2<-rNA
    dat$epi$AI.adult.count.13.3<-rNA
    dat$epi$AI.adult.count.13.4<-rNA
    dat$epi$AI.adult.count.13.5<-rNA
    dat$epi$AI.adult.count.14.1<-rNA
    dat$epi$AI.adult.count.14.2<-rNA
    dat$epi$AI.adult.count.14.3<-rNA
    dat$epi$AI.adult.count.14.4<-rNA
    dat$epi$AI.adult.count.14.5<-rNA
    dat$epi$AI.adult.count.15.1<-rNA
    dat$epi$AI.adult.count.15.2<-rNA
    dat$epi$AI.adult.count.15.3<-rNA
    dat$epi$AI.adult.count.15.4<-rNA
    dat$epi$AI.adult.count.15.5<-rNA
    dat$epi$AI.adult.count.16.1<-rNA
    dat$epi$AI.adult.count.16.2<-rNA
    dat$epi$AI.adult.count.16.3<-rNA
    dat$epi$AI.adult.count.16.4<-rNA
    dat$epi$AI.adult.count.16.5<-rNA
    dat$epi$AI.adult.count.17.1<-rNA
    dat$epi$AI.adult.count.17.2<-rNA
    dat$epi$AI.adult.count.17.3<-rNA
    dat$epi$AI.adult.count.17.4<-rNA
    dat$epi$AI.adult.count.17.5<-rNA
    dat$epi$AI.adult.count.18.1<-rNA
    dat$epi$AI.adult.count.18.2<-rNA
    dat$epi$AI.adult.count.18.3<-rNA
    dat$epi$AI.adult.count.18.4<-rNA
    dat$epi$AI.adult.count.18.5<-rNA


    if (prevfull == TRUE) {
      dat$epi$undiag <- rNA
      dat$epi$undiag.B <- rNA
      dat$epi$undiag.W <- rNA
      dat$epi$diag <- rNA
      dat$epi$diag.B <- rNA
      dat$epi$diag.W <- rNA
      dat$epi$tx.part.supp <- rNA
      dat$epi$tx.part.supp.B <- rNA
      dat$epi$tx.part.supp.W <- rNA
      dat$epi$tx.influx.full <- rNA
      dat$epi$tx.influx.full.B <- rNA
      dat$epi$tx.influx.full.W <- rNA
      dat$epi$tx.influx.part <- rNA
      dat$epi$tx.influx.part.B <- rNA
      dat$epi$tx.influx.part.W <- rNA
      dat$epi$off.tx <- rNA
      dat$epi$off.tx.B <- rNA
      dat$epi$off.tx.W <- rNA
      dat$epi$tx.init.inc <- rNA
      dat$epi$tx.halt.inc <- rNA
      dat$epi$tx.resm.inc <- rNA
      dat$epi$tst.W.inc <- rNA
      dat$epi$tst.B.inc <- rNA
    }

    dat$epi$prepCurr <- rNA
    dat$epi$prepCurr.B <- rNA
    dat$epi$prepCurr.W <- rNA
    dat$epi$prepEver <- rNA
    dat$epi$prepEver.B <- rNA
    dat$epi$prepEver.W <- rNA
    dat$epi$prepCov <- rNA
    dat$epi$prepCov.B <- rNA
    dat$epi$prepCov.W <- rNA
    dat$epi$prepElig <- rNA
    dat$epi$prepElig.B <- rNA
    dat$epi$prepElig.W <- rNA
    dat$epi$prepStart <- rNA
    dat$epi$prepStart.B <- rNA
    dat$epi$prepStart.W <- rNA
    dat$epi$incid.prep0 <- rNA
    dat$epi$incid.prep1 <- rNA
    dat$epi$i.num.prep0 <- rNA
    dat$epi$i.num.prep1 <- rNA
    dat$epi$i.prev.prep0 <- rNA
    dat$epi$i.prev.prep1 <- rNA

    dat$epi$cprob.always.pers <- rNA
    dat$epi$cprob.always.inst <- rNA

    dat$epi$timer <- rNA
  }


  dat$epi$num[at] <- sum(active == 1, na.rm = TRUE)
  dat$epi$num.B[at] <- sum(active == 1 & race == "B", na.rm = TRUE)
  dat$epi$num.W[at] <- sum(active == 1 & race == "W", na.rm = TRUE)
  dat$epi$s.num[at] <- sum(active == 1 & status == 0, na.rm = TRUE)
  dat$epi$i.num[at] <- sum(active == 1 & status == 1, na.rm = TRUE)
  dat$epi$i.num.B[at] <- sum(active == 1 & status == 1 & race == "B", na.rm = TRUE)
  dat$epi$i.num.W[at] <- sum(active == 1 & status == 1 & race == "W", na.rm = TRUE)
  dat$epi$i.prev[at] <- dat$epi$i.num[at] / dat$epi$num[at]
  dat$epi$i.prev.B[at] <- dat$epi$i.num.B[at] / dat$epi$num.B[at]
  dat$epi$i.prev.W[at] <- dat$epi$i.num.W[at] / dat$epi$num.W[at]
  dat$epi$i.prev.rg1[at] <- sum(active == 1 & status == 1 & riskg == 1, na.rm = TRUE)/
                            sum(active == 1 & riskg == 1 & everAI == 1,na.rm = TRUE)
  dat$epi$i.prev.rg2[at] <- sum(active == 1 & status == 1 & riskg == 2, na.rm = TRUE)/
                            sum(active == 1 & riskg == 2 & everAI == 1,na.rm = TRUE)
  dat$epi$i.prev.rg3[at] <- sum(active == 1 & status == 1 & riskg == 3, na.rm = TRUE)/
                            sum(active == 1 & riskg == 3 & everAI == 1,na.rm = TRUE)
  dat$epi$i.prev.rg4[at] <- sum(active == 1 & status == 1 & riskg == 4, na.rm = TRUE)/
                            sum(active == 1 & riskg == 4 & everAI == 1,na.rm = TRUE)
  dat$epi$i.prev.rg5[at] <- sum(active == 1 & status == 1 & riskg == 5, na.rm = TRUE)/
                            sum(active == 1 & riskg == 5 & everAI == 1, na.rm = TRUE)
  
  dat$epi$i.prev.age1[at] <- sum(active == 1 & status == 1 & age == 13, na.rm = TRUE)/
    sum(active == 1 & age == 13 & everAI == 1, na.rm = TRUE)
  dat$epi$i.prev.age2[at] <- sum(active == 1 & status == 1 & age == 14, na.rm = TRUE)/
    sum(active == 1 & age == 14 & everAI == 1, na.rm = TRUE)
  dat$epi$i.prev.age3[at] <- sum(active == 1 & status == 1 & age == 15, na.rm = TRUE)/
    sum(active == 1 & age == 15 & everAI == 1, na.rm = TRUE)
  dat$epi$i.prev.age4[at] <- sum(active == 1 & status == 1 & age == 16, na.rm = TRUE)/
    sum(active == 1 & age == 16 & everAI == 1, na.rm = TRUE)
  dat$epi$i.prev.age5[at] <- sum(active == 1 & status == 1 & age == 17, na.rm = TRUE)/
    sum(active == 1 & age == 17 & everAI == 1, na.rm = TRUE)
  dat$epi$i.prev.age6[at] <- sum(active == 1 & status == 1 & age == 18, na.rm = TRUE)/
    sum(active == 1 & age == 18 & everAI == 1, na.rm = TRUE)
  
  dat$epi$i.prev.age6.B[at] <- sum(active == 1 & status == 1 & age == 18 & race=="B", na.rm = TRUE)/
    sum(active == 1 & age == 18 & everAI == 1 & race=="B", na.rm = TRUE)
  dat$epi$i.prev.age6.W[at] <- sum(active == 1 & status == 1 & age == 18 & race=="W", na.rm = TRUE)/
    sum(active == 1 & age == 18 & everAI == 1 & race=="W", na.rm = TRUE)
  
  dat$epi$evertested[at] <- sum(active == 1 & evertested == 1, na.rm = TRUE)/
    sum(active == 1, na.rm = TRUE)
  
  dat$epi$evertested.exit[at] <- sum(active == 1 & age.c>=18.9 & evertested == 1, na.rm = TRUE)/
    sum(active == 1 & age.c>=18.9, na.rm = TRUE)
  
  dat$epi$heat.prev[at] <- sum(active == 1 & everAI == 1 & infector == "Heat", na.rm = TRUE)/
    sum(active == 1 & everAI == 1, na.rm = TRUE)
  
  dat$epi$tx.naive[at] <- sum(active == 1 & time.on.tx == 0, na.rm = TRUE)
  dat$epi$tx.naive.B[at] <- sum(active == 1 & time.on.tx == 0 & race == "B", na.rm = TRUE)
  dat$epi$tx.naive.W[at] <- sum(active == 1 & time.on.tx == 0 & race == "W", na.rm = TRUE)
  dat$epi$tx.full.supp[at] <- sum(active == 1 & tx.status == 1 &
                                  vl %in% vl.full.supp, na.rm = TRUE)
  
  dat$epi$tx.full.supp.prev[at] <- sum(active == 1 & tx.status == 1 &
                                    vl %in% vl.full.supp, na.rm = TRUE)/
                                    sum(active == 1 & status == 1, na.rm = TRUE)
  
  dat$epi$tx.full.supp.B[at] <- sum(active == 1 & tx.status == 1 &
                                    vl %in% vl.full.supp & race == "B", na.rm = TRUE)
  dat$epi$tx.full.supp.W[at] <- sum(active == 1 & tx.status == 1 &
                                    vl %in% vl.full.supp & race == "W", na.rm = TRUE)
  dat$epi$acute[at] <- sum(active == 1 & stage %in% c("AR", "AF"), na.rm = TRUE)
  dat$epi$acute.B[at] <- sum(active == 1 & stage %in% c("AR", "AF") & race == "B", na.rm = TRUE)
  dat$epi$acute.W[at] <- sum(active == 1 & stage %in% c("AR", "AF") & race == "W", na.rm = TRUE)
  dat$epi$chronic[at] <- sum(active == 1 & stage == "C", na.rm = TRUE)
  dat$epi$chronic.B[at] <- sum(active == 1 & stage == "C" & race == "B", na.rm = TRUE)
  dat$epi$chronic.W[at] <- sum(active == 1 & stage == "C" & race == "W", na.rm = TRUE)
  dat$epi$aids[at] <- sum(active == 1 & stage == "D", na.rm = TRUE)
  dat$epi$aids.B[at] <- sum(active == 1 & stage == "D" & race == "B", na.rm = TRUE)
  dat$epi$aids.W[at] <- sum(active == 1 & stage == "D" & race == "W", na.rm = TRUE)
  
  dat$epi$evertested[at] <- sum(active == 1 & evertested == 1)
  dat$epi$debuted[at] <- sum(active == 1 & debuted == 1)
  dat$epi$debuted.B[at] <- sum(active == 1 & debuted == 1 & race == "B", na.rm = TRUE)
  dat$epi$debuted.W[at] <- sum(active == 1 & debuted == 1 & race == "W", na.rm = TRUE)
  
  dat$epi$everAI[at] <- sum(active == 1 & everAI == 1)
  dat$epi$everAI.B[at] <- sum(active == 1 & everAI == 1 & race == "B", na.rm = TRUE)
  dat$epi$everAI.W[at] <- sum(active == 1 & everAI == 1 & race == "W", na.rm = TRUE)
  
  AI.adult.count.bygroup<-tapply(dat$attr$AI.adult.count[active==1 & everAI==1],dat$attr$risk.age.group[active==1 & everAI==1],mean,rm.na=TRUE)
  dat$epi$AI.adult.count.13.1[at]<-AI.adult.count.bygroup[1]
  dat$epi$AI.adult.count.13.2[at]<-AI.adult.count.bygroup[2]
  dat$epi$AI.adult.count.13.3[at]<-AI.adult.count.bygroup[3]
  dat$epi$AI.adult.count.13.4[at]<-AI.adult.count.bygroup[4]
  dat$epi$AI.adult.count.13.5[at]<-AI.adult.count.bygroup[5]
  dat$epi$AI.adult.count.14.1[at]<-AI.adult.count.bygroup[6]
  dat$epi$AI.adult.count.14.2[at]<-AI.adult.count.bygroup[7]
  dat$epi$AI.adult.count.14.3[at]<-AI.adult.count.bygroup[8]
  dat$epi$AI.adult.count.14.4[at]<-AI.adult.count.bygroup[9]
  dat$epi$AI.adult.count.14.5[at]<-AI.adult.count.bygroup[10]
  dat$epi$AI.adult.count.15.1[at]<-AI.adult.count.bygroup[11]
  dat$epi$AI.adult.count.15.2[at]<-AI.adult.count.bygroup[12]
  dat$epi$AI.adult.count.15.3[at]<-AI.adult.count.bygroup[13]
  dat$epi$AI.adult.count.15.4[at]<-AI.adult.count.bygroup[14]
  dat$epi$AI.adult.count.15.5[at]<-AI.adult.count.bygroup[15]
  dat$epi$AI.adult.count.16.1[at]<-AI.adult.count.bygroup[16]
  dat$epi$AI.adult.count.16.2[at]<-AI.adult.count.bygroup[17]
  dat$epi$AI.adult.count.16.3[at]<-AI.adult.count.bygroup[18]
  dat$epi$AI.adult.count.16.4[at]<-AI.adult.count.bygroup[19]
  dat$epi$AI.adult.count.16.5[at]<-AI.adult.count.bygroup[20]
  dat$epi$AI.adult.count.17.1[at]<-AI.adult.count.bygroup[21]
  dat$epi$AI.adult.count.17.2[at]<-AI.adult.count.bygroup[22]
  dat$epi$AI.adult.count.17.3[at]<-AI.adult.count.bygroup[23]
  dat$epi$AI.adult.count.17.4[at]<-AI.adult.count.bygroup[24]
  dat$epi$AI.adult.count.17.5[at]<-AI.adult.count.bygroup[25]
  dat$epi$AI.adult.count.18.1[at]<-AI.adult.count.bygroup[26]
  dat$epi$AI.adult.count.18.2[at]<-AI.adult.count.bygroup[27]
  dat$epi$AI.adult.count.18.3[at]<-AI.adult.count.bygroup[28]
  dat$epi$AI.adult.count.18.4[at]<-AI.adult.count.bygroup[29]
  dat$epi$AI.adult.count.18.5[at]<-AI.adult.count.bygroup[30]
  


  if (prevfull == TRUE) {
    dat$epi$undiag[at] <- sum(active == 1 & status == 1 & diag.status == 0, na.rm = TRUE)
    dat$epi$undiag.B[at] <- sum(active == 1 & status == 1 & diag.status == 0 &
                                race == "B", na.rm = TRUE)
    dat$epi$undiag.W[at] <- sum(active == 1 & status == 1 & diag.status == 0 &
                                race == "W", na.rm = TRUE)
    dat$epi$diag[at] <- sum(active == 1 & status == 1 & diag.status == 1, na.rm = TRUE)
    dat$epi$diag.B[at] <- sum(active == 1 & status == 1 & diag.status == 1 &
                              race == "B", na.rm = TRUE)
    dat$epi$diag.W[at] <- sum(active == 1 & status == 1 & diag.status == 1 &
                              race == "W", na.rm = TRUE)
    dat$epi$tx.part.supp[at] <- sum(active == 1 & tx.status == 1 &
                                    vl %in% vl.part.supp, na.rm = TRUE)
    dat$epi$tx.part.supp.B[at] <- sum(active == 1 & tx.status == 1 &
                                      vl %in% vl.part.supp & race == "B", na.rm = TRUE)
    dat$epi$tx.part.supp.W[at] <- sum(active == 1 & tx.status == 1 &
                                      vl %in% vl.part.supp & race == "W", na.rm = TRUE)
    dat$epi$tx.influx.full[at] <- sum(active == 1 & tx.status == 1 &
                                      !(vl %in% c(vl.full.supp, vl.part.supp)) &
                                      tt.traj == "YF", na.rm = TRUE)
    dat$epi$tx.influx.full.B[at] <- sum(active == 1 & tx.status == 1 &
                                        !(vl %in% c(vl.full.supp, vl.part.supp)) &
                                        tt.traj == "YF" & race == "B", na.rm = TRUE)
    dat$epi$tx.influx.full.W[at] <- sum(active == 1 & tx.status == 1 &
                                        !(vl %in% c(vl.full.supp, vl.part.supp)) &
                                        tt.traj == "YF" & race == "W", na.rm = TRUE)
    dat$epi$tx.influx.part[at] <- sum(active == 1 & tx.status == 1 &
                                      !(vl %in% c(vl.full.supp, vl.part.supp)) &
                                      tt.traj == "YP", na.rm = TRUE)
    dat$epi$tx.influx.part.B[at] <- sum(active == 1 & tx.status == 1 &
                                        !(vl %in% c(vl.full.supp, vl.part.supp)) &
                                        tt.traj == "YP" & race == "B", na.rm = TRUE)
    dat$epi$tx.influx.part.W[at] <- sum(active == 1 & tx.status == 1 &
                                        !(vl %in% c(vl.full.supp, vl.part.supp)) &
                                        tt.traj == "YP" & race == "W", na.rm = TRUE)

    dat$epi$off.tx[at] <- sum(active == 1 & tx.status == 0 & time.on.tx > 0, na.rm = TRUE)
    dat$epi$off.tx.B[at] <- sum(active == 1 & tx.status == 0 & time.on.tx > 0 &
                                race == "B", na.rm = TRUE)
    dat$epi$off.tx.W[at] <- sum(active == 1 & tx.status == 0 & time.on.tx > 0 &
                                race == "W", na.rm = TRUE)
  }

  dat$epi$prepCurr[at] <- sum(active == 1 & prepStat == 1, na.rm = TRUE)
  dat$epi$prepElig[at] <- sum(active == 1 & dat$attr$prepElig == 1, na.rm = TRUE)
  dat$epi$prepEver[at] <- sum(active == 1 & dat$attr$prepEver == 1, na.rm = TRUE)
  
  dat$epi$prepCurr.B[at] <- sum(active == 1 & prepStat == 1 & race=="B", na.rm = TRUE)
  dat$epi$prepElig.B[at] <- sum(active == 1 & dat$attr$prepElig == 1 & race=="B", na.rm = TRUE)
  dat$epi$prepEver.B[at] <- sum(active == 1 & dat$attr$prepEver == 1 & race=="B", na.rm = TRUE)
  
  dat$epi$prepCurr.W[at] <- sum(active == 1 & prepStat == 1 & race=="W", na.rm = TRUE)
  dat$epi$prepElig.W[at] <- sum(active == 1 & dat$attr$prepElig == 1 & race=="W", na.rm = TRUE)
  dat$epi$prepEver.W[at] <- sum(active == 1 & dat$attr$prepEver == 1 & race=="W", na.rm = TRUE)
  
  dat$epi$i.num.prep0[at] <- sum(active == 1 & (is.na(prepStat) | prepStat == 0) &
                                 status == 1, na.rm = TRUE)
  dat$epi$i.num.prep1[at] <- sum(active == 1 & prepStat == 1 & status == 1, na.rm = TRUE)
  dat$epi$i.prev.prep0[at] <- dat$epi$i.num.prep0[at] /
                              sum(active == 1 & (is.na(prepStat) | prepStat == 0), na.rm = TRUE)
  if (at == 1) {
    dat$epi$i.prev.prep1[1] <- 0
  } else {
    dat$epi$i.prev.prep1[at] <- dat$epi$i.num.prep1[at] /
                                sum(active == 1 & prepStat == 1, na.rm = TRUE)
  }

  # sytem timer
  dat$epi$timer[at] <- proc.time()[3] - dat$epi$timer[at]

  return(dat)
}
