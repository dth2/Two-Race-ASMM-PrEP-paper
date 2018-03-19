

#' @title Counts CAI, PrEP status and HIV status by race age and risk group
#'
#' @description Counts CAI, PrEP status and HIV status by race age and risk group
#'
#' @inheritParams aging.mard
#'
#' @details
#' This module Counts UAI, PrEP status and HIV status by race age and risk group and stores the 
#' information as a series of matricies within dat$counts.  This allows us to keep thi infomrmation
#' without stroring all of dat$attr
#'

#'
#' @return
#' Six matricies that are race specifc age by risk group counts of PreP status, HIV status and CAI
#' stored on \code{dat$counts}.
#'
#' @keywords module
#' @export
#'

counts.adol <- function(dat, at){
  
  
 ##Parameters.
  vl.full.supp<-dat$param$vl.full.supp
  
  #Attributes
  age<-floor(dat$attr$age)
  riskg<-dat$attr$riskg
  race<-dat$attr$race
  active<-dat$attr$active
  prepStat<-dat$attr$prepStat
  status<-dat$attr$status
  
  age.vec<-sort(unique(age))
  riskg.vec<-sort(unique(riskg))

 
#Calculate the number of Contacts.
  for(i in 1:6){
      for(j in 1:5){
        
        ids.b<-which(race=="B" & active==1 & age==age.vec[i] & riskg==riskg.vec[j])
        ids.w<-which(race=="W" & active==1 & age==age.vec[i] & riskg==riskg.vec[j])
        dat$counts$uaiCounts.B[i,j]<-mean(dat$attr$uaicount[ids.b], na.rm = TRUE)
        dat$counts$uaiCounts.W[i,j]<-mean(dat$attr$uaicount[ids.w], na.rm = TRUE)
        dat$counts$prepCounts.B[i,j]<-sum(dat$attr$prepStat[ids.b], na.rm = TRUE)
        dat$counts$prepCounts.W[i,j]<-sum(dat$attr$prepStat[ids.w], na.rm = TRUE)
        dat$counts$status.B[i,j]<-sum(dat$attr$status[ids.b], na.rm = TRUE)
        dat$counts$status.W[i,j]<-sum(dat$attr$status[ids.w], na.rm = TRUE)
        dat$counts$tx.B[i,j] <-sum(dat$attr$tx.status[ids.b], na.rm = TRUE)
        dat$counts$tx.W[i,j] <-sum(dat$attr$tx.status[ids.w], na.rm = TRUE)
        dat$counts$fsupp.B[i,j] <-sum(dat$attr$vl[ids.b] %in% vl.full.supp, na.rm = TRUE)
        dat$counts$fsupp.W[i,j] <-sum(dat$attr$vl[ids.w] %in% vl.full.supp, na.rm = TRUE)
        dat$counts$dx.B[i,j] <-sum(dat$attr$diag.status[ids.b], na.rm = TRUE)
        dat$counts$dx.W[i,j] <-sum(dat$attr$diag.status[ids.w], na.rm = TRUE)

        }
    }
  


    
  return(dat)
}






