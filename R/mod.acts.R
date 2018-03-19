
#' @title Sexual Acts Module
#'
#' @description Module function for setting the number of sexual acts on the
#'              discordant edgelist.
#'
#' @inheritParams aging.mard
#'
#' @details
#' The number of acts at each time step is specified as a function of the race of
#' both members in a pair and the expected values within black-black, black-white,
#' and white-white combinations. For one-off partnerships, this is deterministically
#' set at 1, whereas for main and causal partnerships it is a stochastic draw
#' from a Poisson distribution. The number of total acts may further be modified
#' by the level of HIV viral suppression in an infected person.
#'
#' @return
#' This function returns the \code{dat} object with the updated discordant act
#' list (\code{dal}). Each element of \code{dal} is a data frame with the ids of the
#' discordant pair repeated the number of times they have AI.
#'
#' @keywords module
#' @export
#'
acts.adol <- function(dat, at) {

 # for (type in c("main", "pers", "inst")) {
  for (type in c("main")) {

    ## Variables ##

    # Attributes
    status <- dat$attr$status
    race <- dat$attr$race

    # Parameters
    ai.scale.w <- dat$param$ai.scale.w
    ai.scale.b <- dat$param$ai.scale.b

    if (type == "main") {
      base.ai.BB.rate <- dat$param$base.ai.main.BB.rate
      base.ai.BW.rate <- dat$param$base.ai.main.BW.rate
      base.ai.WW.rate <- dat$param$base.ai.main.WW.rate
      fixed <- FALSE
      el <- get.dyads.active(dat$nw$m, at = at)
    }
    if (type == "pers") {
      base.ai.BB.rate <- dat$param$base.ai.pers.BB.rate
      base.ai.BW.rate <- dat$param$base.ai.pers.BW.rate
      base.ai.WW.rate <- dat$param$base.ai.pers.WW.rate
      fixed <- FALSE
      el <- get.dyads.active(dat$nw$p, at = at)
    }
    if (type == "inst") {
      base.ai.BB.rate <- 1
      base.ai.BW.rate <- 1
      base.ai.WW.rate <- 1
      fixed <- ifelse(ai.scale.w != 1 | ai.scale.b != 1, FALSE, TRUE)
      el <- matrix(as.edgelist(dat$nw$i), ncol = 2)
    }


    ## Processes ##

    # Construct edgelist
    el <- as.data.frame(el)
    names(el) <- c("p1", "p2")
    st1 <- status[el$p1]
    st2 <- status[el$p2]
    disc <- abs(st1 - st2) == 1
    el[which(disc == 1 & st2 == 1), ] <- el[which(disc == 1 & st2 == 1), 2:1]
    el$st1 <- status[el$p1]
    el$st2 <- status[el$p2]

    if (nrow(el) > 0) {

      # Base AI rates
      ai.rate <- rep(NA, nrow(el))
      race.p1 <- race[el$p1]
      race.p2 <- race[el$p2]
      num.B <- (race.p1 == "B") + (race.p2 == "B")
      ai.rate <- (num.B == 2) * base.ai.BB.rate * ai.scale.b +
                 (num.B == 1) * base.ai.BW.rate * ((ai.scale.b + ai.scale.w)/2) +
                 (num.B == 0) * base.ai.WW.rate * ai.scale.w
     

      # Final act number
      if (fixed == FALSE) {
        ai <- rpois(length(ai.rate), ai.rate)
      } else {
        ai <- round(ai.rate)
      }

      # Full edge list
      el$type <- type
      el$ai <- ai
      el$uai <- NA

      if (type == "main") {
        dat$temp$el <- el
      } else {
        dat$temp$el <- rbind(dat$temp$el, el)
      }
    }

  } # loop over type end
  
  # Remove inactive edges from el
  dat$temp$el <- dat$temp$el[-which(dat$temp$el$ai == 0), ]
  
  #set any node with AI to having had AI.
  ids <- unique(c(el$p1, el$p2))
  counts <- sapply(ids, function(x) sum(el$ai[el$p1==x | el$p2==x]))
  mat<-as.data.frame(cbind(ids,counts))
  
  dat$attr$AI.ysmm.count[ids]<-dat$attr$AI.ysmm.count[ids]+mat$counts[mat$ids==ids]
  mat<-subset(mat,mat$counts>0)
  ids<-mat$ids
  dat$attr$everAI[ids]<-1
  dat$attr$AI.time[ids]<-ifelse(dat$attr$AI.time[ids]==0,at,dat$attr$AI.time[ids])
  
 

  return(dat)
}
