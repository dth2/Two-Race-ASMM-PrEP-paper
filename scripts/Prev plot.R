
## whamp analysis file

library(mardham2)
library(EpiModelHPC)


s1 <- merge_simfiles(1, indir = "scenarios/adol2race/data/")
s28 <- merge_simfiles(28, indir = "scenarios/adol2race/data/")


# Figure 1 ----------------------------------------------------------------

# Line plots of cumulative PIA and NNT

steps <- 1:520
df.nia.w <- data.frame(rep(NA, 100))
df.pia.w <- data.frame(rep(NA, 100))
df.nnt.w <- data.frame(rep(NA, 100))

df.nia.b <- data.frame(rep(NA, 100))
df.pia.b <- data.frame(rep(NA, 100))
df.nnt.b <- data.frame(rep(NA, 100))

base.prev.b<-rep(NA,520)
base.prev_hi.b<-rep(NA,520)
base.prev_low.b<-rep(NA,520)

base.prev.w<-rep(NA,520)
base.prev_hi.w<-rep(NA,520)
base.prev_low.w<-rep(NA,520)

inter.prev.b<-rep(NA,520)
inter.prev_hi.b<-rep(NA,520)
inter.prev_low.b<-rep(NA,520)

inter.prev.w<-rep(NA,520)
inter.prev_hi.w<-rep(NA,520)
inter.prev_low.w<-rep(NA,520)

for (i in seq_along(steps)) {
  sim.base <- truncate_sim(s1, at = 1040)
  
  mn.base <- head(as.data.frame(sim.base), steps[i])
 
   ir.base.w <- (sum(mn.base$incid.W)/sum((1 - mn.base$i.prev.W) * mn.base$num.W)) * 52 * 1e5
  incid.base.w <- sum(mn.base$incid.W)
 
   ir.base.b <- (sum(mn.base$incid.B)/sum((1 - mn.base$i.prev.B) * mn.base$num.B)) * 52 * 1e5
  incid.base.b <- sum(mn.base$incid.B)
  
  
  x.w<-sort(as.numeric(sim.base$epi$i.prev.age6.W[i,1:100]))
  base.prev.w[i]<-mean(x.w)
  base.prev_hi.w[i]<-mean(x.w[97],x.w[98])
  base.prev_low.w[i]<-mean(x.w[3],x.w[2])
  
  x.b<-sort(as.numeric(sim.base$epi$i.prev.age6.B[i,1:100]))
  base.prev.b[i]<-mean(x.b)
  base.prev_hi.b[i]<-mean(x.b[97],x.b[98])
  base.prev_low.b[i]<-mean(x.b[3],x.b[2])
  
  sim <- truncate_sim(s28, at = 1040)
  mn <- head(as.data.frame(sim), steps[i])
  
  x.w<-sort(as.numeric(sim$epi$i.prev.age6.W[i,1:100]))
  inter.prev.w[i]<-mean(x.w)
  inter.prev_hi.w[i]<-mean(x.w[97],x.w[98])
  inter.prev_low.w[i]<-mean(x.w[3],x.w[2])
  
  x.b<-sort(as.numeric(sim$epi$i.prev.age6.B[i,1:100]))
  inter.prev.b[i]<-mean(x.b)
  inter.prev_hi.b[i]<-mean(x.b[97],x.b[98])
  inter.prev_low.b[i]<-mean(x.b[3],x.b[2])

  
  # NIA calculation
  ir.w <- (colSums(head(sim$epi$incid.W, steps[i])))/
    sum(head(1 - mn$i.prev.W, steps[i])  * head(mn$num.W, steps[i])) * 52 * 1e5
  df.nia.w[, i] <- round(ir.base.w - unname(ir.w), 1)[1:100]
  
  ir.b <- (colSums(head(sim$epi$incid.B, steps[i])))/
    sum(head(1 - mn$i.prev.B, steps[i])  * head(mn$num.B, steps[i])) * 52 * 1e5
  df.nia.b[, i] <- round(ir.base.b - unname(ir.b), 1)[1:100]
  
  # PIA calculation
  df.pia.w[, i] <- df.nia.w[, i] / ir.base.w
  df.pia.b[, i] <- df.nia.b[, i] / ir.base.b
  
  # NNT calculation
  py.on.prep.w <- (unname(colSums(head(sim$epi$prepCurr.W, steps[i])))/52)[1:100]
  df.nnt.w[, i] <- py.on.prep.w/(incid.base.w - unname(colSums(head(sim$epi$incid.W, steps[i]))))[1:100]
  print(i)
  
  py.on.prep.b <- (unname(colSums(head(sim$epi$prepCurr.B, steps[i])))/52)[1:100]
  df.nnt.b[, i] <- py.on.prep.b/(incid.base.b - unname(colSums(head(sim$epi$incid.B, steps[i]))))[1:100]
  print(i)
  
}

base.prev.w <- base.prev.w * 100
base.prev_hi.w <- base.prev_hi.w * 100
base.prev_low.w <- base.prev_low.w * 100
base.prev.b <- base.prev.b * 100
base.prev_hi.b <- base.prev_hi.b * 100
base.prev_low.b <- base.prev_low.b * 100


inter.prev.w <- inter.prev.w * 100
inter.prev_hi.w <- inter.prev_hi.w * 100
inter.prev_low.w <- inter.prev_low.w * 100
inter.prev.b <- inter.prev.b * 100
inter.prev_hi.b <- inter.prev_hi.b * 100
inter.prev_low.b <- inter.prev_low.b * 100


names(df.nia.w) <- names(df.pia.w) <- names(df.nnt.w) <- (1:520)/52
names(df.nia.b) <- names(df.pia.b) <- names(df.nnt.b) <- (1:520)/52

# boxplot(df.nia, outline = FALSE)
# boxplot(df.pia, outline = FALSE)
# boxplot(df.nnt, outline = FALSE)

library(wesanderson)
pal <- wes_palette("Zissou")[5]
pal2<- wes_palette("Zissou")[2]
  
pia.mean.w <- tail(unname(apply(df.pia.w, 2, mean)), 520)
pia.lwr.w <- tail(unname(apply(df.pia.w, 2, quantile, 0.25)), 520)
pia.upr.w <- tail(unname(apply(df.pia.w, 2, quantile, 0.75)), 520)

pia.mean.b <- tail(unname(apply(df.pia.b, 2, mean)), 520)
pia.lwr.b <- tail(unname(apply(df.pia.b, 2, quantile, 0.25)), 520)
pia.upr.b <- tail(unname(apply(df.pia.b, 2, quantile, 0.75)), 520)

pia.mean.w <- pia.mean.w * 100 
pia.lwr.w <- pia.lwr.w * 100 
pia.upr.w <- pia.upr.w * 100 

pia.mean.b <- pia.mean.b * 100 
pia.lwr.b <- pia.lwr.b * 100 
pia.upr.b <- pia.upr.b * 100 

## For Paper
#pdf(file = "Fig6.pdf", height = 6, width = 12, pointsize = 16)
tiff(filename = "Fig6.tiff", height = 5.5, width = 5.5, units = "in", res = 250)

par(mfrow = c(2,1), mar = c(3,3,2.5,5), mgp = c(2,1,0))
xticks <- seq(0, 520, 52)
yticks <- seq(0,60,10)
yticks2 <- seq(0,16,2)
plot(base.prev.b, type = "n", ylim = c(0, 16), lwd = 3, col="black", axes=FALSE,
     main = "HIV Prevalence", xlab = "Years", ylab = "Prevalence among 18 year-olds")
axis(1, at = xticks, labels = c("0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.6)
axis(2, at = yticks2, col.axis="black", las=1, cex.axis=.6)

##WHITE
xx <- c(1:(length(base.prev.w)), (length(base.prev.w)):1)
yy <- c(base.prev_low.w, rev(base.prev_hi.w))
polygon(xx, yy, col = EpiModel::transco("purple", alpha = 0.3), border = NA)
lines(base.prev.w, lwd = 2, col = "purple")

aa <- c(1:(length(inter.prev.w)), (length(inter.prev.w)):1)
bb <- c(inter.prev_low.w, rev(inter.prev_hi.w))
polygon(aa, bb, col = EpiModel::transco("blue", alpha = 0.3), border = NA)
lines(inter.prev.w, lwd = 2, col = "blue")

##BLACK
xx <- c(1:(length(base.prev.b)), (length(base.prev.b)):1)
yy <- c(base.prev_low.b, rev(base.prev_hi.b))
polygon(xx, yy, col = EpiModel::transco("pink", alpha = 0.3), border = NA)
lines(base.prev.b, lwd = 2, col = "pink")

aa <- c(1:(length(inter.prev.b)), (length(inter.prev.b)):1)
bb <- c(inter.prev_low.b, rev(inter.prev_hi.b))
polygon(aa, bb, col = EpiModel::transco("red", alpha = 0.3), border = NA)
lines(inter.prev.b, lwd = 2, col = "red")



plot(pia.mean.b, type = "n", ylim = c(0, 60), lwd = 3, axes=FALSE,
     main = "Cumulative PIA over 10 Years", xlab = "Years", ylab = "Percent Infections Averted")
axis(1, at = xticks, labels = c("0","1","2","3","4","5","6","7","8","9","10"), col.axis="black", las=1, cex.axis=.6)
axis(2, at = yticks, col.axis="black", las=1, cex.axis=.6)
xx <- c(1:(length(pia.mean.w)), (length(pia.mean.w)):1)
yy <- c(pia.lwr.w, rev(pia.upr.w))
polygon(xx, yy, col = EpiModel::transco("blue", alpha = 0.3), border = NA)
lines(pia.mean.w, lwd = 2, col = "blue")

aa <- c(1:(length(pia.mean.b)), (length(pia.mean.b)):1)
bb <- c(pia.lwr.b, rev(pia.upr.b))
polygon(aa, bb, col = EpiModel::transco("red", alpha = 0.3), border = NA)
lines(pia.mean.b, lwd = 2, col = "red")

legend(x=400, y=60, c("Black ASMM","White ASMM"),lwd=1, col=c("red", "blue"), cex=.5, xjust=0)



dev.off()
