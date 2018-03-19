


library(mardham2)
library(EpiModelHPC)



s1 <- merge_simfiles(1, indir = "scenarios/adol2race/data/")
s2 <- merge_simfiles(2, indir = "scenarios/adol2race/data/")
s3 <- merge_simfiles(3, indir = "scenarios/adol2race/data/")
s4 <- merge_simfiles(4, indir = "scenarios/adol2race/data/")
s5 <- merge_simfiles(5, indir = "scenarios/adol2race/data/")
s6 <- merge_simfiles(6, indir = "scenarios/adol2race/data/")
s7 <- merge_simfiles(7, indir = "scenarios/adol2race/data/")
s8 <- merge_simfiles(8, indir = "scenarios/adol2race/data/")
s9 <- merge_simfiles(9, indir = "scenarios/adol2race/data/")
s10 <- merge_simfiles(10, indir = "scenarios/adol2race/data/")
s11 <- merge_simfiles(11, indir = "scenarios/adol2race/data/")
s12 <- merge_simfiles(12, indir = "scenarios/adol2race/data/")
s13 <- merge_simfiles(13, indir = "scenarios/adol2race/data/")
s14 <- merge_simfiles(14, indir = "scenarios/adol2race/data/")
s15 <- merge_simfiles(15, indir = "scenarios/adol2race/data/")
s16 <- merge_simfiles(16, indir = "scenarios/adol2race/data/")
s17 <- merge_simfiles(17, indir = "scenarios/adol2race/data/")
s18 <- merge_simfiles(18, indir = "scenarios/adol2race/data/")
s19 <- merge_simfiles(19, indir = "scenarios/adol2race/data/")
s20 <- merge_simfiles(20, indir = "scenarios/adol2race/data/")
s21 <- merge_simfiles(21, indir = "scenarios/adol2race/data/")
s22 <- merge_simfiles(22, indir = "scenarios/adol2race/data/")
s23 <- merge_simfiles(23, indir = "scenarios/adol2race/data/")
s24 <- merge_simfiles(24, indir = "scenarios/adol2race/data/")
s25 <- merge_simfiles(25, indir = "scenarios/adol2race/data/")
s26 <- merge_simfiles(26, indir = "scenarios/adol2race/data/")
s27 <- merge_simfiles(27, indir = "scenarios/adol2race/data/")
s28 <- merge_simfiles(28, indir = "scenarios/adol2race/data/")

data<-c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10",
        "s11","s12","s13","s14","s15","s16","s17","s18","s19","s20",
        "s21","s22","s23","s24","s25","s26","s27","s28") 
 


##Basline (no prep)
incid.total.base.w<-rep(NA,100)
prep.pt.base.w<-rep(NA,100)
person.time.base.w<-rep(NA,100)
person.time.deb.base.w<-rep(NA,100)
prev.pop.base.w<-rep(NA,100)
prev.age18.base.w<-rep(NA,100)

incid.total.base.b<-rep(NA,100)
prep.pt.base.b<-rep(NA,100)
person.time.base.b<-rep(NA,100)
person.time.deb.base.b<-rep(NA,100)
prev.pop.base.b<-rep(NA,100)
prev.age18.base.b<-rep(NA,100)

  for (j in 1:100){  
    incid.total.base.w[j]<-sum(s1$epi$incid.W[1041:2080,j])
    prep.pt.base.w[j]<-sum(s1$epi$prepCurr.W[1041:2080,j])
    person.time.base.w[j]<-sum(s1$epi$num.W[1041:2080,j]) - sum(s1$epi$i.num.W[1041:2080,j])
    person.time.deb.base.w[j]<-sum(s1$epi$debuted.W[1041:2080,j]) - sum(s1$epi$i.num.W[1041:2080,j])
    prev.pop.base.w[j]<-s1$epi$i.prev.W[2080,j]
    prev.age18.base.w[j]<-s1$epi$i.prev.age6.W[2080,j]
 
   incid.base.w<-mean(incid.total.base.w)
   
   incid.total.base.b[j]<-sum(s1$epi$incid.B[1041:2080,j])
   prep.pt.base.b[j]<-sum(s1$epi$prepCurr.B[1041:2080,j])
   person.time.base.b[j]<-sum(s1$epi$num.B[1041:2080,j]) - sum(s1$epi$i.num.B[1041:2080,j])
   person.time.deb.base.b[j]<-sum(s1$epi$debuted.B[1041:2080,j]) - sum(s1$epi$i.num.B[1041:2080,j])
   prev.pop.base.b[j]<-s1$epi$i.prev.B[2080,j]
   prev.age18.base.b[j]<-s1$epi$i.prev.age6.B[2080,j]
   
   incid.base.b<-mean(incid.total.base.b)
   
   }


## Condition 1

incid.total.c1.w<-rep(NA,100)
prep.pt.c1.w<-rep(NA,100)
person.time.c1.w<-rep(NA,100)
person.time.deb.c1.w<-rep(NA,100)
prev.pop.c1.w<-rep(NA,100)
prev.age18.c1.w<-rep(NA,100)
NIA.c1.w<-rep(NA,100)
PIA.c1.w<-rep(NA,100)
NNT.c1.w<-rep(NA,100)

incid.total.c1.b<-rep(NA,100)
prep.pt.c1.b<-rep(NA,100)
person.time.c1.b<-rep(NA,100)
person.time.deb.c1.b<-rep(NA,100)
prev.pop.c1.b<-rep(NA,100)
prev.age18.c1.b<-rep(NA,100)
NIA.c1.b<-rep(NA,100)
PIA.c1.b<-rep(NA,100)
NNT.c1.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c1.w[j]<-sum(s2$epi$incid.W[1041:2080,j])
  prep.pt.c1.w[j]<-sum(s2$epi$prepCurr.W[1041:2080,j])
  person.time.c1.w[j]<-sum(s2$epi$num.W[1041:2080,j]) - sum(s2$epi$i.num.W[1041:2080,j])
  person.time.deb.c1.w[j]<-sum(s2$epi$debuted.W[1041:2080,j])  - sum(s2$epi$i.num.W[1041:2080,j])
  prev.pop.c1.w[j]<-s2$epi$i.prev.W[2080,j]
  prev.age18.c1.w[j]<-s2$epi$i.prev.age6.W[2080,j]
  
  incid.total.c1.b[j]<-sum(s2$epi$incid.B[1041:2080,j])
  prep.pt.c1.b[j]<-sum(s2$epi$prepCurr.B[1041:2080,j])
  person.time.c1.b[j]<-sum(s2$epi$num.B[1041:2080,j]) - sum(s2$epi$i.num.B[1041:2080,j])
  person.time.deb.c1.b[j]<-sum(s2$epi$debuted.B[1041:2080,j]) - sum(s2$epi$i.num.B[1041:2080,j])
  prev.pop.c1.b[j]<-s2$epi$i.prev.B[2080,j]
  prev.age18.c1.b[j]<-s2$epi$i.prev.age6.B[2080,j]
}
  

#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c1.w[i]<-((incid.base.w-incid.total.c1.w[i])/person.time.deb.c1.w[i])*52*100000
  NIA.c1.b[i]<-((incid.base.b-incid.total.c1.b[i])/person.time.deb.c1.b[i])*52*100000
  
#Percent of infection averted.

  PIA.c1.w[i]<-(incid.base.w-incid.total.c1.w[i])/incid.base.w
  PIA.c1.b[i]<-(incid.base.b-incid.total.c1.b[i])/incid.base.b
  
#NNT .
  
  NNT.c1.w[i]<-(prep.pt.c1.w[i]/52)/(incid.base.w-incid.total.c1.w[i])
  NNT.c1.b[i]<-(prep.pt.c1.b[i]/52)/(incid.base.b-incid.total.c1.b[i])
}


##Condition 2.

incid.total.c2.w<-rep(NA,100)
prep.pt.c2.w<-rep(NA,100)
person.time.c2.w<-rep(NA,100)
person.time.deb.c2.w<-rep(NA,100)
prev.pop.c2.w<-rep(NA,100)
prev.age18.c2.w<-rep(NA,100)
NIA.c2.w<-rep(NA,100)
PIA.c2.w<-rep(NA,100)
NNT.c2.w<-rep(NA,100)

incid.total.c2.b<-rep(NA,100)
prep.pt.c2.b<-rep(NA,100)
person.time.c2.b<-rep(NA,100)
person.time.deb.c2.b<-rep(NA,100)
prev.pop.c2.b<-rep(NA,100)
prev.age18.c2.b<-rep(NA,100)
NIA.c2.b<-rep(NA,100)
PIA.c2.b<-rep(NA,100)
NNT.c2.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c2.w[j]<-sum(s3$epi$incid.W[1041:2080,j])
  prep.pt.c2.w[j]<-sum(s3$epi$prepCurr.W[1041:2080,j])
  person.time.c2.w[j]<-sum(s3$epi$num.W[1041:2080,j]) - sum(s3$epi$i.num.W[1041:2080,j])
  person.time.deb.c2.w[j]<-sum(s3$epi$debuted.W[1041:2080,j]) - sum(s3$epi$i.num.W[1041:2080,j])         
  prev.pop.c2.w[j]<-s3$epi$i.prev.W[2080,j]
  prev.age18.c2.w[j]<-s3$epi$i.prev.age6.W[2080,j]
  
  incid.total.c2.b[j]<-sum(s3$epi$incid.B[1041:2080,j])
  prep.pt.c2.b[j]<-sum(s3$epi$prepCurr.B[1041:2080,j])
  person.time.c2.b[j]<-sum(s3$epi$num.B[1041:2080,j]) - sum(s3$epi$i.num.B[1041:2080,j])
  person.time.deb.c2.b[j]<-sum(s3$epi$debuted.B[1041:2080,j]) - sum(s3$epi$i.num.B[1041:2080,j])         
  prev.pop.c2.b[j]<-s3$epi$i.prev.B[2080,j]
  prev.age18.c2.b[j]<-s3$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c2.w[i]<-((incid.base.w-incid.total.c2.w[i])/person.time.deb.c2.w[i])*52*100000
  NIA.c2.b[i]<-((incid.base.b-incid.total.c2.b[i])/person.time.deb.c2.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c2.w[i]<-(incid.base.w-incid.total.c2.w[i])/incid.base.w
  PIA.c2.b[i]<-(incid.base.b-incid.total.c2.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c2.w[i]<-(prep.pt.c2.w[i]/52)/(incid.base.w-incid.total.c2.w[i])
  NNT.c2.b[i]<-(prep.pt.c2.b[i]/52)/(incid.base.b-incid.total.c2.b[i])
}


##Condition 3.


incid.total.c3.w<-rep(NA,100)
prep.pt.c3.w<-rep(NA,100)
person.time.c3.w<-rep(NA,100)
person.time.deb.c3.w<-rep(NA,100)
prev.pop.c3.w<-rep(NA,100)
prev.age18.c3.w<-rep(NA,100)
NIA.c3.w<-rep(NA,100)
PIA.c3.w<-rep(NA,100)
NNT.c3.w<-rep(NA,100)

incid.total.c3.b<-rep(NA,100)
prep.pt.c3.b<-rep(NA,100)
person.time.c3.b<-rep(NA,100)
person.time.deb.c3.b<-rep(NA,100)
prev.pop.c3.b<-rep(NA,100)
prev.age18.c3.b<-rep(NA,100)
NIA.c3.b<-rep(NA,100)
PIA.c3.b<-rep(NA,100)
NNT.c3.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c3.w[j]<-sum(s4$epi$incid.W[1041:2080,j])
  prep.pt.c3.w[j]<-sum(s4$epi$prepCurr.W[1041:2080,j])
  person.time.c3.w[j]<-sum(s4$epi$num.W[1041:2080,j]) - sum(s4$epi$i.num.W[1041:2080,j])
  person.time.deb.c3.w[j]<-sum(s4$epi$debuted.W[1041:2080,j]) - sum(s4$epi$i.num.W[1041:2080,j])         
  prev.pop.c3.w[j]<-s4$epi$i.prev.W[2080,j]
  prev.age18.c3.w[j]<-s4$epi$i.prev.age6.W[2080,j]
  
  incid.total.c3.b[j]<-sum(s4$epi$incid.B[1041:2080,j])
  prep.pt.c3.b[j]<-sum(s4$epi$prepCurr.B[1041:2080,j])
  person.time.c3.b[j]<-sum(s4$epi$num.B[1041:2080,j]) - sum(s4$epi$i.num.B[1041:2080,j])
  person.time.deb.c3.b[j]<-sum(s4$epi$debuted.B[1041:2080,j]) - sum(s4$epi$i.num.B[1041:2080,j])         
  prev.pop.c3.b[j]<-s4$epi$i.prev.B[2080,j]
  prev.age18.c3.b[j]<-s4$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c3.w[i]<-((incid.base.w-incid.total.c3.w[i])/person.time.deb.c3.w[i])*52*100000
  NIA.c3.b[i]<-((incid.base.b-incid.total.c3.b[i])/person.time.deb.c3.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c3.w[i]<-(incid.base.w-incid.total.c3.w[i])/incid.base.w
  PIA.c3.b[i]<-(incid.base.b-incid.total.c3.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c3.w[i]<-(prep.pt.c3.w[i]/52)/(incid.base.w-incid.total.c3.w[i])
  NNT.c3.b[i]<-(prep.pt.c3.b[i]/52)/(incid.base.b-incid.total.c3.b[i])
}

##Condition 4.


incid.total.c4.w<-rep(NA,100)
prep.pt.c4.w<-rep(NA,100)
person.time.c4.w<-rep(NA,100)
person.time.deb.c4.w<-rep(NA,100)
prev.pop.c4.w<-rep(NA,100)
prev.age18.c4.w<-rep(NA,100)
NIA.c4.w<-rep(NA,100)
PIA.c4.w<-rep(NA,100)
NNT.c4.w<-rep(NA,100)

incid.total.c4.b<-rep(NA,100)
prep.pt.c4.b<-rep(NA,100)
person.time.c4.b<-rep(NA,100)
person.time.deb.c4.b<-rep(NA,100)
prev.pop.c4.b<-rep(NA,100)
prev.age18.c4.b<-rep(NA,100)
NIA.c4.b<-rep(NA,100)
PIA.c4.b<-rep(NA,100)
NNT.c4.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c4.w[j]<-sum(s5$epi$incid.W[1041:2080,j])
  prep.pt.c4.w[j]<-sum(s5$epi$prepCurr.W[1041:2080,j])
  person.time.c4.w[j]<-sum(s5$epi$num.W[1041:2080,j]) - sum(s5$epi$i.num.W[1041:2080,j])
  person.time.deb.c4.w[j]<-sum(s5$epi$debuted.W[1041:2080,j]) - sum(s5$epi$i.num.W[1041:2080,j])         
  prev.pop.c4.w[j]<-s5$epi$i.prev.W[2080,j]
  prev.age18.c4.w[j]<-s5$epi$i.prev.age6.W[2080,j]
  
  incid.total.c4.b[j]<-sum(s5$epi$incid.B[1041:2080,j])
  prep.pt.c4.b[j]<-sum(s5$epi$prepCurr.B[1041:2080,j])
  person.time.c4.b[j]<-sum(s5$epi$num.B[1041:2080,j]) - sum(s5$epi$i.num.B[1041:2080,j])
  person.time.deb.c4.b[j]<-sum(s5$epi$debuted.B[1041:2080,j]) - sum(s5$epi$i.num.B[1041:2080,j])         
  prev.pop.c4.b[j]<-s5$epi$i.prev.B[2080,j]
  prev.age18.c4.b[j]<-s5$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c4.w[i]<-((incid.base.w-incid.total.c4.w[i])/person.time.deb.c4.w[i])*52*100000
  NIA.c4.b[i]<-((incid.base.b-incid.total.c4.b[i])/person.time.deb.c4.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c4.w[i]<-(incid.base.w-incid.total.c4.w[i])/incid.base.w
  PIA.c4.b[i]<-(incid.base.b-incid.total.c4.b[i])/incid.base.b
  
  #NNT .
  
  NNT.c4.w[i]<-(prep.pt.c4.w[i]/52)/(incid.base.w-incid.total.c4.w[i])
  NNT.c4.b[i]<-(prep.pt.c4.b[i]/52)/(incid.base.b-incid.total.c4.b[i])
}

##Condition 5.

incid.total.c5.w<-rep(NA,100)
prep.pt.c5.w<-rep(NA,100)
person.time.c5.w<-rep(NA,100)
person.time.deb.c5.w<-rep(NA,100)
prev.pop.c5.w<-rep(NA,100)
prev.age18.c5.w<-rep(NA,100)
NIA.c5.w<-rep(NA,100)
PIA.c5.w<-rep(NA,100)
NNT.c5.w<-rep(NA,100)

incid.total.c5.b<-rep(NA,100)
prep.pt.c5.b<-rep(NA,100)
person.time.c5.b<-rep(NA,100)
person.time.deb.c5.b<-rep(NA,100)
prev.pop.c5.b<-rep(NA,100)
prev.age18.c5.b<-rep(NA,100)
NIA.c5.b<-rep(NA,100)
PIA.c5.b<-rep(NA,100)
NNT.c5.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c5.w[j]<-sum(s6$epi$incid.W[1041:2080,j])
  prep.pt.c5.w[j]<-sum(s6$epi$prepCurr.W[1041:2080,j])
  person.time.c5.w[j]<-sum(s6$epi$num.W[1041:2080,j]) - sum(s6$epi$i.num.W[1041:2080,j])
  person.time.deb.c5.w[j]<-sum(s6$epi$debuted.W[1041:2080,j]) - sum(s6$epi$i.num.W[1041:2080,j])    
  prev.pop.c5.w[j]<-s6$epi$i.prev.W[2080,j]
  prev.age18.c5.w[j]<-s6$epi$i.prev.age6.W[2080,j]
  
  incid.total.c5.b[j]<-sum(s6$epi$incid.B[1041:2080,j])
  prep.pt.c5.b[j]<-sum(s6$epi$prepCurr.B[1041:2080,j])
  person.time.c5.b[j]<-sum(s6$epi$num.B[1041:2080,j]) - sum(s6$epi$i.num.B[1041:2080,j])
  person.time.deb.c5.b[j]<-sum(s6$epi$debuted.B[1041:2080,j]) - sum(s6$epi$i.num.B[1041:2080,j])      
  prev.pop.c5.b[j]<-s6$epi$i.prev.B[2080,j]
  prev.age18.c5.b[j]<-s6$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c5.w[i]<-((incid.base.w-incid.total.c5.w[i])/person.time.deb.c5.w[i])*52*100000
  NIA.c5.b[i]<-((incid.base.b-incid.total.c5.b[i])/person.time.deb.c5.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c5.w[i]<-(incid.base.w-incid.total.c5.w[i])/incid.base.w
  PIA.c5.b[i]<-(incid.base.b-incid.total.c5.b[i])/incid.base.b
  
  #NNT.
  
  NNT.c5.w[i]<-(prep.pt.c5.w[i]/52)/(incid.base.w-incid.total.c5.w[i])
  NNT.c5.b[i]<-(prep.pt.c5.b[i]/52)/(incid.base.b-incid.total.c5.b[i])
  
}




#Condition 6.


incid.total.c6.w<-rep(NA,100)
prep.pt.c6.w<-rep(NA,100)
person.time.c6.w<-rep(NA,100)
person.time.deb.c6.w<-rep(NA,100)
prev.pop.c6.w<-rep(NA,100)
prev.age18.c6.w<-rep(NA,100)
NIA.c6.w<-rep(NA,100)
PIA.c6.w<-rep(NA,100)
NNT.c6.w<-rep(NA,100)

incid.total.c6.b<-rep(NA,100)
prep.pt.c6.b<-rep(NA,100)
person.time.c6.b<-rep(NA,100)
person.time.deb.c6.b<-rep(NA,100)
prev.pop.c6.b<-rep(NA,100)
prev.age18.c6.b<-rep(NA,100)
NIA.c6.b<-rep(NA,100)
PIA.c6.b<-rep(NA,100)
NNT.c6.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c6.w[j]<-sum(s7$epi$incid.W[1041:2080,j])
  prep.pt.c6.w[j]<-sum(s7$epi$prepCurr.W[1041:2080,j])
  person.time.c6.w[j]<-sum(s7$epi$num.W[1041:2080,j]) - sum(s7$epi$i.num.W[1041:2080,j])
  person.time.deb.c6.w[j]<-sum(s7$epi$debuted.W[1041:2080,j]) - sum(s7$epi$i.num.W[1041:2080,j])        
  prev.pop.c6.w[j]<-s7$epi$i.prev.W[2080,j]
  prev.age18.c6.w[j]<-s7$epi$i.prev.age6.W[2080,j]
  
  incid.total.c6.b[j]<-sum(s7$epi$incid.B[1041:2080,j])
  prep.pt.c6.b[j]<-sum(s7$epi$prepCurr.B[1041:2080,j])
  person.time.c6.b[j]<-sum(s7$epi$num.B[1041:2080,j]) - sum(s7$epi$i.num.B[1041:2080,j])
  person.time.deb.c6.b[j]<-sum(s7$epi$debuted.B[1041:2080,j]) - sum(s7$epi$i.num.B[1041:2080,j])        
  prev.pop.c6.b[j]<-s7$epi$i.prev.B[2080,j]
  prev.age18.c6.b[j]<-s7$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c6.w[i]<-((incid.base.w-incid.total.c6.w[i])/person.time.deb.c6.w[i])*52*100000
  NIA.c6.b[i]<-((incid.base.b-incid.total.c6.b[i])/person.time.deb.c6.b[i])*52*100000
  
#Percent of infection averted.
  
  PIA.c6.w[i]<-(incid.base.w-incid.total.c6.w[i])/incid.base.w
  PIA.c6.b[i]<-(incid.base.b-incid.total.c6.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c6.w[i]<-(prep.pt.c6.w[i]/52)/(incid.base.w-incid.total.c6.w[i])
  NNT.c6.b[i]<-(prep.pt.c6.b[i]/52)/(incid.base.b-incid.total.c6.b[i])  
}



##Condition 7.

incid.total.c7.w<-rep(NA,100)
prep.pt.c7.w<-rep(NA,100)
person.time.c7.w<-rep(NA,100)
person.time.deb.c7.w<-rep(NA,100)
prev.pop.c7.w<-rep(NA,100)
prev.age18.c7.w<-rep(NA,100)
NIA.c7.w<-rep(NA,100)
PIA.c7.w<-rep(NA,100)
NNT.c7.w<-rep(NA,100)

incid.total.c7.b<-rep(NA,100)
prep.pt.c7.b<-rep(NA,100)
person.time.c7.b<-rep(NA,100)
person.time.deb.c7.b<-rep(NA,100)
prev.pop.c7.b<-rep(NA,100)
prev.age18.c7.b<-rep(NA,100)
NIA.c7.b<-rep(NA,100)
PIA.c7.b<-rep(NA,100)
NNT.c7.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c7.w[j]<-sum(s8$epi$incid.W[1041:2080,j])
  prep.pt.c7.w[j]<-sum(s8$epi$prepCurr.W[1041:2080,j])
  person.time.c7.w[j]<-sum(s8$epi$num.W[1041:2080,j]) - sum(s8$epi$i.num.W[1041:2080,j])
  person.time.deb.c7.w[j]<-sum(s8$epi$debuted.W[1041:2080,j]) - sum(s8$epi$i.num.W[1041:2080,j])   
  prev.pop.c7.w[j]<-s8$epi$i.prev.W[2080,j]
  prev.age18.c7.w[j]<-s8$epi$i.prev.age6.W[2080,j]
  
  incid.total.c7.b[j]<-sum(s8$epi$incid.B[1041:2080,j])
  prep.pt.c7.b[j]<-sum(s8$epi$prepCurr.B[1041:2080,j])
  person.time.c7.b[j]<-sum(s8$epi$num.B[1041:2080,j]) - sum(s8$epi$i.num.B[1041:2080,j])
  person.time.deb.c7.b[j]<-sum(s8$epi$debuted.B[1041:2080,j]) - sum(s8$epi$i.num.B[1041:2080,j])     
  prev.pop.c7.b[j]<-s8$epi$i.prev.B[2080,j]
  prev.age18.c7.b[j]<-s8$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c7.w[i]<-((incid.base.w-incid.total.c7.w[i])/person.time.deb.c7.w[i])*52*100000
  NIA.c7.b[i]<-((incid.base.b-incid.total.c7.b[i])/person.time.deb.c7.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c7.w[i]<-(incid.base.w-incid.total.c7.w[i])/incid.base.w
  PIA.c7.b[i]<-(incid.base.b-incid.total.c7.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c7.w[i]<-(prep.pt.c7.w[i]/52)/(incid.base.w-incid.total.c7.w[i])
  NNT.c7.b[i]<-(prep.pt.c7.b[i]/52)/(incid.base.b-incid.total.c7.b[i])
}


##Condition 8.

incid.total.c8.w<-rep(NA,100)
prep.pt.c8.w<-rep(NA,100)
person.time.c8.w<-rep(NA,100)
person.time.deb.c8.w<-rep(NA,100)
prev.pop.c8.w<-rep(NA,100)
prev.age18.c8.w<-rep(NA,100)
NIA.c8.w<-rep(NA,100)
PIA.c8.w<-rep(NA,100)
NNT.c8.w<-rep(NA,100)

incid.total.c8.b<-rep(NA,100)
prep.pt.c8.b<-rep(NA,100)
person.time.c8.b<-rep(NA,100)
person.time.deb.c8.b<-rep(NA,100)
prev.pop.c8.b<-rep(NA,100)
prev.age18.c8.b<-rep(NA,100)
NIA.c8.b<-rep(NA,100)
PIA.c8.b<-rep(NA,100)
NNT.c8.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c8.w[j]<-sum(s9$epi$incid.W[1041:2080,j])
  prep.pt.c8.w[j]<-sum(s9$epi$prepCurr.W[1041:2080,j])
  person.time.c8.w[j]<-sum(s9$epi$num.W[1041:2080,j]) - sum(s9$epi$i.num.W[1041:2080,j])
  person.time.deb.c8.w[j]<-sum(s9$epi$debuted.W[1041:2080,j]) - sum(s9$epi$i.num.W[1041:2080,j])       
  prev.pop.c8.w[j]<-s9$epi$i.prev.W[2080,j]
  prev.age18.c8.w[j]<-s9$epi$i.prev.age6.W[2080,j]
  
  incid.total.c8.b[j]<-sum(s9$epi$incid.B[1041:2080,j])
  prep.pt.c8.b[j]<-sum(s9$epi$prepCurr.B[1041:2080,j])
  person.time.c8.b[j]<-sum(s9$epi$num.B[1041:2080,j]) - sum(s9$epi$i.num.B[1041:2080,j])
  person.time.deb.c8.b[j]<-sum(s9$epi$debuted.B[1041:2080,j]) - sum(s9$epi$i.num.B[1041:2080,j])      
  prev.pop.c8.b[j]<-s9$epi$i.prev.B[2080,j]
  prev.age18.c8.b[j]<-s9$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c8.w[i]<-((incid.base.w-incid.total.c8.w[i])/person.time.deb.c8.w[i])*52*100000
  NIA.c8.b[i]<-((incid.base.b-incid.total.c8.b[i])/person.time.deb.c8.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c8.w[i]<-(incid.base.w-incid.total.c8.w[i])/incid.base.w
  PIA.c8.b[i]<-(incid.base.b-incid.total.c8.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c8.w[i]<-(prep.pt.c8.w[i]/52)/(incid.base.w-incid.total.c8.w[i])
  NNT.c8.b[i]<-(prep.pt.c8.b[i]/52)/(incid.base.b-incid.total.c8.b[i])
}


##Condition 9.


incid.total.c9.w<-rep(NA,100)
prep.pt.c9.w<-rep(NA,100)
person.time.c9.w<-rep(NA,100)
person.time.deb.c9.w<-rep(NA,100)
prev.pop.c9.w<-rep(NA,100)
prev.age18.c9.w<-rep(NA,100)
NIA.c9.w<-rep(NA,100)
PIA.c9.w<-rep(NA,100)
NNT.c9.w<-rep(NA,100)

incid.total.c9.b<-rep(NA,100)
prep.pt.c9.b<-rep(NA,100)
person.time.c9.b<-rep(NA,100)
person.time.deb.c9.b<-rep(NA,100)
prev.pop.c9.b<-rep(NA,100)
prev.age18.c9.b<-rep(NA,100)
NIA.c9.b<-rep(NA,100)
PIA.c9.b<-rep(NA,100)
NNT.c9.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c9.w[j]<-sum(s10$epi$incid.W[1041:2080,j])
  prep.pt.c9.w[j]<-sum(s10$epi$prepCurr.W[1041:2080,j])
  person.time.c9.w[j]<-sum(s10$epi$num.W[1041:2080,j]) - sum(s10$epi$i.num.W[1041:2080,j])
  person.time.deb.c9.w[j]<-sum(s10$epi$debuted.W[1041:2080,j]) - sum(s10$epi$i.num.W[1041:2080,j])  
  prev.pop.c9.w[j]<-s10$epi$i.prev.W[2080,j]
  prev.age18.c9.w[j]<-s10$epi$i.prev.age6.W[2080,j]
  
  incid.total.c9.b[j]<-sum(s10$epi$incid.B[1041:2080,j])
  prep.pt.c9.b[j]<-sum(s10$epi$prepCurr.B[1041:2080,j])
  person.time.c9.b[j]<-sum(s10$epi$num.B[1041:2080,j]) - sum(s10$epi$i.num.B[1041:2080,j])
  person.time.deb.c9.b[j]<-sum(s10$epi$debuted.B[1041:2080,j]) - sum(s10$epi$i.num.B[1041:2080,j])     
  prev.pop.c9.b[j]<-s10$epi$i.prev.B[2080,j]
  prev.age18.c9.b[j]<-s10$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c9.w[i]<-((incid.base.w-incid.total.c9.w[i])/person.time.deb.c9.w[i])*52*100000
  NIA.c9.b[i]<-((incid.base.b-incid.total.c9.b[i])/person.time.deb.c9.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c9.w[i]<-(incid.base.w-incid.total.c9.w[i])/incid.base.w
  PIA.c9.b[i]<-(incid.base.b-incid.total.c9.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c9.w[i]<-(prep.pt.c9.w[i]/52)/(incid.base.w-incid.total.c9.w[i])
  NNT.c9.b[i]<-(prep.pt.c9.b[i]/52)/(incid.base.b-incid.total.c9.b[i])
}

##Condition 10.


incid.total.c10.w<-rep(NA,100)
prep.pt.c10.w<-rep(NA,100)
person.time.c10.w<-rep(NA,100)
person.time.deb.c10.w<-rep(NA,100)
prev.pop.c10.w<-rep(NA,100)
prev.age18.c10.w<-rep(NA,100)
NIA.c10.w<-rep(NA,100)
PIA.c10.w<-rep(NA,100)
NNT.c10.w<-rep(NA,100)

incid.total.c10.b<-rep(NA,100)
prep.pt.c10.b<-rep(NA,100)
person.time.c10.b<-rep(NA,100)
person.time.deb.c10.b<-rep(NA,100)
prev.pop.c10.b<-rep(NA,100)
prev.age18.c10.b<-rep(NA,100)
NIA.c10.b<-rep(NA,100)
PIA.c10.b<-rep(NA,100)
NNT.c10.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c10.w[j]<-sum(s11$epi$incid.W[1041:2080,j])
  prep.pt.c10.w[j]<-sum(s11$epi$prepCurr.W[1041:2080,j])
  person.time.c10.w[j]<-sum(s11$epi$num.W[1041:2080,j]) - sum(s11$epi$i.num.W[1041:2080,j])
  person.time.deb.c10.w[j]<-sum(s11$epi$debuted.W[1041:2080,j]) - sum(s11$epi$i.num.W[1041:2080,j])      
  prev.pop.c10.w[j]<-s11$epi$i.prev.W[2080,j]
  prev.age18.c10.w[j]<-s11$epi$i.prev.age6.W[2080,j]
  
  incid.total.c10.b[j]<-sum(s11$epi$incid.B[1041:2080,j])
  prep.pt.c10.b[j]<-sum(s11$epi$prepCurr.B[1041:2080,j])
  person.time.c10.b[j]<-sum(s11$epi$num.B[1041:2080,j]) - sum(s11$epi$i.num.B[1041:2080,j])
  person.time.deb.c10.b[j]<-sum(s11$epi$debuted.B[1041:2080,j]) - sum(s11$epi$i.num.B[1041:2080,j])       
  prev.pop.c10.b[j]<-s11$epi$i.prev.B[2080,j]
  prev.age18.c10.b[j]<-s11$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c10.w[i]<-((incid.base.w-incid.total.c10.w[i])/person.time.deb.c10.w[i])*52*100000
  NIA.c10.b[i]<-((incid.base.b-incid.total.c10.b[i])/person.time.deb.c10.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c10.w[i]<-(incid.base.w-incid.total.c10.w[i])/incid.base.w
  PIA.c10.b[i]<-(incid.base.b-incid.total.c10.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c10.w[i]<-(prep.pt.c10.w[i]/52)/(incid.base.w-incid.total.c10.w[i])
  NNT.c10.b[i]<-(prep.pt.c10.b[i]/52)/(incid.base.b-incid.total.c10.b[i])
}

##Condition 11.


incid.total.c11.w<-rep(NA,100)
prep.pt.c11.w<-rep(NA,100)
person.time.c11.w<-rep(NA,100)
person.time.deb.c11.w<-rep(NA,100)
prev.pop.c11.w<-rep(NA,100)
prev.age18.c11.w<-rep(NA,100)
NIA.c11.w<-rep(NA,100)
PIA.c11.w<-rep(NA,100)
NNT.c11.w<-rep(NA,100)

incid.total.c11.b<-rep(NA,100)
prep.pt.c11.b<-rep(NA,100)
person.time.c11.b<-rep(NA,100)
person.time.deb.c11.b<-rep(NA,100)
prev.pop.c11.b<-rep(NA,100)
prev.age18.c11.b<-rep(NA,100)
NIA.c11.b<-rep(NA,100)
PIA.c11.b<-rep(NA,100)
NNT.c11.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c11.w[j]<-sum(s12$epi$incid.W[1041:2080,j])
  prep.pt.c11.w[j]<-sum(s12$epi$prepCurr.W[1041:2080,j])
  person.time.c11.w[j]<-sum(s12$epi$num.W[1041:2080,j]) - sum(s12$epi$i.num.W[1041:2080,j])
  person.time.deb.c11.w[j]<-sum(s12$epi$debuted.W[1041:2080,j])  - sum(s12$epi$i.num.W[1041:2080,j])      
  prev.pop.c11.w[j]<-s12$epi$i.prev.W[2080,j]
  prev.age18.c11.w[j]<-s12$epi$i.prev.age6.W[2080,j]
  
  incid.total.c11.b[j]<-sum(s12$epi$incid.B[1041:2080,j])
  prep.pt.c11.b[j]<-sum(s12$epi$prepCurr.B[1041:2080,j])
  person.time.c11.b[j]<-sum(s12$epi$num.B[1041:2080,j]) - sum(s12$epi$i.num.B[1041:2080,j])
  person.time.deb.c11.b[j]<-sum(s12$epi$debuted.B[1041:2080,j]) - sum(s12$epi$i.num.B[1041:2080,j])     
  prev.pop.c11.b[j]<-s12$epi$i.prev.B[2080,j]
  prev.age18.c11.b[j]<-s12$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c11.w[i]<-((incid.base.w-incid.total.c11.w[i])/person.time.deb.c11.w[i])*52*100000
  NIA.c11.b[i]<-((incid.base.b-incid.total.c11.b[i])/person.time.deb.c11.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c11.w[i]<-(incid.base.w-incid.total.c11.w[i])/incid.base.w
  PIA.c11.b[i]<-(incid.base.b-incid.total.c11.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c11.w[i]<-(prep.pt.c11.w[i]/52)/(incid.base.w-incid.total.c11.w[i])
  NNT.c11.b[i]<-(prep.pt.c11.b[i]/52)/(incid.base.b-incid.total.c11.b[i])
}


##Condition 12.


incid.total.c12.w<-rep(NA,100)
prep.pt.c12.w<-rep(NA,100)
person.time.c12.w<-rep(NA,100)
person.time.deb.c12.w<-rep(NA,100)
prev.pop.c12.w<-rep(NA,100)
prev.age18.c12.w<-rep(NA,100)
NIA.c12.w<-rep(NA,100)
PIA.c12.w<-rep(NA,100)
NNT.c12.w<-rep(NA,100)

incid.total.c12.b<-rep(NA,100)
prep.pt.c12.b<-rep(NA,100)
person.time.c12.b<-rep(NA,100)
person.time.deb.c12.b<-rep(NA,100)
prev.pop.c12.b<-rep(NA,100)
prev.age18.c12.b<-rep(NA,100)
NIA.c12.b<-rep(NA,100)
PIA.c12.b<-rep(NA,100)
NNT.c12.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c12.w[j]<-sum(s13$epi$incid.W[1041:2080,j])
  prep.pt.c12.w[j]<-sum(s13$epi$prepCurr.W[1041:2080,j])
  person.time.c12.w[j]<-sum(s13$epi$num.W[1041:2080,j]) - sum(s13$epi$i.num.W[1041:2080,j])
  person.time.deb.c12.w[j]<-sum(s13$epi$debuted.W[1041:2080,j]) - sum(s13$epi$i.num.W[1041:2080,j])     
  prev.pop.c12.w[j]<-s13$epi$i.prev.W[2080,j]
  prev.age18.c12.w[j]<-s13$epi$i.prev.age6.W[2080,j]
  
  incid.total.c12.b[j]<-sum(s13$epi$incid.B[1041:2080,j])
  prep.pt.c12.b[j]<-sum(s13$epi$prepCurr.B[1041:2080,j])
  person.time.c12.b[j]<-sum(s13$epi$num.B[1041:2080,j]) - sum(s13$epi$i.num.B[1041:2080,j])
  person.time.deb.c12.b[j]<-sum(s13$epi$debuted.B[1041:2080,j]) - sum(s13$epi$i.num.B[1041:2080,j])   
  prev.pop.c12.b[j]<-s13$epi$i.prev.B[2080,j]
  prev.age18.c12.b[j]<-s13$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c12.w[i]<-((incid.base.w-incid.total.c12.w[i])/person.time.deb.c12.w[i])*52*100000
  NIA.c12.b[i]<-((incid.base.b-incid.total.c12.b[i])/person.time.deb.c12.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c12.w[i]<-(incid.base.w-incid.total.c12.w[i])/incid.base.w
  PIA.c12.b[i]<-(incid.base.b-incid.total.c12.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c12.w[i]<-(prep.pt.c12.w[i]/52)/(incid.base.w-incid.total.c12.w[i])
  NNT.c12.b[i]<-(prep.pt.c12.b[i]/52)/(incid.base.b-incid.total.c12.b[i])
}


##Condition c13.


incid.total.c13.w<-rep(NA,100)
prep.pt.c13.w<-rep(NA,100)
person.time.c13.w<-rep(NA,100)
person.time.deb.c13.w<-rep(NA,100)
prev.pop.c13.w<-rep(NA,100)
prev.age18.c13.w<-rep(NA,100)
NIA.c13.w<-rep(NA,100)
PIA.c13.w<-rep(NA,100)
NNT.c13.w<-rep(NA,100)

incid.total.c13.b<-rep(NA,100)
prep.pt.c13.b<-rep(NA,100)
person.time.c13.b<-rep(NA,100)
person.time.deb.c13.b<-rep(NA,100)
prev.pop.c13.b<-rep(NA,100)
prev.age18.c13.b<-rep(NA,100)
NIA.c13.b<-rep(NA,100)
PIA.c13.b<-rep(NA,100)
NNT.c13.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c13.w[j]<-sum(s14$epi$incid.W[1041:2080,j])
  prep.pt.c13.w[j]<-sum(s14$epi$prepCurr.W[1041:2080,j])
  person.time.c13.w[j]<-sum(s14$epi$num.W[1041:2080,j]) - sum(s14$epi$i.num.W[1041:2080,j])
  person.time.deb.c13.w[j]<-sum(s14$epi$debuted.W[1041:2080,j]) - sum(s14$epi$i.num.W[1041:2080,j])  
  prev.pop.c13.w[j]<-s14$epi$i.prev.W[2080,j]
  prev.age18.c13.w[j]<-s14$epi$i.prev.age6.W[2080,j]
  
  incid.total.c13.b[j]<-sum(s14$epi$incid.B[1041:2080,j])
  prep.pt.c13.b[j]<-sum(s14$epi$prepCurr.B[1041:2080,j])
  person.time.c13.b[j]<-sum(s14$epi$num.B[1041:2080,j]) - sum(s14$epi$i.num.B[1041:2080,j])
  person.time.deb.c13.b[j]<-sum(s14$epi$debuted.B[1041:2080,j]) - sum(s14$epi$i.num.B[1041:2080,j])      
  prev.pop.c13.b[j]<-s14$epi$i.prev.B[2080,j]
  prev.age18.c13.b[j]<-s14$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c13.w[i]<-((incid.base.w-incid.total.c13.w[i])/person.time.deb.c13.w[i])*52*100000
  NIA.c13.b[i]<-((incid.base.b-incid.total.c13.b[i])/person.time.deb.c13.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c13.w[i]<-(incid.base.w-incid.total.c13.w[i])/incid.base.w
  PIA.c13.b[i]<-(incid.base.b-incid.total.c13.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c13.w[i]<-(prep.pt.c13.w[i]/52)/(incid.base.w-incid.total.c13.w[i])
  NNT.c13.b[i]<-(prep.pt.c13.b[i]/52)/(incid.base.b-incid.total.c13.b[i])
}

##Condition c14.


incid.total.c14.w<-rep(NA,100)
prep.pt.c14.w<-rep(NA,100)
person.time.c14.w<-rep(NA,100)
person.time.deb.c14.w<-rep(NA,100)
prev.pop.c14.w<-rep(NA,100)
prev.age18.c14.w<-rep(NA,100)
NIA.c14.w<-rep(NA,100)
PIA.c14.w<-rep(NA,100)
NNT.c14.w<-rep(NA,100)

incid.total.c14.b<-rep(NA,100)
prep.pt.c14.b<-rep(NA,100)
person.time.c14.b<-rep(NA,100)
person.time.deb.c14.b<-rep(NA,100)
prev.pop.c14.b<-rep(NA,100)
prev.age18.c14.b<-rep(NA,100)
NIA.c14.b<-rep(NA,100)
PIA.c14.b<-rep(NA,100)
NNT.c14.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c14.w[j]<-sum(s15$epi$incid.W[1041:2080,j])
  prep.pt.c14.w[j]<-sum(s15$epi$prepCurr.W[1041:2080,j])
  person.time.c14.w[j]<-sum(s15$epi$num.W[1041:2080,j]) - sum(s15$epi$i.num.W[1041:2080,j])
  person.time.deb.c14.w[j]<-sum(s15$epi$debuted.W[1041:2080,j]) - sum(s15$epi$i.num.W[1041:2080,j])      
  prev.pop.c14.w[j]<-s15$epi$i.prev.W[2080,j]
  prev.age18.c14.w[j]<-s15$epi$i.prev.age6.W[2080,j]
  
  incid.total.c14.b[j]<-sum(s15$epi$incid.B[1041:2080,j])
  prep.pt.c14.b[j]<-sum(s15$epi$prepCurr.B[1041:2080,j])
  person.time.c14.b[j]<-sum(s15$epi$num.B[1041:2080,j]) - sum(s15$epi$i.num.B[1041:2080,j])
  person.time.deb.c14.b[j]<-sum(s15$epi$debuted.B[1041:2080,j]) - sum(s15$epi$i.num.B[1041:2080,j])      
  prev.pop.c14.b[j]<-s15$epi$i.prev.B[2080,j]
  prev.age18.c14.b[j]<-s15$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c14.w[i]<-((incid.base.w-incid.total.c14.w[i])/person.time.deb.c14.w[i])*52*100000
  NIA.c14.b[i]<-((incid.base.b-incid.total.c14.b[i])/person.time.deb.c14.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c14.w[i]<-(incid.base.w-incid.total.c14.w[i])/incid.base.w
  PIA.c14.b[i]<-(incid.base.b-incid.total.c14.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c14.w[i]<-(prep.pt.c14.w[i]/52)/(incid.base.w-incid.total.c14.w[i])
  NNT.c14.b[i]<-(prep.pt.c14.b[i]/52)/(incid.base.b-incid.total.c14.b[i])
}


##Condition c15.


incid.total.c15.w<-rep(NA,100)
prep.pt.c15.w<-rep(NA,100)
person.time.c15.w<-rep(NA,100)
person.time.deb.c15.w<-rep(NA,100)
prev.pop.c15.w<-rep(NA,100)
prev.age18.c15.w<-rep(NA,100)
NIA.c15.w<-rep(NA,100)
PIA.c15.w<-rep(NA,100)
NNT.c15.w<-rep(NA,100)

incid.total.c15.b<-rep(NA,100)
prep.pt.c15.b<-rep(NA,100)
person.time.c15.b<-rep(NA,100)
person.time.deb.c15.b<-rep(NA,100)
prev.pop.c15.b<-rep(NA,100)
prev.age18.c15.b<-rep(NA,100)
NIA.c15.b<-rep(NA,100)
PIA.c15.b<-rep(NA,100)
NNT.c15.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c15.w[j]<-sum(s16$epi$incid.W[1041:2080,j])
  prep.pt.c15.w[j]<-sum(s16$epi$prepCurr.W[1041:2080,j])
  person.time.c15.w[j]<-sum(s16$epi$num.W[1041:2080,j]) - sum(s16$epi$i.num.W[1041:2080,j])
  person.time.deb.c15.w[j]<-sum(s16$epi$debuted.W[1041:2080,j]) - sum(s16$epi$i.num.W[1041:2080,j])   
  prev.pop.c15.w[j]<-s16$epi$i.prev.W[2080,j]
  prev.age18.c15.w[j]<-s16$epi$i.prev.age6.W[2080,j]
  
  incid.total.c15.b[j]<-sum(s16$epi$incid.B[1041:2080,j])
  prep.pt.c15.b[j]<-sum(s16$epi$prepCurr.B[1041:2080,j])
  person.time.c15.b[j]<-sum(s16$epi$num.B[1041:2080,j]) - sum(s16$epi$i.num.B[1041:2080,j])
  person.time.deb.c15.b[j]<-sum(s16$epi$debuted.B[1041:2080,j]) - sum(s16$epi$i.num.B[1041:2080,j])    
  prev.pop.c15.b[j]<-s16$epi$i.prev.B[2080,j]
  prev.age18.c15.b[j]<-s16$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c15.w[i]<-((incid.base.w-incid.total.c15.w[i])/person.time.deb.c15.w[i])*52*100000
  NIA.c15.b[i]<-((incid.base.b-incid.total.c15.b[i])/person.time.deb.c15.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c15.w[i]<-(incid.base.w-incid.total.c15.w[i])/incid.base.w
  PIA.c15.b[i]<-(incid.base.b-incid.total.c15.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c15.w[i]<-(prep.pt.c15.w[i]/52)/(incid.base.w-incid.total.c15.w[i])
  NNT.c15.b[i]<-(prep.pt.c15.b[i]/52)/(incid.base.b-incid.total.c15.b[i])
}

##Condition c16.


incid.total.c16.w<-rep(NA,100)
prep.pt.c16.w<-rep(NA,100)
person.time.c16.w<-rep(NA,100)
person.time.deb.c16.w<-rep(NA,100)
prev.pop.c16.w<-rep(NA,100)
prev.age18.c16.w<-rep(NA,100)
NIA.c16.w<-rep(NA,100)
PIA.c16.w<-rep(NA,100)
NNT.c16.w<-rep(NA,100)

incid.total.c16.b<-rep(NA,100)
prep.pt.c16.b<-rep(NA,100)
person.time.c16.b<-rep(NA,100)
person.time.deb.c16.b<-rep(NA,100)
prev.pop.c16.b<-rep(NA,100)
prev.age18.c16.b<-rep(NA,100)
NIA.c16.b<-rep(NA,100)
PIA.c16.b<-rep(NA,100)
NNT.c16.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c16.w[j]<-sum(s17$epi$incid.W[1041:2080,j])
  prep.pt.c16.w[j]<-sum(s17$epi$prepCurr.W[1041:2080,j])
  person.time.c16.w[j]<-sum(s17$epi$num.W[1041:2080,j]) - sum(s17$epi$i.num.W[1041:2080,j])
  person.time.deb.c16.w[j]<-sum(s17$epi$debuted.W[1041:2080,j]) - sum(s17$epi$i.num.W[1041:2080,j])  
  prev.pop.c16.w[j]<-s17$epi$i.prev.W[2080,j]
  prev.age18.c16.w[j]<-s17$epi$i.prev.age6.W[2080,j]
  
  incid.total.c16.b[j]<-sum(s17$epi$incid.B[1041:2080,j])
  prep.pt.c16.b[j]<-sum(s17$epi$prepCurr.B[1041:2080,j])
  person.time.c16.b[j]<-sum(s17$epi$num.B[1041:2080,j]) - sum(s17$epi$i.num.B[1041:2080,j])
  person.time.deb.c16.b[j]<-sum(s17$epi$debuted.B[1041:2080,j]) - sum(s17$epi$i.num.B[1041:2080,j])   
  prev.pop.c16.b[j]<-s17$epi$i.prev.B[2080,j]
  prev.age18.c16.b[j]<-s17$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c16.w[i]<-((incid.base.w-incid.total.c16.w[i])/person.time.deb.c16.w[i])*52*100000
  NIA.c16.b[i]<-((incid.base.b-incid.total.c16.b[i])/person.time.deb.c16.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c16.w[i]<-(incid.base.w-incid.total.c16.w[i])/incid.base.w
  PIA.c16.b[i]<-(incid.base.b-incid.total.c16.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c16.w[i]<-(prep.pt.c16.w[i]/52)/(incid.base.w-incid.total.c16.w[i])
  NNT.c16.b[i]<-(prep.pt.c16.b[i]/52)/(incid.base.b-incid.total.c16.b[i])
}

##Condition c17.


incid.total.c17.w<-rep(NA,100)
prep.pt.c17.w<-rep(NA,100)
person.time.c17.w<-rep(NA,100)
person.time.deb.c17.w<-rep(NA,100)
prev.pop.c17.w<-rep(NA,100)
prev.age18.c17.w<-rep(NA,100)
NIA.c17.w<-rep(NA,100)
PIA.c17.w<-rep(NA,100)
NNT.c17.w<-rep(NA,100)

incid.total.c17.b<-rep(NA,100)
prep.pt.c17.b<-rep(NA,100)
person.time.c17.b<-rep(NA,100)
person.time.deb.c17.b<-rep(NA,100)
prev.pop.c17.b<-rep(NA,100)
prev.age18.c17.b<-rep(NA,100)
NIA.c17.b<-rep(NA,100)
PIA.c17.b<-rep(NA,100)
NNT.c17.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c17.w[j]<-sum(s18$epi$incid.W[1041:2080,j])
  prep.pt.c17.w[j]<-sum(s18$epi$prepCurr.W[1041:2080,j])
  person.time.c17.w[j]<-sum(s18$epi$num.W[1041:2080,j]) - sum(s18$epi$i.num.W[1041:2080,j])
  person.time.deb.c17.w[j]<-sum(s18$epi$debuted.W[1041:2080,j]) - sum(s18$epi$i.num.W[1041:2080,j])     
  prev.pop.c17.w[j]<-s18$epi$i.prev.W[2080,j]
  prev.age18.c17.w[j]<-s18$epi$i.prev.age6.W[2080,j]
  
  incid.total.c17.b[j]<-sum(s18$epi$incid.B[1041:2080,j])
  prep.pt.c17.b[j]<-sum(s18$epi$prepCurr.B[1041:2080,j])
  person.time.c17.b[j]<-sum(s18$epi$num.B[1041:2080,j]) - sum(s18$epi$i.num.B[1041:2080,j])
  person.time.deb.c17.b[j]<-sum(s18$epi$debuted.B[1041:2080,j]) - sum(s18$epi$i.num.B[1041:2080,j])      
  prev.pop.c17.b[j]<-s18$epi$i.prev.B[2080,j]
  prev.age18.c17.b[j]<-s18$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c17.w[i]<-((incid.base.w-incid.total.c17.w[i])/person.time.deb.c17.w[i])*52*100000
  NIA.c17.b[i]<-((incid.base.b-incid.total.c17.b[i])/person.time.deb.c17.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c17.w[i]<-(incid.base.w-incid.total.c17.w[i])/incid.base.w
  PIA.c17.b[i]<-(incid.base.b-incid.total.c17.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c17.w[i]<-(prep.pt.c17.w[i]/52)/(incid.base.w-incid.total.c17.w[i])
  NNT.c17.b[i]<-(prep.pt.c17.b[i]/52)/(incid.base.b-incid.total.c17.b[i])
}

##Condition c18.


incid.total.c18.w<-rep(NA,100)
prep.pt.c18.w<-rep(NA,100)
person.time.c18.w<-rep(NA,100)
person.time.deb.c18.w<-rep(NA,100)
prev.pop.c18.w<-rep(NA,100)
prev.age18.c18.w<-rep(NA,100)
NIA.c18.w<-rep(NA,100)
PIA.c18.w<-rep(NA,100)
NNT.c18.w<-rep(NA,100)

incid.total.c18.b<-rep(NA,100)
prep.pt.c18.b<-rep(NA,100)
person.time.c18.b<-rep(NA,100)
person.time.deb.c18.b<-rep(NA,100)
prev.pop.c18.b<-rep(NA,100)
prev.age18.c18.b<-rep(NA,100)
NIA.c18.b<-rep(NA,100)
PIA.c18.b<-rep(NA,100)
NNT.c18.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c18.w[j]<-sum(s19$epi$incid.W[1041:2080,j])
  prep.pt.c18.w[j]<-sum(s19$epi$prepCurr.W[1041:2080,j])
  person.time.c18.w[j]<-sum(s19$epi$num.W[1041:2080,j]) - sum(s19$epi$i.num.W[1041:2080,j])
  person.time.deb.c18.w[j]<-sum(s19$epi$debuted.W[1041:2080,j]) - sum(s19$epi$i.num.W[1041:2080,j])     
  prev.pop.c18.w[j]<-s19$epi$i.prev.W[2080,j]
  prev.age18.c18.w[j]<-s19$epi$i.prev.age6.W[2080,j]
  
  incid.total.c18.b[j]<-sum(s19$epi$incid.B[1041:2080,j])
  prep.pt.c18.b[j]<-sum(s19$epi$prepCurr.B[1041:2080,j])
  person.time.c18.b[j]<-sum(s19$epi$num.B[1041:2080,j]) - sum(s19$epi$i.num.B[1041:2080,j])
  person.time.deb.c18.b[j]<-sum(s19$epi$debuted.B[1041:2080,j]) - sum(s19$epi$i.num.B[1041:2080,j])    
  prev.pop.c18.b[j]<-s19$epi$i.prev.B[2080,j]
  prev.age18.c18.b[j]<-s19$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c18.w[i]<-((incid.base.w-incid.total.c18.w[i])/person.time.deb.c18.w[i])*52*100000
  NIA.c18.b[i]<-((incid.base.b-incid.total.c18.b[i])/person.time.deb.c18.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c18.w[i]<-(incid.base.w-incid.total.c18.w[i])/incid.base.w
  PIA.c18.b[i]<-(incid.base.b-incid.total.c18.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c18.w[i]<-(prep.pt.c18.w[i]/52)/(incid.base.w-incid.total.c18.w[i])
  NNT.c18.b[i]<-(prep.pt.c18.b[i]/52)/(incid.base.b-incid.total.c18.b[i])
}


##Condition c19.


incid.total.c19.w<-rep(NA,100)
prep.pt.c19.w<-rep(NA,100)
person.time.c19.w<-rep(NA,100)
person.time.deb.c19.w<-rep(NA,100)
prev.pop.c19.w<-rep(NA,100)
prev.age18.c19.w<-rep(NA,100)
NIA.c19.w<-rep(NA,100)
PIA.c19.w<-rep(NA,100)
NNT.c19.w<-rep(NA,100)

incid.total.c19.b<-rep(NA,100)
prep.pt.c19.b<-rep(NA,100)
person.time.c19.b<-rep(NA,100)
person.time.deb.c19.b<-rep(NA,100)
prev.pop.c19.b<-rep(NA,100)
prev.age18.c19.b<-rep(NA,100)
NIA.c19.b<-rep(NA,100)
PIA.c19.b<-rep(NA,100)
NNT.c19.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c19.w[j]<-sum(s20$epi$incid.W[1041:2080,j])
  prep.pt.c19.w[j]<-sum(s20$epi$prepCurr.W[1041:2080,j])
  person.time.c19.w[j]<-sum(s20$epi$num.W[1041:2080,j]) - sum(s20$epi$i.num.W[1041:2080,j])
  person.time.deb.c19.w[j]<-sum(s20$epi$debuted.W[1041:2080,j]) - sum(s20$epi$i.num.W[1041:2080,j])    
  prev.pop.c19.w[j]<-s20$epi$i.prev.W[2080,j]
  prev.age18.c19.w[j]<-s20$epi$i.prev.age6.W[2080,j]
  
  incid.total.c19.b[j]<-sum(s20$epi$incid.B[1041:2080,j])
  prep.pt.c19.b[j]<-sum(s20$epi$prepCurr.B[1041:2080,j])
  person.time.c19.b[j]<-sum(s20$epi$num.B[1041:2080,j]) - sum(s20$epi$i.num.B[1041:2080,j])
  person.time.deb.c19.b[j]<-sum(s20$epi$debuted.B[1041:2080,j]) - sum(s20$epi$i.num.B[1041:2080,j])      
  prev.pop.c19.b[j]<-s20$epi$i.prev.B[2080,j]
  prev.age18.c19.b[j]<-s20$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c19.w[i]<-((incid.base.w-incid.total.c19.w[i])/person.time.deb.c19.w[i])*52*100000
  NIA.c19.b[i]<-((incid.base.b-incid.total.c19.b[i])/person.time.deb.c19.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c19.w[i]<-(incid.base.w-incid.total.c19.w[i])/incid.base.w
  PIA.c19.b[i]<-(incid.base.b-incid.total.c19.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c19.w[i]<-(prep.pt.c19.w[i]/52)/(incid.base.w-incid.total.c19.w[i])
  NNT.c19.b[i]<-(prep.pt.c19.b[i]/52)/(incid.base.b-incid.total.c19.b[i])
}


##Condition c20.


incid.total.c20.w<-rep(NA,100)
prep.pt.c20.w<-rep(NA,100)
person.time.c20.w<-rep(NA,100)
person.time.deb.c20.w<-rep(NA,100)
prev.pop.c20.w<-rep(NA,100)
prev.age18.c20.w<-rep(NA,100)
NIA.c20.w<-rep(NA,100)
PIA.c20.w<-rep(NA,100)
NNT.c20.w<-rep(NA,100)

incid.total.c20.b<-rep(NA,100)
prep.pt.c20.b<-rep(NA,100)
person.time.c20.b<-rep(NA,100)
person.time.deb.c20.b<-rep(NA,100)
prev.pop.c20.b<-rep(NA,100)
prev.age18.c20.b<-rep(NA,100)
NIA.c20.b<-rep(NA,100)
PIA.c20.b<-rep(NA,100)
NNT.c20.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c20.w[j]<-sum(s21$epi$incid.W[1041:2080,j])
  prep.pt.c20.w[j]<-sum(s21$epi$prepCurr.W[1041:2080,j])
  person.time.c20.w[j]<-sum(s21$epi$num.W[1041:2080,j]) - sum(s21$epi$i.num.W[1041:2080,j])
  person.time.deb.c20.w[j]<-sum(s21$epi$debuted.W[1041:2080,j]) - sum(s21$epi$i.num.W[1041:2080,j])     
  prev.pop.c20.w[j]<-s21$epi$i.prev.W[2080,j]
  prev.age18.c20.w[j]<-s21$epi$i.prev.age6.W[2080,j]
  
  incid.total.c20.b[j]<-sum(s21$epi$incid.B[1041:2080,j])
  prep.pt.c20.b[j]<-sum(s21$epi$prepCurr.B[1041:2080,j])
  person.time.c20.b[j]<-sum(s21$epi$num.B[1041:2080,j]) - sum(s21$epi$i.num.B[1041:2080,j])
  person.time.deb.c20.b[j]<-sum(s21$epi$debuted.B[1041:2080,j]) - sum(s21$epi$i.num.B[1041:2080,j])     
  prev.pop.c20.b[j]<-s21$epi$i.prev.B[2080,j]
  prev.age18.c20.b[j]<-s21$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c20.w[i]<-((incid.base.w-incid.total.c20.w[i])/person.time.deb.c20.w[i])*52*100000
  NIA.c20.b[i]<-((incid.base.b-incid.total.c20.b[i])/person.time.deb.c20.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c20.w[i]<-(incid.base.w-incid.total.c20.w[i])/incid.base.w
  PIA.c20.b[i]<-(incid.base.b-incid.total.c20.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c20.w[i]<-(prep.pt.c20.w[i]/52)/(incid.base.w-incid.total.c20.w[i])
  NNT.c20.b[i]<-(prep.pt.c20.b[i]/52)/(incid.base.b-incid.total.c20.b[i])
}


##Condition c21.


incid.total.c21.w<-rep(NA,100)
prep.pt.c21.w<-rep(NA,100)
person.time.c21.w<-rep(NA,100)
person.time.deb.c21.w<-rep(NA,100)
prev.pop.c21.w<-rep(NA,100)
prev.age18.c21.w<-rep(NA,100)
NIA.c21.w<-rep(NA,100)
PIA.c21.w<-rep(NA,100)
NNT.c21.w<-rep(NA,100)

incid.total.c21.b<-rep(NA,100)
prep.pt.c21.b<-rep(NA,100)
person.time.c21.b<-rep(NA,100)
person.time.deb.c21.b<-rep(NA,100)
prev.pop.c21.b<-rep(NA,100)
prev.age18.c21.b<-rep(NA,100)
NIA.c21.b<-rep(NA,100)
PIA.c21.b<-rep(NA,100)
NNT.c21.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c21.w[j]<-sum(s22$epi$incid.W[1041:2080,j])
  prep.pt.c21.w[j]<-sum(s22$epi$prepCurr.W[1041:2080,j])
  person.time.c21.w[j]<-sum(s22$epi$num.W[1041:2080,j]) - sum(s22$epi$i.num.W[1041:2080,j])
  person.time.deb.c21.w[j]<-sum(s22$epi$debuted.W[1041:2080,j]) - sum(s22$epi$i.num.W[1041:2080,j])      
  prev.pop.c21.w[j]<-s22$epi$i.prev.W[2080,j]
  prev.age18.c21.w[j]<-s22$epi$i.prev.age6.W[2080,j]
  
  incid.total.c21.b[j]<-sum(s22$epi$incid.B[1041:2080,j])
  prep.pt.c21.b[j]<-sum(s22$epi$prepCurr.B[1041:2080,j])
  person.time.c21.b[j]<-sum(s22$epi$num.B[1041:2080,j]) - sum(s22$epi$i.num.B[1041:2080,j])
  person.time.deb.c21.b[j]<-sum(s22$epi$debuted.B[1041:2080,j]) - sum(s22$epi$i.num.B[1041:2080,j])    
  prev.pop.c21.b[j]<-s22$epi$i.prev.B[2080,j]
  prev.age18.c21.b[j]<-s22$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c21.w[i]<-((incid.base.w-incid.total.c21.w[i])/person.time.deb.c21.w[i])*52*100000
  NIA.c21.b[i]<-((incid.base.b-incid.total.c21.b[i])/person.time.deb.c21.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c21.w[i]<-(incid.base.w-incid.total.c21.w[i])/incid.base.w
  PIA.c21.b[i]<-(incid.base.b-incid.total.c21.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c21.w[i]<-(prep.pt.c21.w[i]/52)/(incid.base.w-incid.total.c21.w[i])
  NNT.c21.b[i]<-(prep.pt.c21.b[i]/52)/(incid.base.b-incid.total.c21.b[i])
}

##Condition c22.


incid.total.c22.w<-rep(NA,100)
prep.pt.c22.w<-rep(NA,100)
person.time.c22.w<-rep(NA,100)
person.time.deb.c22.w<-rep(NA,100)
prev.pop.c22.w<-rep(NA,100)
prev.age18.c22.w<-rep(NA,100)
NIA.c22.w<-rep(NA,100)
PIA.c22.w<-rep(NA,100)
NNT.c22.w<-rep(NA,100)

incid.total.c22.b<-rep(NA,100)
prep.pt.c22.b<-rep(NA,100)
person.time.c22.b<-rep(NA,100)
person.time.deb.c22.b<-rep(NA,100)
prev.pop.c22.b<-rep(NA,100)
prev.age18.c22.b<-rep(NA,100)
NIA.c22.b<-rep(NA,100)
PIA.c22.b<-rep(NA,100)
NNT.c22.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c22.w[j]<-sum(s23$epi$incid.W[1041:2080,j])
  prep.pt.c22.w[j]<-sum(s23$epi$prepCurr.W[1041:2080,j])
  person.time.c22.w[j]<-sum(s23$epi$num.W[1041:2080,j]) - sum(s23$epi$i.num.W[1041:2080,j])
  person.time.deb.c22.w[j]<-sum(s23$epi$debuted.W[1041:2080,j]) - sum(s23$epi$i.num.W[1041:2080,j])     
  prev.pop.c22.w[j]<-s23$epi$i.prev.W[2080,j]
  prev.age18.c22.w[j]<-s23$epi$i.prev.age6.W[2080,j]
  
  incid.total.c22.b[j]<-sum(s23$epi$incid.B[1041:2080,j])
  prep.pt.c22.b[j]<-sum(s23$epi$prepCurr.B[1041:2080,j])
  person.time.c22.b[j]<-sum(s23$epi$num.B[1041:2080,j]) - sum(s23$epi$i.num.B[1041:2080,j])
  person.time.deb.c22.b[j]<-sum(s23$epi$debuted.B[1041:2080,j]) - sum(s23$epi$i.num.B[1041:2080,j])      
  prev.pop.c22.b[j]<-s23$epi$i.prev.B[2080,j]
  prev.age18.c22.b[j]<-s23$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c22.w[i]<-((incid.base.w-incid.total.c22.w[i])/person.time.deb.c22.w[i])*52*100000
  NIA.c22.b[i]<-((incid.base.b-incid.total.c22.b[i])/person.time.deb.c22.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c22.w[i]<-(incid.base.w-incid.total.c22.w[i])/incid.base.w
  PIA.c22.b[i]<-(incid.base.b-incid.total.c22.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c22.w[i]<-(prep.pt.c22.w[i]/52)/(incid.base.w-incid.total.c22.w[i])
  NNT.c22.b[i]<-(prep.pt.c22.b[i]/52)/(incid.base.b-incid.total.c22.b[i])
}

##Condition c23.


incid.total.c23.w<-rep(NA,100)
prep.pt.c23.w<-rep(NA,100)
person.time.c23.w<-rep(NA,100)
person.time.deb.c23.w<-rep(NA,100)
prev.pop.c23.w<-rep(NA,100)
prev.age18.c23.w<-rep(NA,100)
NIA.c23.w<-rep(NA,100)
PIA.c23.w<-rep(NA,100)
NNT.c23.w<-rep(NA,100)

incid.total.c23.b<-rep(NA,100)
prep.pt.c23.b<-rep(NA,100)
person.time.c23.b<-rep(NA,100)
person.time.deb.c23.b<-rep(NA,100)
prev.pop.c23.b<-rep(NA,100)
prev.age18.c23.b<-rep(NA,100)
NIA.c23.b<-rep(NA,100)
PIA.c23.b<-rep(NA,100)
NNT.c23.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c23.w[j]<-sum(s24$epi$incid.W[1041:2080,j])
  prep.pt.c23.w[j]<-sum(s24$epi$prepCurr.W[1041:2080,j])
  person.time.c23.w[j]<-sum(s24$epi$num.W[1041:2080,j]) - sum(s24$epi$i.num.W[1041:2080,j])
  person.time.deb.c23.w[j]<-sum(s24$epi$debuted.W[1041:2080,j]) - sum(s24$epi$i.num.W[1041:2080,j])     
  prev.pop.c23.w[j]<-s24$epi$i.prev.W[2080,j]
  prev.age18.c23.w[j]<-s24$epi$i.prev.age6.W[2080,j]
  
  incid.total.c23.b[j]<-sum(s24$epi$incid.B[1041:2080,j])
  prep.pt.c23.b[j]<-sum(s24$epi$prepCurr.B[1041:2080,j])
  person.time.c23.b[j]<-sum(s24$epi$num.B[1041:2080,j]) - sum(s24$epi$i.num.B[1041:2080,j])
  person.time.deb.c23.b[j]<-sum(s24$epi$debuted.B[1041:2080,j]) - sum(s24$epi$i.num.B[1041:2080,j])   
  prev.pop.c23.b[j]<-s24$epi$i.prev.B[2080,j]
  prev.age18.c23.b[j]<-s24$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c23.w[i]<-((incid.base.w-incid.total.c23.w[i])/person.time.deb.c23.w[i])*52*100000
  NIA.c23.b[i]<-((incid.base.b-incid.total.c23.b[i])/person.time.deb.c23.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c23.w[i]<-(incid.base.w-incid.total.c23.w[i])/incid.base.w
  PIA.c23.b[i]<-(incid.base.b-incid.total.c23.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c23.w[i]<-(prep.pt.c23.w[i]/52)/(incid.base.w-incid.total.c23.w[i])
  NNT.c23.b[i]<-(prep.pt.c23.b[i]/52)/(incid.base.b-incid.total.c23.b[i])
}

##Condition c24.


incid.total.c24.w<-rep(NA,100)
prep.pt.c24.w<-rep(NA,100)
person.time.c24.w<-rep(NA,100)
person.time.deb.c24.w<-rep(NA,100)
prev.pop.c24.w<-rep(NA,100)
prev.age18.c24.w<-rep(NA,100)
NIA.c24.w<-rep(NA,100)
PIA.c24.w<-rep(NA,100)
NNT.c24.w<-rep(NA,100)

incid.total.c24.b<-rep(NA,100)
prep.pt.c24.b<-rep(NA,100)
person.time.c24.b<-rep(NA,100)
person.time.deb.c24.b<-rep(NA,100)
prev.pop.c24.b<-rep(NA,100)
prev.age18.c24.b<-rep(NA,100)
NIA.c24.b<-rep(NA,100)
PIA.c24.b<-rep(NA,100)
NNT.c24.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c24.w[j]<-sum(s25$epi$incid.W[1041:2080,j])
  prep.pt.c24.w[j]<-sum(s25$epi$prepCurr.W[1041:2080,j])
  person.time.c24.w[j]<-sum(s25$epi$num.W[1041:2080,j]) - sum(s25$epi$i.num.W[1041:2080,j])
  person.time.deb.c24.w[j]<-sum(s25$epi$debuted.W[1041:2080,j]) - sum(s25$epi$i.num.W[1041:2080,j])     
  prev.pop.c24.w[j]<-s25$epi$i.prev.W[2080,j]
  prev.age18.c24.w[j]<-s25$epi$i.prev.age6.W[2080,j]
  
  incid.total.c24.b[j]<-sum(s25$epi$incid.B[1041:2080,j])
  prep.pt.c24.b[j]<-sum(s25$epi$prepCurr.B[1041:2080,j])
  person.time.c24.b[j]<-sum(s25$epi$num.B[1041:2080,j]) - sum(s25$epi$i.num.B[1041:2080,j])
  person.time.deb.c24.b[j]<-sum(s25$epi$debuted.B[1041:2080,j]) - sum(s25$epi$i.num.B[1041:2080,j])   
  prev.pop.c24.b[j]<-s25$epi$i.prev.B[2080,j]
  prev.age18.c24.b[j]<-s25$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c24.w[i]<-((incid.base.w-incid.total.c24.w[i])/person.time.deb.c24.w[i])*52*100000
  NIA.c24.b[i]<-((incid.base.b-incid.total.c24.b[i])/person.time.deb.c24.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c24.w[i]<-(incid.base.w-incid.total.c24.w[i])/incid.base.w
  PIA.c24.b[i]<-(incid.base.b-incid.total.c24.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c24.w[i]<-(prep.pt.c24.w[i]/52)/(incid.base.w-incid.total.c24.w[i])
  NNT.c24.b[i]<-(prep.pt.c24.b[i]/52)/(incid.base.b-incid.total.c24.b[i])
}

##Condition c25.


incid.total.c25.w<-rep(NA,100)
prep.pt.c25.w<-rep(NA,100)
person.time.c25.w<-rep(NA,100)
person.time.deb.c25.w<-rep(NA,100)
prev.pop.c25.w<-rep(NA,100)
prev.age18.c25.w<-rep(NA,100)
NIA.c25.w<-rep(NA,100)
PIA.c25.w<-rep(NA,100)
NNT.c25.w<-rep(NA,100)

incid.total.c25.b<-rep(NA,100)
prep.pt.c25.b<-rep(NA,100)
person.time.c25.b<-rep(NA,100)
person.time.deb.c25.b<-rep(NA,100)
prev.pop.c25.b<-rep(NA,100)
prev.age18.c25.b<-rep(NA,100)
NIA.c25.b<-rep(NA,100)
PIA.c25.b<-rep(NA,100)
NNT.c25.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c25.w[j]<-sum(s26$epi$incid.W[1041:2080,j])
  prep.pt.c25.w[j]<-sum(s26$epi$prepCurr.W[1041:2080,j])
  person.time.c25.w[j]<-sum(s26$epi$num.W[1041:2080,j]) - sum(s26$epi$i.num.W[1041:2080,j])
  person.time.deb.c25.w[j]<-sum(s26$epi$debuted.W[1041:2080,j]) - sum(s26$epi$i.num.W[1041:2080,j])    
  prev.pop.c25.w[j]<-s26$epi$i.prev.W[2080,j]
  prev.age18.c25.w[j]<-s26$epi$i.prev.age6.W[2080,j]
  
  incid.total.c25.b[j]<-sum(s26$epi$incid.B[1041:2080,j])
  prep.pt.c25.b[j]<-sum(s26$epi$prepCurr.B[1041:2080,j])
  person.time.c25.b[j]<-sum(s26$epi$num.B[1041:2080,j]) - sum(s26$epi$i.num.B[1041:2080,j])
  person.time.deb.c25.b[j]<-sum(s26$epi$debuted.B[1041:2080,j]) - sum(s26$epi$i.num.B[1041:2080,j])   
  prev.pop.c25.b[j]<-s26$epi$i.prev.B[2080,j]
  prev.age18.c25.b[j]<-s26$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c25.w[i]<-((incid.base.w-incid.total.c25.w[i])/person.time.deb.c25.w[i])*52*100000
  NIA.c25.b[i]<-((incid.base.b-incid.total.c25.b[i])/person.time.deb.c25.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c25.w[i]<-(incid.base.w-incid.total.c25.w[i])/incid.base.w
  PIA.c25.b[i]<-(incid.base.b-incid.total.c25.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c25.w[i]<-(prep.pt.c25.w[i]/52)/(incid.base.w-incid.total.c25.w[i])
  NNT.c25.b[i]<-(prep.pt.c25.b[i]/52)/(incid.base.b-incid.total.c25.b[i])
}

##Condition c26.


incid.total.c26.w<-rep(NA,100)
prep.pt.c26.w<-rep(NA,100)
person.time.c26.w<-rep(NA,100)
person.time.deb.c26.w<-rep(NA,100)
prev.pop.c26.w<-rep(NA,100)
prev.age18.c26.w<-rep(NA,100)
NIA.c26.w<-rep(NA,100)
PIA.c26.w<-rep(NA,100)
NNT.c26.w<-rep(NA,100)

incid.total.c26.b<-rep(NA,100)
prep.pt.c26.b<-rep(NA,100)
person.time.c26.b<-rep(NA,100)
person.time.deb.c26.b<-rep(NA,100)
prev.pop.c26.b<-rep(NA,100)
prev.age18.c26.b<-rep(NA,100)
NIA.c26.b<-rep(NA,100)
PIA.c26.b<-rep(NA,100)
NNT.c26.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c26.w[j]<-sum(s27$epi$incid.W[1041:2080,j])
  prep.pt.c26.w[j]<-sum(s27$epi$prepCurr.W[1041:2080,j])
  person.time.c26.w[j]<-sum(s27$epi$num.W[1041:2080,j]) - sum(s27$epi$i.num.W[1041:2080,j])
  person.time.deb.c26.w[j]<-sum(s27$epi$debuted.W[1041:2080,j]) - sum(s27$epi$i.num.W[1041:2080,j])   
  prev.pop.c26.w[j]<-s27$epi$i.prev.W[2080,j]
  prev.age18.c26.w[j]<-s27$epi$i.prev.age6.W[2080,j]
  
  incid.total.c26.b[j]<-sum(s27$epi$incid.B[1041:2080,j])
  prep.pt.c26.b[j]<-sum(s27$epi$prepCurr.B[1041:2080,j])
  person.time.c26.b[j]<-sum(s27$epi$num.B[1041:2080,j]) - sum(s27$epi$i.num.B[1041:2080,j])
  person.time.deb.c26.b[j]<-sum(s27$epi$debuted.B[1041:2080,j]) - sum(s27$epi$i.num.B[1041:2080,j])      
  prev.pop.c26.b[j]<-s27$epi$i.prev.B[2080,j]
  prev.age18.c26.b[j]<-s27$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c26.w[i]<-((incid.base.w-incid.total.c26.w[i])/person.time.deb.c26.w[i])*52*100000
  NIA.c26.b[i]<-((incid.base.b-incid.total.c26.b[i])/person.time.deb.c26.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c26.w[i]<-(incid.base.w-incid.total.c26.w[i])/incid.base.w
  PIA.c26.b[i]<-(incid.base.b-incid.total.c26.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c26.w[i]<-(prep.pt.c26.w[i]/52)/(incid.base.w-incid.total.c26.w[i])
  NNT.c26.b[i]<-(prep.pt.c26.b[i]/52)/(incid.base.b-incid.total.c26.b[i])
}


##Condition c27.


incid.total.c27.w<-rep(NA,100)
prep.pt.c27.w<-rep(NA,100)
person.time.c27.w<-rep(NA,100)
person.time.deb.c27.w<-rep(NA,100)
prev.pop.c27.w<-rep(NA,100)
prev.age18.c27.w<-rep(NA,100)
NIA.c27.w<-rep(NA,100)
PIA.c27.w<-rep(NA,100)
NNT.c27.w<-rep(NA,100)

incid.total.c27.b<-rep(NA,100)
prep.pt.c27.b<-rep(NA,100)
person.time.c27.b<-rep(NA,100)
person.time.deb.c27.b<-rep(NA,100)
prev.pop.c27.b<-rep(NA,100)
prev.age18.c27.b<-rep(NA,100)
NIA.c27.b<-rep(NA,100)
PIA.c27.b<-rep(NA,100)
NNT.c27.b<-rep(NA,100)

for (j in 1:100){  
  incid.total.c27.w[j]<-sum(s28$epi$incid.W[1041:2080,j])
  prep.pt.c27.w[j]<-sum(s28$epi$prepCurr.W[1041:2080,j])
  person.time.c27.w[j]<-sum(s28$epi$num.W[1041:2080,j]) - sum(s28$epi$i.num.W[1041:2080,j])
  person.time.deb.c27.w[j]<-sum(s28$epi$debuted.W[1041:2080,j]) - sum(s28$epi$i.num.W[1041:2080,j])   
  prev.pop.c27.w[j]<-s28$epi$i.prev.W[2080,j]
  prev.age18.c27.w[j]<-s28$epi$i.prev.age6.W[2080,j]
  
  incid.total.c27.b[j]<-sum(s28$epi$incid.B[1041:2080,j])
  prep.pt.c27.b[j]<-sum(s28$epi$prepCurr.B[1041:2080,j])
  person.time.c27.b[j]<-sum(s28$epi$num.B[1041:2080,j]) - sum(s28$epi$i.num.B[1041:2080,j])
  person.time.deb.c27.b[j]<-sum(s28$epi$debuted.B[1041:2080,j]) - sum(s28$epi$i.num.B[1041:2080,j])      
  prev.pop.c27.b[j]<-s28$epi$i.prev.B[2080,j]
  prev.age18.c27.b[j]<-s28$epi$i.prev.age6.B[2080,j]
}


#Number of infections averted per 100K person years at risk  (define at risk).
#Percent of infection averted.
#NNt persontime on prep / (1/NIA)


#Number of infections averted per 100K person years at risk (in population and in the sexual marketplace).
for (i in 1:100){
  NIA.c27.w[i]<-((incid.base.w-incid.total.c27.w[i])/person.time.deb.c27.w[i])*52*100000
  NIA.c27.b[i]<-((incid.base.b-incid.total.c27.b[i])/person.time.deb.c27.b[i])*52*100000
  
  #Percent of infection averted.
  
  PIA.c27.w[i]<-(incid.base.w-incid.total.c27.w[i])/incid.base.w
  PIA.c27.b[i]<-(incid.base.b-incid.total.c27.b[i])/incid.base.b
  
  #NNT prep.pt/(NIA).
  
  NNT.c27.w[i]<-(prep.pt.c27.w[i]/52)/(incid.base.w-incid.total.c27.w[i])
  NNT.c27.b[i]<-(prep.pt.c27.b[i]/52)/(incid.base.b-incid.total.c27.b[i])
}


###################################################################################################################.
##Figure 2 Boxplot of % of infections averted number need to treat to prevent 1 new infection.
##  Boxes are interquartile rannge and whiskers 95% credibility intervals for 100 simulations of each scenario.
##  Scenarios are across the range of coverage with the same adherence.
##Eligibility is AI + 6 months.

PIA.all.1.w<-cbind(PIA.c1.w,PIA.c2.w,PIA.c3.w,PIA.c4.w,PIA.c5.w,PIA.c6.w)
PIA.all.1.b<-cbind(PIA.c1.b,PIA.c2.b,PIA.c3.b,PIA.c4.b,PIA.c5.b,PIA.c6.b)

PIA.all.2.w<-cbind(PIA.c7.w,PIA.c8.w,PIA.c9.w,PIA.c10.w,PIA.c11.w,PIA.c12.w)
PIA.all.2.b<-cbind(PIA.c7.b,PIA.c8.b,PIA.c9.b,PIA.c10.b,PIA.c11.b,PIA.c12.b)

NIA.all.1.w<-cbind(NIA.c1.w,NIA.c2.w,NIA.c3.w,NIA.c4.w,NIA.c5.w,NIA.c6.w)
NIA.all.1.b<-cbind(NIA.c1.b,NIA.c2.b,NIA.c3.b,NIA.c4.b,NIA.c5.b,NIA.c6.b)

NIA.all.2.w<-cbind(NIA.c7.w,NIA.c8.w,NIA.c9.w,NIA.c10.w,NIA.c11.w,NIA.c12.w)
NIA.all.2.b<-cbind(NIA.c7.b,NIA.c8.b,NIA.c9.b,NIA.c10.b,NIA.c11.b,NIA.c12.b)

NNT.all.1.w<-cbind(NNT.c1.w,NNT.c2.w,NNT.c3.w,NNT.c4.w,NNT.c5.w,NNT.c6.w)
NNT.all.1.b<-cbind(NNT.c1.b,NNT.c2.b,NNT.c3.b,NNT.c4.b,NNT.c5.b,NNT.c6.b)

NNT.all.2.w<-cbind(NNT.c7.w,NNT.c8.w,NNT.c9.w,NNT.c10.w,NNT.c11.w,NNT.c12.w)
NNT.all.2.b<-cbind(NNT.c7.b,NNT.c8.b,NNT.c9.b,NNT.c10.b,NNT.c11.b,NNT.c12.b)

library(wesanderson)
pal <- wes_palette("Zissou")[c(1, 9)]

 #pdf(file = "PIA Elig at AI + 6 months", height = 6, width = 12, pointsize = 16)
tiff(filename = "Fig2a AI_cov.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1,2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)

# Left Panel: PIA.B
PIA.all.1.b<-PIA.all.1.b*100
boxplot(PIA.all.1.b, outline = FALSE, medlwd = 1.1, ylim = c(0,60),
        col = c(rep(pal[1], 6), rep(pal[2], 3)),csi=.5,
        names=c("6.5","12.9","19.4","25.8","32.3","38.7"),las=2,
        #main = "PIA for Black ASMM with increasing coverage",
        ylab = "Percent Infections Averted (Black)",
        xlab = "Coverage among Black ASMM")

# Right Panel: PIA.W
PIA.all.1.w<-PIA.all.1.w*100
boxplot(PIA.all.1.w, outline = FALSE, medlwd = 1.1,  ylim = c(0,60),
        col = c(rep(pal[1], 6), rep(pal[2], 3)),csi=.5,
        names=c("13.6","27.1","40.7","54.2","67.8","81.3"),las=2,
        #main = "PIA for White ASMM with increasing coverage",
        ylab = "Percent Infections Averted (White)",
        xlab = "Coverage among White ASMM")


dev.off()

#pdf(file = "PIA Elig at AI + 6 months", height = 6, width = 12, pointsize = 16)
tiff(filename = "Fig2b AI_cov.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1,2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)

# Left Panel: NIA.B
boxplot(NIA.all.1.b, outline = FALSE, medlwd = 1.1, ylim = c(0,1500),
        col = c(rep(pal[1], 6), rep(pal[2], 3)),csi=.5,
        names=c("6.5","12.9","19.4","25.8","32.3","38.7"),las=2,
        #main = "NIA for Black ASMM with increasing coverage",
        ylab = "Number of Infections Averted (Black)",
        xlab = "Coverage among Black ASMM")

# Right Panel: NIA.W
boxplot(NIA.all.1.w, outline = FALSE, medlwd = 1.1,  ylim = c(0,1500),
        col = c(rep(pal[1], 6), rep(pal[2], 3)),csi=.5,
        names=c("13.6","27.1","40.7","54.2","67.8","81.3"),las=2,
        #main = "NIA for White ASMM with increasing coverage",
        ylab = "Number of Infections Averted (White)",
        xlab = "Coverage among White ASMM")


dev.off()

tiff(filename = "Fig2c AI_cov.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)

# Left Panel: NNT.B
boxplot(NNT.all.1.b, outline = FALSE, medlwd = 1.1, ylim = c(0,100),
        col = c(rep(pal[1], 7), rep(pal[2], 3)),
        names=c("6.5","12.6","19.4","25.8","32.3","38.7"),las=2,
        #main = "NNT for Black ASMM with increasing coverage",
        ylab = "Number Needed to Treat (Black)",
        xlab = "Coverage among Black ASMM")

# Right Panel: NNT.W
boxplot(NNT.all.1.w, outline = FALSE, medlwd = 1.1,  ylim = c(0,1000),
        col = c(rep(pal[1], 7), rep(pal[2], 3)),csi=.5,
        names=c("13.6","27.1","40.7","54.2","67.8","81.3"),las=2,
        #main = "NNT for White ASMM with increasing coverage",
        ylab = "Number Needed to Treat (White)",
        xlab = "Coverage among White ASMM")

dev.off()



##SUPPLEMENT Figure 1  Boxplot of % of infections averted number need to treat to prevent 1 new infection.
##  Boxes are interquartile rannge and whiskers 95% credibility intervals for 100 simulations of each scenario.
##  Scenarios are across the range of coverage with the same adherence.
##Eligibility is 10 + CAI + 6 months.

tiff(filename = "SUP Fig1a_cov_10CAI.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)


# Left Panel: PIA.B
PIA.all.2.b<-PIA.all.2.b*100
boxplot(PIA.all.2.b, outline = FALSE, medlwd = 1.1, ylim = c(0,60),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("6.5","12.6","19.4","25.8","32.3","38.7"),
        #main = "PIA for Black ASMM with increasing coverage",
        ylab = "Percent Infections Averted (Black)",
        xlab = "Coverage among Black ASMM")

# Right Panel: PIA.W
PIA.all.2.w<-PIA.all.2.w*100
boxplot(PIA.all.2.w, outline = FALSE, medlwd = 1.1,  ylim = c(0,60),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("13.6","27.1","40.7","54.2","67.8","81.3"),
        #main = "PIA for White ASMM with increasing coverage",
        ylab = "Percent Infections Averted (White)",
        xlab = "Coverage among White ASMM")


dev.off()


tiff(filename = "SUP Fig1b_cov_10CAI.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)


# Left Panel: NIA.B
boxplot(NIA.all.2.b, outline = FALSE, medlwd = 1.1, ylim = c(0,1500),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("6.5","12.6","19.4","25.8","32.3","38.7"),
        #main = "NIA for Black ASMM with increasing coverage",
        ylab = "Number of Infections Averted (Black)",
        xlab = "Coverage among Black ASMM")

# Right Panel: NIA.W
boxplot(NIA.all.2.w, outline = FALSE, medlwd = 1.1,  ylim = c(0,1500),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("13.6","27.1","40.7","54.2","67.8","81.3"),
        #main = "NIA for White ASMM with increasing coverage",
        ylab = "Number of Infections Averted (White)",
        xlab = "Coverage among White ASMM")



dev.off()



tiff(filename = "SUP Fig1c_cov_10CAI.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)


# Left Panel: NNT.B
boxplot(NNT.all.2.b, outline = FALSE, medlwd = 1.1, ylim = c(0,100),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("6.5","12.6","19.4","25.8","32.3","38.7"),
        #main = "NNT for Black ASMM with increasing coverage",
        ylab = "Number Needed to Treat (Black)",
        xlab = "Coverage among Black ASMM")

# Right Panel: NNT.W
boxplot(NNT.all.2.w, outline = FALSE, medlwd = 1.1,  ylim = c(0,1000),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("13.6","27.1","40.7","54.2","67.8","81.3"),
        #main = "NNT for White ASMM with increasing coverage",
        ylab = "Number Needed to Treat (White)",
        xlab = "Coverage among White ASMM")

dev.off()



###################################################################################
##################################################################################


##Figure 3 Boxplot of % of infections averted number need to treat to prevent 1 new infection.
##  Boxes are interquartile rannge and whiskers 95% credibility intervals for 100 simulations of each scenario.
##  Scenarios are across the range of coverage with race specific adherence.
##ELIGIBILITY AI + 6 months.

PIA.all.1.w<-cbind(PIA.c13.w,PIA.c14.w,PIA.c15.w,PIA.c16.w,PIA.c17.w,PIA.c18.w)
PIA.all.1.b<-cbind(PIA.c13.b,PIA.c14.b,PIA.c15.b,PIA.c16.b,PIA.c17.b,PIA.c18.b)

PIA.all.2.w<-cbind(PIA.c19.w,PIA.c20.w,PIA.c21.w,PIA.c22.w,PIA.c23.w,PIA.c24.w)
PIA.all.2.b<-cbind(PIA.c19.b,PIA.c20.b,PIA.c21.b,PIA.c22.b,PIA.c23.b,PIA.c24.b)

NIA.all.1.w<-cbind(NIA.c13.w,NIA.c14.w,NIA.c15.w,NIA.c16.w,NIA.c17.w,NIA.c18.w)
NIA.all.1.b<-cbind(NIA.c13.b,NIA.c14.b,NIA.c15.b,NIA.c16.b,NIA.c17.b,NIA.c18.b)

NIA.all.2.w<-cbind(NIA.c19.w,NIA.c20.w,NIA.c21.w,NIA.c22.w,NIA.c23.w,NIA.c24.w)
NIA.all.2.b<-cbind(NIA.c19.b,NIA.c20.b,NIA.c21.b,NIA.c22.b,NIA.c23.b,NIA.c24.b)

NNT.all.1.w<-cbind(NNT.c13.w,NNT.c14.w,NNT.c15.w,NNT.c16.w,NNT.c17.w,NNT.c18.w)
NNT.all.1.b<-cbind(NNT.c13.b,NNT.c14.b,NNT.c15.b,NNT.c16.b,NNT.c17.b,NNT.c18.b)

NNT.all.2.w<-cbind(NNT.c19.w,NNT.c20.w,NNT.c21.w,NNT.c22.w,NNT.c23.w,NNT.c24.w)
NNT.all.2.b<-cbind(NNT.c19.b,NNT.c20.b,NNT.c21.b,NNT.c22.b,NNT.c23.b,NNT.c24.b)

library(wesanderson)
pal <- wes_palette("Zissou")[c(1, 9)]

#pdf(file = "PIA Elig at AI + 6 months with race specific adherence", height = 6, width = 12, pointsize = 16)
tiff(filename = "Fig3a AI_cov_adh.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)


# Left Panel: PIA.B
PIA.all.1.b<-PIA.all.1.b*100
boxplot(PIA.all.1.b, outline = FALSE, medlwd = 1.1, ylim = c(0,60),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("6.5","12.6","19.4","25.8","32.3","38.7"),
        #main = "PIA for Black ASMM with increasing coverage and race specific adherence (.32,.19,.19,.31)",
        ylab = "Percent Infections Averted (Black)",
        xlab = "Coverage among Black ASMM")

# Right Panel: PIA.W
PIA.all.1.w<-PIA.all.1.w*100
boxplot(PIA.all.1.w, outline = FALSE, medlwd = 1.1,  ylim = c(0,60),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("13.6","27.1","40.7","54.2","67.8","81.3"),
        #main = "PIA for White ASMM with increasing coverage and race specific adherence (.52,.19,.19,.11)",
        ylab = "Percent Infections Averted (White)",
        xlab = "Coverage among White ASMM")

dev.off()

#pdf(file = "PIA Elig at AI + 6 months with race specific adherence", height = 6, width = 12, pointsize = 16)
tiff(filename = "Fig3b AI_cov_adh.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)


# Left Panel: NIA.B
boxplot(NIA.all.1.b, outline = FALSE, medlwd = 1.1, ylim = c(0,1500),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("6.5","12.6","19.4","25.8","32.3","38.7"),
        #main = "NIA for Black ASMM with increasing coverage and race specific adherence (.32,.19,.19,.31)",
        ylab = "Number of Infections Averted (Black)",
        xlab = "Coverage among Black ASMM")

# Right Panel: NIA.W
boxplot(NIA.all.1.w, outline = FALSE, medlwd = 1.1,  ylim = c(0,1500),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("13.6","27.1","40.7","54.2","67.8","81.3"),
        #main = "NIA for White ASMM with increasing coverage and race specific adherence (.52,.19,.19,.11)",
        ylab = "Number of Infections Averted (White)",
        xlab = "Coverage among White ASMM")

dev.off()


tiff(filename = "Fig3c AI_cov_adh.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)


# Left Panel: NNT.B
boxplot(NNT.all.1.b, outline = FALSE, medlwd = 1.1, ylim = c(0,100),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("6.5","12.6","19.4","25.8","32.3","38.7"),
        #main = "NNT for Black ASMM with increasing coverage and race specific adherence (.32,.19,.19,.31)",
        ylab = "Number Needed to Treat (Black)",
        xlab = "Coverage among Black ASMM")

# Right Panel: NNT.W
boxplot(NNT.all.1.w, outline = FALSE, medlwd = 1.1,  ylim = c(0,1000),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("13.6","27.1","40.7","54.2","67.8","81.3"),
        #main = "NNT for White ASMM with increasing coverage and race specific adherence (.52,.19,.19,.11)",
        ylab = "Number Needed to Treat (White)",
        xlab = "Coverage among White ASMM")

dev.off()

################################################################################################
################################################################################################
##SUPPLEMENT Figure 2 Boxplot of % of infections averted number need to treat to prevent 1 new infection.
##  Boxes are interquartile rannge and whiskers 95% credibility intervals for 100 simulations of each scenario.
##  Scenarios are across the range of coverage with race specific adherence.
##ELIGIBILITY 10+ CAI + 6 months.


tiff(filename = "SUP Fig2a_cov_adh_10CAI.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)


# Left Panel: PIA.B
PIA.all.2.b<-PIA.all.2.b*100
boxplot(PIA.all.2.b, outline = FALSE, medlwd = 1.1, ylim = c(0,60),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("6.5","12.6","19.4","25.8","32.3","38.7"),
        #main = "PIA for Black ASMM with increasing coverage and race specific adherence (.32,.19,.19,.31))",
        ylab = "Percent Infections Averted (Black)",
        xlab = "Coverage among Black ASMM")

# Right Panel: PIA.W
PIA.all.2.w<-PIA.all.2.w*100
boxplot(PIA.all.2.w, outline = FALSE, medlwd = 1.1,  ylim = c(0,60),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("13.6","27.1","40.7","54.2","67.8","81.3"),
        #main = "PIA for White ASMM with increasing coverage and race specific adherence (.52,.19,.19,.11))",
        ylab = "Percent Infections Averted (White)",
        xlab = "Coverage among White ASMM")

dev.off()

tiff(filename = "SUP Fig2b_cov_adh_10CAI.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)


# Left Panel: NIA.B
boxplot(NIA.all.2.b, outline = FALSE, medlwd = 1.1, ylim = c(0,1500),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("6.5","12.6","19.4","25.8","32.3","38.7"),
        #main = "NIA for Black ASMM with increasing coverage and race specific adherence (.32,.19,.19,.31))",
        ylab = "Number of Infections Averted (Black)",
        xlab = "Coverage among Black ASMM")

# Right Panel: NIA.W
boxplot(NIA.all.2.w, outline = FALSE, medlwd = 1.1,  ylim = c(0,1500),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("13.6","27.1","40.7","54.2","67.8","81.3"),
        #main = "NIA for White ASMM with increasing coverage and race specific adherence (.52,.19,.19,.11))",
        ylab = "Number of Infections Averted (White)",
        xlab = "Coverage among White ASMM")

dev.off()


tiff(filename = "SUP Fig2c_cov_adh_10CAI.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)


# Left Panel: NNT.B
boxplot(NNT.all.2.b, outline = FALSE, medlwd = 1.1, ylim = c(0,100),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("6.5","12.6","19.4","25.8","32.3","38.7"),
        #main = "NNT for Black ASMM with increasing coverage and race specific adherence (.32,.19,.19,.31)",
        ylab = "Number Needed to Treat (Black)",
        xlab = "Coverage among Black ASMM")

# Right Panel: NNT.W
boxplot(NNT.all.2.w, outline = FALSE, medlwd = 1.1,  ylim = c(0,1000),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("13.6","27.1","40.7","54.2","67.8","81.3"),
        #main = "NNT for White ASMM with increasing coverage and race specific adherence (.52,.19,.19,.11)",
        ylab = "Number Needed to Treat (White)",
        xlab = "Coverage among White ASMM")

dev.off()



################################################################################################
################################################################################################
##Figure 4 Boxplot of % of infections averted number need to treat to prevent 1 new infection.
##  Boxes are interquartile rannge and whiskers 95% credibility intervals for 100 simulations of each scenario.
##  Scenarios are assuming both races = white coverage at net 40 (54.2) 
## and white adherence (.52,.19,.19,.11).
##ELIGIBILITY AI + 6 months.

PIA.all<-cbind(PIA.c25.w,PIA.c25.b)
NNT.all<-cbind(NNT.c25.w,NNT.c25.b)

tiff(filename = "Fig4 best case AI.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1, 2), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)


# Left Panel: NNT.W
boxplot(NNT.all, outline = FALSE, medlwd = 1.1,  ylim = c(0,250),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("White","Black"),
        #main = "NNT with 54.2% coverage and adherence (.52,.19,.19,.11)",
        ylab = "Number Needed to Treat")

# Right Panel: PIA.W
PIA.all<-PIA.all*100
boxplot(PIA.all, outline = FALSE, medlwd = 1.1,  ylim = c(0,60),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("White","Black"),
        #main = "PIA with 54.2% coverage and adherence (.52,.19,.19,.11)",
        ylab = "Percent Infections Averted")

dev.off()



################################################################################################
################################################################################################
##Figure 1 Boxplot of % of infections averted number need to treat to prevent 1 new infection.
##  Boxes are interquartile rannge and whiskers 95% credibility intervals for 100 simulations of each scenario.
##  Scenarios are assuming both races at 40 % coverage  
## and average adherence (.2085, .2438, .1314, .4163).
##ELIGIBILITY AI + 6 months.

PIA.all<-cbind(PIA.c27.b,PIA.c27.w)
NNT.all<-cbind(NNT.c27.b,NNT.c27.w)

NIA.all<-cbind(NIA.c27.b,NIA.c27.w)



tiff(filename = "Fig1 base case AI no diff.tiff", height = 4, width = 8, units = "in", res = 250)
par(mfrow = c(1,3), mar = c(4,3,2.5,1), mgp = c(2,.5,0), cex=.9)

# Left Panel: PIA.all
PIA.all<-PIA.all*100
boxplot(PIA.all, outline = FALSE, medlwd = 1.1,  ylim = c(0,60),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("Black","White"),
        #main = "PIA with 54.2% coverage and adherence (.52,.19,.19,.11)",
        ylab = "Percent Infections Averted")

# Middle Panel: PIA.all
boxplot(NIA.all, outline = FALSE, medlwd = 1.1,  ylim = c(0,1500),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("Black","White"),
        #main = "PIA with 54.2% coverage and adherence (.52,.19,.19,.11)",
        ylab = "Number of Infections Averted")

# Right Panel: NNT.all
boxplot(NNT.all, outline = FALSE, medlwd = 1.1,  ylim = c(0,300),
        col = c(rep(pal[1], 9), rep(pal[2], 3)),
        names=c("Black","White"),
        #main = "NNT with 54.2% coverage and adherence (.52,.19,.19,.11)",
        ylab = "Number Needed to Treat")

dev.off()

