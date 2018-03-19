
## CAMP Adol 2 race appendix table




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
s29 <- merge_simfiles(29, indir = "scenarios/adol2race/data/")

data<-c("s1","s2","s3","s4","s5","s6","s7","s8","s9","s10",
        "s11","s12","s13","s14","s15","s16","s17","s18","s19","s20",
        "s21","s22","s23","s24","s25","s26","s27","s28","s29") 


#Base incidence.
prev.total.base.w<-rep(NA,100)
prev.total.base.b<-rep(NA,100)
prev.total.base.all<-rep(NA,100)

for (i in 1:100){
  
  prev.total.base.w[i]<-sum(s1$epi$i.prev.W[2080,i])
  prev.total.base.b[i]<-sum(s1$epi$i.prev.B[2080,i])
  prev.total.base.all[i]<-sum(s1$epi$i.prev[2080,i])  
}

prev.total.base.w<-mean(prev.total.base.w)
prev.total.base.w

prev.total.base.b<-mean(prev.total.base.b)
prev.total.base.b

prev.total.base.all<-mean(prev.total.base.all)
prev.total.base.all


prev.age18.w<-rep(NA,length(data))
prev.age18.up95.w<-rep(NA,length(data))
prev.age18.low95.w<-rep(NA,length(data))

prev.age18.b<-rep(NA,length(data))
prev.age18.up95.b<-rep(NA,length(data))
prev.age18.low95.b<-rep(NA,length(data))

prev.age18.all<-rep(NA,length(data))
prev.age18.up95.all<-rep(NA,length(data))
prev.age18.low95.all<-rep(NA,length(data))


for (i in 1:length(data)){


  prev.age18.temp.w<-rep(NA,100)
  prev.age18.temp.b<-rep(NA,100)
  prev.age18.temp.all<-rep(NA,100)

  
  
    for (j in 1:100){  

      prev.age18.temp.w[j]<-get(data[i])$epi$i.prev.age6.W[2080,j]
      prev.age18.temp.b[j]<-get(data[i])$epi$i.prev.age6.B[2080,j]
      prev.age18.temp.all[j]<-get(data[i])$epi$i.prev.age6[2080,j]
    }
  

  prev.age18.w[i]<-median(prev.age18.temp.w)
  prev.age18.temp.ordered.w<-sort(prev.age18.temp.w)
  prev.age18.up95.w[i]<-prev.age18.temp.ordered.w[length(prev.age18.temp.ordered.w)*.97]
  prev.age18.low95.w[i]<-prev.age18.temp.ordered.w[length(prev.age18.temp.ordered.w)*.03]

  prev.age18.b[i]<-median(prev.age18.temp.b)
  prev.age18.temp.ordered.b<-sort(prev.age18.temp.b)
  prev.age18.up95.b[i]<-prev.age18.temp.ordered.b[length(prev.age18.temp.ordered.b)*.97]
  prev.age18.low95.b[i]<-prev.age18.temp.ordered.b[length(prev.age18.temp.ordered.b)*.03]
  
  prev.age18.all[i]<-median(prev.age18.temp.all)
  prev.age18.temp.ordered.all<-sort(prev.age18.temp.all)
  prev.age18.up95.all[i]<-prev.age18.temp.ordered.all[length(prev.age18.temp.ordered.all)*.97]
  prev.age18.low95.all[i]<-prev.age18.temp.ordered.all[length(prev.age18.temp.ordered.all)*.03]
  
  
}

table1.alt<-cbind(prev.age18.all,  prev.age18.low95.all, prev.age18.up95.all,
                  prev.age18.b,  prev.age18.low95.b, prev.age18.up95.b,
                  prev.age18.w,  prev.age18.low95.w, prev.age18.up95.w,
                       data)
table1.alt

library(xlsx) #load the package
write.xlsx(x = table1.alt, file = "scenarios/adol2race/out/table1.alt.xlsx",
           sheetName = "Prevalence", row.names = FALSE)
