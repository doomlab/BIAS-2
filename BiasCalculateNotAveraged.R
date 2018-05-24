
##load data
fulldata <- read.csv("C:/Users/John/Desktop/fulldata.csv")

#calculate partial eta squared
fulldata$RM1.pes = fulldata$RM1.ssm.main / (fulldata$RM1.ssm.main + fulldata$RM1.ssr.main)
fulldata$RM2.pes = fulldata$RM2.ssm.main / (fulldata$RM2.ssm.main + fulldata$RM2.ssr.main)
fulldata$MIX.pes = fulldata$MIX.ssm.main / (fulldata$MIX.ssm.main + fulldata$MIX.ssr.main)
fulldata$BN1.pes = fulldata$BN1.ssm.main / (fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)
fulldata$BN2.pes = fulldata$BN2.ssm.main / (fulldata$BN2.ssm.main + fulldata$BN2.ssr.all)

#calculate full eta squared
fulldata$RM1.fes = fulldata$RM1.ssm.main / (fulldata$RM1.ssm.main + fulldata$RM1.ssr.main + fulldata$RM1.ssr.p)
fulldata$RM2.fes = fulldata$RM2.ssm.main / 
  (fulldata$RM2.ssm.main + fulldata$RM2.ssr.main + fulldata$RM2.ssm.other + fulldata$RM2.ssm.interact + fulldata$RM2.ssr.p + fulldata$RM2.ssr.other + fulldata$RM2.ssr.interact)
fulldata$MIX.fes = fulldata$MIX.ssm.main / 
  (fulldata$MIX.ssm.main + fulldata$MIX.ssr.main + fulldata$MIX.ssm.other + fulldata$MIX.ssm.interact + fulldata$MIX.ssr.p + fulldata$MIX.ssr.other + fulldata$MIX.ssr.interact)
fulldata$BN1.fes = fulldata$BN1.ssm.main / (fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)
fulldata$BN2.fes = fulldata$BN2.ssm.main / (fulldata$BN2.ssm.main + fulldata$BN2.ssr.all + fulldata$BN2.ssm.other + fulldata$BN2.ssm.interact)

#calculate full omega squared
fulldata$RM1.fos = (fulldata$RM1.dfm*((fulldata$RM1.ssm.main/fulldata$RM1.dfm)-(fulldata$RM1.ssr.main/fulldata$RM1.dfr)))/
  ((fulldata$RM1.ssm.main+fulldata$RM1.ssr.main+fulldata$RM1.ssr.p)+(fulldata$RM1.ssm.p/(fulldata$RM1.dfr/fulldata$RM1.dfm)))
fulldata$RM2.fos = (fulldata$RM2.dfm*((fulldata$RM2.ssm.main/fulldata$RM2.dfm)-(fulldata$RM2.ssr.main/fulldata$RM2.dfr)))/
  ((fulldata$RM2.ssm.main+fulldata$RM2.ssr.main+fulldata$RM2.ssm.other+fulldata$RM2.ssr.other+fulldata$RM2.ssm.interact+fulldata$RM2.ssr.interact+fulldata$RM2.ssr.p)+(fulldata$RM2.ssm.p/(fulldata$RM2.dfr/fulldata$RM2.dfm)))
fulldata$MIX.fos = (fulldata$MIX.dfm*((fulldata$MIX.ssm.main/fulldata$MIX.dfm)-(fulldata$MIX.ssr.main/fulldata$MIX.dfr)))/
  (fulldata$MIX.ssm.main+fulldata$MIX.ssr.main+fulldata$MIX.ssr.p+(fulldata$MIX.ssr.main/(fulldata$MIX.dfr/fulldata$MIX.dfm)))
fulldata$BN1.fos = (fulldata$BN1.dfm*((fulldata$BN1.ssm.main/fulldata$BN1.dfm)-(fulldata$BN1.ssr.main/fulldata$BN1.dfr)))/
  ((fulldata$BN1.ssm.main + fulldata$BN1.ssr.main)+(fulldata$BN1.ssr.main/fulldata$BN1.dfr))
fulldata$BN2.fos = (fulldata$BN2.dfm*((fulldata$BN2.ssm.main/fulldata$BN2.dfm)-(fulldata$BN2.ssr.all/fulldata$BN2.dfr)))/
  ((fulldata$BN2.ssm.main + fulldata$BN2.ssr.all + fulldata$BN2.ssm.other + fulldata$BN2.ssm.interact)+(fulldata$BN2.ssr.all/fulldata$BN2.dfr))

#calculate partial omega squared
#RM1 NA
#BN1 NA
fulldata$RM2.pos = (fulldata$RM2.dfm*((fulldata$RM2.ssm.main/fulldata$RM2.dfm)-(fulldata$RM2.ssr.main/fulldata$RM2.dfr)))/
  (fulldata$RM2.ssm.main+fulldata$RM2.ssr.main+fulldata$RM2.ssr.p+(fulldata$RM2.ssr.p/(fulldata$RM2.dfr/fulldata$RM2.dfm)))
fulldata$BN2.pos = (fulldata$BN2.dfm*((fulldata$BN2.ssm.main/fulldata$BN2.dfm)-(fulldata$BN2.ssr.all/fulldata$BN2.dfr)))/
  (fulldata$BN2.ssm.main+(((fulldata$N*fulldata$levels)-fulldata$BN2.dfm)*(fulldata$BN2.ssr.all/fulldata$BN2.dfr)))
fulldata$MIX.pos = (fulldata$MIX.dfm*((fulldata$MIX.ssm.main/fulldata$MIX.dfm)-(fulldata$MIX.ssr.main/fulldata$MIX.dfr)))/
  (fulldata$MIX.ssm.main+fulldata$MIX.ssr.main+fulldata$MIX.ssr.p+(fulldata$MIX.ssr.p/(fulldata$MIX.dfr/fulldata$MIX.dfm)))


data = fulldata[ ,c(3:6,15,27,39,47,56:74)]
data$partno = 1:nrow(data)
library(reshape)
longdata = melt(data = data,
                id = c("partno","N","levels","stdev","correl"),
                measured = c("RM1.ges","RM2.ges","MIX.ges","BN1.ges","BN2.ges","RM1.pes","RM2.pes","MIX.pes",
                             "BN1.pes","BN2.pes","RM1.fes","RM2.fes","MIX.fes","BN1.fes","BN2.fes","RM1.fos",
                             "RM2.fos","MIX.fos","BN1.fos","BN2.fos","RM2.pos","BN2.pos","MIX.pos"))
colnames(longdata) = c("partno","N","levels","stdev","correl","condition","effect")


longdata$design = c(rep("RM1",1152000), rep("RM2",1152000), rep("MIX",1152000), rep("BN1",1152000), rep("BN2",1152000),
                    rep("RM1",1152000), rep("RM2",1152000), rep("MIX",1152000), rep("BN1",1152000), rep("BN2",1152000),
                    rep("RM1",1152000), rep("RM2",1152000), rep("MIX",1152000), rep("BN1",1152000), rep("BN2",1152000),
                    rep("RM1",1152000), rep("RM2",1152000), rep("MIX",1152000), rep("BN1",1152000), rep("BN2",1152000),
                    rep("RM2",1152000), rep("BN2",1152000), rep("MIX",1152000))
longdata$ES = c(rep("ges",5760000), rep("pes",5760000), rep("fes",5760000), rep("fos",5760000), rep("pos",3456000))

longdata = longdata[ , -6]
longdata$design = factor(longdata$design)
longdata$ES = factor(longdata$ES)

#just to check in which order stdev or levels change
nsim = nrow(longdata)
for(i in 1:nsim){
  y = 1
  if(longdata$levels[i] == 3){
    
  } else{
    print(i)
    break
  }
} #18000 for each level

for(i in 1:nsim){
  y = 1
  if(longdata$stdev[i] == 5){
    
  } else{
    print(i)
    break
  }
} #6000 for each stdev

table(longdata$levels) #6624000 for each level total
table(longdata$stdev) # 8832000 for each stdev

#6000*12 for one round = 72000
#26496000/72000 = repeat 368 rounds

eta2plevel3sd5 = 0.03225806
eta2plevel3sd3 = 0.05263158
eta2plevel3sd1 = 0.14285714
eta2plevel4sd5 = 0.05882353
eta2plevel4sd3 = 0.09433962
eta2plevel4sd1 = 0.23809524
eta2plevel5sd5 = 0.09090909
eta2plevel5sd3 = 0.14285714
eta2plevel5sd1 = 0.33333333
eta2plevel6sd5 = 0.12727270
eta2plevel6sd3 = 0.19553070
eta2plevel6sd1 = 0.42168670

tempx = c(rep(eta2plevel3sd5,6000), rep(eta2plevel3sd3,6000), rep(eta2plevel3sd1,6000),
          rep(eta2plevel4sd5,6000), rep(eta2plevel4sd3,6000), rep(eta2plevel4sd1,6000),
          rep(eta2plevel5sd5,6000), rep(eta2plevel5sd3,6000), rep(eta2plevel5sd1,6000),
          rep(eta2plevel6sd5,6000), rep(eta2plevel6sd3,6000), rep(eta2plevel6sd1,6000)
          )
tempy = c(rep(tempx,368))

longdata$eta2p = tempy

#calculate bias
longdata$bias = longdata$effect - longdata$eta2p


write.csv(longdata, "BiasDataNotAv.csv")


