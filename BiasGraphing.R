

setwd("C:/Users/John/Desktop/OneDrive_1_8-9-2017")
source("BiasCalculate.R")

library(ggplot2)
library(reshape)
library(Hmisc)
source("http://peterhaschke.com/Code/multiplot.R")



#drop mix condition
BiasDat = Full.bias.data[Full.bias.data$Design!="MIX",]
BiasDat$Design = factor(BiasDat$Design)
summary(BiasDat)

theme = theme(panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              panel.background = element_blank(), 
              axis.line.x = element_line(colour = "black"), 
              axis.line.y = element_line(colour = "Black"),
              legend.key = element_rect(fill = "white"),
              text = element_text(size = 15))

############################################ Bias/SD for Different Effect Size Types
biasESType = ggplot(BiasDat)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("Type of Effect Size") + theme ; biasESType

sdESType = ggplot(BiasDat)+geom_smooth(aes(x=N, y=SD, group = ES, color = ES), se = T) +
  ggtitle("Type of Effect Size") + theme ; sdESType
############################################ Bias/SD for Different Effect Size Types


############################################ Bias/SD for 1/2 way and BN/RM
BN1dat = subset(BiasDat, Design == "BN1")
BN2dat = subset(BiasDat, Design == "BN2")
RM1dat = subset(BiasDat, Design == "RM1")
RM2dat = subset(BiasDat, Design == "RM2")

BN1biasGraph = ggplot(BN1dat)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("One-Way Between-Subjects") + theme
BN2biasGraph = ggplot(BN2dat)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("Two-Way Between-Subjects") + theme
RM1biasGraph = ggplot(RM1dat)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("One-Way Within-Subjects") + theme
RM2biasGraph = ggplot(RM2dat)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("Two-Way Within-Subjects") + theme
multiplot(BN1biasGraph,BN2biasGraph,RM1biasGraph,RM2biasGraph, cols = 2)

BN1SDGraph = ggplot(BN1dat)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("One-Way Between-Subjects") + theme
BN2SDGraph = ggplot(BN2dat)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("Two-Way Between-Subjects") + theme
RM1SDGraph = ggplot(RM1dat)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("One-Way Within-Subjects") + theme
RM2SDGraph = ggplot(RM2dat)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("Two-Way Within-Subjects") + theme
multiplot(BN1SDGraph,BN2SDGraph,RM1SDGraph,RM2SDGraph, cols = 2)


## or

barbias = ggplot(BiasDat, aes(Design, Bias, fill = ES))
barbias +stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  stat_summary(fun.data = mean_cl_normal,  geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2) + 
  xlab("Design") +ylab("Bias") + theme +
  scale_fill_manual(name="ES", values = c("Red","Gold","Green","Blue","Purple"))

barSD = ggplot(BiasDat, aes(Design, SD, fill = ES))
barSD +stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  stat_summary(fun.data = mean_cl_normal,  geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2) + 
  xlab("Design") +ylab("SD") + theme +
  scale_fill_manual(name="ES", values = c("Red","Gold","Green","Blue","Purple"))
############################################ Bias/SD for 1/2 way and BN/RM



############################################ Bias/SD for number of levels
level3 = subset(BiasDat, Levels == 3)
level4 = subset(BiasDat, Levels == 4)
level5 = subset(BiasDat, Levels == 5)
level6 = subset(BiasDat, Levels == 6)

level3bias = ggplot(level3)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("Three Levels") + theme
level4bias = ggplot(level4)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("Four Levels") + theme
level5bias = ggplot(level5)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("Five Levels") + theme
level6bias = ggplot(level6)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("Six Levels") + theme
multiplot(level3bias, level4bias, level5bias, level6bias, cols = 2)

level3SD = ggplot(level3)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("Three Levels") + theme
level4SD = ggplot(level4)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("Four Levels") + theme
level5SD = ggplot(level5)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("Five Levels") + theme
level6SD = ggplot(level6)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("Six Levels") + theme
multiplot(level3SD, level4SD, level5SD, level6SD, cols = 2)


## or

barbiasLevels = ggplot(BiasDat, aes(Levels, Bias, fill = ES))
barbiasLevels +stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  stat_summary(fun.data = mean_cl_normal,  geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2) + 
  xlab("Number of Levels") +ylab("Bias") + theme +
  scale_fill_manual(name="ES", values = c("Red","Gold","Green","Blue","Purple"))

barSDLevels = ggplot(BiasDat, aes(Levels, SD, fill = ES))
barSDLevels +stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  stat_summary(fun.data = mean_cl_normal,  geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2) + 
  xlab("Number of Levels") +ylab("SD") + theme +
  scale_fill_manual(name="ES", values = c("Red","Gold","Green","Blue","Purple"))
############################################ Bias/SD for number of levels


############################################ Bias/SD for Correlation
#drop all BN1 an BN2
temp = BiasDat
temp$Design = as.character(temp$Design)
temp2 = temp[temp$Design!="BN1",]
temp2 = temp2[temp2$Design!="BN2",]
temp2$Design = factor(temp2$Design)
summary(temp2)

cor0 = subset(temp2, Corr == 0.0)
cor1 = subset(temp2, Corr == 0.1)
cor3 = subset(temp2, Corr == 0.3)
cor5 = subset(temp2, Corr == 0.5)
cor7 = subset(temp2, Corr == 0.7)
cor9 = subset(temp2, Corr == 0.9)

cor0bias = ggplot(cor0)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("0.0 Correlation") + theme
cor1bias = ggplot(cor1)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("0.1 Correlation") + theme
cor3bias = ggplot(cor3)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("0.3 Correlation") + theme
cor5bias = ggplot(cor5)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("0.5 Correlation") + theme
cor7bias = ggplot(cor7)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("0.7 Correlation") + theme
cor9bias = ggplot(cor9)+geom_smooth(aes(x=N, y=Bias, group=ES, color=ES), se = T) +
  ggtitle("0.9 Correlation") + theme
multiplot(cor0bias, cor1bias, cor3bias, cor5bias,
          cor7bias, cor9bias, cols = 3)

cor0SD = ggplot(cor0)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("0.0 Correlation") + theme
cor1SD = ggplot(cor1)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("0.1 Correlation") + theme
cor3SD = ggplot(cor3)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("0.3 Correlation") + theme
cor5SD = ggplot(cor5)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("0.5 Correlation") + theme
cor7SD = ggplot(cor7)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("0.7 Correlation") + theme
cor9SD = ggplot(cor9)+geom_smooth(aes(x=N, y=SD, group=ES, color=ES), se = T) +
  ggtitle("0.9 Correlation") + theme
multiplot(cor0SD, cor1SD, cor3SD, cor5SD,
          cor7SD, cor9SD, cols = 3)

#### or

Cordat = BiasDat
Cordat$Corr = factor(Cordat$Corr)
barbiascor = ggplot(Cordat, aes(Corr, Bias, fill = ES))
barbiascor +stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  stat_summary(fun.data = mean_cl_normal,  geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2) + 
  xlab("Correlation") +ylab("Bias") + theme +
  scale_fill_manual(name="ES", values = c("Red","Gold","Green","Blue","Purple"))

barSDcor = ggplot(Cordat, aes(Corr, SD, fill = ES))
barSDcor +stat_summary(fun.y = mean, geom = "bar", position="dodge") + 
  stat_summary(fun.data = mean_cl_normal,  geom = "errorbar", 
               position = position_dodge(width = 0.90), width = 0.2) + 
  xlab("Correlation") +ylab("SD") + theme +
  scale_fill_manual(name="ES", values = c("Red","Gold","Green","Blue","Purple"))
############################################ Bias/SD for Correlation





