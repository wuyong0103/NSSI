#Set the working directory
setwd("C:/Users/administered3/Desktop/儿少/NSSI孙科")
library("DescTools")
library("rcompanion")
library("ResourceSelection")
#Read the data
data <- read.csv("SuicideIdeation20210816.csv", header = T)

#Sample characteristics
mean(data$Age)
sd(data$Age)
table(data$Grade)
table(data$Gender)
table(data$SelfInjury)
mean(data$Depress)
sd(data$Depress)
mean(data$Anxiety)
sd(data$Anxiety)
mean(data$Stigma)
sd(data$Stigma)
mean(data$Insomnia)
sd(data$Insomnia)
mean(data$EmotionalAbuse)
sd(data$EmotionalAbuse)
mean(data$BodyAbuse)
sd(data$BodyAbuse)
mean(data$SexAbuse)
sd(data$SexAbuse)
mean(data$EmotionalNeglect)
sd(data$EmotionalNeglect)
mean(data$BodyNeglect)
sd(data$BodyNeglect)
table(data$ToSuicidalBehavior)
table(data$ToSuicide)
table(data$ToDependent)
table(data$ToEuthanasia)
mean(data$InterpersonalStress)
sd(data$InterpersonalStress)
mean(data$StudyPressure)
sd(data$StudyPressure)
mean(data$Adaptation)
sd(data$Adaptation)
mean(data$Lose)
sd(data$Lose)
mean(data$Punish)
sd(data$Punish)
mean(na.omit(data$FamilySupport))
sd(na.omit(data$FamilySupport))
mean(na.omit(data$OutFamily))
sd(na.omit(data$OutFamily))
mean(na.omit(data$SumSupport))
sd(na.omit(data$SumSupport))
mean(na.omit(data$ObjectiveSupport))
sd(na.omit(data$ObjectiveSupport))
mean(na.omit(data$SubjectiveSupport))
sd(na.omit(data$SubjectiveSupport))
mean(na.omit(data$SupportUse))
sd(na.omit(data$SupportUse))
mean(na.omit(data$SumSocial))
sd(na.omit(data$SumSocial))

#chisq test
table(data.frame(data$Gender, data$SelfInjury))
chisq.test(table(data.frame(data$Gender, data$SelfInjury)))

table(data.frame(data$Grade, data$SelfInjury))
chisq.test(table(data.frame(data$Grade, data$SelfInjury)))

table(data.frame(data$ToSuicidalBehavior, data$SelfInjury))
chisq.test(table(data.frame(data$ToSuicidalBehavior, data$SelfInjury)))

table(data.frame(data$ToSuicide, data$SelfInjury))
chisq.test(table(data.frame(data$ToSuicide, data$SelfInjury)))

table(data.frame(data$ToDependent, data$SelfInjury))
chisq.test(table(data.frame(data$ToDependent, data$SelfInjury)))

table(data.frame(data$ToEuthanasia, data$SelfInjury))
chisq.test(table(data.frame(data$ToEuthanasia, data$SelfInjury)))

#ANOVA
mean(data$Age[data$SelfInjury==1])
sd(data$Age[data$SelfInjury==1])
mean(data$Age[data$SelfInjury==2])
sd(data$Age[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$Age))

mean(data$Depress[data$SelfInjury==1])
sd(data$Depress[data$SelfInjury==1])
mean(data$Depress[data$SelfInjury==2])
sd(data$Depress[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$Depress))

mean(data$Anxiety[data$SelfInjury==1])
sd(data$Anxiety[data$SelfInjury==1])
mean(data$Anxiety[data$SelfInjury==2])
sd(data$Anxiety[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$Anxiety))

mean(data$Stigma[data$SelfInjury==1])
sd(data$Stigma[data$SelfInjury==1])
mean(data$Stigma[data$SelfInjury==2])
sd(data$Stigma[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$Stigma))

mean(data$Insomnia[data$SelfInjury==1])
sd(data$Insomnia[data$SelfInjury==1])
mean(data$Insomnia[data$SelfInjury==2])
sd(data$Insomnia[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$Insomnia))

mean(data$EmotionalAbuse[data$SelfInjury==1])
sd(data$EmotionalAbuse[data$SelfInjury==1])
mean(data$EmotionalAbuse[data$SelfInjury==2])
sd(data$EmotionalAbuse[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$EmotionalAbuse))

mean(data$BodyAbuse[data$SelfInjury==1])
sd(data$BodyAbuse[data$SelfInjury==1])
mean(data$BodyAbuse[data$SelfInjury==2])
sd(data$BodyAbuse[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$BodyAbuse))

mean(data$SexAbuse[data$SelfInjury==1])
sd(data$SexAbuse[data$SelfInjury==1])
mean(data$SexAbuse[data$SelfInjury==2])
sd(data$SexAbuse[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$SexAbuse))

mean(data$EmotionalNeglect[data$SelfInjury==1])
sd(data$EmotionalNeglect[data$SelfInjury==1])
mean(data$EmotionalNeglect[data$SelfInjury==2])
sd(data$EmotionalNeglect[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$EmotionalNeglect))

mean(data$BodyNeglect[data$SelfInjury==1])
sd(data$BodyNeglect[data$SelfInjury==1])
mean(data$BodyNeglect[data$SelfInjury==2])
sd(data$BodyNeglect[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$BodyNeglect))

mean(data$InterpersonalStress[data$SelfInjury==1])
sd(data$InterpersonalStress[data$SelfInjury==1])
mean(data$InterpersonalStress[data$SelfInjury==2])
sd(data$InterpersonalStress[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$InterpersonalStress))

mean(data$StudyPressure[data$SelfInjury==1])
sd(data$StudyPressure[data$SelfInjury==1])
mean(data$StudyPressure[data$SelfInjury==2])
sd(data$StudyPressure[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$StudyPressure))

mean(data$Adaptation[data$SelfInjury==1])
sd(data$Adaptation[data$SelfInjury==1])
mean(data$Adaptation[data$SelfInjury==2])
sd(data$Adaptation[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$Adaptation))

mean(data$Lose[data$SelfInjury==1])
sd(data$Lose[data$SelfInjury==1])
mean(data$Lose[data$SelfInjury==2])
sd(data$Lose[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$Lose))

mean(data$Punish[data$SelfInjury==1])
sd(data$Punish[data$SelfInjury==1])
mean(data$Punish[data$SelfInjury==2])
sd(data$Punish[data$SelfInjury==2])
summary(aov(data$SelfInjury~data$Punish))

mean(na.omit(data$FamilySupport[data$SelfInjury==1]))
sd(na.omit(data$FamilySupport[data$SelfInjury==1]))
mean(na.omit(data$FamilySupport[data$SelfInjury==2]))
sd(na.omit(data$FamilySupport[data$SelfInjury==2]))
summary(aov(data$SelfInjury~data$FamilySupport))

mean(na.omit(data$OutFamily[data$SelfInjury==1]))
sd(na.omit(data$OutFamily[data$SelfInjury==1]))
mean(na.omit(data$OutFamily[data$SelfInjury==2]))
sd(na.omit(data$OutFamily[data$SelfInjury==2]))
summary(aov(data$SelfInjury~data$OutFamily))

mean(na.omit(data$SumSupport[data$SelfInjury==1]))
sd(na.omit(data$SumSupport[data$SelfInjury==1]))
mean(na.omit(data$SumSupport[data$SelfInjury==2]))
sd(na.omit(data$SumSupport[data$SelfInjury==2]))
summary(aov(data$SelfInjury~data$SumSupport))

mean(na.omit(data$ObjectiveSupport[data$SelfInjury==1]))
sd(na.omit(data$ObjectiveSupport[data$SelfInjury==1]))
mean(na.omit(data$ObjectiveSupport[data$SelfInjury==2]))
sd(na.omit(data$ObjectiveSupport[data$SelfInjury==2]))
summary(aov(data$SelfInjury~data$ObjectiveSupport))

mean(na.omit(data$SubjectiveSupport[data$SelfInjury==1]))
sd(na.omit(data$SubjectiveSupport[data$SelfInjury==1]))
mean(na.omit(data$SubjectiveSupport[data$SelfInjury==2]))
sd(na.omit(data$SubjectiveSupport[data$SelfInjury==2]))
summary(aov(data$SelfInjury~data$SubjectiveSupport))

mean(na.omit(data$SupportUse[data$SelfInjury==1]))
sd(na.omit(data$SupportUse[data$SelfInjury==1]))
mean(na.omit(data$SupportUse[data$SelfInjury==2]))
sd(na.omit(data$SupportUse[data$SelfInjury==2]))
summary(aov(data$SelfInjury~data$SupportUse))

mean(na.omit(data$SumSocial[data$SelfInjury==1]))
sd(na.omit(data$SumSocial[data$SelfInjury==1]))
mean(na.omit(data$SumSocial[data$SelfInjury==2]))
sd(na.omit(data$SumSocial[data$SelfInjury==2]))
summary(aov(data$SelfInjury~data$SumSocial))


#Logical regression
fit <- glm(as.factor(data$SelfInjury) ~ as.factor(data$Gender) + data$Age + data$Depress + data$Anxiety + data$Stigma + data$Insomnia + data$EmotionalAbuse +
             data$BodyAbuse + data$SexAbuse + data$EmotionalNeglect + data$BodyNeglect + as.factor(data$ToSuicidalBehavior) + as.factor(data$ToSuicide) +
             as.factor(data$ToDependent) + as.factor(data$ToEuthanasia) + data$InterpersonalStress + data$StudyPressure + data$Adaptation + data$Lose +
             data$Punish + data$FamilySupport + data$OutFamily + data$SumSupport + data$ObjectiveSupport + data$SubjectiveSupport + data$SupportUse +
             data$SumSocial,family = 'binomial')
summary(fit)
formatFit<-function(fit){
  #取P值
  p<-summary(fit)$coefficients[,4]
  #wald值
  wald<-summary(fit)$coefficients[,3]^2
  #B值
  valueB<-coef(fit)
  #SE
  SE <- summary(fit)$coefficients[,2]
  #OR值
  valueOR<-exp(coef(fit))
  #OR值得95%CI
  confitOR<-exp(confint(fit))
  data.frame(
    B=round(valueB,3),
    SE=round(SE,3),
    Wald=round(wald,3),
    OR=round(valueOR,3),
    CI=paste(round(confitOR[,1],3),"-",round(confitOR[,2],3),sep=""),
    P=format.pval(p,digits = 3,eps=0.001)
  )
}
formatFit(fit)
write.csv(formatFit(fit), "Logit.csv")
HosmerLemeshowTest(fit = fitted(fit), obs = data$SelfInjury, X = cbind(as.factor(data$Gender), data$Age, data$Depress, data$Anxiety, data$Stigma, 
                                                                                  data$Insomnia, data$EmotionalAbuse, data$BodyAbuse, data$SexAbuse, data$EmotionalNeglect, data$BodyNeglect, 
                                                                                  as.factor(data$ToSuicidalBehavior), as.factor(data$ToSuicide), as.factor(data$ToDependent), as.factor(data$ToEuthanasia), 
                                                                                 data$InterpersonalStress, data$StudyPressure, data$Adaptation, data$Lose, data$Punish, data$FamilySupport,
                                                                                 data$OutFamily, data$SumSupport, data$ObjectiveSupport, data$SubjectiveSupport, data$SupportUse,
                                                                                 data$SumSocial))

OddsRatio(fit)
PseudoR2(fit, which="all")
plot(OddsRatio(fit), xlim=c(0.5, 2), main="OddsRatio - glm", pch=NA,
     lblcolor=hred, args.errbars=list(col=horange, pch=21, col.pch=hblue,
                                      bg.pch=hyellow, cex.pch=1.5))
library("rcompanion")
nagelkerke(fit)
hoslem.test(data$SelfInjury, fitted(fit), g=10)






