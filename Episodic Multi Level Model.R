#### Multi-Level Model ####

## A two-level model, with trials (level 1) nested within rats (level 2), and trial type and session type will serve as level 1 predictors (dummy-coded)

#### Reading in data ####

Session_Number<-rep(c(1:25),18)
Transition<-c(rep(1,100), rep(2,100), rep(3,100), rep(1,100), rep(2,100), rep(3,100), rep(1,100), rep(2,100), rep(3,100))
Rat<-c(rep(1,300), rep(2,300), rep(3,300))
Trial_Type<-c(rep(1,25),rep(2,25),rep(3,25),rep(4,25),rep(1,25),rep(2,25),rep(3,25),rep(4,25), rep(1,25),rep(2,25),rep(3,25),rep(4,25),rep(1,25),rep(2,25), rep(3,25),rep(4,25),rep(1,25), rep(2,25),rep(3,25),rep(4,25), rep(1,25), rep(2,25),rep(3,25),rep(4,25),rep(1,25),rep(2,25),rep(3,25),rep(4,25), rep(1,25), rep(2,25),rep(3,25),rep(4,25),rep(1,25), rep(2,25),rep(3,25),rep(4,25))


#Rat Data

H25_PROBE_2TRAN<-X595_data[c(1:25),7]
H25_PROBE_3TRAN<-X595_data[c(26:50),7]
H25_PROBE_4TRAN<-X595_data[c(51:75),7]
H25_CON_2TRAN<-X595_data[c(1:25),8]
H25_CON_3TRAN<-X595_data[c(26:50),8]
H25_CON_4TRAN<-X595_data[c(51:75),8]

H25_SN_2TRAN<-X595_data[c(1:25),5]
H25_SN_3TRAN<-X595_data[c(26:50),5]
H25_SN_4TRAN<-X595_data[c(51:75),5]
H25_CN_2TRAN<-X595_data[c(1:25),6]
H25_CN_3TRAN<-X595_data[c(26:50),6]
H25_CN_4TRAN<-X595_data[c(51:75),6]

H31_PROBE_2TRAN<-X595_data[c(1:25),1]
H31_PROBE_3TRAN<-X595_data[c(26:50),1]
H31_PROBE_4TRAN<-X595_data[c(51:75),1]
H31_CON_2TRAN<-X595_data[c(1:25),4]
H31_CON_3TRAN<-X595_data[c(26:50),4]
H31_CON_4TRAN<-X595_data[c(51:75),4]

H31_CN_2TRAN<-X595_data[c(1:25),3]
H31_CN_3TRAN<-X595_data[c(26:50),3]
H31_CN_4TRAN<-X595_data[c(51:75),3]
H31_SN_2TRAN<-X595_data[c(1:25),2]
H31_SN_3TRAN<-X595_data[c(26:50),2]
H31_SN_4TRAN<-X595_data[c(51:75),2]

H26_PROBE_2TRAN<-X595_data[c(1:25),9]
H26_PROBE_3TRAN<-X595_data[c(26:50),9]
H26_PROBE_4TRAN<-X595_data[c(51:75),9]
H26_CON_2TRAN<-X595_data[c(1:25),10]
H26_CON_3TRAN<-X595_data[c(26:50),10]
H26_CON_4TRAN<-X595_data[c(51:75),10]

H26_SN_2TRAN<-X595_data[c(1:25),12]
H26_SN_3TRAN<-X595_data[c(26:50),12]
H26_SN_4TRAN<-X595_data[c(51:75),12]
H26_CN_2TRAN<-X595_data[c(1:25),11]
H26_CN_3TRAN<-X595_data[c(26:50),11]
H26_CN_4TRAN<-X595_data[c(51:75),11]


Percent_Correct<-c(H25_PROBE_2TRAN,H25_CON_2TRAN,H25_SN_2TRAN, H25_CN_2TRAN,H25_PROBE_3TRAN,H25_CON_3TRAN,H25_SN_3TRAN,H25_CN_3TRAN,H25_PROBE_4TRAN,H25_CON_4TRAN,H25_SN_4TRAN,H25_CN_4TRAN,H31_PROBE_2TRAN,H31_CON_2TRAN,H31_SN_2TRAN,H31_CN_2TRAN,H31_PROBE_3TRAN,H31_CON_3TRAN,H31_SN_3TRAN, H31_CN_3TRAN,H31_PROBE_4TRAN,H31_CON_4TRAN,H31_SN_4TRAN, H31_CN_4TRAN,H26_PROBE_2TRAN,H26_CON_2TRAN,H26_SN_2TRAN,H26_CN_2TRAN,H26_PROBE_3TRAN,H26_CON_3TRAN,H26_SN_3TRAN,H26_CN_3TRAN, H26_PROBE_4TRAN,H26_CON_4TRAN,H26_SN_4TRAN,H26_CN_4TRAN)


# Rat order -> 1= H25, 2=H31, 3=H26

# Trial Type order --> 1= Context, 2= Familiarity, 3= Session Novel, 4= Context Novel

# Session Type order--> 1= 2 transitions, 2= 3 transitions, 3= 4 transitions

Percent_Correct<-unlist(Percent_Correct)


#Create Data frame
EM_data<-data.frame(Rat,Transition,Trial_Type,Percent_Correct)


#Factor Data
EM_data$Rat<-factor(EM_data$Rat, levels = c(1:3), labels = c("H25", "H31","H26"))

EM_data$Transition<-factor(EM_data$Transition, levels = c(1:3), labels = c("2TRAN", "3TRAN", "4TRAN"))

EM_data$Trial_Type<-factor(EM_data$Trial_Type, levels = c(1:4), labels = c("Context Probe", "Familiarity Probe", "Session Novel", "Context Novel"))


#Dummy Coded Variables



EM_data$THREE_TRAN_Dummy<-ifelse(EM_data$Transition=="3TRAN",1,0)
EM_data$FOUR_TRAN_Dummy<-ifelse(EM_data$Transition=="4TRAN",1,0)

# 2TRAN is reference group
EM_data<-EM_data[,-c(7)]
EM_data$Context_Dummy<-ifelse(EM_data$Trial_Type=="Context Probe",1,0)
EM_data$SN_dummy<-ifelse(EM_data$Trial_Type=="Session Novel",1,0)
EM_data$CN_dummy<-ifelse(EM_data$Trial_Type=="Context Novel",1,0)

# Fam Probe is reference group


#### Multilevel Model ####

#Packages
library(backports)
library(effects)       
library(ggplot2)      
library(interactions)  
library(lme4)
library(lmerTest)    
library(psych)         
library(plyr)
library(sjPlot)

ggpairs(EM_data)

#### Assumptions and Descriptives ####
## Descriptives
describeBy(EM_data, list(EM_data$Transition, EM_data$Trial_Type,EM_data$Rat))

##Boxplots
ggplot(EM_data,aes(x=Rat, y=Percent_Correct,fill=Trial_Type))+geom_boxplot()+facet_grid(~Transition)

##Barchart

rat<-c(rep(1,12),rep(2,12),rep(3,12))

trial<-c(1:4,1:4,1:4,1:4,1:4,1:4,1:4,1:4,1:4)

tran<-c(rep(1,4),rep(2,4),rep(3,4),rep(1,4),rep(2,4),rep(3,4),rep(1,4),rep(2,4),rep(3,4))
average<-c(0.93,0.89,0.69,0.8,0.92,0.96,0.78,0.84,0.96,0.87,0.75,0.77,0.92,0.88,0.61,0.83,0.88,0.94,0.69,0.82,0.8,0.89,0.63,0.7,0.9,0.85,0.61,0.74,0.87,0.89,0.69,0.63,0.83,0.92,0.52,0.71)
sd<-c(.02,.03,.08,.06,.04,.04,.06,.05,.01,.025,.05,.11,.02,.04,.08,.03,.03,.05,.06,.09,.03,.05,.08,.09,.02,.05,.09,.07,.03,.01,.07,.09,.03,.05,.08,.06)

df<-data.frame(rat,trial,tran,average,sd)

df$rat<-factor(df$rat,levels = c(1:3), labels = c("H31","H25","H26"))
df$trial<-factor(df$trial,levels = c(1:4),labels = c("SN","CN","Con","Fam"))
df$tran<-factor(df$tran,levels=c(1:3),labels = c("Two","Three","Four"))

ggplot(data=df,aes(x=tran,y=average,fill=trial))+geom_bar(stat = "identity",position = position_dodge(),colour="black")+facet_grid(~rat)+geom_errorbar(aes(ymin=average-sd, ymax=average+sd), width=.2,
                position=position_dodge(.9))+theme_classic()+labs(title="Mean Performance for Each Trial Type Across Session Types", x="Number of Transitions", y = "p(correct")+
  theme_classic()
                          
df<-data.frame(rat,trial,tran)

##Density Plot
ggplot(EM_data,aes(x=Percent_Correct))+geom_density()+facet_grid(~Transition+Trial_Type)

## Mulitvariate Normality
TwoTranSN<-EM_data%>%subset(Transition=="2TRAN")%>%subset(Trial_Type=="Session Novel")
ThreeTranSN<-EM_data%>%subset(Transition=="3TRAN")%>%subset(Trial_Type=="Session Novel")
FourTranSN<-EM_data%>%subset(Transition=="4TRAN")%>%subset(Trial_Type=="Session Novel")

TwoTranCN<-EM_data%>%subset(Transition=="2TRAN")%>%subset(Trial_Type=="Context Novel")
ThreeTranCN<-EM_data%>%subset(Transition=="3TRAN")%>%subset(Trial_Type=="Context Novel")
FourTranCN<-EM_data%>%subset(Transition=="4TRAN")%>%subset(Trial_Type=="Context Novel")

TwoTranCon<-EM_data%>%subset(Transition=="2TRAN")%>%subset(Trial_Type=="Context Probe")
ThreeTranCon<-EM_data%>%subset(Transition=="3TRAN")%>%subset(Trial_Type=="Context Probe")
FourTranCon<-EM_data%>%subset(Transition=="4TRAN")%>%subset(Trial_Type=="Context Probe")

TwoTranFam<-EM_data%>%subset(Transition=="2TRAN")%>%subset(Trial_Type=="Familiarity Probe")
ThreeTranFam<-EM_data%>%subset(Transition=="3TRAN")%>%subset(Trial_Type=="Familiarity Probe")
FourTranFam<-EM_data%>%subset(Transition=="4TRAN")%>%subset(Trial_Type=="Familiarity Probe")

sn2<-t(TwoTranSN[,4])
sn3<-t(ThreeTranSN[,4])
sn4<-t(FourTranSN[,4])

cn2<-t(TwoTranCN[,4])
cn3<-t(ThreeTranCN[,4])
cn4<-t(FourTranCN[,4])

con2<-t(TwoTranCon[,4])
con3<-t(ThreeTranCon[,4])
con4<-t(FourTranCon[,4])

fam2<-t(TwoTranFam[,4])
fam3<-t(ThreeTranFam[,4])
fam4<-t(FourTranFam[,4])

mshapiro.test(sn2)
mshapiro.test(sn3)
mshapiro.test(sn4)

mshapiro.test(cn2)
mshapiro.test(cn3)
mshapiro.test(cn4)

mshapiro.test(con2)
mshapiro.test(con3)
mshapiro.test(con4)

mshapiro.test(fam2)
mshapiro.test(fam3)
mshapiro.test(fam4)


## QQ plot

ggplot(EM_data,aes(sample=Percent_Correct))+stat_qq()+stat_qq_line()+facet_grid(~Transition+Trial_Type)

## Outliers

library(influence.ME)
infl <- influence.ME::influence(model4, obs = TRUE)
cooks.distance(infl)
plot(infl, which = "cook")

library(outliers)
chisq.out.test(EM_data$Percent_Correct,opposite = TRUE)

####Null Model####

model0_fit <- lmer(formula = Percent_Correct ~ 1 + (1|Rat), 
                   data=EM_data,
                   na.action=na.exclude,REML = FALSE)

summary(model0_fit)

VarCorr(model0_fit)


# Intraclass correlation

RandomEffects <- as.data.frame(VarCorr(model0_fit))
RandomEffects

ICC_between <- RandomEffects[1,4]/(RandomEffects[1,4]+RandomEffects[2,4]) 
ICC_between

##2 percent of total variance in percent_correct is attributed to between-rat variance, 97 percent of total variance in percent_correct is attributed to within-rat variance

#### Model with level 1 predictors with fixed components and one random error term ####

model1<-lmer(Percent_Correct~1+Context_Dummy+SN_dummy+CN_dummy+THREE_TRAN_Dummy+FOUR_TRAN_Dummy+Context_Dummy:THREE_TRAN_Dummy+Context_Dummy:FOUR_TRAN_Dummy+(1|Rat), data=EM_data, REML = FALSE)

summary(model1)

tab_model(model1)
anova(model0_fit, model1)


#### Model with two level-one predictors (Trial Type and Transition) and random effect####

#Add random slopes one by one until convergence failure


model2 <- lmer(Percent_Correct~1+Context_Dummy+SN_dummy+CN_dummy+THREE_TRAN_Dummy+FOUR_TRAN_Dummy+Context_Dummy:THREE_TRAN_Dummy+Context_Dummy:FOUR_TRAN_Dummy+(1+Context_Dummy|Rat), data=EM_data, REML = FALSE)

summary(model2)
tab_model(model2)

anova(model1,model2)

anova(model2,model4)

model3<- lmer(Percent_Correct~1+Context_Dummy+SN_dummy+CN_dummy+THREE_TRAN_Dummy+FOUR_TRAN_Dummy+Context_Dummy:THREE_TRAN_Dummy+Context_Dummy:FOUR_TRAN_Dummy+(1+Context_Dummy+CN_dummy|Rat), data=EM_data, REML = FALSE)

summary(model3)
tab_model(model3)

model4<-lmer(Percent_Correct~1+Context_Dummy+SN_dummy+CN_dummy+THREE_TRAN_Dummy+FOUR_TRAN_Dummy+Context_Dummy:THREE_TRAN_Dummy+Context_Dummy:FOUR_TRAN_Dummy+(1+Context_Dummy+CN_dummy+SN_dummy|Rat), data=EM_data, REML = FALSE)

summary(model4)
tab_model(model4)

anova(model3,model4)

anova(model4,model1)


model5<-lmer(Percent_Correct~1+Context_Dummy+SN_dummy+CN_dummy+Context_Dummy:THREE_TRAN_Dummy+(1+Context_Dummy+CN_dummy+SN_dummy|Rat), data=EM_data, REML = FALSE)

summary(model5)

anova(model4,model5)
#null residual variance-current residual variance/null residual variance

Model1_Level1_Var<-((.05-.038)/.05)
Model1_Level1_Var


Model1_Level2_Var<-((.0010-.0011)/.0010)
Model1_Level2_Var


Model2_Level1_var<-((.05-.04)/.05)
Model2_Level2_var<-((.001035-.001955)/.001035)

Model2_Level2_var
Model2_Level1_var

#### Interaction Plots ####


plot_model(MLMLevel1Model_Fixed, type = "int", mdrt.values = "meansd", colors = "bw", title = "", axis.title = c("Trial Type", "p(correct)"), ci.lvl = NA)


?plot_model

plot_model(model3_fit, type = "int", mdrt.values = "meansd", colors = "bw", title = "", axis.title = c("Trial Type", "p(correct)"), ci.lvl = NA)

plot_model(MLMLevel1Model_Fixed, type = "int", mdrt.values = "minmax", colors = "bw", title = "", axis.title = c("Trial Type", "p(correct"))

plot_model(model4, type = "int", mdrt.values = "minmax", colors = "bw", title = "", axis.title = c("Trial Type", "p(correct"))

check_model(model1)
check_model(model2)
check_model(model3)
check_model(model4)

####Power####
library(ICC.Sample.Size)

calculateIccPower(.03,k=300,N=3,desiredPower = .80)
calculateIccSampleSize(p=.10,k=300)


