#Birnir and Satana 2022
#The Challenger's Winning Coalition (CWC) Replication File
#Created 07-22
#This replication file contains: 
#the code for loading the cross-sectional ######## FINISH loading or creating?
#the code for loading the longitudinal data including the variables only used in the supplementary meterials
# and replicating all of the analysis in the supplementary materials
################################################################################################################

#Packages

library("tidyverse")
library("stargazer")
library("sandwich")
library("lmtest")
library("margins")

#################################################
#putting together the data with the robustness check variables

load("/Users/hannahbirnir/Dropbox/Birnir_and_Satana/CWC_paper/CWC_JOP/df.joprnr12722.Rdata")
names(df.joprnr12722)

df.joprnr12722=df.joprnr12722%>%
  mutate(Uganda=ifelse(amar_country=="Uganda",1,0))

names(df.joprnr12722)

CWC_supplementary=df.joprnr12722%>%
  select("year","Religious.incompatibility", "Shared.family.ns.nd",  "Population.balance.nod",
         "Organizational.competition","National.cross.cutting",  "ln.GDP.per.capita", 
         "Democracy", "Separatism", "T1", "T2", "T3", "Shared.sect.nd", "Nepal", "Uganda")

#Checking the data
model3 = glm(Religious.incompatibility ~ Shared.family.ns.nd*Population.balance.nod+Organizational.competition+
               National.cross.cutting+ln.GDP.per.capita+Democracy+Separatism+T1+T2+T3,
             family = "binomial", data = df.CWC.622)
model3.robust.SE=coeftest(model3, vcov = vcovHC(model3, sandwich=TRUE))
stargazer(model3.robust.SE, title="Results", type = "text",align=TRUE)


model3.check = glm(Religious.incompatibility ~ Shared.family.ns.nd*Population.balance.nod+Organizational.competition+
               National.cross.cutting+ln.GDP.per.capita+Democracy+Separatism+T1+T2+T3,
             family = "binomial", data = CWC_supplementary)
model3.robust.SE.check=coeftest(model3.check, vcov = vcovHC(model3.check, sandwich=TRUE))
stargazer(model3.robust.SE.check, title="Results", type = "text",align=TRUE)

#The CWC_supplementary data are the same as the CWC.622, just with more variables

#################################################

#SECTION 1: Replication of descriptive analysis (Table 3) 

#################################################

Table 3. Cross-sectional T-tests, Minority/Majority Population Balance by Shared Religion
and Religious Incompatibility

LE 4. Cross-sectional T-tests, Minority/Majority Population Balance by Shared Religion
and Religious Incompatibili

#################################################

#SECTION 2: Replication of cross-national analysis with robustness checks

#################################################

model1s = glm(Religious.incompatibility ~ Shared.family.ns.nd*Population.balance.nod+Organizational.competition+
                     National.cross.cutting+ln.GDP.per.capita+Democracy+Separatism+T1+T2+T3+Nepal+Uganda,
                   family = "binomial", data = CWC_supplementary)
model1s.robust.SE=coeftest(model1s, vcov = vcovHC(model1s, sandwich=TRUE))
stargazer(model1s.robust.SE, title="Results", type = "text",align=TRUE)

model2s = glm(Religious.incompatibility ~ Shared.sect.nd*Population.balance.nod+Organizational.competition+
                National.cross.cutting+ln.GDP.per.capita+Democracy+Separatism+T1+T2+T3,
              family = "binomial", data = CWC_supplementary)
model2s.robust.SE=coeftest(model2s, vcov = vcovHC(model2s, sandwich=TRUE))
stargazer(model2s.robust.SE, title="Results", type = "text",align=TRUE)

names(CWC_supplementary)
