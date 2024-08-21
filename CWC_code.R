#Birnir and Satana 2022
#The Challenger's Winning Coalition (CWC) Replication File
#Created 06-22
#This replication file contains: 
      #the code for loading the cross-sectional A-Religion primary religion variables and descriptive Figure 4. (Section 1)
      #the code for loading the longitudinal data and replicating the analysis in the paper - all remaining tables and figures. (Section 2)
      #the code for loading the data, formatting and merging all control variables to the CWC data. (Section 3)
################################################################################################################

#Packages

library("tidyverse")
library("stargazer")
library("sandwich")
library("lmtest")
library("margins")


#################################################

#SECTION 1: Replication of descriptive analysis (Figure 4) 

#################################################



##########
#Figure 4.  The number of socially relevant AMAR groups belonging to each of the coded Families of religions.
#########

load("CWC_Figure4.RData") 

# This is a cross-sectional list of AMAR groups coded for religion.
#Out of 1199 groups 29 groups do not have a primary family recorded leaving a total of 1170 groups 
#tabulated for primary religious family

#Counting AMAR group primary family of religion
ggplot(CWC_Figure41) + 
  geom_bar(mapping = aes(x = family.name, fill= family.name), show.legend = FALSE,
           stat = "count",
           position = "stack" )+
  coord_flip()+ #flips the coordinates
  theme_classic()+
  theme(axis.text = element_text(size=10),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

##################################################

#SECTION 2: Replication of the longitudinal analysis in the paper (all remaining figures and tables).

#################################################
rm(list=ls()) #Clearing workspace

load("df.CWC.622.Rdata")
#This is the longitudinal group level data used for analysis in the paper

#######
#Table 1. Summary statistics.
#######

summary.stats1=df.CWC.622%>%
          data.frame() 

stargazer(summary.stats1, type = "text", summary.stat = c("N","mean",  "sd","min", "max" ))

#######
#Table 2. Shared Family, Population Balance and Religious Incompatibility
#######

model1 = glm(Religious.incompatibility ~ Shared.family.ns.nd+Population.balance.nod,
             family = "binomial", data = df.CWC.622)

model2 = glm(Religious.incompatibility ~ Shared.family.ns.nd*Population.balance.nod,
             family = "binomial", data = df.CWC.622)

model3 = glm(Religious.incompatibility ~ Shared.family.ns.nd*Population.balance.nod+Organizational.competition+
               National.cross.cutting+ln.GDP.per.capita+Democracy+Separatism+T1+T2+T3,
             family = "binomial", data = df.CWC.622)


#SE corrections using sandwich 
model1.robust.SE=coeftest(model1, vcov = vcovHC(model1, sandwich=TRUE)) 
model2.robust.SE=coeftest(model2, vcov = vcovHC(model2, sandwich=TRUE))
model3.robust.SE=coeftest(model3, vcov = vcovHC(model3, sandwich=TRUE))

#Creating the table
stargazer(model1.robust.SE, model2.robust.SE, model3.robust.SE, title="Results", type = "text",align=TRUE)
#To retrieve the number of observations
stargazer(model1, model2, model3, title="Results", type = "text",align=TRUE)

#######
#Figure 5.  Marginal effect of Population Balance.
#######

#plot conditional marginal values 
cplot(model3, x="Population.balance.nod", data=df.CWC.622[df.CWC.622$Shared.family.ns.nd == 1, ],
      xlim = c(-.2,0.2),lty=2, lw=2, col = "red",se.fill = rgb(0,1,0,.5), xlab="Majority/Minority Population Balance", ylab="Probability of Religious Incompatibility")
cplot(model3, x="Population.balance.nod", data=df.CWC.622[df.CWC.622$Shared.family.ns.nd == 0, ],
      xlim = c(-.2,0.2),draw = "add", se.fill = rgb(0,0,1,.2))

#######
#Table 3.  Organizational Competition.
#######

#Subset the Data.  Groups with shared family only

Shared_family=df.CWC.622%>%
  filter(Shared.family.ns.nd==1)

#Analysis of the subset of groups with shared family only
model4 = glm(Religious.incompatibility ~ Population.balance.nod+Organizational.competition+
               National.cross.cutting+ln.GDP.per.capita+Democracy+Separatism+T1+T2+T3,
             family = "binomial", data = Shared_family)
model5 = glm(Religious.incompatibility ~ Population.balance.nod*Organizational.competition+
               National.cross.cutting+ln.GDP.per.capita+Democracy+Separatism+T1+T2+T3,
             family = "binomial", data = Shared_family)

model4.robust.SE=coeftest(model4, vcov = vcovHC(model4, sandwich=TRUE))
model5.robust.SE=coeftest(model5, vcov = vcovHC(model5, sandwich=TRUE))

#The final model using the full data including all groups
model6 = glm(Religious.incompatibility ~ Shared.family.ns.nd*Population.balance.nod*Organizational.competition+
               National.cross.cutting+ln.GDP.per.capita+Democracy+Separatism+T1+T2+T3,
             family = "binomial", data = df.CWC.622)

model6.robust.SE=coeftest(model6, vcov = vcovHC(model6, sandwich=TRUE))

#Creating the table
stargazer(model4.robust.SE, model5.robust.SE, model6.robust.SE, title="Results", type = "text",align=TRUE)
#Retrieving the number of observations
stargazer(model5, model6, model4, title="Results", type = "text",align=TRUE)

#######
#Figure 6.  Marginal effect of Organizational Competition.
#######

cplot(model6, x="Organizational.competition", data=df.CWC.622[df.CWC.622$Shared.family.ns.nd == 1, ],
      xlim = c(1,6),lty=2, lw=2, col = "red",se.fill = rgb(0,1,0,.5), xlab="Number of organizations representing the ethnic group", ylab="Probability of Religious Incompatibility")
cplot(model6, x="Organizational.competition", data=df.CWC.622[df.CWC.622$Shared.family.ns == 0, ],
      xlim = c(1,6),lty=1, lw=2, col = "black",draw = "add", se.fill = rgb(0,0,1,.2))

#################################################
#SECTION 3: Below is the code that was used to merge the control variables with the original data

#This is a 3 step process: 
#1: loading and formatting the control variables, 
#2: Merging the control variables into a single dataframe, 
#3: Merging the CWC data with the control variables.

#################################################
rm(list=ls()) #Clearing workspace


#1) LOAD AND FORMAT THE CONTROL DATASETS

#sambanis
sambanis=as.data.frame(
  read.csv("SDM.csv",
           stringsAsFactors = FALSE, 
           strip.white = TRUE, 
           sep = ',' )) %>%  
  filter(year>1974)%>% 
  select("ccode","year","violent")

#create variable of active separatism by country year
sambanis=sambanis%>%
  group_by(ccode, year, add=FALSE)%>%  
  summarize(violent = mean(violent))%>%  #average violent separatism by year by country
  ungroup()%>%
  mutate(violent1=ifelse(violent>0,1,0))%>%
  select("ccode","year","violent1")


#penn world
penn=read_excel("pwt90.xlsx")%>%
  select(countrycode, country, year, rgdpe, pop) %>%
  filter(rgdpe!="NA", year>1974)

#calculate the variable
penn=penn%>%
  mutate(rgdpe.percap=rgdpe/pop)%>%
  mutate(ln.rgdpe.percap=log(rgdpe/pop))%>%
  select("countrycode","year","ln.rgdpe.percap") #note countrycode here is ISO code


#xpolity/jones and lupu
xpolity=as.data.frame(
  read.csv("xpolity.csv",
           stringsAsFactors = FALSE, 
           strip.white = TRUE, 
           sep = ',' 
  ))%>%  
  filter(year>1974)%>%
  #missingness etc is accounted for with - 77 and -88 filter those out
  filter(x_polity!=-88) %>%
  filter(x_polity!=-77) %>%
  filter(x_polity!=-66)

#selway
selway=as.data.frame(
  read.csv("Selway.Aug.2014.155.Country.Dataset.csv",  
           stringsAsFactors = FALSE, 
           strip.white = TRUE, 
           sep = ',' 
  ))%>%  
  select("CCode","erc")%>% 
  filter(erc!="")

names(selway)[names(selway) == "CCode"] <- "ccode"


#Additionally brown and james are used as bridge in merging as penn only has iso codes and no ccodes.
#brown and james - 
brown_james=as.data.frame(read.dta("Religious_Characteristics_of_States_Dataset_Project_2015.dta") %>%
                            select("ccode","ISO3"))

names(brown_james)[names(brown_james) == "ISO3"] <- "countrycode"

#2)MERGING THE CONTROL VARIABLES

#penn with brown_james
p_bj= merge(penn, brown_james, by = c("countrycode"), all= TRUE) #note countrycode stands for ISO code

#add sambanis
p_bj_sa= merge(p_bj, sambanis, by = c("ccode", "year"), all= TRUE) 

#fill in the sambanis variable as 0 if missing because if missing there was no 
#violent SD in the country in that year 
p_bj_sa$violent1[is.na(p_bj_sa$violent1)] = 0

#add in xpolity
p_bj_sa_x= merge(p_bj_sa, xpolity, by = c("ccode", "year"), all= TRUE)

#add in selway
p_bj_sa_x_se= merge(p_bj_sa_x, selway, by = c("ccode"), all= TRUE)

save(p_bj_sa_x_se, file ="p_bj_sa_x_se.Rdata")

#3) MERGE CONTROLS WITH MAIN DATA

#all the control data are in a country-year format and thus merged by country year
#The CWC data-frame for merging (df.CWC.formerging)only differs from the CWC main dataframe 
#(df.CWC.622) in that it includes ccode and year variables.  These are dropped after the 
#merge to create the main data df.CWC.622

load("df.CWC.formerging.RData") 

df.CWC.622=merge(df.CWC.formerging, p_bj_sa_x_se, by = c("ccode", "year"), all= TRUE)%>% 
  filter(Religious.incompatibility!="")%>% 
  select("Religious.incompatibility", "Shared.family.ns.nd", "Population.balance.nod" ,
         "Organizational.competition", "National.cross.cutting",                        
         "ln.GDP.per.capita","Separatism" ,"Democracy",  "T1","T2","T3")


######################
#END