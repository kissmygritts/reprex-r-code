#########
# CLEAR WORKSPACE
##########

rm(list=ls())

#########
# LOAD PACKAGES
#########

#Load LME4 Package

library(lme4)
library(bbmle)
library(glmmTMB)
library(DHARMa)
library(car)
library(lme4)
library(buildmer)

############
# LOAD DATA
############

#Import data, Summarize Data

df=read.csv(file="FinalAbandonmentDataset_2-14-20.csv", header=T)
summary(df)


#########
# PROCESS DATA
#########

###  Ensure that factors are properly specified

names(df)

df$ID=as.factor(df$ID)
df$momID=as.factor(df$MomID)
df$year=factor(df$Year,levels=c("2016","2017","2018","2019"))
df$studyarea=as.factor(df$StudyArea)
df$PA=as.factor(df$PerceivedAbandonment)
df$NFA=as.factor(df$NeonateFleeUponApproach)
df$NASDH=as.factor(df$NeonateAppearStressedDuringHandling)
df$wind=as.factor(df$Wind)
df$species=as.factor(df$Species)
df$M13D=as.factor(df$Mort1st3Days)
df$NSUC=as.factor(df$NeonateStruggledUponCapture)
df$NFUR=as.factor(df$NeonateFleeUponRelease)
df$gloves=as.factor(df$Gloves)

#Converting DOB variable to a Julian Date format

tmp<- as.POSIXlt(df$DOB, format = "%m/%d/%Y")
tmp$yday
df$JD<- tmp$yday
df$JD
head(df)
names(df)

#################
##
##  Attempt to standardize by study area and species; only for the variables listed
##
#################

stand_table <- data.frame(name1=character(4),name2="",orig_mean=NA,orig_sd=NA,stringsAsFactors = F)
stand_table$name1a <- c('stand_weight','stand_JD', 'stand_decfat', 'stand_marfat')

stand_table$name2a <- c('Weight (kg)', 'Julian Date of Birth', 'December IFBFat', 'March IFBFat')

stand_table$name3a <- c('NeonateWeight','JD', 'DecFat', 'MarFat')

which(is.na(df))


# This is what I tried 1st, but it has a problem with the standard deviation one
# The goal was to then throw it in a forloop and standardize each row
# An example of the forloop is at line 139

aggmean <- aggregate(df, by = list(df$studyarea), FUN = mean, na.rm = TRUE)
print(aggmean)

aggsd <- aggregate(df, by = list(df$studyarea), FUN = sd, na.rm = TRUE)
print(aggsd)

for(i in 1:nrow(stand_table)){
  var1a <- stand_table$name1a[i]
  var2a <- stand_table$name3a[i]
  df[[var1a]]=(df[[var2a]]-stand_table$aggmean[i])/stand_table$aggsd[i]
  
}


library(dplyr)

# continuous <- select (df, MomAge, MarFat, DecFat, JD, NeonateAgeAtCapture,
#                       HandlingTime, NeonateWeight, MetatarsusLength, ChestGirth,
#                       BodyLength, Collectors, Bleats, Area_Time) 
# 
# names(continuous)


df %>% 
  group_by(studyarea) %>%
  summarize(mean_SA = mean(, na.rm = TRUE))

summary(df)
write.csv(aggsd, "testing.csv")




#IGNORE PAST HERE, THIS WORKS
#Standardize Variables not including Categorical variables; Using "slope_degree" instead of "slope_percent" for all models


stand_table <- data.frame(name1=character(6),name2="",orig_mean=NA,orig_sd=NA,stringsAsFactors = F)
stand_table$name1 <- c('stand_handletime','stand_momage', 'stand_collectors', 'stand_bleats', 
                       'stand_neonateage', 'stand_areatime')

stand_table$name2 <- c('Handling Time (Minutes)', 'Mother Age', 'Number of Collectors', 
                       'Neonate Bleats', 'Neonate Age at Capture (Hours)', 'Time in the Area (Minutes')

stand_table$name3 <- c('HandlingTime','MomAge', 'Collectors', 'Bleats','NeonateAgeAtCapture', 'Area_Time')

#Replace Nas in dataset with mean values of that column


# for(m in 1:nrow(df)){
#   df[is.na(df[,m]), m] <- mean(df[,m], na.rm = TRUE)
# }

which(is.na(df))

names(df)
i=1
for(i in 1:nrow(stand_table)){
  var1 <- stand_table$name1[i]
  var2 <- stand_table$name3[i]
  stand_table$orig_mean[i] <- mean(df[[var2]], na.rm = TRUE)
  stand_table$orig_sd[i] <- sd(df[[var2]], na.rm = TRUE)
  df[[var1]]=(df[[var2]]-stand_table$orig_mean[i])/stand_table$orig_sd[i]
}

summary(df)

# Change NAs to mean values

for(m in 1:nrow(df)){
  df[is.na(df[,m]), m] <- mean(df[,m], na.rm = TRUE)
}

summary(df)

write.csv(df, "test.csv")


#Looking into the NAs in the dataset
#mean(df$Area_Time, na.rm = TRUE)
#is.na(df$Area_Time)



##########
# Visualize data
##########
i=1
for(i in 1:nrow(stand_table)){
  hist(df[[stand_table$name3[i]]],xlab=stand_table$name3[i])       
  hist(df[[stand_table$name1[i]]],xlab=stand_table$name1[i])
}



table(df$year)

table(df$ID)

table(df$Sex)

table(df$PA)
table(df$M13D)
nrow(df)

summary(df)

########
# Build full model
########
names(df)

#Random Intercepts, random slopes, interactions; could we put a quadratic on age variables?
mod1 <- glm(M13D ~ Stand_Weight + studyarea + species + Sex + year + #gloves + #wind +
                 stand_DecFat + stand_JD + stand_neonateage + stand_bleats + #NSUC + NFA + #NASDH +  
                 stand_areatime + stand_collectors  + stand_handletime + stand_MarFat + #NFUR +
                 stand_momage, df , family = binomial(link = "logit"))

summary(mod1)

#Test for multicollinearity

library(GGally)
ggpairs(mod1)

#####  Dredge the GLM Model  #####
#Making NAs fail, probably not needed twice though
options(na.action = na.fail)

#Getting MuMIn package
require(MuMIn)

stand_handletime2  <- (df$stand_handletime^2)

#Creating another Global Model
options(na.action = na.fail)

Dredge4 <- glm(M13D ~ Stand_Weight + #studyarea + species + Sex + year + #gloves + #wind +
  stand_DecFat + stand_JD + stand_neonateage + stand_bleats + #NSUC + NFA + #NASDH +  
  stand_areatime + stand_collectors  + stand_handletime + stand_handletime2 + stand_MarFat + #NFUR +
  stand_momage + stand_handletime:stand_momage + stand_handletime:stand_areatime + stand_momage +
  #stand_handletime:Stand_Weight + stand_handletime:stand_DecFat + stand_handletime:stand_neonateage + 
  #stand_handletime:stand_collectors + stand_handletime:stand_JD + stand_handletime:stand_MarFat + 
  #stand_areatime:stand_neonateage + stand_areatime:stand_momage + stand_areatime:stand_DecFat + 
  stand_areatime:stand_MarFat + stand_areatime:Stand_Weight, df , family = binomial(link = "logit"))

summary(Dredge4)
#Run the dredge function on global model

a.dred4<- dredge(Dredge4, trace = TRUE, rank = "AICc")

a.dred4

write.csv(a.dred4, file="DredgeResults_21b.csv")



plot(a.dred4)
Avg.4c <- model.avg(a.dred4, subset = delta < 4)
# summary(Avg.4)
# summary(Avg.4b)
summary(Avg.4c)

confset.95p <- get.models(a.dred2, cumsum(weight) <= .95)
avgmod.95p <- model.avg(confset.95p)
summary(avgmod.95p)
confint(avgmod.95p)

##  Test goodness-of-fit

test.mod1 <- DHARMa::simulateResiduals(mod1,n=300)
plot(test.mod1)                        
DHARMa::testUniformity(test.mod1)      # pass
DHARMa::testResiduals(test.mod1)       # fails dispersion and outlier tests
DHARMa::testZeroInflation(test.mod1)   # pass

###########
# MODEL SELECTION (just takes a few minutes)
###########

bestmod <- buildmer::buildglmmTMB(Used~stand_elev+stand_elev:season+stand_elev:sex+stand_slope+stand_slope:season+stand_rugged21+
                                    stand_fire+stand_fire:sex+stand_fire:season+stand_cosaspect+stand_cosaspect:season+stand_sinaspect+
                                    stand_road+stand_road:season+stand_dist2water+stand_dist2water:season+ #+stand_coastalsage+stand_coastalsage:season+
                                    stand_xeric+stand_xeric:season+stand_mesic+stand_mesic:season+stand_oak+stand_oak:season+stand_conifer
                                  +stand_conifer:season#+stand_annualgrass
                                  +(1|ID)+(1|year),
                                  df,
                                  family= binomial(link = "logit"))
bestmod2 <- bestmod@model
summary(bestmod2)


##  Test goodness-of-fit again

test.bestmod <- DHARMa::simulateResiduals(bestmod2,n=300)
plot(test.bestmod)                        
DHARMa::testUniformity(test.bestmod)      # pass
DHARMa::testResiduals(test.bestmod)       # fails dispersion and outlier tests
DHARMa::testZeroInflation(test.bestmod)   # pass


allvars <- c("Used","stand_elev","stand_slope","stand_rugged21",
             "stand_fire","sex","season","year","stand_cosaspect","stand_sinaspect",
             "stand_road","stand_dist2water",#"stand_coastalsage",
             "stand_xeric","stand_mesic","stand_oak","stand_conifer",
             "ID")



###############
# INTERPRET AND VISUALIZE BEST MODEL
###############


#########
# univariate plots
#########

allvars

VisualizeRelation(df,bestmod2,"stand_elev",allvars,stand_table)
VisualizeRelation(df,bestmod2,"stand_slope",allvars,stand_table)   # not a strong relationship
VisualizeRelation(df,bestmod2,"stand_rugged21",allvars,stand_table)
VisualizeRelation(df,bestmod2,"stand_fire",allvars,stand_table)

VisualizeRelation(df,bestmod2,"stand_cosaspect",allvars,stand_table)

VisualizeRelation(df,bestmod2,"stand_sinaspect",allvars,stand_table)
VisualizeRelation(df,bestmod2,"stand_road",allvars,stand_table)

VisualizeRelation(df,bestmod2,"stand_dist2water",allvars,stand_table)
#VisualizeRelation(df,bestmod2,"stand_coastalsage",allvars,stand_table)
VisualizeRelation(df,bestmod2,"stand_xeric",allvars,stand_table)
VisualizeRelation(df,bestmod2,"stand_mesic",allvars,stand_table)
VisualizeRelation(df,bestmod2,"stand_oak",allvars,stand_table)
VisualizeRelation(df,bestmod2,"stand_conifer",allvars,stand_table)


#########################################################
#CARET PACKAGE STUFF
#########################################################

###############

# Select important features using CARET package

###############

library(caret)
library(ranger)
library(randomForest)

#df <- collapse.data

names(df)

summary(df)

# df$ID=as.factor(df$ID)
# df$momID=as.factor(df$MomID)
# df$year=factor(df$Year,levels=c("2016","2017","2018","2019"))
# df$studyarea=as.factor(df$StudyArea)
# df$PA=as.factor(df$PerceivedAbandonment)
# df$NFA=as.factor(df$NeonateFleeUponApproach)
# df$NASDH=as.factor(df$NeonateAppearStressedDuringHandling)
# df$wind=as.factor(df$Wind)
# df$species=as.factor(df$Species)
# df$M13D=as.factor(df$Mort1st3Days)
# df$NSUC=as.factor(df$NeonateStruggledUponCapture)
# df$NFUR=as.factor(df$NeonateFleeUponRelease)
# df$gloves=as.factor(df$Gloves)

pred.vars <- c("stand_JD", "stand_handletime", "stand_DecFat", "stand_MarFat", "stand_momage",
               "stand_collectors", "stand_bleats", "stand_neonateage", "stand_areatime", 
               "Stand_Weight", "wind", "gloves")#, "species", "studyarea", "Sex", "year", "NSUC", "NFA")

resp.var <- "M13D"

all.vars <- c(pred.vars,resp.var)



formula <- as.formula(sprintf("%s~%s",resp.var,paste(pred.vars,collapse="+")))



df2 <- df[complete.cases(df[,all.vars]),all.vars]



summary(df2)



hist(df2$stand_handletime)

hist(df2$stand_weight)

# table(df2$Conflict.Dummy2)
# 
# table(df2$Income.Category)
# 
# hist(df$Political.Stability)
# 
# hist(df$Net.Energy)

# prepare training scheme

control <- caret::trainControl(method="repeatedcv", number=10, repeats=3)

# train the model

model <- caret::train(formula, data=df2, method = 'ranger',
                      
                      preProcess="scale", trControl=control, importance = 'impurity')   # method="ranger"

# estimate variable importance

importance <- caret::varImp(model, scale=FALSE)



# summarize importance

print(importance)

# plot variable importance

#jpeg(file = "M13D_VariableImportance_4.jpeg")
plot(importance, main = "Mortality 1st 3 Days")      
#dev.off()

# define the control using a random forest selection function

control <- caret::rfeControl(functions=rfFuncs, method="cv", number=10)

# run the RFE algorithm

results <- caret::rfe(df2[,pred.vars], df2$M13D, sizes=c(1:15), rfeControl=control)

# summarize the results

print(results)    # 3 variables selected  : Net energy, GDP and SDP

# list the chosen features

predictors(results)      

# plot the results and save them to the workspace

#jpeg(file = "M13D_CrossValidation_4.jpeg")
plot(results, type=c("g", "o"), main = "Perceived Abandonment")
#dev.off()


results$optVariables



###################################################################################################

#########
# LOAD FUNCTIONS
#########

# trim <- function (x) gsub("^\\s+|\\s+$", "", x)
# 
# VisualizeRelation <- function(data=df,model=bestmod2,predvar="stand_slope",allvars,stand_table){
#   len <- 100
#   
#   temp <- subset(stand_table,name1==predvar)
#   
#   predvar2 <- strsplit(predvar,"_")[[1]][2]
#   
#   dataclasses <- sapply(data,class)
#   
#   dim <- data[,predvar]
#   range <- seq(min(dim),max(dim),length=len)
#   
#   realmean <- temp$orig_mean
#   realsd <- temp$orig_sd
#   realname <- temp$name2
#   
#   newdata <- data.frame(temp=range)
#   names(newdata) <- c(predvar)
#   
#   othervars <- allvars[!allvars%in%c(predvar,"Used","season","sex")]
#   
#   
#   var = othervars[5]
#   for(var in othervars){
#     thisvar <- data[[var]]
#     if(is.factor(thisvar)){
#       tab <- table(thisvar)
#       vals <- names(tab)
#       levs <- levels(thisvar)
#       mostcom <- vals[which.max(tab)]
#       newvec <- factor(rep(mostcom,times=nrow(newdata)),levels=levs)
#       newdata[,var] <- newvec
#     }else{
#       newdata[,var] <- 0 #mean(thisvar)
#     }
#   }
#   
#   newdata$sex <- factor("Female",levels=levels(df$sex))
#   newdata$season <- factor("Summer",levels=levels(df$season))
#   
#   newdata_sex <- newdata
#   newdata_season <- newdata
#   
#   newdata_sex$sex <- factor("Male",levels=levels(df$sex))
#   newdata_sex <- rbind(newdata_sex,newdata)
#   
#   newdata_season$season <- factor("Fall",levels=levels(df$season))
#   newdata_season <- rbind(newdata_season,newdata)
#   
#   temp <- newdata
#   temp$season <- factor("Spring",levels=levels(df$season))
#   newdata_season <- rbind(newdata_season,temp)
#   
#   temp$season <- factor("Winter",levels=levels(df$season))
#   newdata_season <- rbind(newdata_season,temp)
#   
#   pred <- predict(model,newdata,se.fit=T,type="response")
#   pred_sex <- predict(model,newdata_sex,se.fit=T,type="response")
#   pred_season <- predict(model,newdata_season,se.fit=T,type="response")
#   
#   
#   mains <- setdiff(allvars,c("season","sex","ID","Used","year"))
#   seasonvars <- intersect(unique(unlist(strsplit(trim(strsplit(as.character(model$call)[2],"\\+")[[1]][grepl("season",(strsplit(as.character(model$call)[2],
#                                                                                                         "\\+")[[1]]))]),"\\:"))),mains)
#   
#   sexvars <- trim(sapply(strsplit(strsplit(as.character(model$call)[2],"\\+")[[1]][grep("sex:",strsplit(as.character(model$call)[2],"\\+")[[1]] )],"\\:"
#   ),function(t) t[2] ))
#   
#   par(mai=c(0.9,0.8,0.1,0.1))
#   
#   ylim_seas <- c(0,(range(pred_season$fit)[2]*1.5))
#   ylim <- c(0,(range(pred$fit)[2]*1.5))
#   
#   if(predvar%in%seasonvars){
#     
#     ndx_wtr <- which(newdata_season$season=="Winter")
#     plot(range,pred_season$fit[ndx_wtr],xlab=realname,ylab="Selection propensity",type="l",lwd=2,ylim=ylim_seas,xaxt="n")  # xaxt="n",
#     polygon(c(range,rev(range)),c(pred_season$fit[ndx_wtr]+pred_season$se.fit[ndx_wtr],rev(pred_season$fit[ndx_wtr]-pred_season$se.fit[ndx_wtr])),col=gray(0.8),lty=0)
#     
#     ndx_spr <- which(newdata_season$season=="Spring")
#     polygon(c(range,rev(range)),c(pred_season$fit[ndx_spr]+pred_season$se.fit[ndx_spr],rev(pred_season$fit[ndx_spr]-pred_season$se.fit[ndx_spr])),col=gray(0.8),lty=0)
#     
#     ndx_sum <- which(newdata_season$season=="Summer")
#     polygon(c(range,rev(range)),c(pred_season$fit[ndx_sum]+pred_season$se.fit[ndx_sum],rev(pred_season$fit[ndx_sum]-pred_season$se.fit[ndx_sum])),col=gray(0.8),lty=0)
#     
#     ndx_fal <- which(newdata_season$season=="Fall")
#     polygon(c(range,rev(range)),c(pred_season$fit[ndx_fal]+pred_season$se.fit[ndx_fal],rev(pred_season$fit[ndx_fal]-pred_season$se.fit[ndx_fal])),col=gray(0.8),lty=0)
#     
#     points(range,pred_season$fit[ndx_wtr],type="l",lty=1,lwd=2)
#     points(range,pred_season$fit[ndx_spr],type="l",lty=2,lwd=2)
#     points(range,pred_season$fit[ndx_sum],type="l",lty=3,lwd=2)
#     points(range,pred_season$fit[ndx_fal],type="l",lty=5,lwd=2)
#     ats <- seq(min(range),max(range),length=6)
#     axis(1,ats,labels = round(realmean+ats*realsd,2))
#     rug(jitter(data[seq(1,nrow(data),50),][[predvar]]), ticksize = 0.03, side = 1, lwd = 0.5, col = par("fg"))
#     
#     
#     legend("topleft",lty=c(1,2,3,5),legend=c("Winter","Spring","Summer","Fall"),bty="n")
#     
#   }else{
#     plot(range,pred$fit,xlab=realname,ylab="Selection propensity",type="l",lwd=2,ylim=ylim,xaxt="n")
#     points(range,pred$fit+pred$se.fit,type="l",lty=2)
#     points(range,pred$fit-pred$se.fit,type="l",lty=2)
#     ats <- seq(min(range),max(range),length=6)
#     axis(1,ats,labels = round(realmean+ats*realsd,2))
#     rug(jitter(data[seq(1,nrow(data),50),][[predvar]]), ticksize = 0.03, side = 1, lwd = 0.5, col = par("fg"))
#   }
#   
#   
#   svg(sprintf("pdplots_collar_%s.svg",predvar2),width=4.5,height = 4.5)
#   
#   if(predvar%in%seasonvars){
#     ndx_wtr <- which(newdata_season$season=="Winter")
#     plot(range,pred_season$fit[ndx_wtr],xlab=realname,ylab="Selection propensity",type="l",lwd=2,ylim=ylim_seas,xaxt="n")  # xaxt="n",
#     polygon(c(range,rev(range)),c(pred_season$fit[ndx_wtr]+pred_season$se.fit[ndx_wtr],rev(pred_season$fit[ndx_wtr]-pred_season$se.fit[ndx_wtr])),col=gray(0.8),lty=0)
#     
#     ndx_spr <- which(newdata_season$season=="Spring")
#     polygon(c(range,rev(range)),c(pred_season$fit[ndx_spr]+pred_season$se.fit[ndx_spr],rev(pred_season$fit[ndx_spr]-pred_season$se.fit[ndx_spr])),col=gray(0.8),lty=0)
#     
#     ndx_sum <- which(newdata_season$season=="Summer")
#     polygon(c(range,rev(range)),c(pred_season$fit[ndx_sum]+pred_season$se.fit[ndx_sum],rev(pred_season$fit[ndx_sum]-pred_season$se.fit[ndx_sum])),col=gray(0.8),lty=0)
#     
#     ndx_fal <- which(newdata_season$season=="Fall")
#     polygon(c(range,rev(range)),c(pred_season$fit[ndx_fal]+pred_season$se.fit[ndx_fal],rev(pred_season$fit[ndx_fal]-pred_season$se.fit[ndx_fal])),col=gray(0.8),lty=0)
#     
#     points(range,pred_season$fit[ndx_wtr],type="l",lty=1,lwd=2)
#     points(range,pred_season$fit[ndx_spr],type="l",lty=2,lwd=2)
#     points(range,pred_season$fit[ndx_sum],type="l",lty=3,lwd=2)
#     points(range,pred_season$fit[ndx_fal],type="l",lty=5,lwd=2)
#     ats <- seq(min(range),max(range),length=6)
#     axis(1,ats,labels = round(realmean+ats*realsd,2))
#     rug(jitter(data[seq(1,nrow(data),50),][[predvar]]), ticksize = 0.03, side = 1, lwd = 0.5, col = par("fg"))
#     
#     legend("topleft",lty=c(1,2,3,5),legend=c("Winter","Spring","Summer","Fall"),bty="n")
#     
#   }else{
#     plot(range,pred$fit,xlab=realname,ylab="Selection propensity",type="l",lwd=2,ylim=ylim,xaxt="n")
#     points(range,pred$fit+pred$se.fit,type="l",lty=2)
#     points(range,pred$fit-pred$se.fit,type="l",lty=2)
#     ats <- seq(min(range),max(range),length=6)
#     axis(1,ats,labels = round(realmean+ats*realsd,2))
#     rug(jitter(data[seq(1,nrow(data),50),][[predvar]]), ticksize = 0.03, side = 1, lwd = 0.5, col = par("fg"))
#   }
#   dev.off()
# }
# 
# 


