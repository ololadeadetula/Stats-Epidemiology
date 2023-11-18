# working directory
getwd()

SMHS<-read.csv("SMHS_sel.csv", header= TRUE)
head(SMHS)

# frequency of categorical variable
idfreq<-table(SMHS$id)
idfreq

deathfreq<-table(SMHS$death)
deathfreq

ihddeathfreq<-table(SMHS$IHD_death)
ihddeathfreq

strokedeathfreq<-table(SMHS$Stroke_death)
strokedeathfreq

lungdeathfreq<-table(SMHS$Lung_death)
lungdeathfreq

cancerdeathfreq<-(SMHS$Cancer_death)
cancerdeathfreq

crcdeathfreq<-(SMHS$CRC_death)
crcdeathfreq

stmfreq<-(SMHS$Stomach_death)
stmfreq

liverdfreq<-(SMHS$Liver_death)
liverdfreq

pandeathfreq<-table(SMHS$Pancreatic_death)
pandeathfreq

deathcausefreq<-table(SMHS$death_cause)
deathcausefreq

allvergcfreq<-table(SMHS$AllVegc)
allvergcfreq

redmeatcfreq<-table(SMHS$Redmeatc)
redmeatcfreq

educfreq<-table(SMHS$education)
educfreq

incomefreq<-table(SMHS$income)
incomefreq

pkyrscfreq<-table(SMHS$pk_yrsc)
pkyrscfreq

everdrkfreq<-table(SMHS$EverDrk)
everdrkfreq

eversmkfreq<-table(SMHS$EverSmk)
eversmkfreq

pametcfreq<-table(SMHS$pa_metc)
pametcfreq

fruitcfreq<-table(SMHS$Fruitc)
fruitcfreq

#Percentage of categorical variable
prop.table(idfreq)
prop.table(deathfreq)
prop.table(ihddeathfreq)
prop.table(strokedeathfreq)
prop.table(lungdeathfreq)
prop.table(cancerdeathfreq)
prop.table(crcdeathfreq)
prop.table(stmfreq)
prop.table(liverdfreq)
prop.table(pandeathfreq)
prop.table(deathcausefreq)
prop.table(allvergcfreq)
prop.table(redmeatcfreq)
prop.table(educfreq)
prop.table(incomefreq)
prop.table(pkyrscfreq)
prop.table(everdrkfreq)
prop.table(eversmkfreq)
prop.table(pametcfreq)
prop.table(fruitcfreq)

# Frequency of categorical variables by death
education.death<-table(SMHS$education,SMHS$death)
EverSmk.death<-table(SMHS$EverSmk,SMHS$death)
EverSmk.death

prop.table(EverSmk.death) # cell percentage
prop.table(EverSmk.death,1) # row percentage
prop.table(EverSmk.death,2) #column percentage

# Chisquare test
# Pearson chi-squared test for catergorical variable freq table
chisq.test(SMHS$EverSmk, SMHS$death)

#Calculating Mean of each continous variable
mean(SMHS[,"age_in"])
mean(SMHS[,"age_out"])
mean(SMHS[,"AllVeg"])
mean(SMHS[,"Redmeat"])
mean(SMHS[,"pa_met"])
mean(SMHS[,"BMI"])
mean(SMHS[,"WHR"])
mean(SMHS[,"Kcal"])
mean(SMHS[,"Age_intv"])
mean(SMHS[,"Fruit"])
# Calculating Median of each continous variable
median(SMHS[,"age_in"])
median(SMHS[,"age_out"])
median(SMHS[,"AllVeg"])
median(SMHS[,"Redmeat"])
median(SMHS[,"pa_met"])
median(SMHS[,"BMI"])
median(SMHS[,"WHR"])
median(SMHS[,"Kcal"])
median(SMHS[,"Age_intv"])
median(SMHS[,"Fruit"])
# Calculating Standard Deviation of each contionus variable
sd(SMHS[,"age_in"])
sd(SMHS[,"age_out"])
sd(SMHS[,"AllVeg"])
sd(SMHS[,"Redmeat"])
sd(SMHS[,"pa_met"])
sd(SMHS[,"BMI"])
sd(SMHS[,"WHR"])
sd(SMHS[,"Kcal"])
sd(SMHS[,"Age_intv"])
sd(SMHS[,"Fruit"])

#Calculating Mean of each continous variable, by death
aggregate(SMHS$AllVeg,by=list(SMHS$death),FUN=mean)
aggregate(SMHS[,c(2,6)],by=list(SMHS$death),FUN=mean)

ff<-function(x) {
  c(Mean=mean(x),SD=sd(x))
}
aggregate(SMHS$AllVeg,by=list(SMHS$death),FUN=ff)
aggregate(SMHS[,c(2,6)],by=list(SMHS$death),FUN=ff)
# student t-test
t.test(SMHS$BMI~SMHS$EverSmk)


