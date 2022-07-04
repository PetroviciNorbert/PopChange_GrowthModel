#-------------------------------------------------------------------
# Regression for population change between 2011 and 2018 in Romania
#-------------------------------------------------------------------
# 1. Load table with the modeled data
# 2. Regression equations: all and selected
#   - Dependent 1: percent change population 2011-2018
#   - Dependent 2: ln ratio population 2018/2011
#   - Dependent 3: Dependent: population 2018, offset ln population 2011 
# 3. Rregression models: 
#   - All independent & control variables 
#     * Model 1 ALL: OLS Dependent 1
#     * Model 2 ALL: OLS Dependent 2
#     * Model 3 ALL: ML NB Dependent 3
#   - Selected independent & control variables
#     * Model 1 SEL: OLS Dependent 1
#     * Model 2 SEL: OLS Dependent 2
#     * Model 3 SEL: ML NB Dependent 3
#   - Model comparisons
# 4. Load Romanian localities shp file (UAT) & merge with modeled data
# 5. Spatial regressions
#   - Model 2, Dependent: ln ratio population 2018/2011, OLS
#   - Model 2, OLS with local spatial lag
#   - Model 3, ML NB with local spatial lag
#   - Model 2, ML Identity link with gobal spatial lag
#   - Model 2, ML Identity link with spatial error
#   - Model comparisons
# 6. Geographical weighted negative binomila regression (under construction)
#-------------------------------------------------------------------

### Install packages if needed 
##-------------------------------------------------------------------
# Install vizualisation libraries for regresssion
install.packages("devtools")
devtools::install_github("jacob-long/jtools")
install.packages("ggstance")
install.packages("broom.mixed")
install.packages("huxtable")

# Install libraries for Negative Binomial Regression 
install.packages("MASS") # Negative Binomial GLM
install.packages("GWmodel") # Negative Binomial Geographicaly Weighted GLM

# Install libraries for Spatial Regression 
install.packages("spdep")
install.packages("rgdal")
install.packages("rgeos")
install.packages("spatialreg")
install.packages("sphet") # spatial lag and error HET

# Install libraries for model performance comparison
install.packages("performance") 

### Load libraries
##-------------------------------------------------------------------
library(psych) # regression
library(performance) # model performance
library (sandwich) # robust errors
library (kableExtra) # table outputs
library (jtools) #vizualization
library (rgdal) # read the shp file
library(spdep) # for spatial weights generation
library (spatialreg)  # spatial regression
library (MASS) # negative binomial GLM
library (GWmodel) # negative binomial Geo Weight GLM
library (sphet) # spatial lag and error HET

### SETTINGS
##------------------------------------------------------------

# Working directory
setwd("C:/Users/Norbert/OneDrive/A03. Cornel, Codruta - Depentent urbanization/PopChange_GrowthModel")

# Report Robust errors, VIFs and three digits
set_summ_defaults (digits = 3, robust=TRUE, vifs=TRUE)

#Sets the number of digits to 3, and scientific notation to 7
options(digits=3, scipen=7)


### LOAD DATA SET
##------------------------------------------------------------
model <- read.csv("model.csv")
View(model)
names(model)

### REGRESSION EQUATIONS 
##------------------------------------------------------------
## EXTENDED MODELS: ALL VARIABLES
##------------------------------------------------------------
# MODEL 1: OLS regression equation (identity scale) 
reg.eq1.all=prCh2011.2018.Pop ~ prPopAct2018.Emp.Foreign + prPopAct2018.Emp.Local + PrAct2018.PublicEmp + PrEmp2018.ComServ.Foreign_CoreCity + PrEmp2018.ComServ.Local_CoreCity + Core.PrEmp2018.ComServ.Foreign + Core.PrEmp2018.ComServ.Local + PrEmp2018.ComServ.Foreign_Periurban + PrEmp2018.ComServ.Local_Periurban + PrEmp2018.ComServ.Foreign_Village + PrEmp2018.ComServ.Local_Village + PrEmp2018.Manufacturing.Foreign_CoreCity + PrEmp2018.Manufacturing.Local_CoreCity + Core.PrEmp2018.Manufacturing.Foreign + Core.PrEmp2018.Manufacturing.Local + PrEmp2018.Manufacturing.Foreign_Periurban + PrEmp2018.Manufacturing.Local_Periurban + PrEmp2018.Manufacturing.Foreign_Village + PrEmp2018.Manufacturing.Local_Village + prCh2009.2019.PIT + prCh2011.2018.Turnover + Turnover.per.Emp.2018..adj.

# MODEL 2: OLS regression equation (ln scale) 
reg.eq2.all=Ln.Pop2018.2011 ~ prPopAct2018.Emp.Foreign + prPopAct2018.Emp.Local + PrAct2018.PublicEmp + PrEmp2018.ComServ.Foreign_CoreCity + PrEmp2018.ComServ.Local_CoreCity + Core.PrEmp2018.ComServ.Foreign + Core.PrEmp2018.ComServ.Local + PrEmp2018.ComServ.Foreign_Periurban + PrEmp2018.ComServ.Local_Periurban + PrEmp2018.ComServ.Foreign_Village + PrEmp2018.ComServ.Local_Village + PrEmp2018.Manufacturing.Foreign_CoreCity + PrEmp2018.Manufacturing.Local_CoreCity + Core.PrEmp2018.Manufacturing.Foreign + Core.PrEmp2018.Manufacturing.Local + PrEmp2018.Manufacturing.Foreign_Periurban + PrEmp2018.Manufacturing.Local_Periurban + PrEmp2018.Manufacturing.Foreign_Village + PrEmp2018.Manufacturing.Local_Village + prCh2009.2019.PIT + prCh2011.2018.Turnover + Turnover.per.Emp.2018..adj.

# MODEL 3: NB Regression equation (with offset Pop2011)
reg.eq3.all=Pop.2018 ~ prPopAct2018.Emp.Foreign + prPopAct2018.Emp.Local + PrAct2018.PublicEmp + PrEmp2018.ComServ.Foreign_CoreCity + PrEmp2018.ComServ.Local_CoreCity + Core.PrEmp2018.ComServ.Foreign + Core.PrEmp2018.ComServ.Local + PrEmp2018.ComServ.Foreign_Periurban + PrEmp2018.ComServ.Local_Periurban + PrEmp2018.ComServ.Foreign_Village + PrEmp2018.ComServ.Local_Village + PrEmp2018.Manufacturing.Foreign_CoreCity + PrEmp2018.Manufacturing.Local_CoreCity + Core.PrEmp2018.Manufacturing.Foreign + Core.PrEmp2018.Manufacturing.Local + PrEmp2018.Manufacturing.Foreign_Periurban + PrEmp2018.Manufacturing.Local_Periurban + PrEmp2018.Manufacturing.Foreign_Village + PrEmp2018.Manufacturing.Local_Village + prCh2009.2019.PIT + prCh2011.2018.Turnover + Turnover.per.Emp.2018..adj. + offset(Ln.Pop2011)

## SHORT MODELS: SELECTED VARIABLES
##------------------------------------------------------------
# MODEL 1: OLS regression equation (identity scale) 
reg.eq1.sel=prCh2011.2018.Pop ~ prPopAct2018.Emp.Local + PrAct2018.PublicEmp + PrEmp2018.ComServ.Foreign_CoreCity + Core.PrEmp2018.ComServ.Foreign + PrEmp2018.ComServ.Local_Periurban + PrEmp2018.Manufacturing.Foreign_CoreCity + Core.PrEmp2018.Manufacturing.Foreign + PrEmp2018.Manufacturing.Foreign_Periurban + PrEmp2018.Manufacturing.Foreign_Village + prCh2009.2019.PIT

# MODEL 2: OLS regression equation (ln scale) 
reg.eq2.sel=Ln.Pop2018.2011 ~ prPopAct2018.Emp.Local + PrAct2018.PublicEmp + PrEmp2018.ComServ.Foreign_CoreCity + Core.PrEmp2018.ComServ.Foreign + PrEmp2018.ComServ.Local_Periurban + PrEmp2018.Manufacturing.Foreign_CoreCity + Core.PrEmp2018.Manufacturing.Foreign + PrEmp2018.Manufacturing.Foreign_Periurban + PrEmp2018.Manufacturing.Foreign_Village + prCh2009.2019.PIT

# MODEL 3: NB Regression equation (with offset Pop2011)
reg.eq3.sel=Pop.2018 ~ prPopAct2018.Emp.Local + PrAct2018.PublicEmp + PrEmp2018.ComServ.Foreign_CoreCity + Core.PrEmp2018.ComServ.Foreign + PrEmp2018.ComServ.Local_Periurban + PrEmp2018.Manufacturing.Foreign_CoreCity + Core.PrEmp2018.Manufacturing.Foreign + PrEmp2018.Manufacturing.Foreign_Periurban + PrEmp2018.Manufacturing.Foreign_Village + prCh2009.2019.PIT + offset(Ln.Pop2011)

## NO INTERACTION MODELS: SELECTED VARIABLES
##------------------------------------------------------------
# MODEL 2: OLS regression equation (ln scale) 
reg.eq2.ni=Ln.Pop2018.2011 ~ prPopAct2018.Emp.Foreign + prPopAct2018.Emp.Local + PrAct2018.PublicEmp + PrEmp2018.Manufacturing.Foreign + PrEmp2018.Manufacturing.Local + PrEmp2018.ComServ.Foreign + PrEmp2018.ComServ.Local + prCh2009.2019.PIT

# MODEL 3: NB Regression equation (with offset Pop2011)
reg.eq3.ni=Pop.2018 ~ prPopAct2018.Emp.Foreign + prPopAct2018.Emp.Local + PrAct2018.PublicEmp + PrEmp2018.Manufacturing.Foreign + PrEmp2018.Manufacturing.Local + PrEmp2018.ComServ.Foreign + PrEmp2018.ComServ.Local + prCh2009.2019.PIT + offset(Ln.Pop2011)


###------------------------------------------------------------
### REGRESSION MODELS
###------------------------------------------------------------

## EXTENDED MODELS: ALL VARIABLES
##------------------------------------------------------------
# MODEL 1: OLS regression equation (identity scale) 
model1.all <- glm(reg.eq1.all, data= model)
summ (model1.all)

# MODEL 2: OLS regression equation (ln scale) 
model2.all <- glm(reg.eq2.all, data= model)
summ (model1.all)

# MODEL 3: NB Regression equation (with offset Pop2011)
model3.all <- glm.nb(reg.eq3.all, data= model, init.theta = 266.66, link = log)
summary (model3.all)

## SHORTEN MODELS: SELECTED VARIABLES
##------------------------------------------------------------
# MODEL 1: OLS regression equation (identity scale) 
model1.sel <- glm(reg.eq1.sel, data= model)
summ (model1.sel)

# MODEL 2: OLS regression equation (ln scale) 
model2.sel <- glm(reg.eq2.sel, data= model)
summ (model2.sel)

# MODEL 3: NB Regression equation (with offset Pop2011)
model3.sel <- glm.nb(reg.eq3.sel, data= model, init.theta = 264.93,link = log)
summary (model3.sel)

## COMPARE MODELS
##------------------------------------------------------------
# All variables
compare_performance (model1.all,model2.all,model3.all)

#Log Likelihood
logLik (model1.all)
logLik (model2.all)
logLik (model3.all)

#nagelkerke Pseudo r-square
r2_nagelkerke (model1.all)
r2_nagelkerke (model2.all)
r2_nagelkerke (model3.all)

# Selected variables
compare_performance (model1.sel,model2.sel,model3.sel)

#Log Likelihood
logLik (model1.sel)
logLik (model2.sel)
logLik (model3.sel)
#nagelkerke Pseudo r-square
r2_nagelkerke (model1.sel)
r2_nagelkerke (model2.sel)
r2_nagelkerke (model3.sel)

#------------------------------------------------------------
# SPATIAL REGRESSION
#------------------------------------------------------------

# Loading the spatial information
#------------------------------------------------------------
uat = readOGR(dsn = ".", layer = "UAT-uri") #citeste shp UAT-uri
names(uat) #afiseaza variabilele
summary(uat)
uat$SIRUTA=as.character(uat$SIRUTA) # recodeaza SIRUTA in dimensiune
spplot(uat,"SIRUTA") #afiseaza harta dupa siruta

# Join as intersection between UAT & model data 
#------------------------------------------------------------
colnames(model)[1] <- "SIRUTA" #make sure the key has the same name in both tables
spat.data <- merge(uat, model, by = "SIRUTA", all.x=F, all.y=F)
# Lists of spatial weights from the shape file 
#------------------------------------------------------------
queen.nb=poly2nb(spat.data) #neighbours list with single shared boundary point 
rook.nb=poly2nb(spat.data,queen=FALSE) #neighbours list with more than one shared point required
queen.listw=nb2listw(queen.nb) #converts neighbours list to listwise oject type
rook.listw=nb2listw(rook.nb) #convert neighbours list to listw oject type
listw1= queen.listw #short heand renameing

#Moran test for OLS with Y=ln(Pop2018/Pop2011) 
#------------------------------------------------------------
model2=lm(reg.eq2.ni,data=spat.data)
summary(model2)
lm.morantest(model2,listw1)
lm.LMtests(model2,listw1,test="all")

#OLS SLX: Local Spatial Lagged Model Y=ln(Pop2018/Pop2011) 
#------------------------------------------------------------
model2.local_lag=lmSLX(reg.eq2.ni,data=spat.data,listw1)
summary(model2.local_lag)
impacts(model2.local_lag,listw=listw1)
summary(impacts(model2.local_lag,listw=listw1),zstats=TRUE) #Add zstats,pvals;

#NB SLX : with local spatial lag
#------------------------------------------------------------
#Model 3 with selected variables, estimated on spat.data
model3.nb=glm.nb(reg.eq3.ni,data=spat.data)

#create lagged x's
x1=model.matrix(model3.nb) #x values used in NB regression 
lagx1=create_WX(x1,listw1,prefix="lagx") #create lagged X values, change name prepending "lagx."
spat.data2=cbind(spat.data,lagx1) #binds toghether the X and lag X

#Regression equation
reg.eq3.sel.lag=Pop.2018 ~ prPopAct2018.Emp.Local + PrAct2018.PublicEmp + PrEmp2018.ComServ.Foreign_CoreCity + Core.PrEmp2018.ComServ.Foreign + PrEmp2018.ComServ.Local_Periurban + PrEmp2018.Manufacturing.Foreign_CoreCity + Core.PrEmp2018.Manufacturing.Foreign + PrEmp2018.Manufacturing.Foreign_Periurban + PrEmp2018.Manufacturing.Foreign_Village + prCh2009.2019.PIT + lagx.prPopAct2018.Emp.Local + lagx.PrAct2018.PublicEmp + lagx.PrEmp2018.ComServ.Foreign_CoreCity + lagx.Core.PrEmp2018.ComServ.Foreign + lagx.PrEmp2018.ComServ.Local_Periurban + lagx.PrEmp2018.Manufacturing.Foreign_CoreCity + lagx.Core.PrEmp2018.Manufacturing.Foreign + lagx.PrEmp2018.Manufacturing.Foreign_Periurban + lagx.PrEmp2018.Manufacturing.Foreign_Village + lagx.prCh2009.2019.PIT + offset(Ln.Pop2011)

#The NB regression with local spatial lag 
model3.nb.local_lag=glm.nb(reg.eq3.sel.lag, data=spat.data2)
summary(model3.nb.local_lag)
r2_nagelkerke(model3.nb.local_lag)

# Impacts unfortunatlu works for glm only
impacts(model3.nb.local_lag)

#ML SAR: Global Spatial Lagged Model Y=ln(Pop2018/Pop2011)
#------------------------------------------------------------
model2.global_lag=lagsarlm(reg.eq2.ni,data=spat.data,listw1)
summary(model2.global_lag, Nagelkerke=TRUE)
summary(impacts(model2.global_lag,listw=listw1,R=500),zstats=TRUE) #Add zstats,pvals;

#ML SEM Spatial Error Model
#------------------------------------------------------------
model2.spat_error=errorsarlm(reg.eq2.sel,data=spat.data, listw1)
summary(model2.spat_error, Nagelkerke=TRUE)
Hausman.test(model2.spat_error)#Spatial Hausman Test

#Compare models
#------------------------------------------------------------
compare_performance (model2, model2.local_lag, model3.nb.local_lag, model2.global_lag, model2.spat_error)
logLik (model2)
logLik (model2.local_lag)
logLik (model3.nb.local_lag) 
logLik (model2.global_lag)
logLik (model2.spat_error)

#------------------------------------------------------------
# Geographicaly weighted -  nu am idee cum functioneaza
#------------------------------------------------------------
DM<-gw.dist(dp.locat=coordinates(londonhp))
bw.f2 <- bw.ggwr(reg.eq3.sel,data=spat.data, dMat=DM,family ="binomial")
res.binomial<-ggwr.basic(reg.eq3.sel, bw=listw1,data=spat.data, family ="binomial")

