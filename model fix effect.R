# Install libraries for Negative Binomial Regression 
install.packages("MASS") # Negative Binomial GLM

# Install libraries for model comparison 
install.packages("performance") # model performance
install.packages("lmtest") # Log-likelihood test

# Install libraries for Spatial Regression 
install.packages("rgdal")
install.packages("rgeos")

# Install libraries for Spatial Weighted Regression
install.packages("spgwr")
install.packages("sp")
install.packages("sf")
install.packages("spdep")
install.packages("spatialreg")  # spatial regression
install.packages("spsur") # Spatial Autoregressive terms and Spatial Autoregressive Disturbance
install.packages("terra") # dependency for spdep
install.packages('spDataLarge',repos='https://nowosad.github.io/drat/', type='source')`# dependency for spdep
install.packages("spdep") # for spatial weights generation

# Install libraries for plots
install.packages('see')
install.packages("ggplots")


# Load libraries 
library(readxl) # read excel files
library(see) # plot results
library(gridExtra)

library (MASS) # negative binomial GLM
library (sandwich) # robust errors
library(performance) # model performance
library(lmtest) #Log likelihood ratio test

# Install libraries for Spatial Weighted Regression
library(sp)
library(sf)
library (rgdal) # read the shp file
library (terra) # dependecy for spdep
library(spdep) # for spatial weights generation
library (spatialreg)  # spatial regression
library (sphet) # spatial lag and error HET
library(spsur) #for sarar
library(spgwr) #Geographically Weighted Regression

# Working directory
setwd("C:/Users/Norbert/OneDrive/A03. Cornel, Codruta - Depentent urbanization/02. Model")

#Sets the number of digits to 3, and scientific notation to 7
options(digits=3, scipen=3)

modeldiff <- read_excel("C:\\Users\\Norbert\\OneDrive\\A03. Cornel, Codruta - Depentent urbanization\\02. Model\\model fix efects pract diff.xlsx", sheet = "model")
model2018 <- read_excel("C:\\Users\\Norbert\\OneDrive\\A03. Cornel, Codruta - Depentent urbanization\\02. Model\\model fix effects pract 2018.xlsx", sheet = "model 2018")
names(modeldiff)
names(model2018)

eq1 = PopAdj ~ D_EmpForeign + D_EmpLocal + D_EmpPublic + D_EmpForeign_BusServ_Core + D_EmpLocal_BusServ_Core + D_EmpForeign_BusServ_CoreofPeri + D_EmpLocal_BusServ_CoreofPeri + D_EmpForeign_BusServ_Periurban + D_EmpLocal_BusServ_Periurban + D_EmpForeign_BusServ_Village + D_EmpLocal_BusServ_Village + D_EmpForeign_Man_Core + D_EmpLocal_Man_Core + D_EmpForeign_Man_CoreofPeri + D_EmpLocal_Man_CoreofPeri + D_EmpForeign_Man_Periurban + D_EmpLocal_Man_Periurban + D_EmpForeign_Man_Village + D_EmpLocal_Man_Village + D_PIT_USDdef + D_Turnover_USDdef  + offset(Pop2011Offset)
eq1A = PopAdj ~ D_EmpForeign + D_EmpLocal + D_EmpPublic + D_EmpForeign_BusServ_Core + D_EmpLocal_BusServ_Core + D_EmpForeign_BusServ_CoreofPeri + D_EmpLocal_BusServ_CoreofPeri + D_EmpForeign_BusServ_Periurban + D_EmpLocal_BusServ_Periurban + D_EmpForeign_BusServ_Village + D_EmpLocal_BusServ_Village + D_EmpForeign_Man_Core + D_EmpLocal_Man_Core + D_EmpForeign_Man_CoreofPeri + D_EmpLocal_Man_CoreofPeri + D_EmpForeign_Man_Periurban + D_EmpLocal_Man_Periurban + D_EmpForeign_Man_Village + D_EmpLocal_Man_Village + D_PIT_USDdef + D_Turnover_USDdef  + offset(Pop2011Offset)
eq2 = PopAdj ~ D_EmpLocal + D_EmpPublic + D_EmpForeign_BusServ_Core + D_EmpLocal_BusServ_Core + D_EmpForeign_BusServ_CoreofPeri + D_EmpLocal_BusServ_CoreofPeri + D_EmpLocal_BusServ_Periurban + D_EmpForeign_Man_CoreofPeri + D_EmpForeign_Man_Periurban + D_EmpForeign_Man_Village + D_PIT_USDdef  + offset(Pop2011Offset)
eq3 = PopAdj ~ EmpForeign + EmpLocal + EmpPublic + EmpForeign_BusServ_Core + EmpLocal_BusServ_Core + EmpForeign_BusServ_CoreofPeri + EmpLocal_BusServ_CoreofPeri + EmpForeign_BusServ_Periurban + EmpLocal_BusServ_Periurban + EmpForeign_BusServ_Village + EmpLocal_BusServ_Village + EmpForeign_Man_Core + EmpLocal_Man_Core + EmpForeign_Man_CoreofPeri + EmpLocal_Man_CoreofPeri + EmpForeign_Man_Periurban + EmpLocal_Man_Periurban + EmpForeign_Man_Village + EmpLocal_Man_Village + D_PIT_USDdef + D_Turnover_USDdef + offset(Pop2011Offset)
eq3A = PopAdj ~ EmpForeign + EmpLocal + EmpPublic + EmpForeign_BusServ_Core + EmpLocal_BusServ_Core + EmpForeign_BusServ_Periurban + EmpLocal_BusServ_Periurban + EmpForeign_BusServ_Village + EmpLocal_BusServ_Village + EmpForeign_Man_Core + EmpLocal_Man_Core + EmpForeign_Man_Periurban + EmpLocal_Man_Periurban + EmpForeign_Man_Village + EmpLocal_Man_Village + D_PIT_USDdef + D_Turnover_USDdef + offset(Pop2011Offset)
eq4 = PopAdj ~ EmpLocal + EmpPublic + EmpForeign_BusServ_Core + EmpLocal_BusServ_Core + EmpForeign_BusServ_CoreofPeri + EmpLocal_BusServ_Periurban + EmpForeign_Man_CoreofPeri + EmpForeign_Man_Periurban + EmpForeign_Man_Village + D_PIT_USDdef + offset(Pop2011Offset)

eq5 = LN_Pop2018_Pop2011 ~ D_EmpForeign + D_EmpLocal + D_EmpPublic + D_EmpForeign_BusServ_Core + D_EmpLocal_BusServ_Core + D_EmpForeign_BusServ_CoreofPeri + D_EmpLocal_BusServ_CoreofPeri + D_EmpForeign_BusServ_Periurban + D_EmpLocal_BusServ_Periurban + D_EmpForeign_BusServ_Village + D_EmpLocal_BusServ_Village + D_EmpForeign_Man_Core + D_EmpLocal_Man_Core + D_EmpForeign_Man_CoreofPeri + D_EmpLocal_Man_CoreofPeri + D_EmpForeign_Man_Periurban + D_EmpLocal_Man_Periurban + D_EmpForeign_Man_Village + D_EmpLocal_Man_Village + D_PIT_USDdef + D_Turnover_USDdef
eq5A = LN_Pop2018_Pop2011 ~ D_EmpForeign + D_EmpLocal + D_EmpPublic + D_EmpForeign_BusServ_Core + D_EmpLocal_BusServ_Core + D_EmpForeign_BusServ_Periurban + D_EmpLocal_BusServ_Periurban + D_EmpForeign_BusServ_Village + D_EmpLocal_BusServ_Village + D_EmpForeign_Man_Core + D_EmpLocal_Man_Core + D_EmpForeign_Man_Periurban + D_EmpLocal_Man_Periurban + D_EmpForeign_Man_Village + D_EmpLocal_Man_Village + D_PIT_USDdef
eq6 = LN_Pop2018_Pop2011 ~ D_EmpLocal + D_EmpPublic + D_EmpForeign_BusServ_Core + D_EmpLocal_BusServ_Core + D_EmpForeign_BusServ_CoreofPeri + D_EmpLocal_BusServ_CoreofPeri + D_EmpLocal_BusServ_Periurban + D_EmpForeign_Man_CoreofPeri + D_EmpForeign_Man_Periurban + D_EmpForeign_Man_Village + D_PIT_USDdef 
eq7 = LN_Pop2018_Pop2011 ~ EmpForeign + EmpLocal + EmpPublic + EmpForeign_BusServ_Core + EmpLocal_BusServ_Core + EmpForeign_BusServ_Periurban + EmpLocal_BusServ_Periurban + EmpForeign_BusServ_Village + EmpLocal_BusServ_Village + EmpForeign_Man_Core + EmpLocal_Man_Core + EmpForeign_Man_Periurban + EmpLocal_Man_Periurban + EmpForeign_Man_Village + EmpLocal_Man_Village + D_PIT_USDdef + D_Turnover_USDdef
eq7A = LN_Pop2018_Pop2011 ~ EmpForeign + EmpLocal + EmpPublic + EmpForeign_BusServ_Core + EmpLocal_BusServ_Core + EmpForeign_BusServ_Periurban + EmpLocal_BusServ_Periurban + EmpForeign_BusServ_Village + EmpLocal_BusServ_Village + EmpForeign_Man_Core + EmpLocal_Man_Core + EmpForeign_Man_Periurban + EmpLocal_Man_Periurban + EmpForeign_Man_Village + EmpLocal_Man_Village + D_PIT_USDdef
eq8 = LN_Pop2018_Pop2011 ~ EmpLocal + EmpPublic + EmpForeign_BusServ_Core + EmpLocal_BusServ_Core + EmpForeign_BusServ_CoreofPeri + EmpLocal_BusServ_CoreofPeri + EmpLocal_BusServ_Periurban + EmpForeign_Man_CoreofPeri + EmpForeign_Man_Periurban + EmpForeign_Man_Village + D_PIT_USDdef

  
model1 <- glm.nb(eq1, data= modeldiff)
summary (model1)
model1A <- glm.nb(eq1A, data= modeldiff)
summary (model1A)

model2 <- glm.nb(eq2, data= modeldiff)
summary (model2)

model3 <- glm.nb(eq3, data= model2018)
summary (model3)
model3A <- glm.nb(eq3A, data= model2018)
summary (model3A)

model4 <- glm.nb(eq4, data= model2018)
summary (model4)

# Model comparison
#-------------------------------------------------------------------------
anova (model1, model1A, model3, model3A)
results <- compare_performance (model1, model1A, model3, model3A, rank = TRUE)
results
plot(results)
compare_performance (model1, model1A, model3, model3A)
logLik(model1)
logLik(model1A)logLik(model3)
logLik(model3A)
lrtest (model1, model1A, model3, model3A) 

# Spatial data preparation
#-------------------------------------------------------------------------
uat = readOGR(dsn = ".", layer = "UAT-uri") #citeste shp UAT-uri
names(uat) #afiseaza variabilele
uat$SIRUTA=as.character(uat$SIRUTA) # recodeaza SIRUTA in dimensiune
spplot(uat,"SIRUTA") #afiseaza harta dupa siruta

# Lists of spatial weights from the shape file DIFF
#------------------------------------------------------------
colnames(modeldiff)[1] <- "SIRUTA" #make sure the key has the same name in both tables
spat.diff <- merge(uat, modeldiff, by = "SIRUTA", all.x=F, all.y=F) #merge files
queen.diff=poly2nb(spat.diff) #neighbours list with single shared boundary point 
queen.listw.diff=nb2listw(queen.diff) #converts neighbours list to listwise oject type
listw.diff= queen.listw.diff #short heand renameing

# Lists of spatial weights from the shape file CROSSSECT2018
#------------------------------------------------------------
colnames(model2018)[1] <- "SIRUTA" #make sure the key has the same name in both tables
spat.2018 <- merge(uat, model2018, by = "SIRUTA", all.x=F, all.y=F) #merg files
queen.2018=poly2nb(spat.2018) #neighbours list with single shared boundary point 
queen.listw.2018=nb2listw(queen.2018) #converts neighbours list to listwise oject type
listw.2018= queen.listw.2018 #short heand renameing


# Spatial models
#-------------------------------------------------------------------------

## FIRST DIFFERENCING MODELS
#Spatial model specification for Model 1, 1A, 3, 3A
#------------------------------------------------------------
model1.spat=glm.nb(eq1, data=spat.diff)
model1A.spat=glm.nb(eq1A, data=spat.diff)
model3.spat=glm.nb(eq3, data=spat.2018)
model3A.spat=glm.nb(eq3A, data=spat.2018)

#Moran test for Model 1, 1A, 3, 3A
#------------------------------------------------------------
lm.morantest(model1.spat,listw.diff)
lm.morantest(model1A.spat,listw.diff)
lm.morantest(model3.spat,listw.2018)
lm.morantest(model3A.spat,listw.2018)

#Lagrange multiplier test Model 1, 1A, 3, 3A
#------------------------------------------------------------
lm.LMtests(model1.spat,listw.diff,test="all")
lm.LMtests(model1A.spat,listw.diff,test="all")
lm.LMtests(model3.spat,listw.2018,test="all")
lm.LMtests(model3A.spat,listw.2018,test="all")

#OLS ML
#------------------------------------------------------------
model7A.ols=lm(eq7A,data=spat.2018)
summary(model7A.ols)

#SARAR HET
#------------------------------------------------------------
model7A.het=sphet::gstslshet(eq7A, data=spat.2018,listw.2018)
summary (model7A.het, digits=3)
model7A.het.R2<-1-sum(model7A.het$residuals^2)/(sum((spat.2018$LN_Pop2018_Pop2011-mean(spat.2018$LN_Pop2018_Pop2011))^2))
print(c('R square',model7A.het.R2))  

model7A.het.pred <- data.frame(spat.2018$SIRUTA, model7A.het$yhat)
write.csv (model7A.het.pred, file="C:/Users/Norbert/OneDrive/A03. Cornel, Codruta - Depentent urbanization/02. Model/model7A.het.yhat.csv")
model7A.het

models <- list(model1, model1A, model3, model3A)
aictab(cand.set = models)


#SUR-SARAR
#------------------------------------------------------------
# library SPATIALREG
model7A.spatialreg <- spatialreg::sacsarlm(formula = eq7A, listw = listw.2018, data = spat.2018)
summary(model7A.spatialreg)

# library SPSUR
model7A.spsur <- spsur::spsurml(formula = eq7A, listw = listw.2018, type ="sarar", data = spat.2018)
summary(model7A.spsur)

#save Yhat
model7A.spsur.pred <- data.frame (spat.2018$SIRUTA, model7A.spsur$fitted.values)
write.csv (model7A.spsur.pred, file="C:/Users/Norbert/OneDrive/A03. Cornel, Codruta - Depentent urbanization/02. Model/model7A.sarar.pred.csv")


#Geographicaly Weighted regression
#------------------------------------------------------------
g.adapt.gauss <- gwr.sel(eq7A, data=spat.2018, adapt=TRUE)
model7A.gwr <- gwr(eq7A, data=spat.2018, adapt=g.adapt.gauss, bandwidth = g.adapt.gauss,  gweight = gwr.Gauss, hatmatrix = TRUE)
model7A.gwr
pairs(as(model7A.gwr$SDF, "data.frame")[,2:16], pch=".")
brks <- c(-0.15, -0.055, -0.04, -0.022, 0.35)
cols <- grey(5:2/6)
plot(res.adpt$SDF, col=cols[findInterval(model7A.gwr$SDF$pred, brks, all.inside=TRUE)])

#Rsquare
model7A.gwr.R2<-1-sum(model7A.gwr$lm$residuals^2)/(sum((spat.2018$LN_Pop2018_Pop2011-mean(spat.2018$LN_Pop2018_Pop2011))^2))
print(c('R square',model7A.gwr.R2))  

#save Yhat
model7A.gwr.pred <- data.frame (spat.2018$SIRUTA, model7A.gwr$SDF)
write.csv (model7A.gwr.pred, file="C:/Users/Norbert/OneDrive/A03. Cornel, Codruta - Depentent urbanization/02. Model/model7A.gwr.pred.csv")
