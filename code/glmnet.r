library(glmnet)
library(useful)
library(coefplot)
library(magrittr)

lots <- readRDS('data/manhattan_Train.rds')
dim(lots)
head(lots)
View(lots)

valueFormula <- TotalValue ~ FireService + 
    ZoneDist1 + ZoneDist2 + Class + LandUse + 
    OwnerType + LotArea + BldgArea + ComArea + 
    ResArea + OfficeArea + RetailArea + 
    GarageArea + FactryArea + NumBldgs + 
    NumFloors + UnitsRes + UnitsTotal + 
    LotFront + LotDepth + BldgFront + 
    BldgDepth + LotType + Landmark + BuiltFAR +
    Built + HistoricDistrict

valueFormula
class(valueFormula)

value1 <- lm(valueFormula, data=lots)
coefplot(value1, sort='magnitude')

valueFormula <- TotalValue ~ FireService + 
    ZoneDist1 + ZoneDist2 + Class + LandUse + 
    OwnerType + LotArea + BldgArea + ComArea + 
    ResArea + OfficeArea + RetailArea + 
    GarageArea + FactryArea + NumBldgs + 
    NumFloors + UnitsRes + UnitsTotal + 
    LotFront + LotDepth + BldgFront + 
    BldgDepth + LotType + Landmark + BuiltFAR +
    Built + HistoricDistrict - 1

lotsX <- build.x(valueFormula, data=lots, contrasts=FALSE, sparse=TRUE)
dim(lotsX)
lotsY <- build.y(valueFormula, data=lots)

value2 <- glmnet(x=lotsX, y=lotsY, family='gaussian')

value2$lambda

plot(value2, xvar='lambda')
coefpath(value2)

coef(value2)

coefplot(value2, sort='magnitude', lambda=500)
coefplot(value2, sort='magnitude', lambda=20000000)
coefplot(value2, sort='magnitude', lambda=2000000)
coefplot(value2, sort='magnitude', lambda=200000)
coefplot(value2, sort='magnitude', lambda=100000)
