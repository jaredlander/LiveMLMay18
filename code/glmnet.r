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

animation::cv.ani(k=10)

value3 <- cv.glmnet(x=lotsX, y=lotsY, family='gaussian', nfolds=5)
plot(value3)
value3$lambda.min
value3$lambda.1se
coefpath(value3)

coefplot(value3, sort='magnitude', lambda='lambda.min')
coefplot(value3, sort='magnitude', lambda='lambda.1se')
coefplot(value3, sort='magnitude', lambda='lambda.1se', plot=FALSE)

value4 <- cv.glmnet(x=lotsX, y=lotsY, family='gaussian', nfolds=5, alpha=1)

value5 <- cv.glmnet(x=lotsX, y=lotsY, family='gaussian', nfolds=5, alpha=0)
value5$lambda.min
plot(value5)
coefpath(value5)
coefpath(value4)

value6 <- cv.glmnet(x=lotsX, y=lotsY, family='gaussian', nfolds=5, alpha=0.3)
coefpath(value6)

coefplot(value6, sort='magnitude', lambda='lambda.1se')

lotsNew <- readRDS('data/manhattan_Test.rds')

lotsX_new <- build.x(valueFormula, data=lotsNew, contrasts=FALSE, sparse=TRUE)

value6$lambda

lotsPreds6 <- predict(value6, newx=lotsX_new, s='lambda.1se')
head(lotsPreds6)

set.seed(1234)