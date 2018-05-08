library(useful)
library(xgboost)
library(coefplot)
library(magrittr)
library(dygraphs)

lotsTrain <- readRDS('data/manhattan_Train.rds')
lotsTest <- readRDS('data/manhattan_Test.rds')
lotsVal <- readRDS('data/manhattan_Validate.rds')

View(lotsTrain)

histFormula <- HistoricDistrict ~ FireService + 
    ZoneDist1 + ZoneDist2 + Class + LandUse + 
    OwnerType + LotArea + BldgArea + ComArea + 
    ResArea + OfficeArea + RetailArea + 
    GarageArea + FactryArea + NumBldgs + 
    NumFloors + UnitsRes + UnitsTotal + 
    LotFront + LotDepth + BldgFront + 
    BldgDepth + LotType + Landmark + BuiltFAR +
    Built + TotalValue - 1

lotsX_train <- build.x(histFormula, data=lotsTrain, contrasts=FALSE, sparse=TRUE)
lotsY_train <- build.y(histFormula, data=lotsTrain) %>% as.integer() - 1
lotsY_train %>% head(n=20)
