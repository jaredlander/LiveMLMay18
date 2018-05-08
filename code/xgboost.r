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

lotsX_test <- build.x(histFormula, data=lotsTest, contrasts=FALSE, sparse=TRUE)
lotsY_test <- build.y(histFormula, data=lotsTest) %>% as.integer() - 1

lotsX_val <- build.x(histFormula, data=lotsVal, contrasts=FALSE, sparse=TRUE)
lotsY_val <- build.y(histFormula, data=lotsVal) %>% as.integer() - 1

xgTrain <- xgb.DMatrix(data=lotsX_train,
                       label=lotsY_train)
xgTrain
xgVal <- xgb.DMatrix(data=lotsX_val,
                     label=lotsY_val)


xg1 <- xgb.train(
    data=xgTrain,
    objective='binary:logistic',
    eval_metric='logloss',
    booster='gbtree',
    nrounds=1
)

xgb.plot.multi.trees(xg1, feature_names=colnames(lotsX_train))

xg2 <- xgb.train(
    data=xgTrain,
    objective='binary:logistic',
    eval_metric='logloss',
    booster='gbtree',
    nrounds=1,
    watchlist=list(train=xgTrain)
)

xg3 <- xgb.train(
    data=xgTrain,
    objective='binary:logistic',
    eval_metric='logloss',
    booster='gbtree',
    nrounds=100,
    watchlist=list(train=xgTrain),
    print_every_n=1
)

xg4 <- xgb.train(
    data=xgTrain,
    objective='binary:logistic',
    eval_metric='logloss',
    booster='gbtree',
    nrounds=300,
    watchlist=list(train=xgTrain),
    print_every_n=1
)
