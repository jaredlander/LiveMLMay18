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
