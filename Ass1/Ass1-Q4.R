BitCoinData <- read_excel("Desktop/UPC 18:19/S1 18:19/SMDE/BitcoinData.xlsx", range = cell_rows(1:365))
EthereumData <- read_excel("Desktop/UPC 18:19/S1 18:19/SMDE/EthereumData.xlsx", range = cell_rows(1:365))
XRPData <- read_excel("Desktop/UPC 18:19/S1 18:19/SMDE/XRPData.xlsx", range = cell_rows(1:365))
CardanoData <- read_excel("Desktop/UPC 18:19/S1 18:19/SMDE/CardanoData.xlsx", range = cell_rows(1:365))
TronData <- read_excel("Desktop/UPC 18:19/S1 18:19/SMDE/TronData.xlsx", range = cell_rows(1:365))
IOTAData <- read_excel("Desktop/UPC 18:19/S1 18:19/SMDE/IOTAData.xlsx", range = cell_rows(1:365))
NEMData <- read_excel("Desktop/UPC 18:19/S1 18:19/SMDE/NEMData.xlsx", range = cell_rows(1:365))
DashData <- read_excel("Desktop/UPC 18:19/S1 18:19/SMDE/DashData.xlsx", range = cell_rows(1:365))
EOSData <- read_excel("Desktop/UPC 18:19/S1 18:19/SMDE/EOSData.xlsx", range = cell_rows(1:365))
StellarData <- read_excel("Desktop/UPC 18:19/S1 18:19/SMDE/StellarData.xlsx", range = cell_rows(1:365))
MarketCapData<- read_excel("Desktop/UPC 18:19/S1 18:19/SMDE/MarketCapData.xlsx", range = cell_rows(1:365))
MarketCapData[,2]<-as.numeric(as.character(MarketCapData$`Altcoin Market Cap`))
Prices <- data.frame()[1:364,]

Prices[,"Bitcoin"]<-BitCoinData$High/BitCoinData$Volume
Prices[,"Ethereum"]<-EthereumData$High/EthereumData$Volume
Prices[,"XRP"]<-XRPData$High/XRPData$Volume
Prices[,"Cardano"]<-CardanoData$High/CardanoData$Volume
Prices[,"Tron"]<-TronData$High/TronData$Volume 
Prices[,"IOTA"]<-IOTAData$High/IOTAData$Volume
Prices[,"NEM"]<-NEMData$High/NEMData$Volume 
Prices[,"Dash"]<-DashData$High/DashData$Volume
Prices[,"EOS"]<-EOSData$High/EOSData$Volume
Prices[,"Stellar"]<-StellarData$High/StellarData$Volume 
Prices[,"MarketCap"]<- MarketCapData$`Altcoin Market Cap`

pairs(BitCoinData)
AnovaModel.1 <- aov(Tron ~ Bitcoin,data = Prices)
summary(AnovaModel.1)
AnovaModel.2 <- aov(Ethereum ~ Bitcoin,data = Prices)
summary(AnovaModel.2)
AnovaModel.3 <- aov(XRP ~ Ethereum,data = Prices)
summary(AnovaModel.3)




princomp(Prices, scale= FALSE)
#Prediction of Cardano

LinearModel.1 <- lm( Bitcoin~ Cardano, data = Prices)
new1<- data.frame(Bitcoin = 0.0000000678)
predict(LinearModel.1, newdata = new1, intercal = "prediction")
