### Spatial Modelling and Prediction
### MIAmaxent package

setwd("C:\\Users/Lenovo/Desktop/")

### loading packages
library(raster)
library(MIAmaxent)

EV1 <- raster(list.files(system.file("extdata", "EV_continuous", 
                                     package="MIAmaxent"), full.names=TRUE)[1])
PO <- read.csv(system.file("extdata", "occurrence_PO.csv", package="MIAmaxent"))
plot(EV1, legend=FALSE)
points(PO$POINT_X, PO$POINT_Y, pch = 20, cex = 0.5, col = 'blue')

# 
grasslandPO <- readData(
  occurrence=system.file("extdata", "occurrence_PO.csv", package="MIAmaxent"), 
  contEV=system.file("extdata", "EV_continuous", package="MIAmaxent"),
  catEV=system.file("extdata", "EV_categorical", package="MIAmaxent"),
  maxbkg=20000)
