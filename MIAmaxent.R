### Spatial Modelling and Prediction
### MIAmaxent package
### Modelling example:
### https://cran.r-project.org/web/packages/MIAmaxent/vignettes/a-modeling-example.html

setwd("C:\\Users/Lenovo/Desktop/")

### loading packages
library(raster)
library(MIAmaxent)

### Introducing the data set

EV1 <- raster(list.files(system.file("extdata", "EV_continuous", 
                                     package="MIAmaxent"), full.names=TRUE)[1])

PO <- read.csv(system.file("extdata", "occurrence_PO.csv", package="MIAmaxent"))
plot(EV1, legend=FALSE)
points(PO$POINT_X, PO$POINT_Y, pch = 20, cex = 0.5, col = 'blue')

# Transform CSV and ASCII (Raster) into one single data fram
# serves as starting point for modelling
# note: continuous and categorical variables must be placed ib seperate directions
grasslandPO <- readData(
  occurrence=system.file("extdata", "occurrence_PO.csv", package="MIAmaxent"), 
  contEV=system.file("extdata", "EV_continuous", package="MIAmaxent"),
  catEV=system.file("extdata", "EV_categorical", package="MIAmaxent"),
  maxbkg=20000)

#### Transforming explanatory variables (EV)
# fit different kind of relationships between explanatory and response variables (RV)
# create a new "derived" variable from originally EV
# produces 20 models and chooses those which explain the nost variation in the RV

grasslandDVs <- deriveVars(grasslandPO,
                           transformtype = c("L","M","D","HF","HR","T","B"))

# output is a list containing of two parts
# data frames of DVs for each EV (dvdata)
# the transformation function used to produce each DV (transformations)
# both list elements contain also the RV vector

### Selecting variables
# aim: explain as much as variation in the RV as efficiently as possible
# more EV or DVs in the model the more variation in the RV we can explain
# but at the cost of model complexity
# benefit of additional variation explained is weighted against the cost in model complexity
# variables are added to the model one by one: nested model comparison
# first selection of DVs for each EV then EVs for the full model

grasslandDVselect <- selectDVforEV(grasslandDVs$dvdata, alpha = .001, quiet = T)

# output is a list of two parts
# the DVs that were selected for each EV (dvdata)
# the trails of nested models that were built and compared for each EV during the selection
# process (selection)

# comparing the list of DVs before and after selection we can see that selectDVforEV() reduced
# reduced the number of DVs considerably

# select a pasimonious set of EVs to comprise the model
# also with nested model comparison

grasslandDVselect <- selectEV(grasslandDVselect$dvdata, alpha = .001, interaction = T)

# output consists of three parts
# EVs that were selected (dvdata)
# trail of nested models (selection)
# selected full model under given alpha value (selectedmodel)

### Choose Model
# choose which model from the forward selection trail to use

# we decide to to use a simpler model with only 5 EVs 
grasslandmodel <- chooseModel(grasslandDVselect$dvdata,
                              formula("~prbygall+geoberg + lcucor1 +tertpi09+geolmja1"))

### Applying the model
# for maxent type models:
# return: model predictions in probability ratio output (PRO) format for each location represented in data
# relative probability of presence
# PRO = 1 is a reference that represents the probability of presence in an "average" location in the training data
# a value of PRO = 1 can be interpreted as the relative probability of presence of a location
# randomly drawn from the training data. 
# Put another way, values above 1 represent higher-than-average probability of presence, 
# and vice versa

# obtain model ouput across the extent of the study area
EVfiles <- c(list.files(system.file("extdata", "EV_continuous", package="MIAmaxent"), 
                        full.names=TRUE),
             list.files(system.file("extdata", "EV_categorical", package="MIAmaxent"), 
                        full.names=TRUE))
EVstack <- raster::stack(EVfiles)
names(EVstack) <- gsub(".asc", "", basename(EVfiles))

grasslandPreds <- projectModel(model = grasslandmodel,
                               transformations = grasslandDVs$transformations,
                               data = EVstack)

# It is often easier to visualize probability-ratio values on a log scale, 
# so we plot the raster object again as log2(PRO + 1)
plot(log2(grasslandPreds$output + 1))

