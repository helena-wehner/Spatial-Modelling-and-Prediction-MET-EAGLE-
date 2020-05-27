### Spatial Modelling and Prediction
### MIAmaxent package
### Modelling example:
### https://cran.r-project.org/web/packages/MIAmaxent/vignettes/a-modeling-example.html
### https://cran.r-project.org/web/packages/MIAmaxent/MIAmaxent.pdf
### adapted by H. Wehner

setwd("C:\\Users/Lenovo/Desktop/")

### loading packages
library(raster)
library(MIAmaxent)

### Alternative to MIAmaxent maxent model
### Logistic Regression
### only possible if presence and absence data is avaible
### algorithm "maxent" used with presence-only data
### algorithm "logistic regression" used with presence-absence data

# below use presence-absence !! data to fit models by logistic regression

# loading presence-absence data and raster file of the study area

# raster file of the study area containing explanatory variables
EVfiles <- c(list.files(system.file("extdata", "EV_continuous", package="MIAmaxent"), 
                        full.names=TRUE),
             list.files(system.file("extdata", "EV_categorical", package="MIAmaxent"), 
                        full.names=TRUE))
EVstack <- raster::stack(EVfiles)

# presence-absence data
grasslandPA <- readData(
  occurrence = system.file("extdata", "occurrence_PA.csv", package="MIAmaxent"), 
  contEV = system.file("extdata", "EV_continuous", package="MIAmaxent"),
  catEV = system.file("extdata", "EV_categorical", package="MIAmaxent"),
  PA = TRUE, XY = FALSE)

str(grasslandPA)

#### Transforming explanatory variables (EV)
# fit different kind of relationships between explanatory and response variables (RV)
# create a new "derived" variable from originally EV
# produces 20 models and chooses those which explain the most variation in the RV

PA.grasslandDVs <- deriveVars(grasslandPA, algorithm = "LR")

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

PA.grasslandDVselect <- selectDVforEV(PA.grasslandDVs$dvdata, alpha = 0.001, 
                                      algorithm = "LR", quiet = TRUE)
# output is a list of two parts
# the DVs that were selected for each EV (dvdata)
# the trails of nested models that were built and compared for each EV during the selection
# process (selection)

# select a parsimonious set of EVs to comprise the model
# also with nested model comparison

PA.grasslandEVselect <- selectEV(PA.grasslandDVselect$dvdata, alpha = 0.001, algorithm = "LR")

# compare the length of EVs before and after comprising

# before
length(PA.grasslandDVselect$dvdata)
# after
length(PA.grasslandEVselect$dvdata)

### Applying the Model

# projectModel() function
# needed information:
# model to be projected --> PA.grasslandEVselect$selectedmodel
# transformation functions used by the model --> PA.grasslandDVs$transformations
# explanatory data to project across (raster data) --> EVstack
# note: names of the raster layers must match names of EVs in the model

PA.grasslandPreds <- projectModel(model = PA.grasslandEVselect$selectedmodel,
                                  transformations = PA.grasslandDVs$transformations,
                                  data = EVstack)
# output values between [0,1]
# these values represent the probability of presence