# Spatial Modelling and Prediction
# Day 1

setwd("")

hw <- read.csv("Master/SoSe_2020/Spatial Modelling and Prediction/weight-height.csv")
head(hw)
summary(hw)

library(measurements)

hw2 <- data.frame(Gender=hw$Gender,
                  Weight=conv_unit(hw$Weight,"lbs","kg"),
                  Height=conv_unit(hw$Height,"inch","cm"))
head(hw2)
summary(hw2)

plot(hw2$Height,hw2$Weight)

library(dplyr)

# provide 10 random samples
dplyr::sample_n(hw2, 10)

# how are the values for male and female?
summary(filter(hw2, Gender == "Female"))
summary(filter(hw2, Gender == "Male"))

# any obvious anomalies or indications of significant correlation between male and female values?
boxplot(filter(hw2, Gender == "Female")$Weight, filter(hw2, Gender == "Male")$Weight, notch = T)
boxplot(filter(hw2, Gender == "Female")$Height, filter(hw2, Gender == "Male")$Height, notch = T)

# data normally distributed?
shapiro.test(hw2$Weight)
# does not work 
# Stichprobe muss zwischen 3 und 5000 liegen
# sample size is to big

shapiro.test(dplyr::sample_n(hw2,5000)$Weight)
shapiro.test(dplyr::sample_n(hw2,5000)$Height)
# data is not normally distributed ?
# why?
# should be

# we mixed male and female

# some more explorative plots
plot(density(hw2$Weight))
plot(density(hw2$Height))

plot(density(filter(hw2, Gender == "Female")$Weight), col="red")
lines(density(filter(hw2, Gender == "Male")$Weight), col="blue")

plot(density(filter(hw2, Gender == "Female")$Height), col="red")
lines(density(filter(hw2, Gender == "Male")$Height), col="blue")

# now back to test for normality
shapiro.test(dplyr::sample_n(filter(hw2, Gender == "Female"),5000)$Weight)
shapiro.test(dplyr::sample_n(filter(hw2,Gender == "Male"),5000)$Weight)

shapiro.test(dplyr::sample_n(filter(hw2, Gender == "Female"),5000)$Height)
shapiro.test(dplyr::sample_n(filter(hw2,Gender == "Male"),5000)$Height)
# that looks much better: all data sets are not significant different from normale distri
# now: with splitted male/female datasets

# different test have different requierements

# Males
# we now just select the values for the males:
hw2.male <- filter(hw2, Gender == "Male")
summary(hw2.male)

# now the actual linear regression
hw.lm <- lm(formula = Weight ~ Height, data = hw2.male)
summary(hw.lm)

# height and weight are significantly correlated for the measurements of males

# Females
# we now just select the values for the females:
hw2.female <- filter(hw2, Gender == "Female")
summary(hw2.female)

# now the actual linear regression
hw.lm2 <- lm(formula = Weight ~ Height, data = hw2.female)
summary(hw.lm2)

# nice to know ... but quit boring
# can we predict??

# we know your height
# and we have height vs. weight regression model
# enter the height of all males in the room
hw.new <- data.frame(name=c("Annika","Sanaz","Kemeng","Sofia","Helena","Luisa"),
                     Height=c(175, 163, 152, 152, 170, 176))
head(hw.new)

hw.lm.p <- predict(object=hw.lm2, newdata = hw.new)

pred.weight <- data.frame(hw.new$name,
                          weight.pred=hw.lm.p)
pred.weight
# does it roughly fit?
# what might be a problem?
# would need global/country data

### Spatial Prediction of Presence Probability
# where are the EAGLEs
library(rgdal)
library(raster)

occ <- readOGR("Master/SoSe_2020/Spatial Modelling and Prediction/occurence.gpkg")

class(occ)
summary(occ)
plot(occ)

bui <- readOGR("Master/SoSe_2020/Spatial Modelling and Prediction/campus_buildings.gpkg")
plot(bui)

# plotting additional infos of the points - ideas?

plot(bui)
plot(occ[occ$students == 1,],col="blue",pch=16, add=T)
plot(occ[occ$students == 0,],col="red",pch=16, add=T)

# data preparation

r <- raster(bui, ncols=100, nrows=100)

rr.0 <- rasterize(bui, r, progress="text")
plot(rr.0)

rr.0.d <- distance(rr.0)

preds <- rr.0.d # just rename it
plot(preds)

# Spatial Prediction

library(sdm)

d <- sdmData(formula = students~layer, train = occ, predictors = preds)
d

m1 <- sdm(students~., data=d, methods = c("glm","svm"))

p1 <- predict(m1, newdata=preds, filename="sdm_preds_1.grd", overwrite=T)
plot(p1)

# not great and not really logical, but statistical

# can we be more specific?
# adding more spatial information, more relevant spatial data

#########
# second run
#########

rr <- rasterize(bui, r, progress="text", field = "id")
plot(rr)

rr.1 <- rr == 1
rr.1[rr.1 == 0] <- NA
plot(rr.1)

rr.2 <- rr == 2
rr.2[rr.2==0] <- NA
plot(rr.2)

rr.3 <- rr==3
rr.3[rr.3==0] <- NA
plot(rr.3)

rr.1.d <- distance(rr.1)
plot(rr.1.d)

rr.2.d <- distance(rr.2)
plot(rr.2.d)

rr.3.d <- distance(rr.3)
plot(rr.3.d)

preds <- stack(rr.1.d, rr.2.d, rr.3.d)
plot(preds)

d <- sdmData(formula = students~layer.1+layer.2+layer.3, train = occ, predictors = preds)
d

m1 <- sdm(students~., data=d, methods=c("glm","svm"))

p1 <- predict(m1, newdata=preds, filename="sdm_preds_2.grd", overwrite=T)

plot(p1[[2]])
plot(bui, add=T)

# subsetting our data for better space-time prediction

# Data Preparation 3
occ.10h <- occ[occ$time == 10,]
occ.13h <- occ[occ$time == 13,]
occ.22h <- occ[occ$time == 22,]

plot(occ.22h)

# Spatial Prediction 3
d.10h <- sdmData(formula = students~layer.1+layer.2+layer.3, train=occ.10h, predictors = preds)
d.13h <- sdmData(formula = students~layer.1+layer.2+layer.3, train=occ.13h, predictors = preds)
d.22h <- sdmData(formula = students~layer.1+layer.2+layer.3, train=occ.22h, predictors = preds)

m.10h <- sdm(students~., data = d.10h, methods = c("glm","svm"))
m.13h <- sdm(students~., data = d.13h, methods = c("glm","svm"))
m.22h <- sdm(students~., data = d.22h, methods = c("glm","svm"))

p.10h <- predict(m.10h, newdata=preds)
p.13h <- predict(m.13h, newdata=preds)
p.22h <- predict(m.22h, newdata=preds)

p.time <- stack(p.10h, p.13h, p.22h)

plot(p.time)

plotRGB(p.time, 1,3,5, stretch="lin") # 2,4,6 would be the svm method
plot(bui, add=T)

# we can add further values




