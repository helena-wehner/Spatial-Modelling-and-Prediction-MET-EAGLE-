### Spatial Prediction and Modelling

### Package Presentation
### KISSMig

# https://www.wsl.ch/en/services-and-products/software-websites-and-apps/kissmig.html

# https://cran.r-project.org/web/packages/kissmig/kissmig.pdf

# --------------------------------------------------------------------------
# 'kissmig' R package - code example
#  Michael Nobis/WSL (michael.nobis@wsl.ch), December 2014
# --------------------------------------------------------------------------

library(kissmig)  # loads also raster package as dependency

# --------------------------------------------------------------------------
# generating some maps for later use

## getting a suitability map with normal distribution of mean annual
## temperature based on worldclim
s <- kissmigDummyS(mean=12,sd=3, download=TRUE)
plot(s, asp=1.0, main='simple climate suitability map')

## defining an origin
o <- kissmigOrigin(s, 8, 44.5, 0.5)
plot(o  , asp=1.0, main='origin')
plot(s+o, asp=1.0, main='suitability + origin')

## defining the land mask
landMask <- s>=0
plot(landMask, asp=1.0, main='land mask')

# --------------------------------------------------------------------------
# runnig kissmig with different type of output

k <- kissmig(o, s, it=150, type='DIS')
plot(k*landMask, asp=1.0, main='final distribution')

k <- kissmig(o, s, it=150, type='FOC')
plot(k*landMask, asp=1.0, main='first iteration step of occurrence')

a <- kissmigAccess(k)
plot(a*landMask, asp=1.0, main='accessibility based on "FOC", absolute values')

a <- kissmigAccess(k, rel=TRUE)
plot(a*landMask, asp=1.0, main='accessibility based on "FOC", relative values')

k <- kissmig(o, s, it=150, type='LOC')
plot(k*landMask, asp=1.0, main='last iteration step of occurrence')

k <- kissmig(o, s, it=150, type='NOC')
plot(k*landMask, asp=1.0, main='number of iteration steps with occurrences')

# --------------------------------------------------------------------------
# controlling stochasticity by setting a seed of the random number generator

## two runs with stochastic differences
k1 <- kissmig(o, s, it=150, type='DIS') # two final distributions with
k2 <- kissmig(o, s, it=150, type='DIS') # same origin and suitability map
plot(k1*landMask, asp=1.0, main='final distribution - no seed set - run1')
plot(k2*landMask, asp=1.0, main='final distribution - no seed set - run2')
plot(abs(k1-k2)*landMask, asp=1.0, main='differences between run1 and run2')

## two runs without stochastic differences
k1 <- kissmig(o, s, it=150, type='DIS', seed=123)
k2 <- kissmig(o, s, it=150, type='DIS', seed=123)
plot(k1*landMask, asp=1.0, main='final distribution - seed set - run1')
plot(k2*landMask, asp=1.0, main='final distribution - same seed set - run2')
plot(abs(k1-k2)*landMask, asp=1.0, main='stochastic differences between run1 and run2')

# --------------------------------------------------------------------------
# creating a "signed" result; the sign returns the final distribution 'DIS' 
# if combined with 'FOC', 'LOC', or 'NOC'; useful for runtime optimization

k <- kissmig(o, s, it=200, type='FOC', signed=TRUE)
plot(    k   *landMask, asp=1.0, main='signed "FOC" call, raw output')

## abs(k): same as type='FOC' and signed=FALSE
plot(abs(k  )*landMask, asp=1.0, main='signed "FOC" call, absolute values')

## extract information of final distribution 'DIS'
plot(abs(k>0)*landMask, asp=1.0, main='signed "FOC" call, values > 0')

## extract cells with 'FOC' information but no final occurrences
plot(abs(k<0)*landMask, asp=1.0, main='signed "FOC" call, values < 0')

# --------------------------------------------------------------------------
# running kissmig with stacks of suitability maps; useful for changing 
# environments during species spread or miration

## single suitability maps simlating climate warming
s1 <- kissmigDummyS(mean=12,sd=3) # 
s2 <- kissmigDummyS(mean=11,sd=3) # simulates suitability for climate warming of 1???C
s3 <- kissmigDummyS(mean=10,sd=3) # simulates suitability for climate warming of 2???C

## building stacks with and without suitability changes
s111 <- stack(s1,s1,s1) # or brick(s1,s1,s1)
s123 <- stack(s1,s2,s3) # or brick(s1,s2,s3)

## run kissmig with and without environmental change
k111 <- kissmig(o, s111, it=50, type='NOC', seed=123) # 3 layers = 150 iteration in total
k123 <- kissmig(o, s123, it=50, type='NOC', seed=123) # steps in total

## kissmig results
plot(k111*landMask, asp=1.0, main='3 suitability layers, 50 it./layer, no env. changes')
plot(k123*landMask, asp=1.0, main='3 suitability layers, 50 it./layer, climate warming')

## changes with climate warming during spread
gain <- (k123 >0) * (k111==0)
same <- (k123 >0) * (k111 >0)
loss <- (k123==0) * (k111 >0)
plot((loss+(2*same)+(3*gain))*landMask, 
     col=colorRampPalette(c(grey(0.9),"red",grey(0.6),"green"))(4), asp=1.0, 
     main='differences between spread patterns under climate warming', legend=FALSE)
legend("topleft", legend=c('gain', 'no change', 'loss'), pch=15, col=c('green', grey(0.6), 'red') )

# --------------------------------------------------------------------------
# exploring geographic space 
# example: size of spread patterns with it=100 from different origins
# remark: runtime limitation are mainly caused by plotting the maps and
# not by running kissmig.

# a suitability map and map for results
par(mfrow=c(1,1))
s <- kissmigDummyS(mean=14,sd=3)
plot(s, asp=1.0, main='simple climate suitability map')
res <- s>9999 # a map to store results

# explore space
par(mfrow=c(2,1))
for (y in (15:17)*2.5) {
  for (x in (-4:11)*2.5) {
    
    # defining the origin and runnung kissmig
    o <- kissmigOrigin(s, x, y, 2.5)
    k <- kissmig(o, s, type='FOC', it=100)
    
    # save the size of the spread pattern (number of colonized cells)  
    # in the cells or the origin in the result map (res)
    values(res)[values(o)==1] <- sum(values(k)>0)
    
    # instead of area, any evaluation procedure might be added here,
    # for instance the performance of accessibility in a SDM framework
    
    # plot spread pattern and results
    plot(k*landMask, asp=1.0, main='spread pattern from single region')
    plot(res*landMask, asp=1.0, main='size of spread pattern (number of colonized cells)')
    
  }
}


