### Terrence D. Jorgensen
### Last updated: 11 January 2021
### example of moderated mediation simulation, in response to this thread:
### https://groups.google.com/d/msg/lavaan/otcOcc7Rclw/1pQ22N9dAAAJ

### 3 (Self-efficacy) x 2 (Feedback condition) design with 5 dummy codes for 6 groups:
### X1:  0 = Control, 0 = Low Self-efficacy, 1 = High Self-efficacy
### X2: 0 = Control, 1 = Low Self-efficacy, 0 = High Self-efficacy
### W: 0 = Ambiguous, 1 = Unambiguous
### Interaction 1: W*X1
### Interaction 2: W*X2


library(simsem)


## -----------------------------------------
## Set population parameters & design matrix
## -----------------------------------------

## paths when moderator (W) dummy code(s) == 0
a1.amb <- .1 # effect of X1 when W == 0
a2.amb <- .3 # effect of X2 when W == 0
## how much "a" paths are moderated by W
a1.unamb <- -.2 # effect of (X1 * W) dummy-code interaction
a2.unamb <- -.4 # effect of (X2 * W) dummy-code interaction

## simple effect of moderator (W) on M or Y when X1 & X2 == 0
w.M <- 0 # no effect? Doesn't really matter, not the effect of interest
w.Y <- 0

## paths unaffected by W (since only X and W interact, and you are
##                        only interested in conditional INDIRECT effects)
b <- .5 # effect of M on Y
c1.amb <- .2 # direct effect of X1 on Y
c2.amb <- .4 # direct effect of X2 on Y
c1.unamb <- 0 # no moderation of direct effects on Y
c2.unamb <- 0


## NOTE:
##       You can write a function to do everything you see BELOW, and use
##       any subset of the parameters ABOVE as arguments to that function.
##       That would facilitate you being able to choose different population
##       parameters for some paths without re-writing the (mostly) same code.


## design matrix to use as template for data generation
designMatrix <- expand.grid(Self.Efficacy = c("High","Low","Control"),
                            Feedback = c("Ambiguous","Unambiguous"))
designMatrix$X1 <- ifelse(designMatrix$Self.Efficacy == "High", 1, 0)
designMatrix$X2 <- ifelse(designMatrix$Self.Efficacy == "Low", 1, 0)
designMatrix$W <- ifelse(designMatrix$Feedback == "Unambiguous", 1, 0)
designMatrix$X1W <- designMatrix$X1 * designMatrix$W
designMatrix$X2W <- designMatrix$X2 * designMatrix$W
designMatrix

## total sample size must be a multiple of 6 (assuming balanced groups)
N.per.group <- 10
N <- N.per.group * nrow(designMatrix)
## stack N.per.group copies of design matrix for a full sample of size N
dummies <- designMatrix[c("X1","X2","W","X1W","X2W")]
covData <- do.call(rbind, lapply(1:N.per.group, function(i) dummies))
## you will pass "covData" to the covData= argument in sim()



## ---------------------------
## Specify parameters matrices
## ---------------------------

## parameters for endogoenous variables (M and Y)

beta.free <- beta.pop <- matrix(0, nrow = 2, ncol = 2,
                                dimnames = list(c("M","Y"), c("M","Y")))
beta.free["Y", "M"] <- "b" # estimate the effect of M on Y, label it "b"
beta.pop["Y", "M"] <- b    # set the population value
(endoPaths <- bind(free = beta.free, popParam = beta.pop))

## set residual correlations to zero, and simsem will automatically set
## residual variances such that the total variances of M and Y == 1.
## That way, population paths are "standardized"
(residCor <- binds(free = diag(as.numeric(NA), 2), popParam = diag(2)))



## parameters for exogenous variables (X * W)
kappa.free <- kappa.pop <- matrix(NA,       # estimate all effects
                                  nrow = 2, # 2 outcomes (M & Y)
                                  ncol = 5, # 5 dummy codes
                                  dimnames = list(rownames(beta.free),
                                                  names(dummies)))
## label free parameters, so you can define indirect / total effects
kappa.free["M","X1"]  <- "a1.amb"
kappa.free["M","X2"]  <- "a2.amb"
kappa.free["M","W"]   <- "w.M"
kappa.free["M","X1W"] <- "a1.unamb"
kappa.free["M","X2W"] <- "a2.unamb"

kappa.free["Y","X1"]  <- "c1.amb"
kappa.free["Y","X2"]  <- "c2.amb"
kappa.free["Y","W"]   <- "w.Y"
kappa.free["Y","X1W"] <- "c1.unamb"
kappa.free["Y","X2W"] <- "c2.unamb"


## fill in population parameters
kappa.pop["M","X1"] <- a1.amb
kappa.pop["M","X2"] <- a2.amb
kappa.pop["M","W"] <- w.M
kappa.pop["M","X1W"] <- a1.unamb
kappa.pop["M","X2W"] <- a2.unamb

kappa.pop["Y","X1"] <- c1.amb
kappa.pop["Y","X2"] <- c2.amb
kappa.pop["Y","W"] <- w.Y
kappa.pop["Y","X1W"] <- c1.unamb
kappa.pop["Y","X2W"] <- c2.unamb

(exoPaths <- bind(free = kappa.free, popParam = kappa.pop))
## NOTE:  kappa will get stored as gamma because the LISREL parameterization
## only allows observed variables (and factors) to be predicted by factors.
## So below it will be passed to the GA= rather than KA= argument.


## Set the intercepts of M and Y to 0. As kappa gets assigned to gamma,
## TY= would get transferred to AL=
(AL <- bind(free = c(NA, NA), popParam = c(0, 0)))



## specify any indirect or total effects you want power for:
userParams <- '
## conditional indirect effects
  ind1.amb   := a1.amb * b
  ind2.amb   := a2.amb * b
  ind1.unamb := (a1.amb + a1.unamb) * b
  ind2.unamb := (a2.amb + a2.unamb) * b
## conditional total effects (necessary?)
  tot1.amb   := ind1.amb + c1.amb
  tot2.amb   := ind2.amb + c2.amb
  tot1.unamb := ind1.unamb + c1.amb
  tot2.unamb := ind2.unamb + c2.amb
'


simMod <- model.path(BE = endoPaths, RPS = residCor, GA = exoPaths, AL = AL,
                     indLab = c("M","Y"), covLab = names(dummies),
                     con = userParams)
simMod



## --------------
## Run simulation
## --------------

out <- sim(nRep = 200, n = N, model = simMod, covData = covData)
summaryParam(out, matchParam = TRUE) #FIXME: average estimates != parameters



