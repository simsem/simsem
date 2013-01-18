library(simsem)

# Because the syntax is long. To type all of it up is quite tedious. Thus, the 'paste0' function is quite useful here.

fac1indlist <- paste0(paste0("0.7*y", 1:12), collapse = " + ")
fac2indlist <- paste0(paste0("0.7*y", 13:24), collapse = " + ")
fac3indlist <- paste0(paste0("0.7*y", 25:36), collapse = " + ")
fac4indlist <- paste0(paste0("0.7*y", 37:48), collapse = " + ")

loading <- paste0("f1 =~ ", fac1indlist, "\nf2 =~ ", fac2indlist, "\nf3 =~ ", fac3indlist, "\nf4 =~ ", fac4indlist) 
cat(loading)

faccor <- "
f1 ~~ 1*f1
f2 ~~ 1*f2
f3 ~~ 1*f3
f4 ~~ 1*f4
f1 ~~ 0.3*f2
f1 ~~ 0.3*f3
f1 ~~ 0.3*f4
f2 ~~ 0.3*f3
f2 ~~ 0.3*f4
f3 ~~ 0.3*f4
"

allindlist <- paste0("y", 1:48)
errorvar <- paste0(paste0(allindlist, " ~~ 0.51*", allindlist), collapse = "\n")
cat(errorvar)

popModel <- paste(loading, faccor, errorvar, sep="\n")
cat(popModel)

fac1indlist <- paste0(paste0("y", 1:12), collapse = " + ")
fac2indlist <- paste0(paste0("y", 13:24), collapse = " + ")
fac3indlist <- paste0(paste0("y", 25:36), collapse = " + ")
fac4indlist <- paste0(paste0("y", 37:48), collapse = " + ")

analyzeModel <- paste0("f1 =~ ", fac1indlist, "\nf2 =~ ", fac2indlist, "\nf3 =~ ", fac3indlist, "\nf4 =~ ", fac4indlist) 
cat(analyzeModel)

setx <- c(1:3, 13:15, 25:27, 37:39)
set1 <- setx + 3
set2 <- set1 + 3
set3 <- set2 + 3
itemGroups <- list(setx, set1, set2, set3)

missModel <- miss(nforms=3, itemGroups=itemGroups, m=5)

Output <- sim(1000, n=1000, analyzeModel, generate=popModel, lavaanfun="cfa", std.lv=TRUE, miss=missModel)
getCutoff(Output, 0.05)
plotCutoff(Output, 0.05)
summary(Output)
