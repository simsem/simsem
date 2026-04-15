# Run simulations to check the bootstrap confidence interval of factor correlation

.libPaths(c("~/R/4.4.0", .libPaths()))
library(lavaan)
library(simsem)

# Define conditions

p <- c(3, 7)
faccor <- c(0.2, 0.5)
loading <- c(0.5, 0.7)
samplesizes <- c(500, 1000)

conds <- expand.grid(p, faccor, loading, samplesizes) # 16 conditions
colnames(conds) <- c("p", "faccor", "loading", "samplesizes")

nRep <- 1000

# Use jobid from slurm (possible values = 2*2*2*2 = 16)

jobid <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
cond <- conds[jobid, ]

# script for data genration
xs <- paste0("x", 1:cond$p)
ys <- paste0("y", 1:cond$p)

load_x <- paste(paste(cond$loading, xs, sep="*"), collapse = "+")
load_y <- paste(paste(cond$loading, ys, sep="*"), collapse = "+")

resid_x <- paste0(xs, "~~", (1 - cond$loading^2), "*", xs)
resid_y <- paste0(ys, "~~", (1 - cond$loading^2), "*", ys)

generatescript <- paste(
  paste0("f1 =~ ", load_x),
  paste0("f2 =~ ", load_y),
  paste0("f1 ~~ ", cond$faccor, "*f2"),
  paste(resid_x, collapse = "\n"),
  paste(resid_y, collapse = "\n"),
  sep = "\n"
)

# Script for data analysis
load_x2 <- paste(xs, collapse = "+")
load_y2 <- paste(ys, collapse = "+")

analyzescript <- paste(
  paste0("f1 =~ ", load_x2),
  paste0("f2 =~ ", load_y2),
  "f1 ~~ cov*f2",
  sep = "\n"
)

# Function used to extract lower and upper bounds
ciextract <- function(out) {
  est <- coef(out)["cov"]
  sewald <- sqrt(vcov(out)["cov", "cov"])
  crit <- qnorm(1 - (1 - 0.95)/2)
  waldlower <- est - crit*sewald
  waldupper <- est + crit*sewald
  percout <- data.frame(parameterestimates(out, boot.ci.type = "perc", level=0.95))
  perclower <- percout[percout$label == "cov", "ci.lower"]
  percupper <- percout[percout$label == "cov", "ci.upper"]
  bcaout <- data.frame(parameterestimates(out, boot.ci.type = "bca.simple", level=0.95))
  bcalower <- bcaout[bcaout$label == "cov", "ci.lower"]
  bcaupper <- bcaout[bcaout$label == "cov", "ci.upper"]
  c(waldlower=waldlower, waldupper=waldupper,
    perclower=perclower, percupper=percupper,
    bcalower=bcalower, bcaupper=bcaupper)
}

# Chunk of codes in case for debug
# dat <- lavaan::simulateData(generatescript, sample.nobs=cond$samplesizes)
# out <- cfa(analyzescript, data=dat, std.lv=TRUE, se="boot", iseed=787221)
# ciextract(out)

name <- paste0("simsem_parallel_", cond$p, "_", as.integer(cond$faccor * 10), "_",
               as.integer(cond$loading * 10), "_", cond$samplesizes)
outfile <- paste0(name, ".rds")

if(!file.exists(outfile)) {
  # Run simulation
  output <- sim(
    nRep=nRep,
    model=analyzescript,
    generate=generatescript,
    n=cond$samplesizes,
    std.lv=TRUE,
    lavaanfun="cfa",
    se="boot",
    iseed=787221, # Set seed for lavaan bootstrap (same row index is used)
    outfun=ciextract,
    seed = 123321, # Set seed for data generation
    numProc = 8, multicore = TRUE
  )
  # Note: `iseed` is fixed here to ensure deterministic bootstrap
  # resampling, which guarantees identical results between single-run
  # and split HPC runs. For general simulation studies, `iseed` can be
  # set to NULL so that bootstrap variability is fully reflected across
  # replications.
  
  saveRDS(output, file = outfile)
}

