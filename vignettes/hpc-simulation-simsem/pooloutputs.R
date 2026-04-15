# Pool the outputs

library(lavaan)
library(simsem)

# Define conditions

p <- c(3, 7)
faccor <- c(0.2, 0.5)
loading <- c(0.5, 0.7)
samplesizes <- c(500, 1000)
nRep <- 1000
nchunks <- 20

conds <- expand.grid(p, faccor, loading, samplesizes) 
colnames(conds) <- c("p", "faccor", "loading", "samplesizes")

cibounds_n <- NULL
cibounds_p8 <- NULL
cibounds_p16 <- NULL
cibounds_p32 <- NULL
cibounds_s <- NULL
for(i in 1:nrow(conds)) {
  cond <- conds[i,]
  cond_expanded <- as.matrix(cond[rep(1, nRep), ])
  faccor <- cond$faccor
  
  namerow <- paste0(cond$p, "_", as.integer(cond$faccor * 10), "_", 
                    as.integer(cond$loading * 10), "_", cond$samplesizes)
  noparallel <- paste0("outputs//simsem_noparallel_", namerow, ".rds")
  output_n <- readRDS(file = noparallel)
  temp_cibounds_n <- getExtraOutput(output_n)
  temp_cibounds_n <- do.call(rbind, temp_cibounds_n)
  temp_cibounds_n <- cbind(temp_cibounds_n, cond_expanded)
  cibounds_n <- rbind(cibounds_n, temp_cibounds_n)

  parallel8 <- paste0("outputs//simsem_parallel_", namerow, ".rds")
  output_p8 <- readRDS(file = parallel8)
  temp_cibounds_p8 <- getExtraOutput(output_p8)
  temp_cibounds_p8 <- do.call(rbind, temp_cibounds_p8)
  temp_cibounds_p8 <- cbind(temp_cibounds_p8, cond_expanded)
  cibounds_p8 <- rbind(cibounds_p8, temp_cibounds_p8)

  parallel16 <- paste0("outputs//simsem_parallel16_", namerow, ".rds")
  output_p16 <- readRDS(file = parallel16)
  temp_cibounds_p16 <- getExtraOutput(output_p16)
  temp_cibounds_p16 <- do.call(rbind, temp_cibounds_p16)
  temp_cibounds_p16 <- cbind(temp_cibounds_p16, cond_expanded)
  cibounds_p16 <- rbind(cibounds_p16, temp_cibounds_p16)

  parallel32 <- paste0("outputs//simsem_parallel32_", namerow, ".rds")
  output_p32 <- readRDS(file = parallel32)
  temp_cibounds_p32 <- getExtraOutput(output_p32)
  temp_cibounds_p32 <- do.call(rbind, temp_cibounds_p32)
  temp_cibounds_p32 <- cbind(temp_cibounds_p32, cond_expanded)
  cibounds_p32 <- rbind(cibounds_p32, temp_cibounds_p32)

  serial <- paste0("outputs//simsem_serial_", namerow)
  temp_cibounds_s <- NULL
  for(j in 1:nchunks) {
    outfile <- sprintf("%s_chunk%02d.rds", serial, j)
    output_s <- readRDS(file = outfile)
    temptemp_cibounds_s <- getExtraOutput(output_s)
    temp_cibounds_s <- c(temp_cibounds_s, temptemp_cibounds_s)
  }
  temp_cibounds_s <- do.call(rbind, temp_cibounds_s)
  temp_cibounds_s <- cbind(temp_cibounds_s, cond_expanded)
  cibounds_s <- rbind(cibounds_s, temp_cibounds_s)
}
identical(cibounds_n, cibounds_p8, cibounds_p16, cibounds_p32, cibounds_s)

# Because all CI bounds are identical, I will use only cibounds_n to summarize the results

cibounds_n <- as.data.frame(cibounds_n)
cibounds_n$coverage_wald <- with(cibounds_n, waldlower.cov < faccor & faccor < waldupper.cov)
cibounds_n$coverage_perc <- with(cibounds_n, perclower < faccor & faccor < percupper)
cibounds_n$coverage_bca <- with(cibounds_n, bcalower < faccor & faccor < bcaupper)

sumout <- aggregate(cbind(coverage_wald, coverage_perc, coverage_bca) 
                    ~ p + faccor + loading + samplesizes, data=cibounds_n, 
                    FUN=mean)

library(tidyr)
library(dplyr)

plotdat <- sumout %>%
  pivot_longer(
    cols = c(coverage_wald, coverage_perc, coverage_bca),
    names_to = "method",
    values_to = "coverage"
  )

plotdat <- plotdat %>%
  mutate(
    method = recode(method,
                    coverage_wald = "Wald",
                    coverage_perc = "Percentile",
                    coverage_bca  = "BCa"),
    faccor = factor(faccor),
    p = factor(p),
    loading = factor(loading)
  )
plotdat$samplesizes <- factor(plotdat$samplesizes)

png("figures//coverage.png", width = 1200, height = 1200, res = 150)
library(ggplot2)

ggplot(plotdat,
       aes(x = samplesizes,
           y = coverage,
           color = method,
           linetype = faccor,
           group = interaction(method, faccor))) +
  
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.5) +
  
  geom_hline(yintercept = 0.95,
             linetype = "dashed",
             linewidth = 0.8,
             color = "black") +
  
  facet_grid(p ~ loading,
             labeller = labeller(
               p = label_both,
               loading = label_both
             )) +
  
  labs(
    x = "Sample Size",
    y = "Coverage Probability",
    color = "Method",
    linetype = "Factor Correlation"
  ) +
  
  theme_bw(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    
    axis.text.x = element_text(size = 12),
    
    strip.background = element_rect(fill = "grey90"),
    strip.text = element_text(size = 12),
    
    panel.grid.minor = element_blank()
  ) + 
  
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "black") +
  
  guides(
    color = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  )

dev.off()
