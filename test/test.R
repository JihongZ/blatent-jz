library(tidyverse)
load("~/Library/CloudStorage/OneDrive-Personal/2022_Projects/1_PhD_Thesis/Reproduciable_example/N1000_Rho50_id_Example1_Blatent_all.RData")
source("~/Library/CloudStorage/OneDrive-Personal/2022_Projects/1_PhD_Thesis/Reproduciable_example/functions_analysis_models.R")
ppmc_m2_res1 <- blatentPPMC(model = blatent_res[[1]], nSamples = 2000, parallel = TRUE, type = "m2")
ppmc_m2_res2 <- blatentPPMC(model = blatent_res[[2]], nSamples = 2000, parallel = TRUE, type = "m2")
ppmc_m2_res3 <- blatentPPMC(model = blatent_res[[3]], nSamples = 2000, parallel = TRUE, type = "m2")
ppmc_m2_res4 <- blatentPPMC(model = blatent_res[[4]], nSamples = 2000, parallel = TRUE, type = "m2")
ppmc_m2_res5 <- blatentPPMC(model = blatent_res[[5]], nSamples = 2000, parallel = TRUE, type = "m2")

PP_M2dist_long <- data.frame(
  M1 = ppmc_m2_res1$m2$samples,
  M2 = ppmc_m2_res2$m2$samples,
  M3 = ppmc_m2_res3$m2$samples,
  M4 = ppmc_m2_res4$m2$samples,
  M5 = ppmc_m2_res5$m2$samples
) |>
  pivot_longer(everything(), names_to = "model", values_to = "m2_values")


m2_summary <- PP_M2dist_long %>%
  group_by(model) %>%
  summarise(
    mean = mean(m2_values),
    max = max(m2_values),
    median = median(m2_values)
  )

#
ggplot(PP_M2dist_long) +
  geom_freqpoly(aes(x = m2_values,
                    col = factor(model,
                                 levels = c("M1", "M2", "M3", "M4", "M5", "M_bl"))),
                alpha = 0.5, size = 1.5) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_colour_brewer(
    labels = c(
      "Model1: Correct model",
      "Model2: 10% underspecied model",
      "Model3: 20% underspecied model",
      "Model4: 10% misspecified model",
      "Model5: 20% misspecified model",
      "Model6: baseline model"
    ), name = "",
    type = "qual",
    palette = "Set1",
    direction = 1,
    aesthetics = "colour"
  ) +
  labs(title = "M2 values for 5 LCDM models across 2000 replications") +
  theme_classic()
ggsave("M2_density.png", width = 13, height = 9)
library(ggplot2)
