
################################################################################
##                                   Figures                                  ##
################################################################################

library(ggplot2)
library(patchwork)

dlaplace <- function(x, mu = 0, sd = 1) exp(-abs(x-mu)/(sd / sqrt(2))) / (2*(sd / sqrt(2)))
dratio_lap_norm <- function(x, mu = 0, sd = 1) {
  dnorm(x, mu, sd) / dlaplace(x, mu, sd)
}

ggplot() +
  stat_function(fun = dnorm, args = list(mean = 1, sd = 1),
                col = "lightblue", linewidth = 1, linetype = 1) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = sqrt(2)),
                col = "navy", linewidth = 1, linetype = 4) +
  theme_classic() +
  xlim(-5, 5) +
  ylim(0, 0.5) +
  ylab(NULL)

ggsave("densities.png", device = "png",
       path = "presentation", dpi = 500,
       width = 9, height = 6)


ggplot() +
  stat_function(fun = dlaplace, args = list(mu = 0, sd = 1),
                col = "lightblue", linewidth = 1, linetype = 1) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                col = "navy", linewidth = 1, linetype = 4) +
  xlim(-5, 5) +
  ylim(0, 0.8) +
  theme_classic() +
  ylab(NULL) +
ggplot() +
  stat_function(fun = dratio_lap_norm, args = list(mu = 0, sd = 1),
                linewidth = 1, linetype = 1) +
  xlim(-5, 5) +
  ylim(0, 2) +
  theme_classic() +
  ylab(NULL) +
ggplot() +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                col = "lightblue", linewidth = 1, linetype = 1) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1),
                col = "navy", linewidth = 1, linetype = 4) +
  xlim(-5, 5) +
  ylim(0, 0.8) +
  theme_classic() +
  ylab(NULL) +
ggplot() +
  geom_abline(intercept = 1, slope = 0, linewidth = 1, linetype = 1) +
  theme_classic() +
  xlim(-5, 5) +
  ylim(0, 2) +
  ylab(NULL)

ggsave("densityratios.png", device = "png",
       path = "presentation", dpi = 500,
       width = 9, height = 6)
