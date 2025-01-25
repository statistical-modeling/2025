library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(rethinking)
library(ggridges)
library(viridis)
library(latex2exp)

## ggplot theme
theme_publication <- function(base_size = 14, base_family = "helvetica") {
  (theme_foundation(base_size = base_size, base_family = base_family)
   + theme(plot.title = element_text(face = "bold",
                                     size = rel(1.2), hjust = 0.5),
           text = element_text(),
           panel.border = element_rect(colour = NA),
           panel.background = element_rect(fill = 'transparent'),
           plot.background = element_rect(fill = 'transparent', color = NA),
           axis.title = element_text(face = "bold",size = rel(1)),
           axis.title.y = element_text(angle=90,vjust =2),
           axis.title.x = element_text(vjust = -0.2),
           axis.text = element_text(), 
           axis.line = element_line(colour="black"),
           axis.ticks = element_line(),
           panel.grid.major = element_line(colour=NA),
           panel.grid.minor = element_blank(),
           legend.key = element_rect(colour = NA),
           legend.position = "bottom",
           legend.direction = "horizontal",
           legend.key.size= unit(0.2, "cm"),
           ##legend.margin = unit(0, "cm"),
           legend.spacing = unit(0.2, "cm"),
           legend.title = element_text(face="italic"),
           plot.margin = unit(c(10,5,5,5),"mm"),
           strip.background = element_rect(colour = NA,fill = "transparent"),
           strip.text = element_text(face="bold")
   ))
  
}

## Aux functions
f.rib <- function(X, dpad = 5)
  geom_ribbon(aes(ymin= y - X*dpad, ymax = y + X*dpad), fill = "blue", alpha = 0.01)

make.q <- function(from, to, cfs, normal=TRUE, sig, size=100){
  if(normal)
    df <- data.frame(x = seq(from, to, length =size)) %>%
      mutate(y = cfs[1]+cfs[2]*x,
             low.1 =  qnorm(0.05, size, mean = y, sd =sig),
             upp.1 =  qnorm(0.95, size, mean = y, sd =sig),
             low.2 =  qnorm(0.25, size, mean = y, sd =sig),
             upp.2 =  qnorm(0.75, size, mean = y, sd =sig))
  else
    df <- data.frame(x = seq(from, to, length =size)) %>%
      mutate(y = cfs[1]+cfs[2]*x,
             low.1 =  qpois(0.05, size, lambda = y),
             upp.1 =  qpois(0.95, size, lambda = y),
             low.2 =  qpois(0.25, size, lambda = y),
             upp.2 =  qpois(0.75, size, lambda = y))
} 

plot.q <- function(df2, df1){
  ggplot(df2, aes(x, y)) +
    geom_line(col="navy") +
    geom_ribbon(aes(ymin = low.1, ymax = upp.1), fill="blue", alpha =0.25) +
    geom_ribbon(aes(ymin = low.2, ymax = upp.2), fill="blue", alpha =0.25) +
    geom_point(data = df1, color ="red", size=1.25, stroke=1.25)  +
    theme_Publication(18)
}

# Normal -----------------------------------------------------------------------

df1 <- data.frame(
  case_number = factor(1:5),
  caseMean = c(-4, -2, 0, 2, 4),
  caseSD = 1
)

n <- 100
df3 <-
  df1 %>%
  mutate(low  = caseMean - 3 * caseSD, high = caseMean + 3 * caseSD) %>%
  uncount(n, .id = "row") %>%
  dplyr::mutate(x    = (1 - row / n) * low + row / n * high,
                norm = dnorm(x, caseMean, caseSD))

ggplot(df3, aes(x, factor(case_number), height = norm, color = case_number)) +
  geom_ridgeline(
    scale = 3,
    aes(fill = case_number),
    alpha = 0.5,
    linetype = 0
  ) +
  xlim(-9, 7) +
  theme_publication(18) +
  scale_color_viridis_d() +
  theme(
    legend.position = "none",
    axis.line.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank()
  ) +
  annotate(
    "text",
    x = rep(-8.5, 5),
    y = 1:5 + 0.05,
    label = c(
      TeX(r"($\mu = - 4$)"),
      TeX(r"($\mu = - 2$)"),
      TeX(r"($\mu = 0$)"),
      TeX(r"($\mu = 2$)"),
      TeX(r"($\mu = 4$)")
    ),
    size = 6
  )

ggsave("04_glm/figs/normal_mu.png")


df1 <- data.frame(
  case_number = factor(1:5),
  caseMean = 0,
  caseSD = rev(seq(0.5, 2.5,  by =0.5))
)

n <- 100
df3 <-
  df1 %>%
  mutate(low  = caseMean - 3 * caseSD, high = caseMean + 3 * caseSD) %>%
  uncount(n, .id = "row") %>%
  dplyr::mutate(x    = (1 - row/n) * low + row/n * high, 
                norm = dnorm(x, caseMean, caseSD))

ggplot(df3, aes(x, factor(case_number), height = norm, color = case_number)) +
  geom_ridgeline(scale = 3, aes(fill = case_number), alpha =0.5, linetype = 0) +
  xlim(-9,7) + 
  theme_publication(18) +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()
  ) +
  annotate("text", x = rep(-8.5,5),
           y = 1:5 + 0.05,
           label = c(TeX(r"($\sigma = 2.5$)"), TeX(r"($\sigma = 2$)"), TeX(r"($\sigma = 1.5$)"),
                     TeX(r"($\sigma = 1$)"), TeX(r"($\sigma = 0.5$)")),
           size = 6)

ggsave("04_glm/figs/normal_sigma.png") # Saving 9.94 x 11.5 in image


# Exponential ------------------------------------------------------------------
df_exp <- data.frame(
  case_number = factor(1:5),
  rate = seq(0.5, 2.5, by = 0.5)
)

n <- 100
df_exp_ridges <- 
  df_exp %>%
  mutate(low = 0, high = 10) %>%
  uncount(n, .id = "row") %>%
  mutate(x = (1 - row/n) * low + row/n * high, 
         density = dexp(x, rate = rate))

ggplot(df_exp_ridges, aes(x, factor(case_number), height = density, color = case_number)) +
  geom_ridgeline(scale = 3, aes(fill = case_number), alpha = 0.5, linetype = 0) +
  xlim(0, 10) + 
  theme_publication(18) +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  annotate("text", x = rep(6.5, 5),
           y = 1:5 + 0.05,
           label = c(TeX(r"($\lambda = 0.5$)"), TeX(r"($\lambda = 1$)"), TeX(r"($\lambda = 1.5$)"),
                     TeX(r"($\lambda = 2$)"), TeX(r"($\lambda = 2.5$)")),
           size = 6)

ggsave("04_glm/figs/exponential.png")

# Gamma ------------------------------------------------------------------------
df_gamma <- data.frame(
  case_number = factor(1:5),
  shape = 1:5,
  rate = 1
)

n <- 100
df_gamma_ridges <- 
  df_gamma %>%
  mutate(low = 0, high = 10) %>%
  uncount(n, .id = "row") %>%
  mutate(x = (1 - row/n) * low + row/n * high, 
         density = dgamma(x, shape = shape, rate = rate))

ggplot(df_gamma_ridges, aes(x, factor(case_number), height = density, color = case_number)) +
  geom_ridgeline(scale = 3, aes(fill = case_number), alpha = 0.5, linetype = 0) +
  xlim(0, 10) + 
  theme_publication(18) +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  annotate("text", x = rep(9.5, 5),
           y = 1:5 + 0.05,
           label = paste("k = ", 1:5),
           size = 6)

ggsave("04_glm/figs/gamma.png")

# Poisson ----------------------------------------------------------------------
df_poisson <- data.frame(
  case_number = factor(1:5),
  lambda = seq(1, 5, by = 1)
)

n <- 100
df_poisson_ridges <- 
  df_poisson %>%
  mutate(low = 0, high = 10) %>%
  uncount(n, .id = "row") %>%
  mutate(x = (1 - row/n) * low + row/n * high, 
         density = dpois(round(x), lambda = lambda))

ggplot(df_poisson_ridges, aes(x, factor(case_number), height = density, color = case_number)) +
  geom_ridgeline(scale = 3, aes(fill = case_number), alpha = 0.5, linetype = 0) +
  xlim(0, 10) + 
  theme_publication(18) +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  annotate("text", x = rep(9.5, 5),
           y = 1:5 + 0.05,
           label = c(TeX(r"($\lambda = 1$)"), TeX(r"($\lambda = 2$)"), TeX(r"($\lambda = 3$)"),
                     TeX(r"($\lambda = 4$)"), TeX(r"($\lambda = 5$)")),
           size = 6)

ggsave("04_glm/figs/poisson.png")

# Binomial ---------------------------------------------------------------------
df_binom <- data.frame(
  case_number = factor(1:5),
  size = 10,
  prob = seq(0.2, 1, by = 0.2)
)

n <- 100
df_binom_ridges <- 
  df_binom %>%
  mutate(low = 0, high = size) %>%
  uncount(n, .id = "row") %>%
  mutate(x = round((1 - row/n) * low + row/n * high), 
         density = dbinom(x, size = size, prob = prob))

ggplot(df_binom_ridges, aes(x, factor(case_number), height = density, color = case_number)) +
  geom_ridgeline(scale = 3, aes(fill = case_number), alpha = 0.5, linetype = 0) +
  xlim(0, 10) + 
  theme_publication(18) +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank()) +
  annotate("text", x = rep(0, 5),
           y = 1:5 + 0.05,
           label = c(TeX(r"($p = 0.2$)"), TeX(r"($p = 0.4$)"), TeX(r"($p = 0.6$)"),
                     TeX(r"($p = 0.8$)"), TeX(r"($p = 1.0$)")),
           size = 6)

ggsave("04_glm/figs/binomial.png")

# Distributions pannel ---------------------------------------------------------

# Data for the binomial distribution (discrete)
n <- 5
p <- 0.3
x_binom <- 0:n
y_binom <- dbinom(x_binom, size = n, prob = p)
cum_binom <- pbinom(x_binom, size = n, prob = p)

# Data for the exponential distribution (continuous)
x_exp <- seq(0, 6, length.out = 100)
lambda <- 1.5
y_exp <- dexp(x_exp, rate = lambda)
cum_exp <- pexp(x_exp, rate = lambda)

# Create the binomial probability plot
g1 <- ggplot(data.frame(x = x_binom, y = y_binom), aes(x = x, y = y)) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = y)) +
  labs(title = "Probability", x = "", y = "Probability") +
  annotate("text", label = "n = 5, p = 0.3", x = 4, y = 0.34, size = 6) +
  theme_publication()

# Create the binomial cumulative probability plot
g2 <- ggplot(data.frame(x = x_binom, y = cum_binom), aes(x = x, y = y)) +
  geom_segment(aes(x = x, xend = x, y = 0, yend = y)) +
  labs(title = "Cumulative probability", x = "", y = "Cumulative probability") +
  theme_publication()

# Create the exponential probability density plot
g3 <- ggplot(data.frame(x = x_exp, y = y_exp), aes(x = x, y = y)) +
  geom_line() +
  labs(title = "Probability density", x = "x", y = "Density") +
  annotate("text", label = TeX(r"($\lambda = 1.5$)"), x = 5, y = 1.4, size = 6) +
  theme_publication()

# Create the exponential cumulative probability plot
g4 <- ggplot(data.frame(x = x_exp, y = cum_exp), aes(x = x, y = y)) +
  geom_line() +
  labs(title = "Cumulative probability", x = "x", y = "Cumulative probability") +
  theme_publication()

# Combine the four plots into a 2x2 grid using patchwork
library(patchwork)
(g1 + theme_publication() | g2 + theme_publication())
ggsave("04_glm/figs/discrete.png", bg = "transparent", width = 9, height = 4.5)

(g3 + theme_publication()| g4+ theme_publication())
ggsave("04_glm/figs/continuous.png", bg = "transparent", width = 9, height = 4.5)
