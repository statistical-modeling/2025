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


# Binomial vs poisson ----------------------------------------------------------
# Load ggplot2
library(ggplot2)
library(dplyr)

# Parameters for the binomial distribution
n_trials <- 20
p_success <- 0.05

# Generate probability data
binomial_data <- data.frame(
  x = 0:n_trials,
  prob = dbinom(0:n_trials, size = n_trials, prob = p_success)
)

# Create the plot with vertical lines
ggplot(binomial_data, aes(x = x, y = prob)) +
  geom_segment(aes(xend = x, yend = 0), linetype = "solid", size = 1) +
  scale_x_continuous(limits = c(0, 7)) +
  labs(
    title = sprintf("n = %d, p = %.2f", n_trials, p_success),
    x = "Number of successes",
    y = "Probability"
  ) +
  theme_publication()
ggsave("05_glm2/figs/binomial.png", heigh = 4.5, width = 4.5)

# Second binomial

n_trials2 <- 10000

# Generate probability data
binomial_data2 <- data.frame(
  x = 0:n_trials2,
  prob = dbinom(0:n_trials2, size = n_trials2, prob = p_success)
)

ggplot(binomial_data2, aes(x = x, y = prob)) +
  geom_segment(aes(xend = x, yend = 0), linetype = "solid", size = 1) +
  scale_x_continuous(limits = c(420, 580)) +
  labs(
    title = sprintf("n = %d, p = %.2f", n_trials2, p_success),
    x = "Number of successes",
    y = "Probability"
  ) +
  theme_publication()
ggsave("05_glm2/figs/binomial2.png", heigh = 4.5, width = 4.5)

# Third binomial monk ----------------------------------------------------------

n_trials <- 1000
p_success <- 0.001

# Generate probability data
binomial_data <- data.frame(
  x = 0:1e5,
  prob = dbinom(0:1e5, size = n_trials, prob = p_success)
)

y <- rbinom(n = 1e5, size = 1000, prob = 1/1000)

prob_data <- as.data.frame(table(y))  # Count occurrences
colnames(prob_data) <- c("x", "frequency")  # Rename columns
prob_data$x <- as.numeric(as.character(prob_data$x))  # Convert x to numeric
prob_data$prob <- prob_data$frequency / sum(prob_data$frequency)  # Normalize to probabilities

# Create the plot with vertical lines
ggplot(prob_data, aes(x = x, y = prob)) +
  geom_segment(aes(xend = x, yend = 0), linetype = "solid", size = 1) +
  scale_x_continuous(limits = c(0, 10)) +
  labs(
    title = sprintf("Binomial Distribution (n = %d, p = %.3f)", n_trials, p_success),
    x = "Number of successes",
    y = "Probability"
  ) +
  theme_publication()
ggsave("05_glm2/figs/binomial.png", heigh = 4.5, width = 4.5)

