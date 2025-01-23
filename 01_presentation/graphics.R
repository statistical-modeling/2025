# Load the necessary libraries
library(rethinking)
library(ggplot2)
library(dplyr)
library(readr)

area <- sort(c(321000, 290000, 144000, 70000, 48000, 25000, 4500, 8870, 18000, 4600))
species <- c(540, 420, 368, 220, 337, 232, 131, 143, 137, 108)
island <- c("New \nGuinea", "Borneo", "Phillipines", "Celebes", 
            "Java", "Ceylon", "Palawan", "Flores", "Timor", "Sumba")


fake_df <- data.frame(area = area, 
                 species = species, 
                 island = island)


## ggplot theme
my_theme <- theme(
  panel.background = element_rect(fill='transparent'),
  plot.background = element_rect(fill='transparent', color=NA),
  #panel.grid.major = element_blank(),
  #panel.grid.minor = element_blank(),
  legend.background = element_rect(fill='transparent', color = NA),
  legend.box.background = element_rect(fill='transparent', color = NA),
  axis.title = element_text(face = "bold",size = rel(1.2)),
  axis.title.y = element_text(angle=90,vjust =2),
  axis.title.x = element_text(vjust = -0.2),
  axis.line = element_line(colour="black"),
  axis.ticks = element_line(),
  axis.text.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  legend.key = element_rect(colour = NA),
  legend.position = "bottom",
  legend.direction = "horizontal",
  legend.key.size= unit(0.2, "cm")
)



df <- read_delim("01_presentation/BiomassDataset.csv",
                   delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ","),
                   trim_ws = TRUE)


# Define the model
model <- alist(
  Biomass ~ dnorm(mu, sigma),
  mu <- a + b * AGCWP,
  a ~ dnorm(10, 5),  # Prior for intercept
  b ~ dnorm(0, 1),   # Prior for slope
  sigma ~ dnorm(0, 1)    # Prior for residual SD
)

# Fit the model
fit <- quap(
  model,
  data = df
)

# Extract posterior samples
post <- extract.samples(fit)

# Generate posterior predictions for a sequence of AGCWP values
AGCWP_seq <- seq(min(df$AGCWP), max(df$AGCWP), length = 100)
mu_pred <- sapply(AGCWP_seq, function(x) post$a + post$b * x)

# Calculate mean and credible intervals
mu_df <- data.frame(
  AGCWP = AGCWP_seq,
  mean = apply(mu_pred, 2, mean),
  lower = apply(mu_pred, 2, PI, prob = 0.999)[1, ],
  upper = apply(mu_pred, 2, PI, prob = 0.999)[2, ]
)

# Combine datasets for ggplot compatibility
data_combined <- df %>% 
  rename(mean = Biomass) %>% 
  mutate(lower = NA, upper = NA)

mu_df$source <- "Model"
data_combined$source <- "Observed"

final_data <- bind_rows(mu_df, data_combined)

# Data plot --------------------------------------------------------------------------
data_plot <- ggplot() +
  geom_point(data = df, aes(x = AGCWP, y = Biomass), color = "forestgreen", size = 3) +
  labs(
    x = "AGCWP (Mg C ha^-1 yr^-1)",
    y = "Biomass (Mg DW ha^-1 / Basal area)"
  ) +
  my_theme

data_plot
ggsave("01_presentation/figs/data_plot.png", bg = "transparent", width = 6, height = 6)



# Fit --------------------------------------------------------------------------
fit <- ggplot() +
  geom_point(data = df, aes(x = AGCWP, y = Biomass), color = "forestgreen", size = 3) +
  geom_line(data = mu_df, aes(x = AGCWP, y = mean), color = "black") +
  geom_ribbon(data = mu_df, aes(x = AGCWP, ymin = lower, ymax = upper), fill = "gray", alpha = 0.5) +
  labs(
    x = "AGCWP (Mg C ha^-1 yr^-1)",
    y = "Biomass (Mg DW ha^-1 / Basal area)"
  ) +
  my_theme

fit
ggsave("01_presentation/figs/fit.png", bg = "transparent", width = 6, height = 6)

# Expect variation -------------------------------------------------------------
expect_variation <- ggplot(data = fake_df, aes(x = area, y = species)) +
  xlim(0, max(df$area + 100000)) +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10", labels = scales::comma) +
  geom_point() +
  labs(x = "Wood productivity", y = "Biomass") +
  geom_smooth(method = "gam",  level = 0.9995,
              color = "#586e75", fill = "#586e75", alpha = .2) + 
  annotate("text", x = 45000, y = 180, label= "Expected", color="#586e75", 
           size=7, angle = -40) +
  annotate("text", x = 3900, y = 420, label= "Variation", color="#586e75", 
           size=7, angle = 90) +
  my_theme

expect_variation
ggsave("01_presentation/figs/expect_variation.png", bg = "transparent", width = 6, height = 6)


# Expect var2 ------------------------------------------------------------------
ggplot(data = fake_df, aes(x = area, y = species)) +
  xlim(0, max(df$area + 100000)) +
  geom_point() +
  labs(x = "Wood productivity", y = "Biomass") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10", labels = scales::comma) +
  geom_smooth(method = "gam",  level = 0.9995,
              color = "#586e75", fill = "#586e75", alpha = .2) + 
  annotate("text", x = 45000, y = 180, label= "mu", color="#586e75", size=7, angle = -40, parse = TRUE) +
  annotate("text", x = 3900, y =420, label= "sigma", color="#586e75", size=7, angle = 90, parse = TRUE) +
  my_theme

ggsave("01_presentation/figs/expect_variation2.png", bg = "transparent", width = 6, height = 6)


# Expect var3 ------------------------------------------------------------------
ggplot(data = fake_df, aes(x = area, y = species)) +
  xlim(0, max(df$area + 100000)) +
  geom_point() +
  labs(x = "Wood productivity", y = "Biomass") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10", labels = scales::comma) +
  geom_smooth(method = "gam",  level = 0.9995,
              color = "#586e75", fill = "#586e75", alpha = .2) + 
  my_theme

ggsave("01_presentation/figs/expect_variation3.png", bg = "transparent", width = 6, height = 6)


# Theory -----------------------------------------------------------------------
ggplot() +
  geom_abline(slope = 0.6, intercept = 0.1, lwd = 1.5) +
  labs(x = "Wood productivity", y = "Biomass") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + my_theme

ggsave("01_presentation/figs/theory1.png", bg = "transparent", width = 6, height = 6)

# Theory alt -------------------------------------------------------------------
ggplot() +
  geom_abline(slope = -0.05, intercept = .525, lwd = 1.5) +
  geom_hline(yintercept = .5, lwd = 1.5) +
  labs(x = "Wood productivity", y = "Biomass") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + my_theme

ggsave("01_presentation/figs/theory2.png", bg = "transparent", width = 6, height = 6)

# Theory alt2 -------------------------------------------------------------------
ggplot() +
  geom_abline(slope = -0.6, intercept = .7, lwd = 1.5) +
  #geom_hline(yintercept = .5, lwd = 1.5) +
  labs(x = "Wood productivity", y = "Biomass") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + my_theme

ggsave("01_presentation/figs/theory3.png", bg = "transparent", width = 6, height = 6)

