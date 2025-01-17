# Graphics for presentation

library(ggplot2)

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

## Data


area <- c(321000, 290000, 144000, 70000, 48000, 25000, 4500, 8870, 18000, 4600)
species <- c(540, 420, 368, 220, 337, 232, 111, 143, 137, 108)
island <- c("New \nGuinea", "Borneo", "Phillipines", "Celebes", 
            "Java", "Ceylon", "Palawan", "Flores", "Timor", "Sumba")


df <- data.frame(area = area, 
                 species = species, 
                 island = island)


# Theory -----------------------------------------------------------------------
set.seed(1492)


p_theory <- ggplot() +
  xlim(-5, 5) +
  geom_function(aes(colour = "Extinction"), fun = dnorm, args = list(mean = 90, sd = 18.5), lwd = 1) +
  geom_function(aes(colour = "Extinction"), fun = dnorm, args = list(mean = 90, sd = 19), lwd = 1) +
  geom_function(aes(colour = "Immigration"), fun = dnorm, args = list(mean = -90, sd = 19), lwd = 1) +
  labs(colour = "", x = "Number of species", y = "Rate") +
  annotate("text", x = 1.5, y = 5e-7, label = "Small", size = 5) +
  annotate("text", x = 1.5, y = 3e-7, label = "Large", size = 5) +
  my_theme
  

p_theory
ggsave("01_presentation/figs/theory.png", bg = "transparent", width = 6, height = 6)

# Expect -----------------------------------------------------------------------

p_expect <- ggplot() +
  geom_abline(slope = 0.6, intercept = 0.1, lwd = 1.5) +
  my_theme +
  labs(x = "Area", y = "Number of species") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + my_theme

p_expect
ggsave("01_presentation/figs/expect.png", bg = "transparent", width = 6, height = 6)

# Plot data --------------------------------------------------------------------
data_plot <- ggplot(data = df, aes(x = area, y = species)) +
  geom_point() +
  labs(x = "Area", y = "Species") +
  #scale_y_continuous(trans = "log10") +
  scale_x_continuous(labels = scales::comma) +
  #geom_smooth(method = "lm", color = "#586e75", fill = "#586e75", alpha = .2) +
  geom_text(label = island, vjust = 1.5) +
  my_theme

data_plot
ggsave("01_presentation/figs/data_plot.png", bg = "transparent", width = 6, height = 6)

data_plot2 <- ggplot(data = df, aes(x = area, y = species)) +
  geom_point() +
  labs(x = "Area (log)", y = "Species (log)") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10", labels = scales::comma) +
  #geom_smooth(method = "lm", color = "#586e75", fill = "#586e75", alpha = .2) +
  geom_text(label = island, vjust = 1.5) +
  my_theme


data_plot2
ggsave("01_presentation/figs/data_plot2.png", bg = "transparent", width = 6, height = 6)


# Expect variation -------------------------------------------------------------
expect_variation <- ggplot(data = df, aes(x = area, y = species)) +
  xlim(0, max(df$area + 100000)) +
  geom_point() +
  labs(x = "Area", y = "Species") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10", labels = scales::comma) +
  geom_smooth(method = "gam",  level = 0.9995,
              color = "#586e75", fill = "#586e75", alpha = .2) + 
  annotate("text", x = 50000, y = 240, label= "Expected", color="#586e75", size=7, angle = 36) +
  annotate("text", x = 3900, y = 110, label= "Variation", color="#586e75", size=7, angle = 90) +
  my_theme

expect_variation
ggsave("01_presentation/figs/expect_variation.png", bg = "transparent", width = 6, height = 6)

# Dnorm ------------------------------------------------------------------------
x1 <- seq(-3.5,3.5, by=0.001)

df2 <- data.frame(
  X = x1,
  Y = dnorm(x1)
) 

p.g <-
  ggplot(df2, aes(X,Y)) +
  geom_line() +
  theme_void() +
  theme(legend.position = "none") +
  geom_segment(aes(xend=X, yend=0, colour = "#586e75", alpha = .3)) +
  geom_segment(aes(x=0, y=dnorm(0), xend=0, yend=0), color="#586e75")

p.g + annotate("text", x = 0, y = 0.42, label= "Expected", color="#586e75", size=10) +
  annotate("segment", x = -3.6, xend = 3.6, y = -0.015, yend = -0.015,
           colour = "#586e75", linewidth = 1.25) +
  annotate("text", x = 3.5, y = -0.05, label= "Y", color="#586e75", size=18)

ggsave("01_presentation/figs/dnorm.png", bg = "transparent", width = 6, height = 6)


# Dnorm2 -----------------------------------------------------------------------
p.g +
    annotate("text", x = 0, y = 0.42, label= "mu", size=15, parse=TRUE) +
    annotate("text", x = 0.5, y = 0.17, label= "sigma", size=15, parse=TRUE) +
    annotate("segment", x = -0.5, xend = 0.5, y = .15, yend = .15,
             colour = "black", linewidth = 1.5, arrow = arrow(ends="both")) +
    annotate("segment", x = -3.6, xend = 3.6, y = -0.015, yend = -0.015,
             colour = "#586e75", size = 1.25) +
    annotate("text", x = 3.5, y = -0.05, label= "Y", color="#586e75", size=18)

ggsave("01_presentation/figs/dnorm2.png", bg = "transparent", width = 6, height = 6)

# Expect var2 ------------------------------------------------------------------
ggplot(data = df, aes(x = area, y = species)) +
  xlim(0, max(df$area + 100000)) +
  geom_point() +
  labs(x = "Area (log)", y = "Species (log)") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10", labels = scales::comma) +
  geom_smooth(method = "gam",  level = 0.9995,
              color = "#586e75", fill = "#586e75", alpha = .2) + 
  annotate("text", x = 50000, y = 240, label= "mu", color="#586e75", size=7, angle = 36, parse = TRUE) +
  annotate("text", x = 3900, y = 110, label= "sigma", color="#586e75", size=7, angle = 90, parse = TRUE) +
  my_theme

ggsave("01_presentation/figs/expect_variation2.png", bg = "transparent", width = 6, height = 6)

# Theory 2 ---------------------------------------------------------------------
ggplot() +
  geom_abline(slope = 0.6, intercept = 0.1, lwd = 1.5) +
  theme_Publication() +
  labs(x = "Area (log)", y = "Number of species (log)") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + my_theme

ggsave("01_presentation/figs/theory_log.png", bg = "transparent", width = 6, height = 6)


# Theory 3----------------------------------------------------------------------
ggplot(data = df, aes(x = area, y = species)) +
  labs(x = "Area (log)", y = "Species (log)") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10", labels = scales::comma) +
  geom_smooth(method = "gam",  level = 0.9995,
              color = "#586e75", fill = "#586e75", alpha = .2) + 
  my_theme

ggsave("01_presentation/figs/theory3.png", bg = "transparent", width = 6, height = 6)

# Fit --------------------------------------------------------------------------
p_fit <- ggplot(data = df, aes(x = area, y = species)) +
  geom_point() +
  labs(x = "Area (log)", y = "Species (log)") +
  scale_y_continuous(trans = "log10") +
  scale_x_continuous(trans = "log10", labels = scales::comma) +
  geom_smooth(method = "lm",  color = "#586e75", se = FALSE) + 
  my_theme

p_fit
ggsave("01_presentation/figs/fit.png", bg = "transparent", width = 6, height = 6)



pred <- predict(mod)

log_c <-  coef(mod)[1]
z <- coef(mod)[2]

log_c + z * log(df$area)


coef(mod)[1] + coef(mod)[2] * log(df$area)
pred

S <- exp(log_c) * df$area ^ z
S

