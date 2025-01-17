# Graphics


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

# Expect -----------------------------------------------------------------------
p_expect <- ggplot() +
  #geom_abline(slope = -0.6, intercept = .7, lwd = 1.5) +
  my_theme +
  labs(x = "Tannin", y = "Growth") +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) + my_theme

p_expect
ggsave("03_linear_models/figs/expect0.png", bg = "transparent", width = 6, height = 6)

plot(0, 0, type = "n", bty = "l", las = 1, 
     xaxt = "n", yaxt = "n", xlab = "X", ylab  = "Y")
abline(a = 0, b = 0.7)
segments(x0 = -0.42, y0 = -0.3, x1 = 0.13, y1 = -0.3)
segments(x0 = 0.13, y0 = -0.3, x1 = 0.13, y1 = .09)
text(x = 0.2, y = -.1, '}', cex = 4)
text(x = 0.33, y = -.1, expression(paste(beta, 1)), cex = 2)
arrows(x0 = -0.42, y0 = -0.4, x1 = 0.13, y1 = -0.4, angle = 90, 
       length = 0.05, code = 3)
text(x = -.145, y = -0.45, "1")
text(x = -1, y = -.89, '}', cex = 3.7)
text(x = -0.85, y = -.89, expression(paste(beta, "0")), cex = 2)


read.csv("03_linear_models/crawley_regression.csv")
