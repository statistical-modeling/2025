## load required libraries
library(tidyverse)
library(rethinking)
library(lme4)

## all data sets compiled by Ohyama et al
raw <- read.csv("https://github.com/piLaboratory/bie5781/raw/master/data/jbi14149-sup-0003-appendixs3.csv")

## Archipelagoes with more than 4 islands
more.than.4 <-
    count(raw, Study.location) %>%
    filter(n > 4) %>%
    select(Study.location) %>%
    unique() %>%
    unlist()
## Data set with selected archipelagoes
## and transformed area and richness
islands <-
    filter(raw, Study.location %in% more.than.4 & SAR_TYPE == "Insular") %>%
    mutate(l_area = log(Island_area), l_S = log(Species.Richness),
           site_fac= factor(Study.location), site = as.integer(site_fac),
           ls_area = (l_area - mean(l_area))/sd(l_area),
           lc_S = l_S - mean(l_S))

p2 <-
    islands %>%
    ggplot(aes(x = ls_area, y = lc_S)) +
    geom_point() +
    facet_wrap(~ site_fac) +
    xlab("Standardized ln Island Area (m2)") +
    ylab("Centered ln Species Richness") +
    theme_bw()

p2



## Bayesian fitting
## Model with a single slope and intercept
m0 <- ulam(
    alist(
        lc_S ~ dnorm( mu, sigma ) ,
        mu <- alpha + beta * ls_area,
        alpha ~ dnorm(0, 2),
        beta ~ dlnorm(-1, 0.87), ## prior probability of 95% that beta is lower than 1.5
        sigma ~ dexp(1)
    ) , data = islands[, c("ls_area", "lc_S")] , chains=4 , cores=4 , log_lik=TRUE )

(m0.cf <- precis(m0, digits =3))


islands %>%
    ggplot(aes(x = ls_area, y = lc_S)) +
    geom_point() +
    geom_abline(aes(intercept = m0.cf["alpha", 1], slope = m0.cf["beta",1]), col="navy") +
    xlab("ln Island Area (m2)") +
    ylab("ln Species Richness") +
    theme_bw()

p2 +
    geom_abline(aes(intercept = m0.cf["alpha", 1], slope = m0.cf["beta",1]), col="navy")

## Model with independent slopes
m1 <- ulam(
    alist(
        lc_S ~ dnorm( mu, sigma ) ,
        mu <- a[site] + b[site] * ls_area,
        a[site] ~ dnorm(0, 4),
        b[site] ~ dlnorm(-1, 0.575),## prior probability of 95% that beta is lower than 1
        sigma ~ dexp(1)
    ) , data = islands[, c("ls_area", "lc_S", "site")] , chains=4 , cores=4 , log_lik=TRUE )

(m1.cf <- precis(m1, digits =3, depth =2))

##
nsites <- max(islands$site)
m1.ab <- data.frame(
    site = 1:nsites,
    site_fac = levels(islands$site_fac),
    intercept = m1.cf[1:nsites,1],
    slope = m1.cf[(nsites+1):(2*nsites),1])

p2 +
    geom_abline(data = m1.ab,
                aes(intercept = intercept, slope = slope),
                col="navy")


## Model with pooled slopes
m2 <- ulam(
    alist(
        lc_S ~ dnorm( mu, sigma ) ,
        mu <- a[site] + b[site] * ls_area,
        a[site] ~ dnorm(mu_a, sigma_a),
        b[site] ~ dnorm(mu_b, sigma_b),
        ##prior
        sigma ~ dexp(1),
        ## hyper-priors
        mu_a ~ dnorm(0, 4),
        mu_b~ dlnorm(0, 0.7), ## prior probability of 95% that beta is lower than 1.5
        sigma_a ~ dexp(1),
        sigma_b ~ dexp(0.25)
    ) , data = islands[, c("ls_area", "lc_S", "site")] , chains=4 , cores=4 , log_lik=TRUE )


(m2.cf <- precis(m2, depth = 2, digits=3))



m2.ab <- data.frame(
    site = 1:nsites,
    site_fac = levels(islands$site_fac),
    intercept = m2.cf[1:nsites,1],
    slope = m2.cf[(nsites+1):(2*nsites),1])

p2 +
    geom_abline(data = m1.ab,
                 aes(intercept = intercept, slope = slope),
                col="navy") +
     geom_abline(data = m2.ab,
                 aes(intercept = intercept, slope = slope),
                col="red")

plot(m1.ab$slope, rep(1,nrow(m1.ab)),
     ylim = c(0.5, 2.5),
     xlab = "Mean posterior slopes",
     yaxt="n", ylab="" , cex.axis=2,
     cex.lab = 2, pch = 19)
points(m2.ab$slope, rep(2,nrow(m2.ab)), pch =19)
segments(y0 = rep(1,nrow(m1.ab)), x0 = m1.ab$slope,
         y1 = rep(2,nrow(m1.ab)), x1 = m2.ab$slope,
         lty =2)
axis(2, at = c(1, 2), labels = c("M1", "M2"), cex.axis=2, adj = 1, las = 2)       

## Sample posterior and simulate posterior of slopes
m2.samp <- extract.samples(m2, pars = c("mu_b", "sigma_b"))
sim.slopes <- rnorm(length(m2.samp$mu_b), mean = m2.samp$mu_b, sd = m2.samp$sigma_b)
## Simulate prior of slopes
m2.priors <- data.frame(mu_b = rlnorm(1e4, meanlog = -1, sdlog = 0.87),
                        sigma_b = rexp(1e4, rate =1))
sim.prior.slopes <- rnorm(length(m2.priors$mu_b), mean = m2.priors$mu_b, sd = m2.priors$sigma_b)

plot(density(sim.slopes),
     xlab = "Slopes", main ="")
lines(density(sim.prior.slopes), col ="blue")
legend("topleft", legend = c("Prior", "Posterior"),
       lty =1, col=c("blue","black"), bty = "n")

compare(m0, m1, m2)



## ML
m0.ML <- lm(lc_S ~ ls_area, data = islands)
m1.ML <- lm(lc_S ~ ls_area + ls_area:site_fac, data=islands)
m2.ML <- lmer(lc_S ~ ls_area + (ls_area|site_fac), data = islands, REML=FALSE)

AICtab(m0.ML, m1.ML, m2.ML)


summary(m2.ML)

(m2.ML.cf <- coef(m2.ML)$site_fac)
m2.ML.cf$site_fac <- factor(rownames(m2.ML.cf))
names(m2.ML.cf)[1:2] <- c("intercept", "slope")

p2 +
    geom_abline(data = m2.ML.cf,
                 aes(intercept = intercept, slope = slope),
                col="green") +
     geom_abline(data = m2.ab,
                 aes(intercept = intercept, slope = slope),
                col="red")
