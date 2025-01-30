library(rethinking)
library(rstan)

data(Kline)

load("05_glm2/poisson.RData")

d <- Kline

d$P <- scale(log(d$population))
d$contact_id <- ifelse(d$contact == "high", 2, 1)

dat <- list(
  T = d$total_tools, 
  P = d$P, 
  cid = d$contact_id
)

# Model 01 ---------------------------------------------------------------------
m01 <- ulam(
  alist(
    T ~ dpois(lambda), 
    log(lambda) <- a + b * P, 
    a ~ dnorm(3, 0.5), 
    b ~ dnorm(0, 0.2)
  ), 
  data = dat, chains = 4,log_lik = TRUE
)


precis(m01)

k <- PSIS(m01,pointwise=TRUE)$k
k

plot( dat$P,dat$T,xlab="log population (std)",ylab="total tools",
      col=rangi2,lwd=2, pch=ifelse(dat$cid==1,1,16),
      ylim=c(0,75), cex=1+normalize(k))

ns <-100
P_seq <-seq(from=-1.4,to=3,length.out=ns)

# predictions 
lambda <-link(m01,data=data.frame(P=P_seq))
lmu <-apply(lambda,2,mean)
lci <-apply(lambda,2,PI)
lines( P_seq,lmu,lty=1,lwd=1.5)
shade( lci,P_seq,xpd=TRUE)

# Model 02 ---------------------------------------------------------------------


m02 <- ulam(
  alist(
    T ~ dpois(lambda), 
    log(lambda) <- a[cid] + b[cid] * P, 
    a[cid] ~ dnorm(3, 0.5),
    b[cid] ~ dnorm(0, 0.2)
  ), 
  data = dat, chains = 4, log_lik = TRUE
)

par(bty = "l")

save(m01, m02, file = "05_glm2/poisson.RData")

# Data -------------------------------------------------------------------------
png("05_glm2/figs/poisson_data.png", bg = "transparent", height = 432, 
    width = 432)
plot( dat$P,dat$T,xlab="log population (std)",ylab="total tools",
      col=rangi2,lwd=2, pch=1,
      ylim=c(0,75), cex=1.5, cex.lab = 1.5, cex.axis = 1.5)
dev.off()

png("05_glm2/figs/poisson_data2.png", bg = "transparent", height = 432, 
    width = 432)
plot( dat$P,dat$T,xlab="log population (std)",ylab="total tools",
      col=rangi2,lwd=2, pch=ifelse(dat$cid==1,1,16),
      ylim=c(0,75), cex=1.5, cex.lab = 1.5, cex.axis = 1.5)
legend("topleft",                           # Position of the legend
       legend = c("Low contact", "High contact"), # Labels
       pch = c(1, 16),                       # Symbols for each group
       col = rangi2,                         # Colors for the symbols
       title = "Contact Type")  
dev.off()

# Fit m01 ----------------------------------------------------------------------

ns <-100
P_seq <-seq(from=-1.4,to=3,length.out=ns)

lambda01 <-link(m01,data=data.frame(P=P_seq))
lmu <-apply(lambda,2,mean)
lci <-apply(lambda,2,PI)

png("05_glm2/figs/poisson_data_fitm01.png", bg = "transparent")
plot( dat$P,dat$T,xlab="log population (std)",ylab="total tools",
      col=rangi2,lwd=2, pch=1,
      ylim=c(0,75), cex=1.5, cex.lab = 1.5, cex.axis = 1.5)
lines( P_seq,lmu,lty=2,lwd=1.5, col=rangi2)
shade( lci,P_seq,xpd=TRUE)
dev.off()

# Fit m02 ----------------------------------------------------------------------
png("05_glm2/figs/poisson_fit_m02.png", bg = "transparent")
plot( dat$P,dat$T,xlab="log population (std)",ylab="total tools",
      col=rangi2,lwd=2, pch=ifelse(dat$cid==1,1,16),
      ylim=c(0,75), cex=1+normalize(k))

# predictionsforcid=1(lowcontact)
lambda <-link(m02,data=data.frame(P=P_seq,cid=1))
lmu <-apply(lambda,2,mean)
lci <-apply(lambda,2,PI)
lines( P_seq,lmu,lty=2,lwd=1.5, col=rangi2)
shade( lci,P_seq,xpd=TRUE)
# predictionsforcid=2(highcontact)
lambda <-link(m02,data=data.frame(P=P_seq,cid=2))
lmu <-apply(lambda,2,mean)
lci <-apply(lambda,2,PI)
lines( P_seq,lmu,lty=1,lwd=1.5, col=rangi2)
shade( lci,P_seq,xpd=TRUE)
legend("topleft",                           # Position of the legend
       legend = c("Low contact", "High contact"), # Labels
       pch = c(1, 16),                       # Symbols for each group
       col = rangi2,                         # Colors for the symbols
       lty = c(2, 1),                        # Line types for each group
       lwd = 1.5,                            # Line width for each group
       title = "Contact Type")               # Optional title for the legend

dev.off()



# Prediction -------------------------------------------------------------------
png("05_glm2/figs/predict_poisson.png", bg = "transparent")
plot(NULL,xlab="Population (log)",ylab="Tools",
     xlim = range(P_seq), cex.lab = 2,
     col=NA,lwd=2, pch=ifelse(dat$cid==1,1,16),
     ylim=c(0,75), cex=1+normalize(k), xaxt = "n", yaxt = "n")

# predictionsforcid=1(lowcontact)
lambda <-link(m02,data=data.frame(P=P_seq,cid=1))
lmu <-apply(lambda,2,mean)
lci <-apply(lambda,2,PI)
lines( P_seq,lmu,lty=1,lwd=2, col="black")
dev.off()


# Priors -----------------------------------------------------------------------
set.seed(10)
N <-100
a <-rnorm(N,3,0.5)
b_large <-rnorm(N,0,10)
b_small <-rnorm(N,0,0.2)

x_seq <-seq(from=log(100),to=log(200000),length.out=100)

lambda_large <- sapply(x_seq,function(x) exp(a + b_large * x))
lambda_small <- sapply(x_seq,function(x) exp(a + b_small * x))

png("05_glm2/figs/prior_poisson.png", bg = "transparent", height = 432, 
    width = 2*432)
par(mfrow = c(1, 2))
plot( NULL,xlim=c(-2,2),ylim=c(0,100),  xlab="log population (std)",
      ylab="total tools", main = "b ~ dnorm(0, 10)")
for (i in 1:N)curve(exp(a[i]+b_large[i]*x),add=TRUE,col=grau())

plot( NULL,xlim=c(-2,2),ylim=c(0,100),  xlab="log population (std)",
      ylab="total tools", main = "b ~ dnorm(0, 0.02)")
for (i in 1:N)curve(exp(a[i]+b_small[i]*x),add=TRUE,col=grau())
dev.off()

png("05_glm2/figs/prior_poisson2.png", bg = "transparent")
lambda_small <- sapply(x_seq,function(x) exp(a + b_small * x))
plot(NULL, xlim=range(x_seq), ylim=c(0,500), xlab="log population",
     ylab="total tools")
for (i in 1:N)lines(x_seq,lambda_small[i,],col=grau(),lwd=1.5)
dev.off()

# Overlay prior posterior ------------------------------------------------------
# Extract prior samples for model m01
prior_samples <- extract.prior(m01, n = 1000)

# Extract posterior samples
posterior_samples <- extract.samples(m01)

png("05_glm2/figs/prior_posterior.png", bg = "transparent")
plot(density(prior_samples$b), col = "blue", lwd = 2, main = "",
     xlab = "b", ylab = "Density", ylim = c(0, 8.5), cex.axis = 2, cex.lab = 1.5)
lines(density(posterior_samples$b), col = "red", lwd = 2)
legend("topright", legend = c("Prior", "Posterior"), col = c("blue", "red"), lwd = 2)
dev.off()
