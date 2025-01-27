library(rethinking)
library(rstan)

data(Kline)

?Kline

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

k<-PSIS(m01,pointwise=TRUE)$k
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

png("05_glm/")
par(bty = "l")
plot( dat$P,dat$T,xlab="log population (std)",ylab="total tools",
      col=rangi2,lwd=2, pch=ifelse(dat$cid==1,1,16),
      ylim=c(0,75), cex=1+normalize(k))

ns <-100
P_seq <-seq(from=-1.4,to=3,length.out=ns)


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



save(m01, m02, file = "05_glm2/poisson.RData")