p <- list()

p$A <- c(0, 0, 10, 0, 0)
p$B <- c(0, 1, 8, 1, 0)
p$C <- c(0, 2, 6, 2, 0)
p$D <- c(1, 2, 4, 2, 1)
p$E <- c(2, 2, 2, 2, 2)

norm <- function(q) q/sum(q)
entropy <- function(q){
  -sum(ifelse(q == 0, 0, q * log(q))) 
}

p_norm <- lapply(p, norm)


( H <- sapply(p_norm, entropy) )

ways <- c(1, 90, 1260, 37800, 113400)
logwayspp <- log(ways) / 10


