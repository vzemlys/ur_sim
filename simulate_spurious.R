
# Initialize --------------------------------------------------------------


library(dplyr)


# Simulation functions ----------------------------------------------------



f <- function(n) {
    u <- rnorm(n)
    v <- rnorm(n)
    
    x <- cumsum(u)
    y <- cumsum(v)
    coef(lsfit(x,y))
}

f1 <- function(n) {
    u <- rnorm(n)
    v <- rnorm(n)
    
    coef(lsfit(u,v))
}


# Simulation example for white case --------------------------------------

res1_100 <- rep(100,1000) %>% sapply(f1)
res1_500 <- rep(500,1000) %>% sapply(f1)
res1_1000 <- rep(1000,1000) %>% sapply(f1)
res1_5000 <- rep(5000,1000) %>% sapply(f1)


mapply(function(r,n)
    hist(r[2,],
        main = paste("Sample size:",n, "\nQuantiles: ",paste(round(quantile(sqrt(n)*r[2,],prob=c(0.025,0.975)),3), collapse = ", ")), 
        xlab = "Slope of the regression"), 
    list(res1_100,res1_500,res1_1000,res1_5000),
    c(100,500,1000,5000),
    SIMPLIFY = FALSE)