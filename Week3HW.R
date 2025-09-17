#week 3 solutions 2023

#question 1
library(rethinking) 
data(foxes) 
d <- foxes 
d$W <- standardize(d$weight) 
d$A <- standardize(d$area) 
d$F <- standardize(d$avgfood) 
d$G <- standardize(d$groupsize) 
m1 <- quap( alist( F ~ dnorm( mu , sigma ), 
                   mu <- a + bA*A, a ~ dnorm(0,0.2), 
                   bA ~ dnorm(0,0.5), sigma ~ dexp(1) ), 
            data=d ) 
precis(m1)

#question 2

m2 <- quap( alist( W ~ dnorm( mu , sigma ), 
                   mu <- a + bF*F, 
                   a ~ dnorm(0,0.2), 
                   bF ~ dnorm(0,0.5), 
                   sigma ~ dexp(1) ), 
            data=d ) 
precis(m2)


m3a <- quap( alist( W ~ dnorm( mu , sigma ), 
                    mu <- a + bF*F + bG*G, 
                    a ~ dnorm(0,0.2), 
                    c(bF,bG) ~ dnorm(0,0.5), 
                    sigma ~ dexp(1) ), 
             data=d ) 
precis(m3a)


m3b <- quap( alist( G ~ dnorm( mu , sigma ), 
                    mu <- a + bF*F, 
                    a ~ dnorm(0,0.2), 
                    bF ~ dnorm(0,0.5), 
                    sigma ~ dexp(1) ), 
             data=d ) 
precis(m3b)

