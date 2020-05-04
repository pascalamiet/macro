###########################################################################.
##                   Macroeconomics III - Exercise 4                     ##.
###########################################################################.

# install packages
library(tidyverse)
library(gdata)

# Ex. 5.1

# b) ----------------------------------------------------------------------

# ECONOMY 1
# set constants
alpha <- 0.33
alpha_sem <- 0.4
phi <- 0.4
lambda <- 0.8
delta <- 0.1
s <- 0.25 
s_R <- 0.04
rho <- 1
n <- 0.04
L0 <- 1
K0 <- 1
A0 <- 1

# create time
periods <- c(0:1000) 

# create data frame
x <- numeric(1001)
econ1 <- data.frame(periods,k=x,y=x,c=x,sy=x,A=x,L=x,L_A=x,g=x,lnk=x,lny=x,lnc=x,gy=x)

# insert initial values
econ1[1,2] <- K0/(L0*K0)
econ1[1,6] <- A0
econ1[1,7] <- L0

# initiate time series
# L
for (i in 2:1001) {
  econ1[i,7] <- (1+n)*econ1[i-1,7]
}

econ1 <- mutate(econ1, L_A = s_R*L) # L_A

# A
for (i in 2:1001) {
  econ1[i,6] <- alpha_sem*rho*econ1[i-1,6]^phi*econ1[i-1,8]^lambda+(1-alpha_sem)*econ1[i-1,6]*s_R^lambda+econ1[i-1,6]
}

# g
econ1 <- mutate(econ1, g = A/lag(A)-1)

# k
for (i in 2:1001) { # i here is t
  econ1[i,2] <- (1/((1+n)*(1+econ1[i,9]))) * (s*econ1[i-1,2]^alpha*(1-s_R)^(1-alpha) + (1-delta)*econ1[i-1,2])
}

econ1 <- mutate(econ1, y=k^alpha*(1-s_R)^(1-alpha)) # y
econ1 <- mutate(econ1, c=(1-s)*y) # c
econ1 <- mutate(econ1, sy=s*y) # sy

# ln(k), ln(y), ln(c)
econ1 <- mutate(econ1, lnk=log(k*A)) # ln(k)
econ1 <- mutate(econ1, lny=log(y*A)) # ln(y)
econ1 <- mutate(econ1, lnc=log(c*A)) # ln(c)

# gy
econ1 <- mutate(econ1, gy=lny-lag(lny))

# round
econ1 <- round(econ1, digits=3)


# ECONOMY 2
# set constants
alpha <- 0.33
alpha_sem <- 0.4
phi <- 0.4
delta <- 0.1
s <- 0.25 
s_R <- 0.04
rho <- 1
n <- 0.02
L0 <- 1
K0 <- 1
A0 <- 1

# create time
periods <- c(0:1000) 

# create data frame
x <- numeric(1001)
econ2 <- data.frame(periods,k=x,y=x,c=x,sy=x,A=x,L=x,L_A=x,g=x,lnk=x,lny=x,lnc=x,gy=x)

# insert initial values
econ2[1,2] <- K0/(L0*K0)
econ2[1,6] <- A0
econ2[1,7] <- L0

# initiate time series
# L
for (i in 2:1001) {
  econ2[i,7] <- (1+n)*econ2[i-1,7]
}

econ2 <- mutate(econ2, L_A = s_R*L) # L_A

# A
for (i in 2:1001) {
  econ2[i,6] <- alpha_sem*rho*econ2[i-1,6]^phi*econ2[i-1,8]^lambda+(1-alpha_sem)*econ2[i-1,6]*s_R^lambda+econ2[i-1,6]
}

# g
econ2 <- mutate(econ2, g = (A)/lag(A)-1)

# k
for (i in 2:1001) { # i here is t
  econ2[i,2] <- (1/((1+n)*(1+econ2[i,9]))) * (s*econ2[i-1,2]^alpha*(1-s_R)^(1-alpha) + (1-delta)*econ2[i-1,2])
}



econ2 <- mutate(econ2, y=k^alpha*(1-s_R)^(1-alpha)) # y
econ2 <- mutate(econ2, c=(1-s)*y) # c
econ2 <- mutate(econ2, sy=s*y) # sy

# ln(k), ln(y), ln(c)
econ2 <- mutate(econ2, lnk=log(k*A)) # ln(k)
econ2 <- mutate(econ2, lny=log(y*A)) # ln(y)
econ2 <- mutate(econ2, lnc=log(c*A)) # ln(c)

# gy
econ2 <- mutate(econ2, gy=lny-lag(lny))

# round
econ2 <- round(econ2, digits=3)


# c) ----------------------------------------------------------------------

# ln(y)
ggplot(econ1, aes(econ1$periods)) +
  geom_line(aes(y = econ1$lny, colour = "ln(y) Economy 1")) +
  geom_line(aes(y = econ2$lny, colour = "ln(y) Economy 2")) +
  scale_colour_manual("", 
                      breaks = c("ln(y) Economy 1", "ln(y) Economy 2"),
                      values = c("tomato2", "springgreen1")) +
  labs(title = "Evolution of ln(y)", y="ln(y)", x="Periods") +
  theme_bw() +
  theme(aspect.ratio = 1)

# gy
ggplot(econ1, aes(econ1$periods)) +
  geom_line(aes(y = econ1$gy, colour = "Growth Rate Economy 1")) +
  geom_line(aes(y = econ2$gy, colour = "Growth Rate Economy 2")) +
  scale_colour_manual("", 
                      breaks = c("Growth Rate Economy 1", "Growth Rate Economy 2"),
                      values = c("tomato2", "springgreen1")) +
  labs(title = "Growth Rates of per capita Output", y="Growth Rate", x="Periods") +
  theme_bw() +
  ylim(0,0.1) +
  theme(aspect.ratio = 1)

# gy & g
ggplot(econ1, aes(econ1$periods)) +
  geom_line(aes(y = econ1$g, colour = "Growth of Technology Economy 1")) +
  geom_line(aes(y = econ1$gy, colour = "Growth of per capita Output Economy 1")) +
  geom_line(aes(y = econ2$g, colour = "Growth of Technology Economy 2")) +
  geom_line(aes(y = econ2$gy, colour = "Growth of per capita Output Economy 2")) +
  scale_colour_manual("", 
                      breaks = c("Growth of Technology Economy 1", "Growth of per capita Output Economy 1", "Growth of Technology Economy 2", "Growth of per capita Output Economy 2"),
                      values = c("springgreen1", "skyblue", "tomato2", "mediumorchid2")) +
  labs(title = "Growth Rates of Technology and per capita Output", y="Growth Rates", x="Periods") +
  theme_bw() +
  ylim(0,0.1) +
  theme(aspect.ratio = 1)
