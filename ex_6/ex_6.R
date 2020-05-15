# Unemployment Rate in the US 1948-2020

library(tidyverse)

df1 <- read_csv("unempl_rate.csv")

df1 <- mutate(df1, total = (Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec)/12)
df1[73,14] <- 14.7

ggplot(df1, aes(x=Year, y=total)) +
  geom_line() +
  labs(title="Unemployment Rate in the US from 1948-2020",x="Year",y="Unemployment Rate") +
  theme_bw()

df1[is.na(df1)] <- 0
sum <- sum(df1$Jan)+sum(df1$Feb)+sum(df1$Mar)+sum(df1$Apr)+sum(df1$May)+sum(df1$Jun)+sum(df1$Jul)+sum(df1$Aug)+sum(df1$Sep)+sum(df1$Oct)+sum(df1$Nov)+sum(df1$Dec)
mean <- sum/((2019-1947)*12+4)
mean
