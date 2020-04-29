###########################################################################.
##                   Macroeconomics III - Exercise 4                     ##.
###########################################################################.

# test
library(ggplot2)

# install packages
library(tidyverse)
library(gdata)

# Exercise 4 f) - h)

# f) ----------------------------------------------------------------------
# import the excel table
df1 <- read.xls("semi-endogenous.xlsx")
colnames(df1) <- c("t","s","n","alpha","phi","delta","~k","~y","~c","s~y","L","K","A","ln(y)","ln(k)","ln(c)","g")

# g) ----------------------------------------------------------------------
ggplot(df1, aes(df1$t,df1$`ln(y)`)) +
  geom_line(col="steelblue2") +
  theme_bw() +
  labs(title = "Evolution of ln(y)", y="ln(y)", x="Periods") +
  geom_vline(xintercept=10, col="black", linetype="dashed") +
  theme(aspect.ratio = 1) +
  ylim(0,8.5)

ggplot(df1, aes(df1$t,df1$`ln(c)`)) +
  geom_line(col="springgreen1") +
  theme_bw() +
  labs(title = "Evolution of ln(c)", y="ln(c)", x="Periods") +
  geom_vline(xintercept=10, col="black", linetype="dashed") +
  theme(aspect.ratio = 1) +
  ylim(0,8.5)

ggplot(df1, aes(df1$t,df1$g)) +
  geom_line(col="mediumorchid1") +
  theme_bw() +
  labs(title = "Evolution of the Growth Rate", y="Growth Rate", x="Periods") +
  geom_vline(xintercept=10, col="black", linetype="dashed") +
  theme(aspect.ratio = 1) +
  ylim(0,8.5)

# combined
ggplot(df1, aes(df1$t)) +
  geom_line(aes(y = df1$`ln(y)`, colour = "ln(y)")) +
  geom_line(aes(y = df1$`ln(c)`, colour = "ln(c)")) +
  geom_line(aes(y = df1$g, colour = "Growth Rate")) +
  geom_vline(xintercept=10, col="black", linetype="dashed") +
  scale_colour_manual("", 
                      breaks = c("ln(y)", "ln(c)", "Growth Rate"),
                      values = c("mediumorchid1", "springgreen1", "steelblue2")) +
  labs(title = "Evolution over 200 Periods", y="ln(y) / ln(c) / Growth Rate", x="Periods") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  ylim(0,8.5)

# h) ----------------------------------------------------------------------
# import the excel table
df2 <- read.xls("endogenous.xlsx")
colnames(df2) <- c("t","s","n","alpha","phi","delta","~k","~y","~c","s~y","L","K","A","ln(y)","ln(k)","ln(c)","g","k")

# plot
ggplot(df2, aes(df2$t,df2$`ln(y)`)) +
  geom_line(col="steelblue2") +
  theme_bw() +
  labs(title = "Evolution of ln(y)", y="ln(y)", x="Periods") +
  geom_vline(xintercept=10, col="black", linetype="dashed") +
  theme(aspect.ratio = 1) +
  ylim(0,45)

ggplot(df2, aes(df2$t,df2$`ln(c)`)) +
  geom_line(col="springgreen1") +
  theme_bw() +
  labs(title = "Evolution of ln(c)", y="ln(c)", x="Periods") +
  geom_vline(xintercept=10, col="black", linetype="dashed") +
  theme(aspect.ratio = 1) +
  ylim(0,45)

ggplot(df2, aes(df2$t,df2$g)) +
  geom_line(col="mediumorchid1") +
  theme_bw() +
  labs(title = "Evolution of the Growth Rate", y="Growth Rate", x="Periods") +
  geom_vline(xintercept=10, col="black", linetype="dashed") +
  theme(aspect.ratio = 1) +
  ylim(0,2)

# combined
ggplot(df2, aes(df2$t)) +
  geom_line(aes(y = df2$`ln(y)`, colour = "ln(y)")) +
  geom_line(aes(y = df2$`ln(c)`, colour = "ln(c)")) +
  geom_line(aes(y = df2$g, colour = "Growth Rate")) +
  geom_vline(xintercept=10, col="black", linetype="dashed") +
  scale_colour_manual("", 
                      breaks = c("ln(y)", "ln(c)", "Growth Rate"),
                      values = c("mediumorchid1", "springgreen1", "steelblue2")) +
  labs(title = "Evolution over 200 Periods", y="ln(y) / ln(c) / Growth Rate", x="Periods") +
  theme_bw() +
  theme(aspect.ratio = 1) +
  ylim(0,45)
