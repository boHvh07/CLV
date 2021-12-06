# install.packages(c("memisc", "foreign", "data.table", "dplyr","VGAM"))
rm(list=ls())
options("scipen"=100, "digits"=3, width = 150)
library(knitr)
library(memisc)
library(foreign)
library(data.table)
library(dplyr)
library(VGAM)

#### bASIC tERMS ###
active_cust=c(12489,
              7356,
              5258,
              4309,
              3747,
              3435,
              3123,
              2948,
              2786,
              2711,
              2624)
data<-cbind(0:10,active_cust)
colnames(data)<-c("Period", "Active Customers")
data