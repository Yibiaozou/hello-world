# clean work space & load required packages
rm(list=ls())
library(tidyverse)
library(cowplot)
library(ggpmisc)
library(ggpubr)
require(data.table)
library("readxl")
library(GGally)
library(openxlsx)
library(smatr)
HOME <- "D:/Zeus/ETH_zurich_MSc/ETHz_S1/ETH_Eco&Evo/Ecology_and_Evolution_Term_Paper/Code"
setwd(HOME)

save.image(file='Data/workspace_2021_1_31.RData')
load('Data/workspace_2021_1_31.RData')

# Pt <- function(t, a, b){
#   return(a*(1-t/b))
# }
# 
# Mt <- function(t, m, b){
#   return(m*(1-t/b))
# }

# calculate lifetime net carbon gain rate from purely single leaf perspective
lifetime_carbon_gain_rate_1 <- function(t, a, m, b, C, f){
  # t: realized leaf lifespan
  # a: maximum photosynthetic rate
  # m: maximum respiration rate
  # b: potential leaf lifespan
  # C: leaf construction cost
  # f: favorable season length
  
  n <- floor(t) # (n+1) is the number of growing seasons
  d <- t - n
  
  # define two functions
  Pt <- function(t1){
    return(a*(1-t1/b))
  }
  
  Mt <- function(t1){
    return(m*(1-t1/b))
  }
  
  # calculate accumulated respiration
  REP <- integrate(Mt, lower=0, upper=t)$value
  
  # calculate accumulated gross primary production
  GPP <- 0
  if((d >= f) & (n > 0)){
    for(i in 0:n){
      GPP <- GPP + integrate(Pt, lower=i, upper=(i+f))$value
    }
  }else if((d < f) & (n > 0)){
    for(i in 0:(n-1)){
      GPP <- GPP + integrate(Pt, lower=i, upper=(i+f))$value
    }
    GPP <- GPP + integrate(Pt, lower=n, upper=t)$value
  }else if((d >= f) & (n == 0)){
    GPP <- GPP + integrate(Pt, lower=0, upper=f)$value
  }else if((d < f) & (n == 0)){
    GPP <- GPP + integrate(Pt, lower=0, upper=t)$value
  }
  
  # calculate life time net carbon gain rate
  g <- (GPP - REP - C)/t
  
  return(g)
}

# calculate lifetime net carbon gain rate from the whole-plant perspective
lifetime_carbon_gain_rate_2 <- function(t, a, m, b, C, f, e, Nu){
  # t: realized leaf lifespan
  # a: maximum photosynthetic rate
  # m: maximum respiration rate
  # b: potential leaf lifespan
  # C: leaf construction cost
  # f: favorable season length
  # e:  marginal transpiration cost * maximum transpiration rate
  # Nu: nutrient availability
  
  uf <- 0.1 # productivity/transpiration ratio during unfavorable seasons 
  n <- floor(t) # (n+1) is the number of growing seasons
  d <- t - n
  
  # Photosynthesis
  Pt <- function(t1){
    return(a*(1-t1/b))
  }
  
  # Respiration
  Mt <- function(t1){
    return(m*(1-t1/b))
  }
  
  # Transpiration
  Et <- function(t1){
    return(e*(1-t1/b)/Nu)
  }
  
  # calculate accumulated respiration
  # REP <- integrate(Mt, lower=0, upper=t)$value
  REP <- 0
  
  # calculate accumulated gross primary production & root cost
  GPP <- 0
  TRP <- 0
  if((d >= f) & (n > 0)){
    for(i in 0:(n-1)){
      GPP <- GPP + integrate(Pt, lower=i, upper=(i+f))$value + (integrate(Pt, lower=(i+f), upper=(i+1))$value)*uf
      TRP <- TRP + integrate(Et, lower=i, upper=(i+f))$value + (integrate(Et, lower=(i+f), upper=(i+1))$value)*uf
      REP <- REP + integrate(Mt, lower=i, upper=(i+f))$value + (integrate(Mt, lower=(i+f), upper=(i+1))$value)*uf
    }
    GPP <- GPP + integrate(Pt, lower=n, upper=(n+f))$value + (integrate(Pt, lower=(n+f), upper=t)$value)*uf
    TRP <- TRP + integrate(Et, lower=n, upper=(n+f))$value + (integrate(Et, lower=(n+f), upper=t)$value)*uf
    REP <- REP + integrate(Mt, lower=n, upper=(n+f))$value + (integrate(Mt, lower=(n+f), upper=t)$value)*uf
  }else if((d < f) & (n > 0)){
    for(i in 0:(n-1)){
      GPP <- GPP + integrate(Pt, lower=i, upper=(i+f))$value + (integrate(Pt, lower=(i+f), upper=(i+1))$value)*uf
      TRP <- TRP + integrate(Et, lower=i, upper=(i+f))$value + (integrate(Et, lower=(i+f), upper=(i+1))$value)*uf
      REP <- REP + integrate(Mt, lower=i, upper=(i+f))$value + (integrate(Mt, lower=(i+f), upper=(i+1))$value)*uf
    }
    GPP <- GPP + integrate(Pt, lower=n, upper=t)$value
    TRP <- TRP + integrate(Et, lower=n, upper=t)$value
    REP <- REP + integrate(Mt, lower=n, upper=t)$value
  }else if((d >= f) & (n == 0)){
    GPP <- GPP + integrate(Pt, lower=0, upper=f)$value + (integrate(Pt, lower=f, upper=t)$value)*uf
    TRP <- TRP + integrate(Et, lower=0, upper=f)$value + (integrate(Et, lower=f, upper=t)$value)*uf
    REP <- REP + integrate(Mt, lower=0, upper=f)$value + (integrate(Mt, lower=f, upper=t)$value)*uf
  }else if((d < f) & (n == 0)){
    GPP <- GPP + integrate(Pt, lower=0, upper=t)$value
    TRP <- TRP + integrate(Et, lower=0, upper=t)$value
    REP <- REP + integrate(Mt, lower=0, upper=t)$value
  }
  
  # calculate life time net carbon gain rate
  g <- (GPP - REP - TRP - C)/t
  
  return(g)
}

# parameter set for test
a = 20
f = 0.4
m = 11
b = 2
C = 5
# r = 3
e = 10
Nu = 2

lifetime_carbon_gain_rate_1(t=0.4, a, m, b, C, f)
lifetime_carbon_gain_rate_2(t=2, a, m, b, C, f, e, Nu=1)

Optimal_leave_lifespan_1 <- function(a, m, b, C, f){
  
  
  longevity <- seq(0.1, b, by = 0.1)
  g_series <- sapply(longevity, function(t) lifetime_carbon_gain_rate_1(t, a, m, b, C, f))
  index <- which(g_series==max(g_series))[1]
  
  return(longevity[index])
}

Optimal_leave_lifespan_2 <- function(a, m, b, C, f, e, Nu){
  
  longevity <- seq(0.1, b, by = 0.1)
  g_series <- sapply(longevity, function(t) lifetime_carbon_gain_rate_2(t, a, m, b, C, f, e, Nu))
  index <- which(g_series==max(g_series))[1]
  
  return(longevity[index])
}


Optimal_leave_lifespan(a, m, b, C, f)

a_vector <- seq(20, 100, by=20)
C_vector <- seq(3, 18, by=3)
b_vector <- seq(3, 12, by=3)
m_vector <- seq(3, 18, by=3)
f_vector <- seq(0.1, 1, by=0.1)
Nu_vector <- seq(1, 19, by = 2)
e_vector <- seq(6, 16, by=2)
# r_vector

Biogeography_leave_lifespan_1 <- function(a_vector, C_vector, b_vector, m_vector, f_vector){
  # a_vector: vector for maximum photosynthetic rate
  # m_vector: vector for maximum respiration rate
  # b_vector: vector for potential leaf lifespan
  # C_vector: vector for leaf construction cost
  # f_vector: vector for favorable season length
  
  N <- length(a_vector)*length(C_vector)*length(b_vector)*length(m_vector)
  LLS_Mx <- matrix(NA, nrow=length(f_vector), ncol=N)
  for(i in 1:10){
    LLS_vec <- NULL
    for(a in a_vector){
      for(C in C_vector){
        for(b in b_vector){
          for(m in m_vector){
            LLS_vec <- c(LLS_vec, Optimal_leave_lifespan_1(a, m, b, C, f_vector[i]))
          }
        }
      }
    }
    LLS_Mx[i,] <- LLS_vec
  }
  LLS_Mx <- (LLS_Mx >= 1)*1
  EvDeProb <- rowSums(LLS_Mx)/N
  
  return(EvDeProb)
}

Biogeography_leave_lifespan_2 <- function(a_vector, C_vector, b_vector, m_vector, Nu_vector, f, e_vector){
  # a_vector: vector for maximum photosynthetic rate
  # m_vector: vector for maximum respiration rate
  # b_vector: vector for potential leaf lifespan
  # C_vector: vector for leaf construction cost
  # Nu_vector: vector for nutrient availability
  # f: favorable season length
  # e:  marginal transpiration cost * maximum transpiration rate
  
  N <- length(a_vector)*length(C_vector)*length(b_vector)*length(m_vector)*length(e_vector)
  LLS_Mx <- matrix(NA, nrow=length(Nu_vector), ncol=N)
  for(i in 1:10){
    LLS_vec <- NULL
    for(a in a_vector){
      for(C in C_vector){
        for(b in b_vector){
          for(m in m_vector){
            for(e in e_vector){
              LLS_vec <- c(LLS_vec, Optimal_leave_lifespan_2(a, m, b, C, f, e, Nu_vector[i]))
            }
          }
        }
      }
    }
    LLS_Mx[i,] <- LLS_vec
  }
  LLS_Mx <- (LLS_Mx >= 1)*1
  EvDeProb <- rowSums(LLS_Mx)/N
  
  return(EvDeProb)
}

# Kikuzawa 1991 simulation reproduction
start.time <- Sys.time()
EvDeProb <- Biogeography_leave_lifespan_1(a_vector, C_vector, b_vector, m_vector, f_vector)
plot(f_vector, EvDeProb)
end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)

EvDeDf <- data.frame(EvDeProb=EvDeProb, f=f_vector)

ggplot(EvDeDf, aes(x=f, y=EvDeProb)) +
  geom_bar(stat="identity")+
  xlab("Favorable season length")+
  ylab("Evergreen proportion")+
  scale_x_continuous(breaks=f_vector)+
  geom_hline(yintercept=1, colour="red", lty=2, size=1.5)+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

# new model simulations
EvDeProb_2 <- Biogeography_leave_lifespan_2(a_vector, C_vector, b_vector, m_vector, Nu_vector, f, e)
plot(Nu_vector, EvDeProb_2)

start.time <- Sys.time()
# EvDeProb_Lst <- list()
EvDeProb_Lst <- lapply(f_vector, function(f) Biogeography_leave_lifespan_2(a_vector, C_vector, b_vector, m_vector, Nu_vector, f, e_vector))

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)

plot(Nu_vector, EvDeProb_Lst[[7]])

EvDeProb_Nu_Df <- data.frame(f=rep(f_vector, each=10), Nu=rep(Nu_vector, 10), EvDeProb=rep(NA, 100))
for(i in 1:10){EvDeProb_Nu_Df$EvDeProb[((i-1)*10+1):(i*10)]=EvDeProb_Lst[[i]]}

gpED <- list()
for(i in 1:10){
  gpED[[i]] <- ggplot(EvDeProb_Nu_Df[EvDeProb_Nu_Df$f==f_vector[i],], aes(x=Nu, y=EvDeProb))+
    geom_bar(stat="identity")+
    geom_smooth(aes(x=Nu, y=EvDeProb),
                method = "lm", se= FALSE, color = "blue", size = 1, lty=2) +
    xlab("Nutrient availability")+
    ylab("Evergreen proportion")+
    scale_x_continuous(breaks=Nu_vector)+
    geom_hline(yintercept=1, colour="red", lty=2, size=1.5)+
    ggtitle(paste("f = ", f_vector[i]))+
    theme_classic()+
    theme(axis.text.x=element_text(size=8, face="bold"),
          axis.text.y=element_text(size=8, face="bold"),
          axis.title.x=element_text(size=15,face="bold"),
          axis.title.y=element_text(size=15,face="bold"),
          legend.text = element_text(colour = "black", size = 10, face = "bold"),
          legend.title = element_text(colour = "black", size = 12, face = "bold"))
}

plot_grid(gpED[[1]], gpED[[6]], gpED[[2]], gpED[[7]], gpED[[3]], gpED[[8]], gpED[[4]], gpED[[9]], gpED[[5]], gpED[[10]], ncol=2)

ggplot(EvDeProb_Nu_Df, aes(x=Nu, y=EvDeProb, color=as.factor(f), group=as.factor(f)))+
  geom_point(alpha=0.7, size=3)+
  geom_line(alpha=0.7, lty=3, size=1.5)+
  xlab("Nutrient availability")+
  ylab("Evergreen proportion")+
  scale_x_continuous(breaks=Nu_vector)+
  geom_hline(yintercept=1, colour="black", lty=2, size=1.5)+
  scale_color_manual(name="f", values=rainbow(10))+
  # ggtitle(paste("f = ", f_vector[i]))+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

HOME <- "D:/Zeus/ETH_zurich_MSc/ETHz_S1/ETH_Eco&Evo/Ecology_and_Evolution_Term_Paper/Code"
setwd(HOME)
save.image(file='workspace_2021_1_8.RData')
load('workspace_2021_1_8.RData')

##################################################################
#############--------------CBLP_YZ--------------------############

## define functions that will be used
Sample_parameter <- function(vector, n){
  mu <- mean(vector, na.rm=T)
  sigma <- sd(vector, na.rm=T)
  sampleVector <- 10^(rnorm(n, mean=mu, sd=sigma))
  return(sampleVector)
}

# calculate lifetime net carbon gain rate from the whole-plant perspective
lifetime_carbon_gain_rate_3 <- function(t, a, m, b, C, k, f, e, Nu){
  # t: realized leaf lifespan
  # a: maximum photosynthetic rate
  # m: maximum respiration rate
  # b: potential leaf lifespan
  # C: leaf construction cost
  # k: the ratio of construction/respiration cost at the whole plant scale
  # f: favorable season length
  # e:  marginal transpiration cost * maximum transpiration rate
  # Nu: nutrient availability
  
  uf <- 0.1 # productivity/transpiration ratio during unfavorable seasons 
  n <- floor(t) # (n+1) is the number of growing seasons
  d <- t - n
  
  # Photosynthesis
  Pt <- function(t1){
    return(a*(1-t1/b))
  }
  
  # Respiration
  Mt <- function(t1){
    return(m*(1-t1/b))
  }
  
  # Transpiration
  Et <- function(t1){
    return(e*(1-t1/b)/Nu)
  }
  
  # calculate accumulated respiration
  # REP <- integrate(Mt, lower=0, upper=t)$value
  REP <- 0
  
  # calculate accumulated gross primary production & root cost
  GPP <- 0
  TRP <- 0
  if((d >= f) & (n > 0)){
    for(i in 0:(n-1)){
      GPP <- GPP + integrate(Pt, lower=i, upper=(i+f))$value + (integrate(Pt, lower=(i+f), upper=(i+1))$value)*uf
      TRP <- TRP + integrate(Et, lower=i, upper=(i+f))$value + (integrate(Et, lower=(i+f), upper=(i+1))$value)*uf
      REP <- REP + integrate(Mt, lower=i, upper=(i+f))$value + (integrate(Mt, lower=(i+f), upper=(i+1))$value)*uf
    }
    GPP <- GPP + integrate(Pt, lower=n, upper=(n+f))$value + (integrate(Pt, lower=(n+f), upper=t)$value)*uf
    TRP <- TRP + integrate(Et, lower=n, upper=(n+f))$value + (integrate(Et, lower=(n+f), upper=t)$value)*uf
    REP <- REP + integrate(Mt, lower=n, upper=(n+f))$value + (integrate(Mt, lower=(n+f), upper=t)$value)*uf
  }else if((d < f) & (n > 0)){
    for(i in 0:(n-1)){
      GPP <- GPP + integrate(Pt, lower=i, upper=(i+f))$value + (integrate(Pt, lower=(i+f), upper=(i+1))$value)*uf
      TRP <- TRP + integrate(Et, lower=i, upper=(i+f))$value + (integrate(Et, lower=(i+f), upper=(i+1))$value)*uf
      REP <- REP + integrate(Mt, lower=i, upper=(i+f))$value + (integrate(Mt, lower=(i+f), upper=(i+1))$value)*uf
    }
    GPP <- GPP + integrate(Pt, lower=n, upper=t)$value
    TRP <- TRP + integrate(Et, lower=n, upper=t)$value
    REP <- REP + integrate(Mt, lower=n, upper=t)$value
  }else if((d >= f) & (n == 0)){
    GPP <- GPP + integrate(Pt, lower=0, upper=f)$value + (integrate(Pt, lower=f, upper=t)$value)*uf
    TRP <- TRP + integrate(Et, lower=0, upper=f)$value + (integrate(Et, lower=f, upper=t)$value)*uf
    REP <- REP + integrate(Mt, lower=0, upper=f)$value + (integrate(Mt, lower=f, upper=t)$value)*uf
  }else if((d < f) & (n == 0)){
    GPP <- GPP + integrate(Pt, lower=0, upper=t)$value
    TRP <- TRP + integrate(Et, lower=0, upper=t)$value
    REP <- REP + integrate(Mt, lower=0, upper=t)$value
  }
  
  # calculate life time net carbon gain rate
  g <- (GPP - REP - TRP - C/365)/t
  
  return(g)
}

Optimal_leave_lifespan_3 <- function(a, m, b, C, k, f, e, Nu){
  if((b >= 0.1)&&(b<=1)){
    longevity <- seq(0.1, b, by = 0.1)
  }else if(b < 0.1){
    longevity <- b
  }else if(b>1){
    longevity <- c(seq(0.1, 0.9, by = 0.1),seq(1, b, by=((b-1)/30)))
    }

  g_series <- sapply(longevity, function(t) lifetime_carbon_gain_rate_3(t, a, m, b, C, k, f, e, Nu))
  index <- which(g_series==max(g_series))[1]
  
  return(list(LL=longevity[index], g=g_series[index]))
}

Biogeography_leave_lifespan_3 <- function(a_vector, LMA_vector, b_vector, m_vector, Nu_vector, k, f_vector, e_vector, N){
  # a_vector: vector for maximum photosynthetic rate
  # m_vector: vector for maximum respiration rate
  # b_vector: vector for potential leaf lifespan
  # LMA_vector: vector for leaf mass per area
  # k: the ratio of construction/respiration cost at the whole plant scale
  # Nu_vector: vector for nutrient availability
  # f_vector: vector for favorable season length
  # e_vector: vector for marginal transpiration cost * maximum transpiration rate

  
  # construction cost per unit leaf C mass
  c <- 1.2
  
  # N <- length(a_vector)*length(C_vector)*length(b_vector)*length(m_vector)*length(e_vector)
  LLS_Df <- NULL
  for(f in f_vector){
    for(Nu in Nu_vector){
     for(i in 1:N){
       C <- LMA_vector[i] * c * k
       OLL <- Optimal_leave_lifespan_3(a=a_vector[i], m=m_vector[i], b=b_vector[i], C=C, k=k, f=f, e=e_vector[i], Nu=Nu)
       LL <- OLL$LL
       ED <- (LL >= 1)*1
       g <- OLL$g
       LLS_Df <- rbind(LLS_Df, data.frame(f=f, Nu=Nu, LL=LL, g=g, ED=ED, a=a_vector[i], LMA=LMA_vector[i], b=b_vector[i], m=m_vector[i], e=e_vector[i]))
       #LLS_Df
     } 
    }
  }
  
  return(LLS_Df)
}

# GLOPNET for basic parameters (b, m, a, LMA)
GLOPNET <- read_excel("Data/GLOPNET.xls", skip=10)
mean(GLOPNET$`log LMA`, na.rm=T)
sd(GLOPNET$`log LMA`, na.rm=T)

GLOPNET_SUP <- read.csv("Data/Glopnet_10117.csv")

## add MAT, MAP, ppt_pet to GLOPNET
# CodeList <- GLOPNET$Code
CodeList <- unique(GLOPNET_SUP$ObservationID)
GLOPNET$MAT <- sapply(CodeList, function(i) as.numeric(GLOPNET_SUP$OrigValueStr[(GLOPNET_SUP$ObservationID==i)&(GLOPNET_SUP$OriglName=="MAT")]))
GLOPNET$MAP <- sapply(CodeList, function(i) as.numeric(GLOPNET_SUP$OrigValueStr[(GLOPNET_SUP$ObservationID==i)&(GLOPNET_SUP$OriglName=="ann_rain")]))
GLOPNET$PPT_PET <- sapply(CodeList, function(i) as.numeric(GLOPNET_SUP$OrigValueStr[(GLOPNET_SUP$ObservationID==i)&(GLOPNET_SUP$OriglName=="ppt_pet")]))
GLOPNET$`log LMA` <- as.numeric(GLOPNET$`log LMA`)
GLOPNET$`log LL` <- as.numeric(GLOPNET$`log LL`)

write.xlsx(GLOPNET, file="Data/GLOPNET_1.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
save(GLOPNET, file="Data/GLOPNET_1.RData")

# extract b from Kikuzawa 2006
Potential_leaf_longevity <- read_excel("Potential_leaf_longevity.xlsx")$`b (days)`/365

# TRYdata for transpiration related parameter
TRY_13240 <- fread("TRY_13240_09012021130536/13240.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
TRY_13281 <- fread("TRY_13281_13012021181733/13281.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
TRY_13282 <- fread("TRY_13282_13012021194734/13282.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)
TRY_13283 <- fread("TRY_13283_13012021204352/13283.txt", header = T, sep = "\t", dec = ".", quote = "", data.table = T)

write.xlsx(TRY_13240, file="Data/TRY_13240.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(TRY_13281, file="Data/TRY_13281.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(TRY_13282, file="Data/TRY_13282.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(TRY_13283, file="Data/TRY_13283.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)

leaf_transpiration_rate <- as.numeric(TRY_13240$OrigValueStr[TRY_13240$OriglName=="E"])
water_use_efficiency <- as.numeric(TRY_13240$OrigValueStr[TRY_13240$OriglName=="WUE"])[1:length(leaf_transpiration_rate)]



hist(log10(leaf_transpiration_rate), breaks = 10)
hist(leaf_transpiration_rate, breaks = 10)
hist(log10(water_use_efficiency), breaks = 20)
hist(water_use_efficiency, breaks=20)
hist(log10(Potential_leaf_longevity), breaks=7)
hist(Potential_leaf_longevity, breaks=7)

# sample basic parameters
N <- 4000
Aarea <- Sample_parameter(GLOPNET$`log Aarea`, N) # umol*g-1*s-1
Rarea <- Sample_parameter(GLOPNET$`log Rdarea`, N) # umol*g-1*s-1
LMA_vector <- Sample_parameter(GLOPNET$`log LMA`, N) # g*m-2
b_vector <- Sample_parameter(log10(Potential_leaf_longevity), N) # year
Earea <- Sample_parameter(log10(leaf_transpiration_rate*water_use_efficiency), N)
# E <- Sample_parameter(log10(leaf_transpiration_rate), 1000) # mmol*m-2*s-1
# WUE <- Sample_parameter(log10(water_use_efficiency), 1000) # umol/mmol*m-2*s-1

# mean labour time
tl <- 5.5
k <- 3

# calculate parameter vector for simulations
a_vector <- Aarea*12*3600*tl*10^(-6)
m_vector <- Rarea*12*3600*12*k*10^(-6)
e_vector <- Earea*12*3600*tl*10^(-6)
Nu_vector <- c(0.25, 0.5, 1, 2, 4, 8, 16, 32, 64)
f_vector <- seq(1/12, 1, by=1/12)

start.time <- Sys.time()

EvDeDf_2 <- Biogeography_leave_lifespan_3(a_vector, LMA_vector, b_vector, m_vector, Nu_vector, k, f_vector, e_vector, N)
EvDeDf_2 <- EvDeDf_2%>%
  mutate(SppID = rep(1:N, (length(f_vector)*length(Nu_vector))), PFT=rep(NA, nrow(EvDeDf_2)))
save(EvDeDf_2, file="Data/CBLP_YZ_Df_2_4000N.RData")
load("Data/CBLP_YZ_Df_2_4000N.RData")
# remove(EvDeDf_2)

end.time <- Sys.time()
time.taken <- round(end.time - start.time,2)
save.image(file='Data/workspace_2021_1_14.RData')
load("Data/workspace_2021_1_14.RData")


# create new data.frame to only contain positive carbon budget species 
EvDeDf_3 <- EvDeDf_2[EvDeDf_2$g>0,]

## visualize CBLP_YB output along f gradient and group by nutrient availability
gP_f_1_lst <- list()
gP_f_2_lst <- list()
gP_f_3_lst <- list()
gP_f_4_lst <- list()

ED_vector <- c("D", "E")
for(i in 1:length(Nu_vector)){
  nf <- length(f_vector)
  # LL_Df <- data.frame(f=rep(f_vector,2), LL_m=rep(NA, nf*2), LL_sd=rep(NA, nf*2), ED=rep(NA, nf*2))
  LL_Df_1 <- NULL
  LL_Df_2 <- NULL
  LL_Df_5 <- NULL
  # LL_Df_5 <- NULL
  for(f in f_vector){
    for(ED in 0:1){
      DFt <- EvDeDf_3[(EvDeDf_3$f==f)&(EvDeDf_3$ED==ED)&(EvDeDf_3$Nu==Nu_vector[i]),]
      LL_Df_1 <- rbind(LL_Df_1, data.frame(f=f, LL_m=mean(DFt$LL), LL_sd=sd(log10(DFt$LL)), ED=ED_vector[(ED+1)]))
    }
  }
  LL_Df_5 <- EvDeDf_2[(EvDeDf_2$Nu==Nu_vector[i]),]
  SppVector <- unique(LL_Df_5[LL_Df_5$g>0,]$SppID)
  LL_Df_5$PFT[LL_Df_5$g<=0] <- "NC"
  ns <- length(SppVector)
  for(s in SppVector){
    EDs <- LL_Df_5$ED[LL_Df_5$SppID==s]
    ne <- length(EDs)
    if(sum((EDs==0)*1)==0){
      LL_Df_5$PFT[LL_Df_5$SppID==s] <- "E" # strict evergreen
    }else if(sum((EDs==1)*1)==0){
      LL_Df_5$PFT[LL_Df_5$SppID==s] <- "D" # strict deciduous
    }else if(EDs[1]==1&EDs[ne]==0){
      LL_Df_5$PFT[LL_Df_5$SppID==s] <- "D->E" # strict deciduous
    }else if(EDs[1]==0&EDs[ne]==1){
      LL_Df_5$PFT[LL_Df_5$SppID==s] <- "E->D" # strict deciduous
    }else if(EDs[1]==1&EDs[ne]==1&(sum((EDs[2:(ne-1)]==0)*1)>0)){
      LL_Df_5$PFT[LL_Df_5$SppID==s] <- "E->D->E" # strict deciduous
    }
  }
 
  # create data frame for scatter plot
  LL_Df_6 <- LL_Df_5[1:N,]%>%
    select(a=a, b=b, LMA=LMA, e=e, m=m, PFT=PFT, SppID=SppID)%>%
    mutate(C=LMA*3.6)
  LL_Df_5 <- LL_Df_5[LL_Df_5$g>0,]
  LL_Df_5$LL <- jitter(LL_Df_5$LL, factor=4)
  
  
  LL_Df_6 <- LL_Df_6%>%
    mutate(PFTN=1*(PFT=="D")+2*(PFT=="D->E")+3*(PFT=="E")+4*(PFT=="E->D")+5*(PFT=="E->D->E")+6*(PFT=="NC"))
  my_cols <- c("blue","purple","red","darkcyan","gold","grey")
  # my_shape <- seq(1,6,by=1)
  gP_f_4_lst[[i]] <- pairs(log10(LL_Df_6[1:5]), pch = LL_Df_6$PFTN,  cex = 1.5,
                           col = my_cols[LL_Df_6$PFTN],
                           lower.panel=NULL)
    # ggpairs(LL_Df_6, columns = 1:5, mapping=ggplot2::aes(colour=PFT),
    #                          lower = list(continuous = wrap(alpha = 0.3, size=0.1)))+
    # theme(legend.position = "bottom") 
  
  gP_f_3_lst[[i]] <- ggplot(LL_Df_5, aes(x=f, y=LL, group=PFT, color=PFT))+
    geom_point(size=2, shape=1)+
    geom_line(size=1.5, alpha=0.3, aes(x=f, y=LL, group=SppID))+
    scale_y_log10()+
    xlab("Favourable season length (yr yr-1)")+
    ylab("Optimal leaf longevity (log10(year))")+
    scale_color_manual(name="EDT", values=c("blue","purple","red","darkcyan","gold"))+
    scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
    ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
    theme_classic()+
    theme(axis.text.x=element_text(size=8, face="bold"),
          axis.text.y=element_text(size=8, face="bold"),
          axis.title.x=element_text(size=15,face="bold"),
          axis.title.y=element_text(size=15,face="bold"),
          legend.text = element_text(colour = "black", size = 10, face = "bold"),
          legend.title = element_text(colour = "black", size = 12, face = "bold"))
  
  
  LL_Df_2 <- EvDeDf_3[EvDeDf_3$Nu==Nu_vector[i],]%>%
    select(f, LL, LMA)%>%
    mutate(fg=f)
  LL_Df_2$fg[(LL_Df_2$f)%in%f_vector[1:3]] <- "f=(1~3)/12"
  LL_Df_2$fg[(LL_Df_2$f)%in%f_vector[4:6]] <- "f=(4~6)/12"
  LL_Df_2$fg[LL_Df_2$f%in%f_vector[7:9]] <- "f=(7~9)/12"
  LL_Df_2$fg[LL_Df_2$f%in%f_vector[10:12]] <- "f=(10~12)/12"
  LL_Df_2$fg <- factor(LL_Df_2$fg, levels=c("f=(1~3)/12", "f=(4~6)/12", "f=(7~9)/12", "f=(10~12)/12"))
  
  gP_f_1_lst[[i]] <- ggplot(LL_Df_1[1:24,], aes(x=f, y=LL_m, group=ED, color=ED, shape=ED))+
    geom_point(size=5, alpha=0.7)+
    geom_smooth(aes(x=f, y=LL_m),
                method = "lm", se= FALSE, size = 1, lty=2) +
    # geom_bar(stat="identity", color="black", 
    #          position=position_dodge()) +
    # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
    #               position=position_dodge(.9)) +
    geom_pointrange(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd))+
    xlab("Favourable season length (yr yr-1)")+
    ylab("Optimal leaf longevity (log10(year))")+
    scale_color_manual(name="ED", values=c("blue", "red"))+
    scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
    ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
    scale_y_log10()+
    theme_classic()+
    theme(axis.text.x=element_text(size=8, face="bold"),
          axis.text.y=element_text(size=8, face="bold"),
          axis.title.x=element_text(size=15,face="bold"),
          axis.title.y=element_text(size=15,face="bold"),
          legend.text = element_text(colour = "black", size = 10, face = "bold"),
          legend.title = element_text(colour = "black", size = 12, face = "bold"))
  
  gP_f_2_lst[[i]] <- ggplot(LL_Df_2, aes(x=LMA, y=LL, group=fg, color=fg, shape=fg))+
    geom_point(size=2, alpha=0.5)+
    geom_smooth(aes(x=LMA, y=LL),
                method = "lm", se= FALSE, size = 1, lty=2) +
    xlab("LMA (g m-2)")+
    ylab("Optimal leaf longevity (year)")+
    scale_color_manual(name="fg", values=c("purple", "blue", "gold", "red"))+
    ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
    scale_y_log10()+
    scale_x_log10()+
    theme_classic()+
    theme(axis.text.x=element_text(size=8, face="bold"),
          axis.text.y=element_text(size=8, face="bold"),
          axis.title.x=element_text(size=15,face="bold"),
          axis.title.y=element_text(size=15,face="bold"),
          legend.text = element_text(colour = "black", size = 10, face = "bold"),
          legend.title = element_text(colour = "black", size = 12, face = "bold"))
}

plot_grid(gP_f_1_lst[[1]], gP_f_1_lst[[2]], gP_f_1_lst[[3]], gP_f_1_lst[[4]], gP_f_1_lst[[5]], 
          gP_f_1_lst[[6]], gP_f_1_lst[[7]], gP_f_1_lst[[8]], gP_f_1_lst[[9]], ncol=3)

plot_grid(gP_f_2_lst[[1]], gP_f_2_lst[[2]], gP_f_2_lst[[3]], gP_f_2_lst[[4]], gP_f_2_lst[[5]], 
          gP_f_2_lst[[6]], gP_f_2_lst[[7]], gP_f_2_lst[[8]], gP_f_2_lst[[9]], ncol=3)

## visualize LL-f relationship for different PFTs, and demonstrate scatter plots for all species traits along Nu gradient
plot_grid(gP_f_3_lst[[1]], gP_f_3_lst[[2]], gP_f_3_lst[[3]], gP_f_3_lst[[4]], gP_f_3_lst[[5]], 
          gP_f_3_lst[[6]], gP_f_3_lst[[7]], gP_f_3_lst[[8]], gP_f_3_lst[[9]], ncol=3)

## visualize CBLP_YB output along nutrient gradient and group by f
gP_Nu_1_lst <- list()
gP_Nu_2_lst <- list()
gP_Nu_3_lst <- list()

for(i in 1:length(f_vector)){
  nf <- length(f_vector)
  # LL_Df <- data.frame(f=rep(f_vector,2), LL_m=rep(NA, nf*2), LL_sd=rep(NA, nf*2), ED=rep(NA, nf*2))
  LL_Df_3 <- NULL
  LL_Df_4 <- NULL
  LL_Df_7 <- NULL
  for(Nu in Nu_vector[3:9]){
    for(ED in 0:1){
      DFt <- EvDeDf_3[(EvDeDf_3$f==f_vector[i])&(EvDeDf_3$ED==ED)&(EvDeDf_3$Nu==Nu),]
      LL_Df_3 <- rbind(LL_Df_3, data.frame(Nu=Nu, LL_m=mean(DFt$LL), LL_sd=sd(log10(DFt$LL)), ED=ED_vector[(ED+1)]))
    }
  }
  for(Nu in Nu_vector){
    DFt2 <- EvDeDf_3[(EvDeDf_3$f==f_vector[i])&(EvDeDf_3$Nu==Nu),]
    LL_Df_7 <- rbind(LL_Df_7, data.frame(f=f, Nu=Nu, EDP=(sum(DFt2$ED)/nrow(DFt2))))
  }
  
  LL_Df_4 <- EvDeDf_3[EvDeDf_3$f==f_vector[i],]%>%
    select(Nu, LL, LMA)%>%
    mutate(Nug=Nu)
  LL_Df_4$Nug[(LL_Df_4$Nu)%in%Nu_vector[1:3]] <- "Nu=0.25~1"
  LL_Df_4$Nug[(LL_Df_4$Nu)%in%Nu_vector[4:6]] <- "Nu=2~8"
  LL_Df_4$Nug[LL_Df_4$Nu%in%Nu_vector[7:9]] <- "Nu=16~64"
  # LL_Df_2$fg[LL_Df_2$f%in%f_vector[10:12]] <- "f=(10~12)/12"
  LL_Df_4$Nug <- factor(LL_Df_4$Nug, levels=c("Nu=0.25~1", "Nu=2~8", "Nu=16~64"))
  
  gP_Nu_1_lst[[i]] <- ggplot(LL_Df_3[1:18,], aes(x=Nu, y=LL_m, group=ED, color=ED, shape=ED))+
    geom_point(size=5, alpha=0.7)+
    geom_smooth(aes(x=Nu, y=LL_m),
                method = "lm", se= FALSE, size = 1, lty=2) +
    # geom_bar(stat="identity", color="black", 
    #          position=position_dodge()) +
    # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
    #               position=position_dodge(.9)) +
    geom_pointrange(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd))+
    xlab("Nutrient availability")+
    ylab("Optimal leaf longevity (log10(year))")+
    scale_color_manual(name="ED", values=c("blue", "red"))+
    # scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
    ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
    scale_y_log10()+
    scale_x_log10()+
    theme_classic()+
    theme(axis.text.x=element_text(size=8, face="bold"),
          axis.text.y=element_text(size=8, face="bold"),
          axis.title.x=element_text(size=15,face="bold"),
          axis.title.y=element_text(size=15,face="bold"),
          legend.text = element_text(colour = "black", size = 10, face = "bold"),
          legend.title = element_text(colour = "black", size = 12, face = "bold"))
  
  gP_Nu_2_lst[[i]] <- ggplot(LL_Df_4, aes(x=LMA, y=LL, group=Nug, color=Nug, shape=Nug))+
    geom_point(size=2, alpha=0.5)+
    geom_smooth(aes(x=LMA, y=LL),
                method = "lm", se= FALSE, size = 1, lty=2) +
    xlab("LMA (g m-2)")+
    ylab("Optimal leaf longevity (year)")+
    scale_color_manual(name="Nug", values=c("purple", "blue", "red"))+
    ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
    scale_y_log10()+
    scale_x_log10()+
    theme_classic()+
    theme(axis.text.x=element_text(size=8, face="bold"),
          axis.text.y=element_text(size=8, face="bold"),
          axis.title.x=element_text(size=15,face="bold"),
          axis.title.y=element_text(size=15,face="bold"),
          legend.text = element_text(colour = "black", size = 10, face = "bold"),
          legend.title = element_text(colour = "black", size = 12, face = "bold"))
  
  # gP_Nu_2_lst[[i]] <-ggplot(LL_Df_4, aes(x=LMA, y=LL, group=as.factor(Nu), color=as.factor(Nu), shape=as.factor(Nu)))+
  #   geom_point(size=2, alpha=0.5)+
  #   geom_smooth(aes(x=LMA, y=LL),
  #               method = "lm", se= FALSE, size = 1, lty=2) +
  #   xlab("LMA (g m-2)")+
  #   ylab("Optimal leaf longevity (year)")+
  #   scale_color_manual(name="Nug", values=rainbow(9))+
  #   scale_shape_manual(name="Nug", values=seq(1,9,by=1))+
  #   ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
  #   scale_y_log10()+
  #   scale_x_log10()+
  #   theme_classic()+
  #   theme(axis.text.x=element_text(size=8, face="bold"),
  #         axis.text.y=element_text(size=8, face="bold"),
  #         axis.title.x=element_text(size=15,face="bold"),
  #         axis.title.y=element_text(size=15,face="bold"),
  #         legend.text = element_text(colour = "black", size = 10, face = "bold"),
  #         legend.title = element_text(colour = "black", size = 12, face = "bold"))
  
  gP_Nu_3_lst[[i]] <- ggplot(LL_Df_7, aes(x=Nu, y=EDP))+
    geom_bar(stat="identity")+
    geom_smooth(aes(x=Nu, y=EDP),
                method = "lm", se= FALSE, color = "blue", size = 1, lty=2) +
    xlab("Nutrient availability")+
    ylab("Evergreen proportion")+
    scale_x_log10()+
    # scale_x_continuous(breaks=Nu_vector)+
    geom_hline(yintercept=1, colour="red", lty=2, size=1.5)+
    ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
    theme_classic()+
    theme(axis.text.x=element_text(size=8, face="bold"),
          axis.text.y=element_text(size=8, face="bold"),
          axis.title.x=element_text(size=15,face="bold"),
          axis.title.y=element_text(size=15,face="bold"),
          legend.text = element_text(colour = "black", size = 10, face = "bold"),
          legend.title = element_text(colour = "black", size = 12, face = "bold"))
}

plot_grid(gP_Nu_1_lst[[1]], gP_Nu_1_lst[[2]], gP_Nu_1_lst[[3]], gP_Nu_1_lst[[4]], gP_Nu_1_lst[[5]], 
          gP_Nu_1_lst[[6]], gP_Nu_1_lst[[7]], gP_Nu_1_lst[[8]], gP_Nu_1_lst[[9]], gP_Nu_1_lst[[10]], 
          gP_Nu_1_lst[[11]], gP_Nu_1_lst[[12]], ncol=3)

plot_grid(gP_Nu_2_lst[[1]], gP_Nu_2_lst[[2]], gP_Nu_2_lst[[3]], gP_Nu_2_lst[[4]], gP_Nu_2_lst[[5]], 
          gP_Nu_2_lst[[6]], gP_Nu_2_lst[[7]], gP_Nu_2_lst[[8]], gP_Nu_2_lst[[9]], gP_Nu_2_lst[[10]], 
          gP_Nu_2_lst[[11]], gP_Nu_2_lst[[12]], ncol=3)

plot_grid(gP_Nu_3_lst[[1]], gP_Nu_3_lst[[2]], gP_Nu_3_lst[[3]], gP_Nu_3_lst[[4]], gP_Nu_3_lst[[5]], 
          gP_Nu_3_lst[[6]], gP_Nu_3_lst[[7]], gP_Nu_3_lst[[8]], gP_Nu_3_lst[[9]], gP_Nu_3_lst[[10]], 
          gP_Nu_3_lst[[11]], gP_Nu_3_lst[[12]], ncol=3)

## plot LL-LMA based on Nmass/Pmass from GLOPNET
GLO_Df_1 <- GLOPNET%>%
  select('log LL', 'log LMA', 'log Nmass', 'PPT_PET', 'MAP')%>%
  drop_na()
colnames(GLO_Df_1) <- c("log_LL", "log_LMA", "log_Nmass", 'PPT_PET', 'MAP')
GLO_Df_1 <- GLO_Df_1%>%
  mutate(LL=10^log_LL/12, LMA=10^log_LMA, Nmass=10^log_Nmass, Nclass=log_Nmass)


plot(log10(as.numeric(GLO_Df_1$PPT_PET)), GLO_Df_1$Nmass)
plot(as.numeric(GLO_Df_1$PPT_PET), 10^GLO_Df_1$Nmass)
# lm_stat(log10(as.numeric(GLO_Df_1$PPT_PET)), GLO_Df_1$Nmass)
# lm_stat(as.numeric(GLO_Df_1$PPT_PET), 10^GLO_Df_1$Nmass)

hist(GLO_Df_1$Nmass, breaks=10)
min(GLO_Df_1$Nmass)
max(GLO_Df_1$Nmass)

# Breakpoint <- median(GLO_Df_1$Nmass)
Breakpoints <- unname(quantile(GLO_Df_1$Nmass, c(0, 1/5, 2/5, 3/5, 4/5, 1)))
GLO_Df_1$Nclass[(GLO_Df_1$Nmass>=Breakpoints[1])&(GLO_Df_1$Nmass<=Breakpoints[2])] <- "1"
GLO_Df_1$Nclass[(GLO_Df_1$Nmass>Breakpoints[2])&(GLO_Df_1$Nmass<=Breakpoints[3])] <- "2"
GLO_Df_1$Nclass[(GLO_Df_1$Nmass>Breakpoints[3])&(GLO_Df_1$Nmass<=Breakpoints[4])] <- "3"
GLO_Df_1$Nclass[(GLO_Df_1$Nmass>Breakpoints[4])&(GLO_Df_1$Nmass<=Breakpoints[5])] <- "4"
GLO_Df_1$Nclass[(GLO_Df_1$Nmass>Breakpoints[5])&(GLO_Df_1$Nmass<=Breakpoints[6])] <- "5"
# # 
GLO_Df_1$Nclass <- factor(GLO_Df_1$Nclass, levels=c("1", "2", "3", "4", "5"))

 # GLO_Df_1$Nclass[(GLO_Df_1$Nmass>0)&(GLO_Df_1$Nmass<=Breakpoint)] <- "L"
 # GLO_Df_1$Nclass[(GLO_Df_1$Nmass>Breakpoint)&(GLO_Df_1$Nmass<=4.8)] <- "H"

 # GLO_Df_1$Nclass <- factor(GLO_Df_1$Nclass, levels=c("L", "H"))
GLO_Df_1 <- GLO_Df_1%>%
  mutate(LLg=LL, MAPg=MAP)

Bins_ME_G1 <- seq(min(GLO_Df_1$log_LMA), max(GLO_Df_1$log_LMA), length=21)
Bins_ME_G1.1 <- seq(min(log10(GLO_Df_1$MAP)), max(log10(GLO_Df_1$MAP)), length=21)
# Bins_ME_2[1] <- Bins_ME_2[1]-0.1 
for(i in 1:20){
  GLO_Df_1$LLg[(GLO_Df_1$log_LMA >= Bins_ME_G1[i])&(GLO_Df_1$log_LMA <= Bins_ME_G1[i+1])] <- 10^((Bins_ME_G1[i]+Bins_ME_G1[i+1])/2)
}

for(i in 1:20){
  GLO_Df_1$MAPg[(log10(GLO_Df_1$MAP) >= Bins_ME_G1.1[i])&(log10(GLO_Df_1$MAP) <= Bins_ME_G1.1[i+1])]  <- 10^((Bins_ME_G1.1[i]+Bins_ME_G1.1[i+1])/2)
}

ln_G1 <- length(unique(GLO_Df_1$LLg))
ln_G1.1 <- length(unique(GLO_Df_1$MAPg))

LL_Df_ME_G1 <- data.frame(Mu_LL=rep(NA, 5*ln_G1), SD_logLL=rep(NA, 5*ln_G1), 
                         Nclass=rep(unique(GLO_Df_1$Nclass), each=ln_G1), 
                         LLg=rep(unique(GLO_Df_1$LLg), 5))

LL_Df_ME_G1.1 <- data.frame(Mu_N=rep(NA, ln_G1.1), SD_logN=rep(NA, ln_G1.1), 
                          MAPg=unique(GLO_Df_1$MAPg))

for(i in 1:(5*ln_G1)){
  Nclass=LL_Df_ME_G1$Nclass[i]
  LLg=LL_Df_ME_G1$LLg[i]
  LL_Df_ME_G1$Mu_LL[i] = mean(GLO_Df_1$LL[(GLO_Df_1$Nclass==Nclass)&(GLO_Df_1$LLg==LLg)])
  LL_Df_ME_G1$SD_logLL[i] = sd(GLO_Df_1$log_LL[(GLO_Df_1$Nclass==Nclass)&(GLO_Df_1$LLg==LLg)])
}

for(i in 1:ln_G1.1){
  MAPg=LL_Df_ME_G1.1$MAPg[i]
  LL_Df_ME_G1.1$Mu_N[i] = mean(GLO_Df_1$Nmass[GLO_Df_1$MAPg==MAPg])
  LL_Df_ME_G1.1$SD_logN[i] = sd(GLO_Df_1$log_Nmass[GLO_Df_1$MAPg==MAPg])
}

gloP1.4 <- ggplot(LL_Df_ME_G1.1, aes(x=MAPg, y=Mu_N))+
  geom_point(size=5, alpha=0.7)+
  geom_smooth(aes(x=MAPg, y=Mu_N),
              method = "lm", se= FALSE, size = 1, lty=2, color="red") +
  # geom_bar(stat="identity", color="black", 
  #          position=position_dodge()) +
  # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
  #               position=position_dodge(.9)) +
  geom_pointrange(aes(ymin=Mu_N-SD_logN, ymax=Mu_N+SD_logN))+
  xlab("MAP (mm)")+
  ylab("Nmass (%)")+
  # scale_color_manual(name="Nclass", values=c("purple", "blue", "darkcyan", "red", "gold"))+
  # scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
  # ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
  scale_x_log10()+
  scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

SMA_S_11 <- SMA_stat(log10(LL_Df_ME_G1.1$MAPg), log10(LL_Df_ME_G1.1$Mu_N))
CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=11, X="log10(MAP)", Y="log10(Nmass)", 
                                                       Group=NA, Intercept=SMA_S_11$Intercept, Ymx=SMA_S_11$Ymx, 
                                                       Slope=SMA_S_11$Slope, Slope_lower=SMA_S_11$Slope_lower, Slope_upper=SMA_S_11$Slope_upper,
                                                       P=SMA_S_11$P, R_sqaure=SMA_S_11$R_square))

gloP1 <- ggplot(GLO_Df_1, aes(x=LMA, y=LL, color=Nclass, shape=Nclass))+
  geom_point(size=2, alpha=0.5)+
  geom_smooth(aes(x=LMA, y=LL),
              method = "lm", se= FALSE, size = 1, lty=2) +
  xlab("LMA (g m-2)")+
  ylab("Optimal leaf longevity (year)")+
  scale_color_manual(name="Nclass", values=c("purple", "blue", "darkcyan", "red", "gold"))+
  # ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
  scale_y_log10()+
  scale_x_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

gloP1.1 <- ggplot(LL_Df_ME_G1, aes(x=LLg, y=Mu_LL, group=Nclass, color=Nclass, shape=Nclass))+
  geom_point(size=5, alpha=0.7)+
  geom_smooth(aes(x=LLg, y=Mu_LL),
              method = "lm", se= FALSE, size = 1, lty=2) +
  # geom_bar(stat="identity", color="black", 
  #          position=position_dodge()) +
  # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
  #               position=position_dodge(.9)) +
  geom_pointrange(aes(ymin=Mu_LL-SD_logLL, ymax=Mu_LL+SD_logLL))+
  xlab("LMA (g m-2)")+
  ylab("Optimal leaf longevity (year)")+
  scale_color_manual(name="Nclass", values=c("purple", "blue", "darkcyan", "red", "gold"))+
  # scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
  # ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
  scale_x_log10()+
  scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))


gloP1.2 <- ggplot(GLO_Df_1, aes(x=MAP, y=Nmass))+
  geom_point(size=3, alpha=0.7)+
  geom_smooth(aes(x=MAP, y=Nmass),
              method = "lm", se= FALSE, size = 1, lty=2) +
  # geom_bar(stat="identity", color="black", 
  #          position=position_dodge()) +
  # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
  #               position=position_dodge(.9)) +
  # geom_pointrange(aes(ymin=Mu_LL-SD_logLL, ymax=Mu_LL+SD_logLL))+
  xlab("MAP (mm)")+
  ylab("Nmass (%)")+
  # scale_color_manual(name="Nclass", values=c("purple", "blue", "darkcyan", "red", "gold"))+
  # scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
  # ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
  scale_x_log10()+
  scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

gloP1.3 <- ggplot(GLO_Df_1, aes(x=PPT_PET, y=Nmass))+
  geom_point(size=3, alpha=0.7)+
  geom_smooth(aes(x=PPT_PET, y=Nmass),
              method = "lm", se= FALSE, size = 1, lty=2) +
  # geom_bar(stat="identity", color="black", 
  #          position=position_dodge()) +
  # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
  #               position=position_dodge(.9)) +
  # geom_pointrange(aes(ymin=Mu_LL-SD_logLL, ymax=Mu_LL+SD_logLL))+
  xlab("PPT / PET")+
  ylab("Nmass (%)")+
  # scale_color_manual(name="Nclass", values=c("purple", "blue", "darkcyan", "red", "gold"))+
  # scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
  # ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
  scale_x_log10()+
  scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))
# ggplot(GLO_Df_1, aes(x=LMA, y=LL, color=Nclass))+
#   geom_point(size=2, alpha=0.5, aes(shape=ED))+
#   geom_smooth(aes(x=LMA, y=LL),
#               method = "lm", se= FALSE, size = 1, lty=2) +
#   xlab("LMA (g m-2)")+
#   ylab("Optimal leaf longevity (year)")+
#   scale_color_manual(name="Nclass", values=c("blue", "red"))+
#   # ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
#   scale_y_log10()+
#   scale_x_log10()+
#   theme_classic()+
#   theme(axis.text.x=element_text(size=8, face="bold"),
#         axis.text.y=element_text(size=8, face="bold"),
#         axis.title.x=element_text(size=15,face="bold"),
#         axis.title.y=element_text(size=15,face="bold"),
#         legend.text = element_text(colour = "black", size = 10, face = "bold"),
#         legend.title = element_text(colour = "black", size = 12, face = "bold"))

## plot LL-LMA grouped by ppt_pet from GLOPNET
GLO_Df_2 <- GLOPNET%>%
  select('log LL', 'log LMA', 'PPT_PET', 'MAP')%>%
  drop_na()
colnames(GLO_Df_2) <- c("log_LL", "log_LMA", 'PPT_PET', 'MAP')
GLO_Df_2 <- GLO_Df_2%>%
  mutate(LL=10^log_LL/12, LMA=10^log_LMA, log_ER = log10(as.numeric(PPT_PET)), ERclass=PPT_PET, Pclass=as.numeric(MAP))

# plot(log(as.numeric(GLOPNET$PPT_PET)), log(as.numeric(GLOPNET$MAP)))
# plot(as.numeric(GLOPNET$PPT_PET), as.numeric(GLOPNET$MAP))
# 
# lm_1 <- lm(formula=y~x, data=data.frame(x=as.numeric(GLOPNET$PPT_PET), y=as.numeric(GLOPNET$MAP)))
# summary(lm_1)

GLO_Df_2$PPT_PET <- as.numeric(GLO_Df_2$PPT_PET)
GLO_Df_2$MAP <- as.numeric(GLO_Df_2$MAP)

hist(GLO_Df_2$log_ER, breaks=10)
min(GLO_Df_2$log_ER)
max(GLO_Df_2$log_ER)

# Breakpoint <- median(GLO_Df_2$PPT_PET)
# Breakpoints_2 <- unname(quantile(GLO_Df_2$log_ER, c(0, 1/3, 2/3, 1)))
Breakpoints_2 <- c(min(GLO_Df_2$log_ER),seq(0, max(GLO_Df_2$log_ER), length=4))
# bin_ER <- max(GLO_Df_2$PPT_PET)/8
# Breakpoints_2 <- c(0, bin_ER*2, bin_ER*4, bin_ER*8)
 Breakpoints_2_p <- c(0, 300, 600, 1200, 2400, 4800)

 GLO_Df_2$Pclass[(GLO_Df_2$MAP >=Breakpoints_2_p[1])&(GLO_Df_2$MAP <=Breakpoints_2_p[2])] <- "1"
 GLO_Df_2$Pclass[(GLO_Df_2$MAP >=Breakpoints_2_p[2])&(GLO_Df_2$MAP <=Breakpoints_2_p[3])] <- "2"
 GLO_Df_2$Pclass[(GLO_Df_2$MAP >=Breakpoints_2_p[3])&(GLO_Df_2$MAP <=Breakpoints_2_p[4])] <- "3"
 GLO_Df_2$Pclass[(GLO_Df_2$MAP >=Breakpoints_2_p[4])&(GLO_Df_2$MAP <=Breakpoints_2_p[5])] <- "4"
 GLO_Df_2$Pclass[(GLO_Df_2$MAP >=Breakpoints_2_p[5])&(GLO_Df_2$MAP <=Breakpoints_2_p[6])] <- "5"

 GLO_Df_2$Pclass <- factor(GLO_Df_2$Pclass, levels=c("1", "2", "3", "4", "5"))

GLO_Df_2$ERclass[(GLO_Df_2$log_ER >=Breakpoints_2[1])&(GLO_Df_2$log_ER <=Breakpoints_2[2])] <- "1"
GLO_Df_2$ERclass[(GLO_Df_2$log_ER >Breakpoints_2[2])&(GLO_Df_2$log_ER <=Breakpoints_2[3])] <- "2"
GLO_Df_2$ERclass[(GLO_Df_2$log_ER >Breakpoints_2[3])&(GLO_Df_2$log_ER <=Breakpoints_2[4])] <- "3"
GLO_Df_2$ERclass[(GLO_Df_2$log_ER >Breakpoints_2[4])&(GLO_Df_2$log_ER <=Breakpoints_2[5])] <- "4"
# GLO_Df_2$ERclass[(GLO_Df_2$log_ER >Breakpoints_2[5])&(GLO_Df_2$log_ER <=Breakpoints_2[6])] <- "5"

# GLO_Df_2$ERclass[(GLO_Df_2$PPT_PET >=Breakpoints_2[1])&(GLO_Df_2$PPT_PET <=Breakpoints_2[2])] <- "1"
# GLO_Df_2$ERclass[(GLO_Df_2$PPT_PET >Breakpoints_2[2])&(GLO_Df_2$PPT_PET <=Breakpoints_2[3])] <- "2"
# GLO_Df_2$ERclass[(GLO_Df_2$PPT_PET >Breakpoints_2[3])&(GLO_Df_2$PPT_PET <=Breakpoints_2[4])] <- "3"
# GLO_Df_2$ERclass[(GLO_Df_2$PPT_PET >Breakpoints_2[4])&(GLO_Df_2$PPT_PET <=Breakpoints_2[5])] <- "4"
# GLO_Df_2$ERclass[(GLO_Df_2$PPT_PET >Breakpoints_2[5])&(GLO_Df_2$PPT_PET <=Breakpoints_2[6])] <- "5"
#
# GLO_Df_2$ERclass <- factor(GLO_Df_2$ERclass, levels=c("1", "2", "3", "4", "5"))
GLO_Df_2$ERclass <- factor(GLO_Df_2$ERclass, levels=c("1", "2", "3","4"))
GLO_Df_2 <- GLO_Df_2%>%
  mutate(LLg=LL)

Bins_ME_G2 <- seq(min(GLO_Df_2$log_LMA), max(GLO_Df_2$log_LMA), length=21)
# Bins_ME_2[1] <- Bins_ME_2[1]-0.1 
for(i in 1:20){
  GLO_Df_2$LLg[(GLO_Df_2$log_LMA >= Bins_ME_G2[i])&(GLO_Df_2$log_LMA <= Bins_ME_G2[i+1])] <- 10^((Bins_ME_G2[i]+Bins_ME_G2[i+1])/2)
}

ln_G2 <- length(unique(GLO_Df_2$LLg))

LL_Df_ME_G2 <- data.frame(Mu_LL=rep(NA, 4*ln_G2), SD_logLL=rep(NA, 4*ln_G2), 
                          ERclass=rep(unique(GLO_Df_2$ERclass), each=ln_G2), 
                          LLg=rep(unique(GLO_Df_2$LLg), 4))

LL_Df_ME_G2_P <- data.frame(Mu_LL=rep(NA, 5*ln_G2), SD_logLL=rep(NA, 5*ln_G2), 
                          Pclass=rep(unique(GLO_Df_2$Pclass), each=ln_G2), 
                          LLg=rep(unique(GLO_Df_2$LLg), 5))

for(i in 1:(4*ln_G1)){
  ERclass=LL_Df_ME_G2$ERclass[i]
  LLg=LL_Df_ME_G2$LLg[i]
  LL_Df_ME_G2$Mu_LL[i] = mean(GLO_Df_2$LL[(GLO_Df_2$ERclass==ERclass)&(GLO_Df_2$LLg==LLg)])
  LL_Df_ME_G2$SD_logLL[i] = sd(GLO_Df_2$log_LL[(GLO_Df_2$ERclass==ERclass)&(GLO_Df_2$LLg==LLg)])
}

for(i in 1:(5*ln_G1)){
  Pclass=LL_Df_ME_G2_P$Pclass[i]
  LLg=LL_Df_ME_G2_P$LLg[i]
  LL_Df_ME_G2_P$Mu_LL[i] = mean(GLO_Df_2$LL[(GLO_Df_2$Pclass==Pclass)&(GLO_Df_2$LLg==LLg)])
  LL_Df_ME_G2_P$SD_logLL[i] = sd(GLO_Df_2$log_LL[(GLO_Df_2$Pclass==Pclass)&(GLO_Df_2$LLg==LLg)])
}


for(ER in unique(GLO_Df_2$ERclass)){
  lm_S_8 <- lm_stat(GLO_Df_2$log_LMA[GLO_Df_2$ERclass==ER], GLO_Df_2$log_LL[GLO_Df_2$ERclass==ER])
  CBLP_YB_Stat <- rbind(CBLP_YB_Stat, data.frame(Figure=8, X="log10(LMA)", Y="log10(LL)", 
                                                 Group=paste("ERclass=",ER, sep=""), Intercept=lm_S_8$Intercept, Ymx=lm_S_8$Ymx, 
                                                 Slope=lm_S_8$Slope, P=lm_S_8$P, R_sqaure=lm_S_8$R_square))
}

for(ER in unique(GLO_Df_2$ERclass)){
  SMA_S_8 <- SMA_stat(GLO_Df_2$log_LMA[GLO_Df_2$ERclass==ER], GLO_Df_2$log_LL[GLO_Df_2$ERclass==ER])
  CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=8, X="log10(LMA)", Y="log10(LL)", 
                                                 Group=paste("ERclass=",ER, sep=""), Intercept=SMA_S_8$Intercept, Ymx=SMA_S_8$Ymx, 
                                                 Slope=SMA_S_8$Slope, Slope_lower=SMA_S_8$Slope_lower, Slope_upper=SMA_S_8$Slope_upper,
                                                 P=SMA_S_8$P, R_sqaure=SMA_S_8$R_square))
}
for(ER in unique(GLO_Df_2$ERclass)){
  SMA_S_8.1 <- SMA_stat(log10(LL_Df_ME_G2$LLg[LL_Df_ME_G2$ERclass==ER]), log10(LL_Df_ME_G2$Mu_LL[LL_Df_ME_G2$ERclass==ER]))
  CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=8.1, X="log10(LMA)", Y="log10(LL)", 
                                                         Group=paste("ERclass=",ER, sep=""), Intercept=SMA_S_8.1$Intercept, Ymx=SMA_S_8.1$Ymx, 
                                                         Slope=SMA_S_8.1$Slope, Slope_lower=SMA_S_8.1$Slope_lower, Slope_upper=SMA_S_8.1$Slope_upper,
                                                         P=SMA_S_8.1$P, R_sqaure=SMA_S_8.1$R_square))
}

lm_S_9 <- lm_stat(log10(as.numeric(GLOPNET$MAP)), log10(as.numeric(GLOPNET$PPT_PET)))
CBLP_YB_Stat <- rbind(CBLP_YB_Stat, data.frame(Figure=9, X="log10(MAP)", Y="log10(PPT_PET)", 
                                               Group=NA, Intercept=lm_S_9$Intercept, Ymx=lm_S_9$Ymx, 
                                               Slope=lm_S_9$Slope, P=lm_S_9$P, R_sqaure=lm_S_9$R_square))

SMA_S_9 <- SMA_stat(log10(as.numeric(GLOPNET$MAP)), log10(as.numeric(GLOPNET$PPT_PET)))
CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=9, X="log10(MAP)", Y="log10(PPT_PET)", 
                                               Group=NA, Intercept=SMA_S_9$Intercept, Ymx=SMA_S_9$Ymx, 
                                               Slope=SMA_S_9$Slope, Slope_lower=SMA_S_9$Slope_lower, Slope_upper=SMA_S_9$Slope_upper,
                                               P=SMA_S_9$P, R_sqaure=SMA_S_9$R_square))

for(P in unique(GLO_Df_2$Pclass)){
  # SMA_S_10 <- SMA_stat(GLO_Df_2$log_LMA[GLO_Df_2$Pclass==P], GLO_Df_2$log_LL[GLO_Df_2$Pclass==P])
  # CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=10, X="log10(LMA)", Y="log10(LL)", 
  #                                                        Group=paste("Pclass=",P, sep=""), Intercept=SMA_S_10$Intercept, Ymx=SMA_S_10$Ymx, 
  #                                                        Slope=SMA_S_10$Slope, Slope_lower=SMA_S_10$Slope_lower, Slope_upper=SMA_S_10$Slope_upper,
  #                                                        P=SMA_S_10$P, R_sqaure=SMA_S_10$R_square))
  
  SMA_S_10.1 <- SMA_stat(log10(LL_Df_ME_G2_P$LLg[LL_Df_ME_G2_P$Pclass==P]), log10(LL_Df_ME_G2_P$Mu_LL[LL_Df_ME_G2_P$Pclass==P]))
  CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=10.1, X="log10(LMA)", Y="log10(LL)", 
                                                         Group=paste("Pclass=",P, sep=""), Intercept=SMA_S_10.1$Intercept, Ymx=SMA_S_10.1$Ymx, 
                                                         Slope=SMA_S_10.1$Slope, Slope_lower=SMA_S_10.1$Slope_lower, Slope_upper=SMA_S_10.1$Slope_upper,
                                                         P=SMA_S_10.1$P, R_sqaure=SMA_S_10.1$R_square))
}

gloP2.3 <- ggplot(LL_Df_ME_G2, aes(x=LLg, y=Mu_LL, group=ERclass, color=ERclass, shape=ERclass))+
  geom_point(size=5, alpha=0.7)+
  geom_smooth(aes(x=LLg, y=Mu_LL),
              method = "lm", se= FALSE, size = 1, lty=2) +
  # geom_bar(stat="identity", color="black", 
  #          position=position_dodge()) +
  # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
  #               position=position_dodge(.9)) +
  geom_pointrange(aes(ymin=Mu_LL-SD_logLL, ymax=Mu_LL+SD_logLL))+
  xlab("LMA (g m-2)")+
  ylab("Optimal leaf longevity (year)")+
  # scale_color_manual(name="ERclass", values=c("purple", "blue", "darkcyan", "red", "gold"))+
  scale_color_manual(name="ERclass", values=c("purple", "blue", "darkcyan", "red"))+
  # scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
  # ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
  scale_x_log10()+
  scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

gloP2.4 <- ggplot(LL_Df_ME_G2_P, aes(x=LLg, y=Mu_LL, group=Pclass, color=Pclass, shape=Pclass))+
  geom_point(size=5, alpha=0.7)+
  geom_smooth(aes(x=LLg, y=Mu_LL),
              method = "lm", se= FALSE, size = 1, lty=2) +
  # geom_bar(stat="identity", color="black", 
  #          position=position_dodge()) +
  # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
  #               position=position_dodge(.9)) +
  geom_pointrange(aes(ymin=Mu_LL-SD_logLL, ymax=Mu_LL+SD_logLL))+
  xlab("LMA (g m-2)")+
  ylab("Optimal leaf longevity (year)")+
  # scale_color_manual(name="ERclass", values=c("purple", "blue", "darkcyan", "red", "gold"))+
  scale_color_manual(name="Pclass", values=c("purple", "blue", "darkcyan", "red", "gold"))+
  # scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
  # ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
  scale_x_log10()+
  scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))


gloP2 <- ggplot(GLOPNET, aes(x=MAP, y=PPT_PET))+
  geom_point(size=2, alpha=0.5)+
  geom_smooth(aes(x=MAP, y=PPT_PET),
              method = "lm", se= FALSE, size = 1, lty=2) +
  xlab("MAP (mm)")+
  ylab("ER")+
  # scale_color_manual(name="ERclass", values=c("purple", "blue", "red"))+
  # ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
  scale_y_log10()+
  scale_x_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))


GLOPNET_2 <- GLOPNET%>%
  select('log LMA', 'log LL', Dataset)%>%
  drop_na()
colnames(GLOPNET_2) <- c("log_LMA", "log_LL", "Dataset")
GLOPNET_2 <- GLOPNET_2%>%
  mutate(LMA=log_LMA^10, LL=log_LL^10)

ggplot(GLOPNET_2, aes(x=LMA, y=LL))+
  geom_point(size=2, alpha=0.5, aes(color=Dataset))+
  geom_smooth(aes(x=LMA, y=LL),
              method = "lm", se= FALSE, size = 1, lty=2) +
  xlab("LMA (g m-2)")+
  ylab("Optimal leaf longevity (year)")+
  # scale_color_manual(name="ERclass", values=c("purple", "blue", "red"))+
  # ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
  scale_y_log10()+
  scale_x_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        # legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.position = "none")


gloP2.1 <- ggplot(GLO_Df_2, aes(x=LMA, y=LL, color=ERclass, shape=ERclass))+
  geom_point(size=2, alpha=0.5)+
  geom_smooth(aes(x=LMA, y=LL),
              method = "lm", se= FALSE, size = 1, lty=2) +
  xlab("LMA (g m-2)")+
  ylab("Optimal leaf longevity (year)")+
  scale_color_manual(name="ERclass", values=c("purple", "blue", "darkcyan", "red", "gold"))+
  # ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
  scale_y_log10()+
  scale_x_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

gloP2.2 <- ggplot(GLO_Df_2, aes(x=LMA, y=LL, color=Pclass))+
  geom_point(size=2, alpha=0.5)+
  geom_smooth(aes(x=LMA, y=LL),
              method = "lm", se= FALSE, size = 1, lty=2) +
  xlab("LMA (g m-2)")+
  ylab("Optimal leaf longevity (year)")+
  scale_color_manual(name="Pclass", values=c("purple", "blue", "darkcyan", "red", "gold"))+
  # ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
  scale_y_log10()+
  scale_x_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))



##########################################################################
##----------visualize sum plot and calculate corresponding statistics--###
CBLP_YB_Stat <- NULL
CBLP_YB_SMA_Stat <- NULL

write.xlsx(CBLP_YB_Stat, file="Data/CBLP_YB_Stat_1.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
write.xlsx(CBLP_YB_SMA_Stat, file="Data/CBLP_YB_SMA_Stat_2.xlsx", 
           col.names = TRUE, row.names = TRUE, append = FALSE)
X= log10(LL_Df_S_2$LMA[LL_Df_S_2$fg==fg])
Y= log10(LL_Df_S_2$LL[LL_Df_S_2$fg==fg])
lm_stat <- function(X, Y){
  mx=mean(X)
  lm_modl <- lm(formula = y~x, data=data.frame(x=X, y=Y)) 
  lm_summ <- summary(lm_modl)
  Slope <- lm_summ$coefficients[2,1]
  Intercept <- lm_summ$coefficients[1,1]
  P <- lm_summ$coefficients[2,4]
  Ymx <- predict(lm_modl, newdata=(data.frame(x=mx)))
  R_square <- lm_summ$r.squared
  return(list(Slope=Slope, Intercept=Intercept, P=P, Ymx=Ymx, R_square=R_square))
}

SMA_stat <- function(X, Y){
  mx=mean(X)
  sma_modl <- sma(y~x, slope.test=1, data=data.frame(x=X, y=Y))
  Slope <- sma_modl$coef[[1]][2,1]
  Slope_lower <-  sma_modl$coef[[1]][2,2]
  Slope_upper <-  sma_modl$coef[[1]][2,3]
  
  Intercept <- sma_modl$coef[[1]][1,1]
  Intercept_lower <-  sma_modl$coef[[1]][1,2]
  Intercept_upper <-  sma_modl$coef[[1]][1,3]
  
  P <- sma_modl$pval[[1]]
  Ymx <- Slope*mx+Intercept
  R_square <- sma_modl$r2[[1]]
  return(list(Slope=Slope, Slope_lower=Slope_lower, Slope_upper=Slope_upper,
              Intercept=Intercept, Intercept_lower=Intercept_lower, Intercept_upper=Intercept_upper,
              P=P, Ymx=Ymx, R_square=R_square))
}

## Figure.1
for(f in f_vector){
  for(ED in 0:1){
    DFt <- EvDeDf_3[(EvDeDf_3$f==f)&(EvDeDf_3$ED==ED),]
    LL_Df_S_1 <- rbind(LL_Df_1, data.frame(f=f, LL_m=mean(DFt$LL), LL_sd=sd(log10(DFt$LL)), ED=ED_vector[(ED+1)]))
  }
}
LL_Df_S_1 <- LL_Df_S_1[1:24, ]

gsP1 <- ggplot(LL_Df_S_1, aes(x=f, y=LL_m, group=ED, color=ED, shape=ED))+
  geom_point(size=3.5, alpha=0.5)+
  geom_smooth(aes(x=f, y=LL_m),
              method = "lm", se= FALSE, size = 1, lty=2) +
  # geom_bar(stat="identity", color="black", 
  #          position=position_dodge()) +
  # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
  #               position=position_dodge(.9)) +
  geom_pointrange(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd))+
  xlab("Favourable season length (yr yr-1)")+
  ylab("Optimal leaf longevity (year)")+
  scale_color_manual(name="ED", values=c("blue", "red"))+
  scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
  # ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
  scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=10, face="bold"),
        axis.text.y=element_text(size=10, face="bold"),
        axis.title.x=element_text(size=18,face="bold"),
        axis.title.y=element_text(size=18,face="bold"),
        legend.text = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_text(colour = "black", size = 15, face = "bold"))

for(ED in ED_vector){
  lm_S_1 <- lm_stat(LL_Df_S_1$f[LL_Df_S_1$ED==ED], log10(LL_Df_S_1$LL_m[LL_Df_S_1$ED==ED]))
  CBLP_YB_Stat <- rbind(CBLP_YB_Stat, data.frame(Figure=1, X="f", Y="log10(LL)", 
                                                 Group=ED, Intercept=lm_S_1$Intercept, Ymx=lm_S_1$Ymx, 
                                                 Slope=lm_S_1$Slope, P=lm_S_1$P, R_sqaure=lm_S_1$R_square))
}

for(ED in ED_vector){
  SMA_S_1 <- SMA_stat(LL_Df_S_1$f[LL_Df_S_1$ED==ED], log10(LL_Df_S_1$LL_m[LL_Df_S_1$ED==ED]))
  CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=1, X="f", Y="log10(LL)", 
                                                 Group=ED, Intercept=SMA_S_1$Intercept, Ymx=SMA_S_1$Ymx, 
                                                 Slope=SMA_S_1$Slope, Slope_lower=SMA_S_1$Slope_lower, Slope_upper=SMA_S_1$Slope_upper,
                                                 P=SMA_S_1$P, R_sqaure=SMA_S_1$R_square))
}

## Figure.2
LL_Df_S_2 <- EvDeDf_3%>%
  select(f, LL, LMA)%>%
  mutate(fg=f)
LL_Df_S_2$fg[(LL_Df_S_2$f)%in%f_vector[1:3]] <- "f=(1~3)/12"
LL_Df_S_2$fg[(LL_Df_S_2$f)%in%f_vector[4:6]] <- "f=(4~6)/12"
LL_Df_S_2$fg[LL_Df_S_2$f%in%f_vector[7:9]] <- "f=(7~9)/12"
LL_Df_S_2$fg[LL_Df_S_2$f%in%f_vector[10:12]] <- "f=(10~12)/12"
LL_Df_S_2$fg <- factor(LL_Df_S_2$fg, levels=c("f=(1~3)/12", "f=(4~6)/12", "f=(7~9)/12", "f=(10~12)/12"))

LL_Df_S_2 <- LL_Df_S_2%>%
  mutate(log_LL=log10(LL), log_LMA=log10(LMA), LLg=LL)

Bins_ME_2 <- seq(min(LL_Df_S_2$log_LMA), max(LL_Df_S_2$log_LMA), length=21)
# Bins_ME_2[1] <- Bins_ME_2[1]-0.1 
for(i in 1:20){
  LL_Df_S_2$LLg[(LL_Df_S_2$log_LMA >= Bins_ME_2[i])&(LL_Df_S_2$log_LMA <= Bins_ME_2[i+1])] <- 10^((Bins_ME_2[i]+Bins_ME_2[i+1])/2)
}

ln_2 <- length(unique(LL_Df_S_2$LLg))

LL_Df_ME_2 <- data.frame(Mu_LL=rep(NA, 4*ln_2), SD_logLL=rep(NA, 4*ln_2), 
                         fg=rep(unique(LL_Df_S_2$fg), each=ln_2), 
                         LLg=rep(unique(LL_Df_S_2$LLg), 4))

for(i in 1:(4*ln_2)){
  fg=LL_Df_ME_2$fg[i]
  LLg=LL_Df_ME_2$LLg[i]
  LL_Df_ME_2$Mu_LL[i] = mean(LL_Df_S_2$LL[(LL_Df_S_2$fg==fg)&(LL_Df_S_2$LLg==LLg)])
  LL_Df_ME_2$SD_logLL[i] = sd(LL_Df_S_2$log_LL[(LL_Df_S_2$fg==fg)&(LL_Df_S_2$LLg==LLg)])
}

gmeP2 <-  ggplot(LL_Df_ME_2, aes(x=LLg, y=Mu_LL, group=fg, color=fg, shape=fg))+
  geom_point(size=5, alpha=0.7)+
  geom_smooth(aes(x=LLg, y=Mu_LL),
              method = "lm", se= FALSE, size = 1, lty=2) +
  # geom_bar(stat="identity", color="black", 
  #          position=position_dodge()) +
  # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
  #               position=position_dodge(.9)) +
  geom_pointrange(aes(ymin=Mu_LL-SD_logLL, ymax=Mu_LL+SD_logLL))+
  xlab("LMA (g m-2)")+
  ylab("Optimal leaf longevity (year)")+
  scale_color_manual(name="fg", values=c("purple", "blue", "gold", "red"))+
  # scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
  # ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
  scale_x_log10()+
  scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))


for(fg in unique(LL_Df_S_2$fg)){
  lm_S_2 <- lm_stat(log10(LL_Df_S_2$LMA[LL_Df_S_2$fg==fg]), log10(LL_Df_S_2$LL[LL_Df_S_2$fg==fg]))
  CBLP_YB_Stat <- rbind(CBLP_YB_Stat, data.frame(Figure=2, X="log10(LMA)", Y="log10(LL)", 
                                                 Group=fg, Intercept=lm_S_2$Intercept, Ymx=lm_S_2$Ymx, 
                                                 Slope=lm_S_2$Slope, P=lm_S_2$P, R_sqaure=lm_S_2$R_square))
}

for(fg in unique(LL_Df_S_2$fg)){
  SMA_S_2 <- SMA_stat(log10(LL_Df_S_2$LMA[LL_Df_S_2$fg==fg]), log10(LL_Df_S_2$LL[LL_Df_S_2$fg==fg]))
  CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=2, X="log10(LMA)", Y="log10(LL)", 
                                                 Group=fg, Intercept=SMA_S_2$Intercept, Ymx=SMA_S_2$Ymx, 
                                                 Slope=SMA_S_2$Slope, Slope_lower=SMA_S_2$Slope_lower, Slope_upper=SMA_S_2$Slope_upper,
                                                 P=SMA_S_2$P, R_sqaure=SMA_S_2$R_square))
  SMA_S_2.1 <- SMA_stat(log10(LL_Df_ME_2$LLg[LL_Df_ME_2$fg==fg]), log10(LL_Df_ME_2$Mu_LL[LL_Df_ME_2$fg==fg]))
  CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=2.1, X="log10(LMA)", Y="log10(LL)", 
                                                         Group=fg, Intercept=SMA_S_2.1$Intercept, Ymx=SMA_S_2.1$Ymx, 
                                                         Slope=SMA_S_2.1$Slope, Slope_lower=SMA_S_2.1$Slope_lower, Slope_upper=SMA_S_2.1$Slope_upper,
                                                         P=SMA_S_2.1$P, R_sqaure=SMA_S_2.1$R_square))
}

gsP2 <- ggplot(LL_Df_S_2, aes(x=LMA, y=LL, group=fg, color=fg, shape=fg))+
  geom_point(size=2, alpha=0.5)+
  geom_smooth(aes(x=LMA, y=LL),
              method = "lm", se= FALSE, size = 1, lty=2) +
  xlab("LMA (g m-2)")+
  ylab("Optimal leaf longevity (year)")+
  scale_color_manual(name="fg", values=c("purple", "blue", "gold", "red"))+
  # ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
  scale_y_log10()+
  scale_x_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

# ggplot(EvDeDf_3, aes(x=a, y=LL))+
#   geom_point(size=2, alpha=0.5)+
#   geom_smooth(aes(x=a, y=LL),
#               method = "lm", se= FALSE, size = 1, lty=2)+
#   scale_y_log10()+
#   scale_x_log10()

## Figure.3
LL_Df_S_3 <- NULL
for(Nu in Nu_vector){
  for(ED in 0:1){
    DFt <- EvDeDf_3[(EvDeDf_3$ED==ED)&(EvDeDf_3$Nu==Nu),]
    LL_Df_S_3 <- rbind(LL_Df_S_3, data.frame(Nu=Nu, LL_m=mean(DFt$LL), LL_sd=sd(log10(DFt$LL)), ED=ED_vector[(ED+1)]))
  }
}

for(ED in ED_vector){
  lm_S_3 <- lm_stat(log10(LL_Df_S_3$Nu[LL_Df_S_3$ED==ED]), log10(LL_Df_S_3$LL_m[LL_Df_S_3$ED==ED]))
  CBLP_YB_Stat <- rbind(CBLP_YB_Stat, data.frame(Figure=3, X="log10(Nu)", Y="log10(LL)", 
                                                 Group=ED, Intercept=lm_S_3$Intercept, Ymx=lm_S_3$Ymx, 
                                                 Slope=lm_S_3$Slope, P=lm_S_3$P, R_sqaure=lm_S_3$R_square))
}

for(ED in ED_vector){
  SMA_S_3 <- SMA_stat(log10(LL_Df_S_3$Nu[LL_Df_S_3$ED==ED]), log10(LL_Df_S_3$LL_m[LL_Df_S_3$ED==ED]))
  CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=3, X="log10(Nu)", Y="log10(LL)", 
                                                 Group=ED, Intercept=SMA_S_3$Intercept, Ymx=SMA_S_3$Ymx, 
                                                 Slope=SMA_S_3$Slope, Slope_lower=SMA_S_3$Slope_lower, Slope_upper=SMA_S_3$Slope_upper,
                                                 P=SMA_S_3$P, R_sqaure=SMA_S_3$R_square))
}

gsP3 <- ggplot(LL_Df_S_3[1:18,], aes(x=Nu, y=LL_m, group=ED, color=ED, shape=ED))+
  geom_point(size=3.5, alpha=0.5)+
  geom_smooth(aes(x=Nu, y=LL_m),
              method = "lm", se= FALSE, size = 1, lty=2) +
  # geom_bar(stat="identity", color="black", 
  #          position=position_dodge()) +
  # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
  #               position=position_dodge(.9)) +
  geom_pointrange(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd))+
  xlab("Nutrient availability")+
  ylab("Optimal leaf longevity (year)")+
  scale_color_manual(name="ED", values=c("blue", "red"))+
  # scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
  # ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
  scale_y_log10()+
  scale_x_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=10, face="bold"),
        axis.text.y=element_text(size=10, face="bold"),
        axis.title.x=element_text(size=18,face="bold"),
        axis.title.y=element_text(size=18,face="bold"),
        legend.text = element_text(colour = "black", size = 12, face = "bold"),
        legend.title = element_text(colour = "black", size = 15, face = "bold"))

## Figure.4
LL_Df_S_4 <- EvDeDf_3%>%
  select(Nu, LL, LMA)%>%
  mutate(Nug=Nu)
LL_Df_S_4$Nug[(LL_Df_S_4$Nu)%in%Nu_vector[1:3]] <- "Nu=0.25~1"
LL_Df_S_4$Nug[(LL_Df_S_4$Nu)%in%Nu_vector[4:6]] <- "Nu=2~8"
LL_Df_S_4$Nug[LL_Df_S_4$Nu%in%Nu_vector[7:9]] <- "Nu=16~64"
# LL_Df_2$fg[LL_Df_2$f%in%f_vector[10:12]] <- "f=(10~12)/12"
LL_Df_S_4$Nug <- factor(LL_Df_S_4$Nug, levels=c("Nu=0.25~1", "Nu=2~8", "Nu=16~64"))

LL_Df_S_4 <- LL_Df_S_4%>%
  mutate(log_LL=log10(LL), log_LMA=log10(LMA), LLg=LL)

for(i in 1:20){
  LL_Df_S_4$LLg[(LL_Df_S_4$log_LMA >= Bins_ME_2[i])&(LL_Df_S_4$log_LMA <= Bins_ME_2[i+1])] <- 10^((Bins_ME_2[i]+Bins_ME_2[i+1])/2)
}

ln_4 <- length(unique(LL_Df_S_4$LLg))

LL_Df_ME_4 <- data.frame(Mu_LL=rep(NA, 3*ln_4), SD_logLL=rep(NA, 3*ln_4), 
                         Nug=rep(unique(LL_Df_S_4$Nug), each=ln_4), 
                         LLg=rep(unique(LL_Df_S_4$LLg), 3))

LL_Df_ME_4_N9 <- data.frame(Mu_LL=rep(NA, 9*ln_4), SD_logLL=rep(NA, 9*ln_4), 
                         Nu=rep(unique(LL_Df_S_4$Nu), each=ln_4), 
                         LLg=rep(unique(LL_Df_S_4$LLg), 9))

for(i in 1:(3*ln_4)){
  Nug=LL_Df_ME_4$Nug[i]
  LLg=LL_Df_ME_4$LLg[i]
  LL_Df_ME_4$Mu_LL[i] = mean(LL_Df_S_4$LL[(LL_Df_S_4$Nug==Nug)&(LL_Df_S_4$LLg==LLg)])
  LL_Df_ME_4$SD_logLL[i] = sd(LL_Df_S_4$log_LL[(LL_Df_S_4$Nug==Nug)&(LL_Df_S_4$LLg==LLg)])
}

for(i in 1:(9*ln_4)){
  Nu=LL_Df_ME_4_N9$Nu[i]
  LLg=LL_Df_ME_4_N9$LLg[i]
  LL_Df_ME_4_N9$Mu_LL[i] = mean(LL_Df_S_4$LL[(LL_Df_S_4$Nu==Nu)&(LL_Df_S_4$LLg==LLg)])
  LL_Df_ME_4_N9$SD_logLL[i] = sd(LL_Df_S_4$log_LL[(LL_Df_S_4$Nu==Nu)&(LL_Df_S_4$LLg==LLg)])
}

gmeP4 <-  ggplot(LL_Df_ME_4, aes(x=LLg, y=Mu_LL, group=Nug, color=Nug, shape=Nug))+
  geom_point(size=5, alpha=0.7)+
  geom_smooth(aes(x=LLg, y=Mu_LL),
              method = "lm", se= FALSE, size = 1, lty=2) +
  # geom_bar(stat="identity", color="black", 
  #          position=position_dodge()) +
  # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
  #               position=position_dodge(.9)) +
  geom_pointrange(aes(ymin=Mu_LL-SD_logLL, ymax=Mu_LL+SD_logLL))+
  xlab("LMA (g m-2)")+
  ylab("Optimal leaf longevity (year)")+
  scale_color_manual(name="Nug", values=c("purple", "blue", "red"))+
  # scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
  # ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
  scale_x_log10()+
  scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

gmeP4_N9 <-  ggplot(LL_Df_ME_4_N9, aes(x=LLg, y=Mu_LL, group=as.factor(Nu), color=as.factor(Nu)))+
  geom_point(size=5, alpha=0.7)+
  geom_smooth(aes(x=LLg, y=Mu_LL),
              method = "lm", se= FALSE, size = 1, lty=2) +
  # geom_bar(stat="identity", color="black", 
  #          position=position_dodge()) +
  # geom_errorbar(aes(ymin=LL_m-LL_sd, ymax=LL_m+LL_sd), width=.2,
  #               position=position_dodge(.9)) +
  geom_pointrange(aes(ymin=Mu_LL-SD_logLL, ymax=Mu_LL+SD_logLL))+
  xlab("LMA (g m-2)")+
  ylab("Optimal leaf longevity (year)")+
  scale_color_manual(name="Nu", values=rainbow(9))+
  # scale_x_continuous(breaks=seq(0.2, 1, by=0.2))+
  # ggtitle(paste("Nu = ", Nu_vector[i], sep=""))+
  scale_x_log10()+
  scale_y_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

for(Nug in unique(LL_Df_S_4$Nug)){
  lm_S_4 <- lm_stat(log10(LL_Df_S_4$LMA[LL_Df_S_4$Nug==Nug]), log10(LL_Df_S_4$LL[LL_Df_S_4$Nug==Nug]))
  CBLP_YB_Stat <- rbind(CBLP_YB_Stat, data.frame(Figure=4, X="log10(LMA)", Y="log10(LL)", 
                                                 Group=Nug, Intercept=lm_S_4$Intercept, Ymx=lm_S_4$Ymx, 
                                                 Slope=lm_S_4$Slope, P=lm_S_4$P, R_sqaure=lm_S_4$R_square))
}

for(Nug in unique(LL_Df_S_4$Nug)){
  SMA_S_4 <- SMA_stat(log10(LL_Df_S_4$LMA[LL_Df_S_4$Nug==Nug]), log10(LL_Df_S_4$LL[LL_Df_S_4$Nug==Nug]))
  CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=4, X="log10(LMA)", Y="log10(LL)", 
                                                 Group=Nug, Intercept=SMA_S_4$Intercept, Ymx=SMA_S_4$Ymx, 
                                                 Slope=SMA_S_4$Slope, Slope_lower=SMA_S_4$Slope_lower, Slope_upper=SMA_S_4$Slope_upper,
                                                 P=SMA_S_4$P, R_sqaure=SMA_S_4$R_square))
  
  SMA_S_4.1 <- SMA_stat(log10(LL_Df_ME_4$LLg[LL_Df_ME_4$Nug==Nug]), log10(LL_Df_ME_4$Mu_LL[LL_Df_ME_4$Nug==Nug]))
  CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=4.1, X="log10(LMA)", Y="log10(LL)", 
                                                         Group=Nug, Intercept=SMA_S_4.1$Intercept, Ymx=SMA_S_4.1$Ymx, 
                                                         Slope=SMA_S_4.1$Slope, Slope_lower=SMA_S_4.1$Slope_lower, Slope_upper=SMA_S_4.1$Slope_upper,
                                                         P=SMA_S_4.1$P, R_sqaure=SMA_S_4.1$R_square))
}

gsP4 <- ggplot(LL_Df_S_4, aes(x=LMA, y=LL, group=Nug, color=Nug, shape=Nug))+
  geom_point(size=2, alpha=0.5)+
  geom_smooth(aes(x=LMA, y=LL),
              method = "lm", se= FALSE, size = 1, lty=2) +
  xlab("LMA (g m-2)")+
  ylab("Optimal leaf longevity (year)")+
  scale_color_manual(name="Nug", values=c("purple", "blue", "red"))+
  # ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
  scale_y_log10()+
  scale_x_log10()+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

## Figure.7
LL_Df_S_7 <- NULL
LL_Df_S_7.1 <- NULL
for(Nu in Nu_vector){
  DFt2 <- EvDeDf_3[(EvDeDf_3$Nu==Nu),]
  LL_Df_S_7 <- rbind(LL_Df_S_7, data.frame(f=f, Nu=Nu, EDP=(sum(DFt2$ED)/nrow(DFt2))))
}

for(f in f_vector){
  for(Nu in Nu_vector){
    DFt2 <- EvDeDf_3[(EvDeDf_3$Nu==Nu)&(EvDeDf_3$f==f),]
    LL_Df_S_7.1 <- rbind(LL_Df_S_7.1, data.frame(f=f*12, Nu=Nu, EDP=(sum(DFt2$ED)/nrow(DFt2))))
  }
}

lm_S_7 <- lm_stat(log10(LL_Df_S_7$Nu), LL_Df_S_7$EDP)
CBLP_YB_Stat <- rbind(CBLP_YB_Stat, data.frame(Figure=7, X="log10(Nu)", Y="EveProb", 
                                               Group=NA, Intercept=lm_S_7$Intercept, Ymx=lm_S_7$Ymx, 
                                               Slope=lm_S_7$Slope, P=lm_S_7$P, R_sqaure=lm_S_7$R_square))

SMA_S_7 <- SMA_stat(log10(LL_Df_S_7$Nu), LL_Df_S_7$EDP)
CBLP_YB_SMA_Stat <- rbind(CBLP_YB_SMA_Stat, data.frame(Figure=7, X="log10(Nu)", Y="EveProb", 
                                               Group=NA, Intercept=SMA_S_7$Intercept, Ymx=SMA_S_7$Ymx, 
                                               Slope=SMA_S_7$Slope, Slope_lower=SMA_S_7$Slope_lower, Slope_upper=SMA_S_7$Slope_upper,
                                               P=SMA_S_7$P, R_sqaure=SMA_S_7$R_square))

gsP7 <- ggplot(LL_Df_S_7, aes(x=Nu, y=EDP))+
  geom_bar(stat="identity")+
  geom_smooth(aes(x=Nu, y=EDP),
              method = "lm", se= FALSE, color = "blue", size = 1, lty=2) +
  xlab("Nutrient availability")+
  ylab("Evergreen proportion")+
  scale_x_log10()+
  # scale_x_continuous(breaks=Nu_vector)+
  geom_hline(yintercept=1, colour="red", lty=2, size=1.5)+
  # ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

gsP7.1 <- ggplot(LL_Df_S_7.1[LL_Df_S_7.1$Nu==64,], aes(x=f/12, y=EDP))+
  geom_bar(stat="identity")+
  # geom_smooth(aes(x=f, y=EDP),
  #             method = "lm", se= FALSE, color = "blue", size = 1, lty=2) +
  xlab("Favorable season length")+
  ylab("Evergreen proportion")+
  # scale_x_log10()+
  # scale_x_continuous(breaks=Nu_vector)+
  geom_hline(yintercept=1, colour="red", lty=2, size=1.5)+
  # ggtitle(paste("f = ", f_vector[i]*12," mon", sep=""))+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))

gsP7.2 <- ggplot(LL_Df_S_7.1, aes(x=Nu, y=EDP, color=as.factor(f), group=as.factor(f)))+
  geom_point(alpha=0.7, size=3)+
  geom_line(alpha=0.7, lty=3, size=1.5)+
  xlab("Nutrient availability")+
  ylab("Evergreen proportion")+
  scale_x_log10()+
  # scale_x_continuous(breaks=Nu_vector)+
  geom_hline(yintercept=1, colour="black", lty=2, size=1.5)+
  scale_color_manual(name="f", values=rainbow(12))+
  # ggtitle(paste("f = ", f_vector[i]))+
  theme_classic()+
  theme(axis.text.x=element_text(size=8, face="bold"),
        axis.text.y=element_text(size=8, face="bold"),
        axis.title.x=element_text(size=15,face="bold"),
        axis.title.y=element_text(size=15,face="bold"),
        legend.text = element_text(colour = "black", size = 10, face = "bold"),
        legend.title = element_text(colour = "black", size = 12, face = "bold"))




# plot(GLOPNET$PPT_PET, GLOPNET$MAP)