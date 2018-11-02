#Load packages
install.packages("ggplot2")
library(ggplot)
install.packages("ggrepel")
library(ggrepel)

#Import Data
capm <- read.table(file="Data for CAPM.csv",header=T,sep=",")

#Examine and Clean Data
str(capm)
dim(capm)

L <- ncol(capm)
I <- L-2

#Naming beta, alpha, t-value
Betas <- rep(0,10)
Alphas <- rep(0,10)
t_values <- rep(0,10)
Average <- rep(0,10)

#Excess Return of market and stock 1
Rm <- capm[,2] - capm[,1]
Ri <- capm[,3] - capm[,1]

#Regression
myResult <- lm(Ri~Rm)
mysummary <- summary(myResult)
mycoef <- coef(mysummary)

Betas[1] <- mycoef[2,1]
Alphas[1] <- mycoef[1,1]
t_values[1] <- mycoef[1,3]
Average[1] <- mean(Ri)

#Create a loop
for(i in 1:I){
  Ri <- capm[,2+i] - capm[,1]
  myResult <- lm(Ri~Rm)
  mysummary <- summary(myResult)
  mycoef <- coef(mysummary)
  Betas[i] <- mycoef[2,1]
  Alphas[i] <- mycoef[1,1]
  t_values[i] <- mycoef[1,3]
  Average[i] <- mean(Ri)
}

#Visualize
plot(Betas, Average)
x <- c(1:250)/100
points(x, mean(Rm)*x, col="red", type="l")
text(Betas,Average,label=companies2, cex=0.7)

#find out which company has high Beta or low Beta

companies <- colnames(capm)
companies2 <- companies[3:L]

data <- data.frame(companies2,Betas) #contain all companies & their betas

myOrder <- order(Betas, decreasing=T)

y <- data[myOrder, ]

Beta.high <- y[1:3, ]
Beta.low <- y[I:(I-2),]

#Visualizing
df <- data.frame(x=Betas, y=Average, z= companies2) # creating new data frame

ggplot(df, aes(x=x,y=y,label=z))+
  geom_point(colour="white",fill="chartreuse3",shape=21,alpha=0.9,size=4)+
  geom_line(aes(x=x,y=mean(Rm)*x),colour="red")+
  labs(title="Tesing of 10 Stocks CAPM in JP Market",
       subtitle="Market Return = Nikkei 225",
       caption="Source: Yahoo Finance",
       x="Beta",
       y="Return")+
  geom_text_repel(aes(label=z), box.padding=unit(0.45,"lines"))+
  theme_classic()
