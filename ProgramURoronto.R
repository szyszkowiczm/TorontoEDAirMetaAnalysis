# https://cmdlinetips.com/2019/02/how-to-make-grouped-boxplots-with-ggplot2/
#https://cmdlinetips.com/2019/02/how-to-make-grouped-boxplots-with-ggplot2/ 

library(tidyverse)
library(gapminder)
library(forcats)

  
   dataHB <- read.csv("DataEDToronto12.csv", header = TRUE, sep=";")
   attach(dataHB)

  head( dataHB )
  ICDstr = "J00-J99" #resp
  # ICDstr = "I00-I99"  #card
  # ICDstr = "H00-H59" #Eye
  # ICDstr = "G00-G99" #Nerv
  # ICDstr = "F00-F99" #Mental
  # ICDstr = "A00-B99" #Infection

dataJA <- dataHB[dataHB$ICD10== ICDstr, ]
     
# Distribuion by Lags - box plot 

  dataJA %>%
 #
  ggplot(aes(x=ICD10,y=SO2, fill=factor(LAG))) +
  geom_boxplot() + #geom_boxplot(outlier.colour = "hotpink") +
  labs(fill = "Lag") + 
  geom_point(position=position_jitterdodge(),alpha=0.3) +
  theme_bw(base_size = 16)


#######################################################################
#  library(MASS); library(minpack.lm)
# Use nls + plot

  dataHA<- dataHB[dataHB$ICD10== ICDstr,"SO2" ]
  dataHE <- dataHB[dataHB$ICD10== ICDstr,"SO" ]
  dataHL <-  dataHA -1.96*dataHE
  dataHU <-  dataHA +1.96*dataHE
  
  D=10; #dose
  
# Relative risks and 95% CI
  RR = exp(dataHA*D)
  TL = exp(dataHL*D)
  TU = exp(dataHU*D)
 # Slopes, Beta 
 # RR = (dataHA)
 # TL = (dataHL)
 # TU = (dataHU)


########################################################################
#  library(MASS); library(minpack.lm)
 
 RM=c(0:14); RD= RX=RI=R25=RX= R75=RM
 LM=c(0:14); LD= LX=LI=L25=LX= L75=LM
 UM=c(0:14); UD= UX=UI=U25=UX= U75=UM
 
# Creat the X-axis; Lag 0-14
x=seq(0, 14, by=1);

#Prepare the data frame by lags 0-14
i=0; N=14
for (k in 0:N) { i=i+1
ip=k*18+1; ik=ip+17
ci = c(ip,ik)
print(ci)
  rr = RR[ip:ik];   
  rl = TL[ip:ik];  
  ru = TU[ip:ik]
 # Find minimum and maximum values 
RM[i]= mean(rr)
RI[i]=min(rr)
RX[i]=max(rr)
m=quantile(rr); RD[i]=m[3]
R25[i] = m[2]; R75[i]=m[4]

LM[i]= mean(rl)
LI[i]= min(rl)
LX[i]= max(rl)
m=quantile(rl); LD[i]=m[3]
L25[i] = m[2]; L75[i]=m[4]

UM[i]= mean(ru)
UI[i]=min(ru)
UX[i]=max(ru)
m=quantile(ru); UD[i]=m[3]
U25[i] = m[2];  U75[i]=m[4]
}# # Define frame with X, RR, RRlower, and RRupper (for each lag)
######################################################################
#####################################################################
  Dat=cbind(x,RM,RD,RI,RX,R25,R75,LM,LD,LI,LX,L25,L75,UM,UD,UI,UX,U25,U75)
  DatF = data.frame(Dat)

################Define plot area (white) #############
plot(xa,TRR[,3:3], ylim=c(1.,1.1), col="white"); abline(h=1)
################


 fitM <- nlsLM(
  DatF$RD ~  A*DatF$x^3 + B * DatF$x^2 + C * DatF$x + D,
  start = list( A=2, B = 3, C = 4, D = 1),
  data = DatF)

# summary(fitM)
#

   lines(DatF$x,fitted(fitM), lwd=4,col="black")  #max
   points(DatF$x, DatF$RD)


fitM <- nlsLM(
  DatF$LD ~  A*DatF$x^3 + B * DatF$x^2 + C * DatF$x + D,
  start = list( A=2, B = 3, C = 4, D = 1),
  data = DatF)

# summary(fitM)
#

   lines(DatF$x,fitted(fitM), lwd=4,col="blue")  #max
  # points(DatF$x, DatF$LD)

  fitM <- nlsLM(
  DatF$UD ~  A*DatF$x^3 + B * DatF$x^2 + C * DatF$x + D,
  start = list( A=2, B = 3, C = 4, D = 1),
  data = DatF)

# summary(fitM)
#

   lines(DatF$x,fitted(fitM), lwd=4,col="red")  #max
   #points(DatF$x, DatF$UD)

#############################################################
##########################################################


