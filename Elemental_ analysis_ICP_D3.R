#*************************************************************
# Soil elemental changes during human decomposition
# L. Stacy Taylor
#*************************************************************
library(dplyr)

ICP_D3 <-read.csv(file="ICP_D3.csv")

#***************************
# Kruskal-Wallace tests 
#***************************

kruskal.test(ICP_D3$pH, ICP_D3$Treatment)
kruskal.test(ICP_D3$EC, ICP_D3$Treatment)
kruskal.test(ICP_D3$Na, ICP_D3$Treatment)
kruskal.test(ICP_D3$K, ICP_D3$Treatment)
kruskal.test(ICP_D3$Mg, ICP_D3$Treatment)
kruskal.test(ICP_D3$Ca, ICP_D3$Treatment)
kruskal.test(ICP_D3$Al, ICP_D3$Treatment)
kruskal.test(ICP_D3$Cu, ICP_D3$Treatment)
kruskal.test(ICP_D3$Fe, ICP_D3$Treatment)
kruskal.test(ICP_D3$Mn, ICP_D3$Treatment)
kruskal.test(ICP_D3$Zn, ICP_D3$Treatment)
kruskal.test(ICP_D3$Co, ICP_D3$Treatment)
kruskal.test(ICP_D3$S180, ICP_D3$Treatment)
kruskal.test(ICP_D3$P, ICP_D3$Treatment)
kruskal.test(ICP_D3$B, ICP_D3$Treatment)
kruskal.test(ICP_D3$Se, ICP_D3$Treatment)

#********************
# boxplots
#********************
str(ICP_D3)

pdf("pH_box.pdf", width =6,height=4, useDingbats = FALSE)
#png("pH_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(pH~Sorting1, data=ICP_D3, ylim=c(5.5,8.2), xlab="Day", ylab="pH", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$pH, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("EC_box.pdf", width =6,height=4, useDingbats = FALSE)
#png("pH_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(EC~Sorting1, data=ICP_D3, ylim=c(0,700), xlab="Day", ylab="EC", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$EC, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("Na_box.pdf", width =6,height=4, useDingbats = FALSE)
#png("Na_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(Na~Sorting1, data=ICP_D3, ylim=c(0, 425),xlab="Day", ylab="Na", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$Na, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("K_box.pdf", width =6,height=4, useDingbats = FALSE)
#png("K_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(K~Sorting1, data=ICP_D3, ylim=c(0,225), xlab="Day", ylab="K", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$K, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("Ca_box.pdf", width =6,height=4, useDingbats = FALSE)
#png("Ca_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(Ca~Sorting1, data=ICP_D3, ylim=c(0,600),xlab="Day", ylab="Ca", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$Ca, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("Mg_box.pdf", width =6,height=4, useDingbats = FALSE)
#png("Mg_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(Mg~Sorting1, data=ICP_D3, ylim=c(0,45), xlab="Day", ylab="Mg", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$Mg, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("S180_box.pdf", width =6,height=4, useDingbats = FALSE)
#png("S180_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(S180~Sorting1, data=ICP_D3, ylim=c(0,65), xlab="Day", ylab="S180", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$S180, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("P_box.pdf", width =6,height=4, useDingbats = FALSE)
#png("P_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(P~Sorting1, data=ICP_D3, ylim=c(0,43), xlab="Day", ylab="P", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$P, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("Mn.pdf", width =6,height=4, useDingbats = FALSE)#note: trying to sort a control in a factor string isn't possible, so sample_days_a lists the control as 0, and the b_4.9 (day 0) sample as 1
#png("Mn_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(Mn~Sorting1, data=ICP_D3, ylim=c(0,120),xlab="Day", ylab="Mn", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$Mn, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("Se.pdf", width =6,height=4, useDingbats = FALSE)#note: trying to sort a control in a factor string isn't possible, so sample_days_a lists the control as 0, and the b_4.9 (day 0) sample as 1
#png("Mn_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(Se~Sorting1, data=ICP_D3, ylim=c(0,0.1),xlab="Day", ylab="Se", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$Se, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("B_box.pdf", width =6,height=4, useDingbats = FALSE)#note: trying to sort a control in a factor string isn't possible, so sample_days_a lists the control as 0, and the b_4.9 (day 0) sample as 1
#png("Mn_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(B~Sorting1, data=ICP_D3, ylim=c(0,2.25),xlab="Day", ylab="B", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$B, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("Fe_box.pdf", width =6,height=4, useDingbats = FALSE)#note: trying to sort a control in a factor string isn't possible, so sample_days_a lists the control as 0, and the b_4.9 (day 0) sample as 1
#png("Fe_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(Fe~Sorting1, data=ICP_D3, ylim=c(0,50), xlab="Day", ylab="Fe", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$Fe, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("Cu_box.pdf", width =6,height=4, useDingbats = FALSE)#note: trying to sort a control in a factor string isn't possible, so sample_days_a lists the control as 0, and the b_4.9 (day 0) sample as 1
#png("Fe_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(Cu~Sorting1, data=ICP_D3, ylim=c(0,0.25), xlab="Day", ylab="Cu", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$Cu, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("Zn_box.pdf", width =6,height=4, useDingbats = FALSE)#note: trying to sort a control in a factor string isn't possible, so sample_days_a lists the control as 0, and the b_4.9 (day 0) sample as 1
#png("Fe_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(Zn~Sorting1, data=ICP_D3, ylim=c(0,1.25), xlab="Day", ylab="Zn", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$Zn, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("Co_box.pdf", width =6,height=4, useDingbats = FALSE)#note: trying to sort a control in a factor string isn't possible, so sample_days_a lists the control as 0, and the b_4.9 (day 0) sample as 1
#png("Fe_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(Co~Sorting1, data=ICP_D3, ylim=c(0,0.9), xlab="Day", ylab="Co", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$Co, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

pdf("Al_box.pdf", width =6,height=4, useDingbats = FALSE)#note: trying to sort a control in a factor string isn't possible, so sample_days_a lists the control as 0, and the b_4.9 (day 0) sample as 1
#png("Fe_box_v2.png", width =7,height=4, units = 'in', res=300)
sample_days_a <- as.factor(c(0,1,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122))
boxplot(Al~Sorting1, data=ICP_D3, ylim=c(0,80), xlab="Day", ylab="Al", las=2, cex.axis=0.9)
element_means <-as.vector(tapply(ICP_D3$Al, INDEX=ICP_D3$Sorting1, FUN=mean, simplify = TRUE), mode="numeric")
points (sample_days_a,element_means,cex=1, pch=5, bg="red", col="black")
dev.off()

#********************************
# T-tests
#********************************

con3 <-filter(ICP_D3,Treatment == "D3_con")
grave3 <-filter(ICP_D3,Treatment == "D3_grave")

T_day<- filter(ICP_D3, Study_day==35)
T_con <- filter(T_day, Treatment=="D3_con") 
T_treat <-filter(T_day, Treatment=="D3_grave") 

t.test(T_con$pH,T_treat$pH, var.equal = FALSE) 
t.test(T_con$EC,T_treat$EC, var.equal = FALSE) 
t.test(T_con$Ca,T_treat$Ca, var.equal = FALSE) 
t.test(T_con$P,T_treat$P, var.equal = FALSE) 
t.test(T_con$K,T_treat$K, var.equal = FALSE) 
t.test(T_con$S180,T_treat$S180, var.equal = FALSE) 
t.test(T_con$Na,T_treat$Na, var.equal = FALSE) 
t.test(T_con$Mg,T_treat$Mg, var.equal = FALSE) 
t.test(T_con$Fe,T_treat$Fe, var.equal = FALSE) 
t.test(T_con$Cu,T_treat$Cu, var.equal = FALSE) 
t.test(T_con$Mn,T_treat$Mn, var.equal = FALSE) 
t.test(T_con$Zn,T_treat$Zn, var.equal = FALSE) 
t.test(T_con$Se,T_treat$Se, var.equal = FALSE) 
t.test(T_con$Co,T_treat$Co, var.equal = FALSE) 
t.test(T_con$B,T_treat$B, var.equal = FALSE) 
t.test(T_con$Al,T_treat$Al, var.equal = FALSE) 

#*********************************************
# PCA
#*********************************************
library(dplyr)
library(vegan)
install.packages("tibble")
library(tibble)

ICP_D3 <-read.csv(file="ICP_D3.csv")

vars=c("pH","EC","Na","K","Mg","Ca","Al","Cu","Fe","Mn","Zn","Co","S180","P","B","Se")
ICP_D3a=ICP_D3[,c("SampleID","Date","Study_day","ADH","ADD","ARF_sector","Sorting1","Sorting","Treatment",vars)]

# run the PCA
ICP.pca=rda(ICP_D3a[,vars],scale=T)
summary(ICP.pca)

#**********************************************
# extract the eigenvalues and scores manually and run the straightforward vegan plot
ICP_eigen <- ICP.pca$CA$eig #extracts eigenvalues
variance <- ICP_eigen*100/sum(ICP_eigen) #calculates the variance by axis
cumulative_variance <-cumsum(variance)
# combine everything into one dataframe
eigen.pca <- data.frame(ICP_eigen = ICP_eigen, variance = variance, cum_var = cumulative_variance)
# calculate the scores to graph (and extract) seperately if preferred
ICP_scores = scores(ICP.pca, display=c("sites","species"),choices=c(1,2,3))

# biplot: text for species (columns) and points for sites (rows) - this is the version from Vegan
# this the version from vegan than plots scores as points and loadings as arrows
# choices extracts the components you want (PCa nd PC2 in this case)
biplot(ICP.pca, choices = c(1,2), scaling = "species", display = c("sites","species"),type = c("text","points"), col = c(1,2))

# now join sites to the original dataframe
ICP_D3a=cbind(ICP_D3a,ICP_scores$sites)

library(ggplot2)
library(RColorBrewer)

theme_set(theme_classic())
nb.cols <- 28
ordcolors <- colorRampPalette(brewer.pal(8, "RdYlBu"))(nb.cols)

pdf("SPOTelemental_PCA_V2.pdf", width =6,height=5, useDingbats = FALSE)
ggplot(ICP_D3a) + 
  geom_point(aes(x = PC1, y = PC2, col = Sorting1),size = 2.5, stroke = 1.5) +
  scale_color_manual(values=ordcolors) +
  scale_fill_manual(values=ordcolors) +
  scale_shape_manual(values = c(21,22)) +
  geom_text(data = vscores, aes(x = 2.5*PC1, y = 2.5*PC2, label = rownames(vscores)), col = 'black') +
  geom_segment(data = vscores, aes(x = 0, y = 0, xend = 2*PC1, yend = 2*PC2), arrow=arrow(length=unit(0.2,"cm")),
               alpha = 0.75, color = 'black')+
  theme(strip.text.y = element_text(angle = 0))
dev.off()

#******************************
# charge line graphs
#******************************

library(dplyr)

ICP_D3 <-read.csv(file="ICP_D3_charge.csv")
con3 <-filter(ICP_D3,Treatment == "D3_con")
grave3 <-filter(ICP_D3,Treatment == "D3_grave")
D3_date <-c(0,3,5,7,10,14,17,19,21,28,33,35,38,40,42,45,47,49,54,56,61,66,75,89,103,117,122)

pdf("D3_ICP_Na_charge.pdf", width =5,height=4)
plot(1, type="n", xlim=c(0,122),ylim=c(0,20), ylab="Na umol charge gdw-1 soil",las=1, cex.axis=1, xlab = "Study Day", main= "", col="white")
# points and lines for the controls
meanscon <-as.vector(tapply(con3$Na..umolcharge.gdw.soil., INDEX=con3$Study_day, FUN=mean, simplify = TRUE), mode="numeric")
lines (D3_date, meanscon,lty=1, lwd=2, col="black")
errorbarscon <- as.vector(tapply(con3$Na..umolcharge.gdw.soil., INDEX=con3$Study_day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (D3_date, meanscon, D3_date, meanscon + errorbarscon, length=0.03, angle=90, col="black")
arrows (D3_date, meanscon, D3_date, meanscon - errorbarscon, length=0.03, angle=90,col="black")
# points and lines for the samples
meansgrave <-as.vector(tapply(grave3$Na..umolcharge.gdw.soil., INDEX=grave3$Study_day, FUN=mean, simplify = TRUE), mode="numeric")
lines (D3_date, meansgrave,lty=1, lwd=4, col="red2")
errorbarsgr <- as.vector(tapply(grave3$Na..umolcharge.gdw.soil., INDEX=grave3$Study_day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (D3_date, meansgrave, D3_date, meansgrave + errorbarsgr, length=0.03, angle=90, col="red2")
arrows (D3_date, meansgrave, D3_date, meansgrave - errorbarsgr, length=0.03, angle=90,col="red2")
legend("topleft", legend=c("Na"),col=c("black"), cex=c(2),bty="n")
dev.off()

pdf("D3_ICP_K_charge.pdf", width =5,height=4)
plot(1, type="n", xlim=c(0,122),ylim=c(0,6), ylab="K umol charge gdw-1 soil",las=1, cex.axis=1, xlab = "Study Day", main= "", col="white")
# points and lines for the controls
meanscon <-as.vector(tapply(con3$K..umolcharge.gdw.soil., INDEX=con3$Study_day, FUN=mean, simplify = TRUE), mode="numeric")
lines (D3_date, meanscon,lty=1, lwd=2, col="black")
errorbarscon <- as.vector(tapply(con3$K..umolcharge.gdw.soil., INDEX=con3$Study_day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (D3_date, meanscon, D3_date, meanscon + errorbarscon, length=0.03, angle=90, col="black")
arrows (D3_date, meanscon, D3_date, meanscon - errorbarscon, length=0.03, angle=90,col="black")
# points and lines for the samples
meansgrave <-as.vector(tapply(grave3$K..umolcharge.gdw.soil., INDEX=grave3$Study_day, FUN=mean, simplify = TRUE), mode="numeric")
lines (D3_date, meansgrave,lty=1, lwd=4, col="red2")
errorbarsgr <- as.vector(tapply(grave3$K..umolcharge.gdw.soil., INDEX=grave3$Study_day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (D3_date, meansgrave, D3_date, meansgrave + errorbarsgr, length=0.03, angle=90, col="red2")
arrows (D3_date, meansgrave, D3_date, meansgrave - errorbarsgr, length=0.03, angle=90,col="red2")
legend("topleft", legend=c("K"),col=c("black"), cex=c(2),bty="n")
dev.off()

pdf("D3_ICP_Mg_charge.pdf", width =5,height=4)
plot(1, type="n", xlim=c(0,122),ylim=c(0,6), ylab="Mg umol charge gdw-1 soil",las=1, cex.axis=1, xlab = "Study Day", main= "", col="white")
# points and lines for the controls
meanscon <-as.vector(tapply(con3$Mg..umolcharge.gdw.soil., INDEX=con3$Study_day, FUN=mean, simplify = TRUE), mode="numeric")
lines (D3_date, meanscon,lty=1, lwd=2, col="black")
errorbarscon <- as.vector(tapply(con3$Mg..umolcharge.gdw.soil., INDEX=con3$Study_day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (D3_date, meanscon, D3_date, meanscon + errorbarscon, length=0.03, angle=90, col="black")
arrows (D3_date, meanscon, D3_date, meanscon - errorbarscon, length=0.03, angle=90,col="black")
# points and lines for the samples
meansgrave <-as.vector(tapply(grave3$Mg..umolcharge.gdw.soil., INDEX=grave3$Study_day, FUN=mean, simplify = TRUE), mode="numeric")
lines (D3_date, meansgrave,lty=1, lwd=4, col="red2")
errorbarsgr <- as.vector(tapply(grave3$Mg..umolcharge.gdw.soil., INDEX=grave3$Study_day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (D3_date, meansgrave, D3_date, meansgrave + errorbarsgr, length=0.03, angle=90, col="red2")
arrows (D3_date, meansgrave, D3_date, meansgrave - errorbarsgr, length=0.03, angle=90,col="red2")
legend("topleft", legend=c("Mg"),col=c("black"), cex=c(2),bty="n")
dev.off()

pdf("D3_ICP_Ca_charge.pdf", width =5,height=4)
plot(1, type="n", xlim=c(0,122),ylim=c(0,30), ylab="Ca umol charge gdw-1 soil",las=1, cex.axis=1, xlab = "Study Day", main= "", col="white")
# points and lines for the controls
meanscon <-as.vector(tapply(con3$Ca..umolcharge.gdw.soil., INDEX=con3$Study_day, FUN=mean, simplify = TRUE), mode="numeric")
lines (D3_date, meanscon,lty=1, lwd=2, col="black")
errorbarscon <- as.vector(tapply(con3$Ca..umolcharge.gdw.soil., INDEX=con3$Study_day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (D3_date, meanscon, D3_date, meanscon + errorbarscon, length=0.03, angle=90, col="black")
arrows (D3_date, meanscon, D3_date, meanscon - errorbarscon, length=0.03, angle=90,col="black")
# points and lines for the samples
meansgrave <-as.vector(tapply(grave3$Ca..umolcharge.gdw.soil., INDEX=grave3$Study_day, FUN=mean, simplify = TRUE), mode="numeric")
lines (D3_date, meansgrave,lty=1, lwd=4, col="red2")
errorbarsgr <- as.vector(tapply(grave3$Ca..umolcharge.gdw.soil., INDEX=grave3$Study_day, FUN=sd, simplify = TRUE), mode="numeric")
arrows (D3_date, meansgrave, D3_date, meansgrave + errorbarsgr, length=0.03, angle=90, col="red2")
arrows (D3_date, meansgrave, D3_date, meansgrave - errorbarsgr, length=0.03, angle=90,col="red2")
legend("topleft", legend=c("Ca"),col=c("black"), cex=c(2),bty="n")
dev.off()

#********************************************************
# correlation matrix
#********************************************************

install.packages("GGally")
library(GGally)
ICP_corr <-read.csv(file="ICP_D3_corr_noCon.csv")
pdf("ICP_corr_noControls_ggversion_12in_adj.pdf", width =12,height=12, useDingbats = FALSE)
ggpairs(ICP_corr[, c("pH", "EC", "Ca", "P", "K", "S", "Na", "Mg", "Fe","Cu", "Mn", "Zn", "Se", "Co", "Al", "B")])
dev.off()

#***********************************************************************
# HEATMAPS
#***********************************************************************

library(RColorBrewer)
install.packages("pheatmap")
library(pheatmap)
# DO NOT attempt to use pdf language to save the plot--the program crashes. Instead export it to a 5x7 in portrait
# Want genera as rows and the first row NOT to show up as a named column since this dataframe has to used as a matrix

# wide version
heat_ICP <- read.csv(file="ICP_heatmap_wide.csv", row.names = 1)#, header=TRUE)
#heat_core <-subset(heat_core, select= -c(Trophic)) #removes the trophic row
heat_ICPa <-as.matrix(heat_ICP) #sets the data as a matrix
pheatmap(heat_ICPa,
         scale = "row",cluster_cols = FALSE, 
         clustering_distance_cols = "correlation",
         cellwidth = 14, cellheight = 12,
         cutree_rows = 6, treeheight_col = 60, 
         fontsize_row = 10, fontsize_col = 10, angle_col = c("45"))

#***********************************************************
# Mineralogy XRD
#***********************************************************
XRD <- read.csv(file="XRD.csv")

pdf("XRD_SP.pdf", width =9,height=5)
plot(1, type="n", xlim=c(0,40),ylim=c(0,1500), ylab="Intensity",las=1, cex.axis=1, xlab = "Degrees 2 theta", main= "", col="white")
# Ksat 25C
lines (XRD$X2.Theta,XRD$SP_K_room,lty=1, lwd=2, col="black")
# Ksat 300C
lines (XRD$X2.Theta,((XRD$SP_K_300)+100),lty=1, lwd=2, col="black")
# Ksat 550C
lines (XRD$X2.Theta,((XRD$SP_K_550)+200),lty=1, lwd=2, col="black")
# Mgsat
lines (XRD$X2.Theta,((XRD$SP_Mg)+300),lty=1, lwd=2, col="gray")
# Mgsat + glycol
lines (XRD$X2.Theta,((XRD$SP_Mg_gly)+400),lty=1, lwd=2, col="gray")
abline(v=6.3, lwd = 1.5, lty=1, col="black")#vermiculite open d=1.4, also same as mica first order
abline(v=7.2, lwd = 1.5, lty=2, col="black")#vermiculite and mica interstratification
abline(v=8.8, lwd = 1.5, lty=3, col="black")#vermiculite closed/mica d=1
abline(v=12.4, lwd = 1.5, lty=4, col="black")#kaolinite d=0.7
legend("topright", legend=c("Vermiculite open d=1.4", "Interstratification d=1.23", "Vermiculite closed/mica d=1", "Kaolinite d=0.7"),lty=c(1,2,3,4),lwd=c(1.5,1.5,1.5,1.5),col=c("black", "black", "black", "black"), cex=c(0.9, 0.9, 0.9, 0.9),bty="n")
dev.off()

pdf("XRD_WIN.pdf", width =9,height=5)
plot(1, type="n", xlim=c(0,40),ylim=c(0,1500), ylab="Intensity",las=1, cex.axis=1, xlab = "Degrees 2 theta", main= "", col="white")
# Ksat 25C
lines (XRD$X2.Theta,XRD$WIN_K_room,lty=1, lwd=2, col="black")
# Ksat 300C
lines (XRD$X2.Theta,((XRD$WIN_K_300)+100),lty=1, lwd=2, col="black")
# Ksat 550C
lines (XRD$X2.Theta,((XRD$WIN_K_550)+200),lty=1, lwd=2, col="black")
# Mgsat
lines (XRD$X2.Theta,((XRD$WIN_Mg)+300),lty=1, lwd=2, col="gray")
# Mgsat + glycol
lines (XRD$X2.Theta,((XRD$WIN_Mg_gly)+700),lty=1, lwd=2, col="gray")
abline(v=6.3, lwd = 1.5, lty=1, col="black")#vermiculite open d=1.4, also same as mica first order
abline(v=7.2, lwd = 1.5, lty=2, col="black")#vermiculite and mica interstratification
abline(v=8.8, lwd = 1.5, lty=3, col="black")#vermiculite closed/mica d=1
abline(v=12.4, lwd = 1.5, lty=4, col="black")#kaolinite d=0.7
legend("topright", legend=c("Vermiculite open d=1.4", "Interstratification d=1.23", "Vermiculite closed/mica d=1", "Kaolinite d=0.7"),lty=c(1,2,3,4),lwd=c(1.5,1.5,1.5,1.5),col=c("black", "black", "black", "black"), cex=c(0.9, 0.9, 0.9, 0.9),bty="n")
dev.off()

