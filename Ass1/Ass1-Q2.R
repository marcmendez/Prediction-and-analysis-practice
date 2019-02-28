Norm_v1=rnorm(200, mean=5, sd=1)
Norm_v2=rnorm(200, mean=10, sd=1)
Norm_v3=rnorm(200, mean=5, sd=1)

Norm_v1n=data.frame(x1=Norm_v1, x2="v1")
Norm_v2n=data.frame(x1=Norm_v2, x2="v2")
Norm_v3n=data.frame(x1=Norm_v3, x2="v3")
data=mergeRows(Norm_v1n, Norm_v2n, common.only=FALSE)
data=mergeRows(as.data.frame(data), Norm_v3n, common.only=FALSE)

AnovaModel.1 <- aov(x1 ~ x2, data=data)
summary(AnovaModel.1)
Boxplot(x1~x2, data=data, id.method="y")
#Samples are Independents
dwtest(AnovaModel.1, alternative ="two.sided")
#Samples are normals
shapiro.test(residuals(AnovaModel.1))
shapiro.test(Norm_v1)
#Samples have equal variances
lmtest::bptest(AnovaModel.1)
#Which means are different 
TukeyHSD(AnovaModel.1)


data(decathlon, package = "FactoMineR") #Load decathlon data
AnovaModel.2 <- aov(Long.jump ~ Competition, data = decathlon)  # ANOVA Model for Long Jump on different competitions
summary(AnovaModel.2)
AnovaModel.3 <- aov(High.jump ~ Competition, data = decathlon)
summary(AnovaModel.3)
AnovaModel.4 <- aov(Javeline ~ Competition, data = decathlon)
summary(AnovaModel.4)
