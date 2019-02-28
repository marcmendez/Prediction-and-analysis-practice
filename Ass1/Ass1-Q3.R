data(decathlon, package = "FactoMineR") #Load decathlon data
new <- data.frame(decathlon)

# To see which one has more relation to 1500m style
cor(decathlon[,c("400m", "1500m")], use = "complete")
cor(decathlon[,c("100m", "1500m")], use = "complete")
cor(decathlon[,c("Long.jump", "1500m")], use = "complete")
cor(decathlon[,c("Shot.put", "1500m")], use = "complete")
cor(decathlon[,c("110m.hurdle", "1500m")], use = "complete")
cor(decathlon[,c("High.jump", "1500m")], use = "complete")
cor(decathlon[,c("Discus", "1500m")], use = "complete")
cor(decathlon[,c("Pole.vault", "1500m")], use = "complete")
cor(decathlon[,c("Javeline", "1500m")], use = "complete")
cor(decathlon[,c("Rank", "1500m")], use = "complete")
cor(decathlon[,c("Points", "1500m")], use = "complete")

LinearModel.1 <- lm(X400m ~ X1500m, data = new) # Model 1
summary(LinearModel.1)
plot(LinearModel.1)

#Assumptions 1
dwtest(LinearModel.1, alternative = "two.sided") #Independece of the samples
shapiro.test(residuals(LinearModel.1)) #Normal samples
lmtest::bptest(LinearModel.1) #Homogeneity of variance


#Box-Cox Transformation
X400mModz<- caret::BoxCoxTrans(decathlon$`400m`)
decathlon <- cbind(decathlon, X400m_new = predict(X400mModz, decathlon$`400m`))


#Model 2 with the Box -Cox transformation done
new2 <- data.frame(decathlon)
LinearModel.2 <- lm(X400m_new ~ X1500m, data = new2)
summary(LinearModel.2)

#Assumptions 2
dwtest(LinearModel.2, alternative = "two.sided") #Independece of the samples
shapiro.test(residuals(LinearModel.2)) #Normal samples
lmtest::bptest(LinearModel.2) #Homogeneity of variance
scatterplot(`400m` ~ `1500m`,regLine=lm,data = decathlon)



new1<- data.frame(X400m = 50)
predict(LinearModel.1, newdata = new1, intercal = "prediction")
predict(LinearModel.1, newdata = new1, intercal = "confidence")
