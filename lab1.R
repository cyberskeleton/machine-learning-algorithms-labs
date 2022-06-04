data = read.csv("MedCPISmooth.csv", header=TRUE, sep=",")
par(mfrow=c(2,3))
bound <- floor((nrow(data)/5)*4)
data <- data[sample(nrow(data)), ] 
data.train <- data[1:bound, ]
data.test <- data[(bound+1):nrow(data), ]  

plot(data.train$PerMEDCPI, data.train$value, pch=21, cex=1, col="blue", bg="lightblue", xlab="predictor-PerMEDCPI", ylab="response-y")
abline(lm(data$value~data$PerMEDCPI))
plot(data.train$YEAR, data.train$value, pch=21, cex=1, col="blue", bg="lightblue", xlab="predictor-YEAR", ylab="response-y")
abline(lm(data.train$value~data.train$YEAR))
plot(data.train$MCPISM4, data.train$value, pch=21, cex=1, col="blue", bg="lightblue", xlab="predictor-MCPISM4", ylab="response-y")
abline(lm(data.train$value~data.train$MCPISM4))
plot(data.train$MCPISM8, data.train$value, pch=21, cex=1, col="blue", bg="lightblue", xlab="predictor-MCPISM8", ylab="response-y")
abline(lm(data.train$value~data.train$MCPISM8))
plot(data.train$MCPISMw_2, data.train$value, pch=21, cex=1, col="blue", bg="lightblue", xlab="predictor-MCPISMw_2", ylab="response-y")
abline(lm(data.train$value~data.train$MCPISMw_2))
plot(data.train$MCPISMw_8, data.train$value, pch=21, cex=1, col="blue", bg="lightblue", xlab="predictor-MCPISMw_8", ylab="response-y")
abline(lm(data.train$value~data.train$MCPISMw_8))

df <- data.frame(PerMEDCPI=data.train$PerMEDCPI, YEAR=data.train$YEAR, MCPISM4=data.train$MCPISM4,
                            MCPISM8=data.train$MCPISM8, MCPISMw_2=data.train$MCPISMw_2, MCPISMw_8=data.train$MCPISMw_8)
model <- lm(data.train$value~., data=df)
summary(model)
out <- summary(model)
wilcox.test(model$residuals)
#qqnorm(model$residuals, pch=21, cex=1.5, col="blue", bg="lightblue")
#qqline(model$residuals, col="red", lwd=2)
plot(model$residuals, pch=21, cex=1, col="blue", bg="lightblue", ylab="residuals", xlab="observation number")
abline(a=0, b=0, col="green")
points(out$coefficients[ , 2], pch=21, cex=1, col="red", bg="pink")
plot(model)

cor(cbind(data$value, data$PerMEDCPI, data$YEAR, data$MCPISM4, data$MCPISM8, data$MCPISMw_2, data$MCPIMSw_8))
library(glmnet)
library(Rcpp)
y <- data$value
data.ridge <- cbind(data$PerMEDCPI, data$YEAR, data$MCPISM4, data$MCPISM8, data$MCPISMw_2, data$MCPIMSw_8)
cv.ridge <- cv.glmnet(data.ridge, y)
lambda <- cv.ridge$lambda.min
lambda
model.ridge <- glmnet(data.ridge, y, lambda=lambda)
y.est <- predict(model.ridge, s=lambda, newx=data.ridge)
residuals.ridge <- data$value - y.est
SSE <- sum((data$value - y.est)^2)
SST <- sum((data$value - mean(data$value))^2)
Rsquare <- 1-SSE/SST
Rsquare
plot(residuals.ridge, pch=21, cex=1, col="blue", bg="lightblue", ylab="residuals", xlab="observation number")
abline(a=0, b=0, col="green")
points(model$residuals, pch=21, cex=1, col="green", bg="lightgreen")

data2 <- data.frame(value=log(data.train$value), PerMEDCPI=data.train$PerMEDCPI, YEAR=data.train$YEAR^2, MCPISM4=data.train$MCPISM4,
                    MCPISM8=data.train$MCPISM8, MCPISMw_2=data.train$MCPISMw_2, MCPISMw_8=data.train$MCPISMw_8)
model.nl <- lm(data2$value~., data=data2)
summary(model.nl)
plot(model.nl)
plot(data.train$YEAR, log(data.train$value), pch=21, cex=1, col="blue", bg="lightblue", xlab="predictor-YEAR", ylab="response-y")
abline(lm(log(data.train$value)~data.train$YEAR))
plot(data.train$PerMEDCPI, log(data.train$value), pch=21, cex=1, col="blue", bg="lightblue", xlab="predictor-PerMEDCPI", ylab="response-y")
abline(lm(log(data.train$value)~data$PerMEDCPI))
plot(data.train$MCPISM4, log(data.train$value), pch=21, cex=1, col="blue", bg="lightblue", xlab="predictor-MCPISM4", ylab="response-y")
abline(lm(log(data.train$value)~data.train$MCPISM4))
plot(data.train$MCPISM8, log(data.train$value), pch=21, cex=1, col="blue", bg="lightblue", xlab="predictor-MCPISM8", ylab="response-y")
abline(lm(log(data.train$value)~data.train$MCPISM8))
plot(data.train$MCPISMw_2, log(data.train$value), pch=21, cex=1, col="blue", bg="lightblue", xlab="predictor-MCPISMw_2", ylab="response-y")
abline(lm(log(data.train$value)~data.train$MCPISMw_2))
plot(data.train$MCPISMw_8, log(data.train$value), pch=21, cex=1, col="blue", bg="lightblue", xlab="predictor-MCPISMw_8", ylab="response-y")
abline(lm(log(data.train$value)~data.train$MCPISMw_8))

df1 <- data.frame(data.train$YEAR, data.train$MCPISM4, data.train$MCPISM8, data.train$MCPISMw_2, data.train$MCPISMw_8)
model1 <- lm(data.train$value~., data=df1)
summary(model1)
df2 <- data.frame(data.train$YEAR, data.train$MCPISM8, data.train$MCPISMw_8, data.train$MCPISMw_2)
model2 <- lm(data.train$value~., data=df2)
summary(model2)
df3 <- data.frame(data.train$YEAR, data.train$MCPISM8, data.train$MCPISMw_8)
model3 <- lm(data.train$value~., data=df3)
summary(model3)

