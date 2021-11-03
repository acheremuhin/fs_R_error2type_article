library("tidyverse")
library("FSinR")
library("broom")
library("gvlma")
# Генерация базы
n<-1000 #Количество наблюдений в БД
m <- 10 #Количество повторов
evaluator <- determinationCoefficient()
Res_all <- array(0, c(8,7,m))
Res <- matrix(0, nrow = 8, ncol = 7)
j <- 1 # Выбор распределения (от 1 до 8)
k <- 1 # Номер коэффициента (от 1 до 7)
for(j in 1:8) {
for(k in 1:7) {
R0 <- Sys.time()
if (k==1){
  method1 <- simulatedAnnealing()
} else if (k==2) {
  method1 <- selectDifference()
} else if (k==3) {
  method1 <- hillClimbing()
} else if (k==4) {
  method1 <- LasVegas()
} else if (k==5) {
  method1 <- sequentialForwardSelection()
} else if (k==6) {
  method1 <- selectSlope()
} else {
  method1 <- whaleOptimization()  
}
for(s in 1:m) {
X1 <- rbeta(n, shape1 = sample((1:100)/10, 1), shape2 = sample((1:100)/10, 1)) 
X2 <- rcauchy(n, location = sample((-10:10), 1), scale = sample((1:100)/10, 1)) 
X3 <- rexp(n, rate = sample((1:100)/10, 1)) 
X4 <- rgamma(n, shape = sample((1:100)/10, 1), rate = sample((1:100)/10, 1)) 
X5 <- rlnorm(n, meanlog = sample((-10:10), 1), sdlog = sample((1:100)/10, 1)) 
X6 <- rnorm(n, mean = sample((-10:10), 1), sd = sample((1:100)/10, 1)) 
X7 <- runif(n, min = sample((-30:0), 1), max = sample((1:31), 1)) 
X8 <- rweibull(n, shape = sample((1:100)/10, 1), scale = sample((1:100)/10, 1)) 
X9 <- rbeta(n, shape1 = sample((1:100)/10, 1), shape2 = sample((1:100)/10, 1)) 
X10 <- rcauchy(n, location = sample((-10:10), 1), scale = sample((1:100)/10, 1)) 
X11 <- rexp(n, rate = sample((1:100)/10, 1)) 
X12 <- rgamma(n, shape = sample((1:100)/10, 1), rate = sample((1:100)/10, 1)) 
X13 <- rlnorm(n, meanlog = sample((-10:10), 1), sdlog = sample((1:100)/10, 1)) 
X14 <- rnorm(n, mean = sample((-10:10), 1), sd = sample((1:100)/10, 1)) 
X15 <- runif(n, min = sample((-30:0), 1), max = sample((1:31), 1)) 
X16 <- rweibull(n, shape = sample((1:100)/10, 1), scale = sample((1:100)/10, 1))
Base_0 <- cbind(X1,X2,X3,X4,X5,X6,X7,X8,X9,X10,X11,X12,X13,X14,X15,X16)
Base_0_names <- colnames(Base_0)
Base <- as.data.frame(cbind(Base_0[,j],Base_0[,-j]))
colnames(Base) <- c("Y",Base_0_names[-j])
# FS

model<-method1(Base, 'Y', evaluator)
x <- which(model$bestFeatures == 1)
# Модель
mod<-lm(Y ~ ., data = Base[,c(1,x+1)])
k1 <- sum(tidy(mod)$p.value<0.05)/length(tidy(mod)$p.value)
k2 <- ifelse(glance(mod)$p.value<0.05,1,0)
coef <- (sum(tidy(mod)$p.value<0.05)+ifelse(glance(mod)$p.value<0.05,1,0))/(length(tidy(mod)$p.value)+1)
Res_all[j,k,s] <- coef
}
Res[j,k] <- mean(Res_all[j,k,])
R1 <- Sys.time()
print(j)
print(k)
print(R1-R0)
}
}
Res
library(openxlsx)
write.xlsx(Res, 'Res.xlsx')
