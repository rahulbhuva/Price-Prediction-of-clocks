
library(xlsx)

excel_path <- "clock.xlsx"

Age <- read.xlsx(excel_path, sheetName = "Sheet1", colIndex = 1)
Bidders <- read.xlsx(excel_path, sheetName = "Sheet1", colIndex = 2)
Price <- read.xlsx(excel_path, sheetName = "Sheet1", colIndex = 3)

clock.data <- data.frame(Age, Bidders, Price)

head(clock.data)
summary(clock.data)
str(clock.data)
plot(clock.data, pch=16)

mlrm1 <- lm(Price ~ Age + Bidders, data=clock.data)
summary(mlrm1)



summary(mlrm1)
anova(mlrm1)


#For Bidders = 10
exp.value <- predict(mlrm1, newdata = data.frame(Age = 100, Bidders = 10),interval = "confidence", level = .95)
exp.value[2]-500

#For Bidders = 11
exp.value <- predict(mlrm1, newdata = data.frame(Age = 100, Bidders = 11),interval = "confidence", level = .95)
exp.value[2]-500

#For Bidders = 12
exp.value <- predict(mlrm1, newdata = data.frame(Age = 100, Bidders = 12),interval = "confidence", level = .95)
exp.value[2]-500


MSE <- mean(mlrm1$residuals^2)

beta_matrix <- as.matrix(mlrm1$coefficients, ncol=1)


new_data <- as.matrix(c(1, 150,15))

prediction_price <- t(new_data) %*% beta_matrix
t.value <- qt(0.99,30)
x <- cbind(clock.data$Age,clock.data$Bidders)
x_h <- matrix(c(150,15), nrow=1,ncol = 2)

val <- x_h %*% solve( t(x) %*% x ) %*% t(x_h)
lower.bound.price <- prediction_price-t.value*sqrt(MSE)*sqrt(1+val)
lower.bound.price


std.clock <- clock.data

std.clock$Price <- (clock.data$Price - mean(clock.data$Price))/sd(clock.data$Price)
std.clock$Age <- (clock.data$Age - mean(clock.data$Age))/sd(clock.data$Age)
std.clock$Bidders <- (clock.data$Bidders - mean(clock.data$Bidders))/sd(clock.data$Bidders)

standard.model <- lm(Price ~ -1 + Age + Bidders,data = std.clock)
summary(standard.model)
vcov(standard.model)
val <- (0.88752-0.61985)/sqrt(2*(0.0038223614-0.0009699208))
p.val <- 2*(1-pt(val,30))
cat("p-value for Statistical significance between Coefficients of Age and Bidders",p.val)


#res_analysis(mlrm1, clock.data)

sres<-residuals(mlrm1)/(133.1*sqrt(1-influence(mlrm1)$hat))
hist(sres)

boxplot(sres,main="Sres")

plot(mlrm1)

plot(clock.data$Age,sres)

plot(clock.data$Bidders,sres)




second.mlrm <- lm(Price ~ Age + Bidders + I(Age*Bidders), data = clock.data)
summary(second.mlrm)

#fit <- lm(formula = Price ~ Age + Bidders + I(Age*Bidders),data = clock.data)
fit.aov <- anova(second.mlrm)
tab <- as.table(cbind(
  'SS' = c("SSR(x1, x2, x3)" = sum(fit.aov[1:3, 2]),
           "SSR(x1)"           = fit.aov[1, 2],
           "SSR(x2|x1)"        = fit.aov[2, 2],
           "SSR(x3|x1, x2)"    = fit.aov[3, 2],
           "SSE"               = fit.aov[4, 2],
           "Total"             = sum(fit.aov[, 2])),
  
  'Df' = c(                    sum(fit.aov[1:3, 1]),
                               fit.aov[1, 1],
                               fit.aov[2, 1],
                               fit.aov[3, 1],
                               fit.aov[4, 1],
                               sum(fit.aov$Df)),
  
  'MS' = c(                    sum(fit.aov[1:3, 2]) / sum(fit.aov[1:3, 1]),
                               fit.aov[1, 3],
                               fit.aov[2, 3],
                               fit.aov[3, 3],
                               fit.aov[4, 3],
                               NA)
))

round(tab, 2)



#For Bidders = 10
exp.value1 <- predict(second.mlrm, newdata = data.frame(Age = 100, Bidders = 10),interval = "confidence", level = .95)
exp.value1[2] - 500

#For Bidders = 11
exp.value2 <- predict(second.mlrm, newdata = data.frame(Age = 100, Bidders = 11),interval = "confidence", level = .95)
exp.value2[2]-500

#For Bidders = 12
exp.value3 <- predict(second.mlrm, newdata = data.frame(Age = 100, Bidders = 12),interval = "confidence", level = .95)
exp.value3[2]-500




MSE <- mean(second.mlrm$residuals^2)

beta_matrix <- as.matrix(second.mlrm$coefficients, ncol=1)
beta_matrix

new_data <- as.matrix(c(1, 150,15, 2250))
new_data

prediction_price <- t(new_data) %*% beta_matrix
t.value <- qt(0.99,30) #df = 30
x <- cbind(clock.data$Age,clock.data$Bidders, clock.data$Age*clock.data$Bidders)
x_h <- matrix(c(150,15,2250), nrow=1,ncol = 3)

val <- x_h %*% solve( t(x) %*% x ) %*% t(x_h)
lower.bound.price <- prediction_price-t.value*sqrt(MSE)*sqrt(1+val)
lower.bound.price

