library(readxl)
data = read_xlsx("r_assignment.xlsx")

library(rgl)
library(ggplot2)
library(AER)
library(stargazer)
library(urca)
library(car)
model1 <- lm(Y ~ X + Z, data = data)
summary(model1)
C = coef(model1)

ts.plot(data$Y)
ts.plot(data$X)
ts.plot(data$Z)

plot(model1)
abline(0, 0)

dwtest(model1)
#Positive DW value:1.43 significant autocorrelation
#prob[dw] = 0.06 > 0.05 -> do not RHo -> AC problem

durbinWatsonTest(model1)
#autocorrelation = 0.18
#dw = 1.47, p value = 0.16

bptest(model1)
#Reject Ho, there is HC at 0.05 level of significance.

#add lag
lagpad <- function(x, k) {
  if (k>0) {
    return (c(rep(NA, k), x)[1 : length(x)] );
  }
  else {
    return (c(x[(-k+1) : length(x)], rep(NA, -k)));
  }
}

data <- cbind(data, lagpad(data$Y, 1), lagpad(data$X, 1), lagpad(data$Z, 1))
names(data) <- c("Years", "Y", "X", "Z", "Y_1", "X_1", "Z_1")

data

model2 <- lm(Y ~ X + Z + Y_1 + X_1 + Z_1, data = data)
summary(model2)

durbinWatsonTest(model2)
# lag Autocorrelation D-W Statistic p-value
#1      -0.2008903      1.657395   0.274
#Alternative hypothesis: rho != 0

dwtest(model2)
#DW = 1.6574, p-value = 0.1331
#alternative hypothesis: true autocorrelation is greater than 0

data <- cbind(data, lagpad(data$Y, 2), lagpad(data$X, 2), lagpad(data$Z, 2))
names(data) <- c("Years", "Y", "X", "Z", "Y_1", "X_1", "Z_1", "Y_2", "X_2", "Z_2")
data

model3 <- lm(Y ~ X + Z + Y_1 + X_1 + Z_1 + Y_2 + X_2 + Z_2, data = data)
summary(model3)

durbinWatsonTest(model3)
dwtest(model3)

data <- cbind(data, lagpad(data$Y, 3), lagpad(data$X, 3), lagpad(data$Z, 3))
names(data) <- c("Years", "Y", "X", "Z", "Y_1", "X_1", "Z_1", "Y_2", "X_2", "Z_2", 
                 "Y_3", "X_3", "Z_3")
data

model4 <- lm(Y ~ X + Z + Y_1 + X_1 + Z_1 + Y_2 + X_2 + Z_2 + Y_3 + X_3 + Z_3, data = data)
model4
summary(model4)

dwtest(model4)
durbinWatsonTest(model3)

plot(data$Y)

plot(fit, which=1, col=c("blue"))

fit <- lm(data$Y ~ data$X, data = data) # fit the model
data$predicted <- predict(fit)   # Save the predicted values
data$residuals <- residuals(fit) # Save the residual values
ggplot(data, aes(x = X, y = Y)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = X, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()

fit2 <- lm(data$Y ~ data$Z, data = data) # fit the model
data$predicted <- predict(fit2)   # Save the predicted values
data$residuals <- residuals(fit2) # Save the residual values
ggplot(data, aes(x = Z, y = Y)) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +     # regression line  
  geom_segment(aes(xend = Z, yend = predicted), alpha = .2) +      # draw line from point to line
  geom_point(aes(color = abs(residuals), size = abs(residuals))) +  # size of the points
  scale_color_continuous(low = "green", high = "red") +             # colour of the points mapped to residual size - green smaller, red larger
  guides(color = FALSE, size = FALSE) +                             # Size legend removed
  geom_point(aes(y = predicted), shape = 1) +
  theme_bw()


library(rgl)
library(rglwidget)
scatter3d(Y ~ X + Z, fill=FALSE, data=dfRegr)


