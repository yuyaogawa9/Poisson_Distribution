library(stats)
library(dplyr)
library(lmtest)
library(stargazer)
library(ggplot2)


#work with the data set crime1
library(readxl)
crime1 <- read_excel("crime.xlsx")
View(crime1)

#interested in the number of arrests men have as a function of their 
#income, measured in hundreds of dollars, using data from 1986.

ggplot(data = crime1,aes(x=inc86,y=narr86))+
  geom_point()

#there appears to be a negative relationship. i.e. as income decrease, arrests
#increase. Let's model that with OLS

ggplot(data = crime1,aes(x=inc86,y=narr86))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)

#this fitted line makes no sense. No one be arrested a negative number of times.

#poisson regression is another glm model, so we'll use the glm function 
PoissonReg1 = glm(narr86~inc86,data = crime1,family=poisson())
summary(PoissonReg1)

poissonFitted = data.frame(
  fit = PoissonReg1$fitted.values,
  inc86 = crime1$inc86
)

ggplot(data = crime1,aes(x=inc86,y=narr86))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  geom_smooth(data = poissonFitted,aes(x=inc86,y=fit),se=FALSE,color="green")

#include more than 1 independent variable in a poisson regression
PoissonReg2 = glm(narr86~inc86+inc86sq+qemp86+durat+black+hispan+tottime,
                  data = crime1,family=poisson())
summary(PoissonReg2)

poissonFitted2 = data.frame(
  fit = PoissonReg2$fitted.values,
  inc86 = crime1$inc86
)

ggplot(data = crime1,aes(x=inc86,y=narr86))+
  geom_point()+
  geom_smooth(method = "lm",se=FALSE)+
  geom_smooth(data = poissonFitted2,aes(x=inc86,y=fit),se=FALSE,color="green")

#calculate odds ratios the same way we did for multinomial regression
rr1 = exp(coef(PoissonReg1))
stargazer(PoissonReg1,type = "text",coef = list(rr1),p.auto = FALSE)

rr2 = exp(coef(PoissonReg2))
stargazer(PoissonReg2,type = "text",coef = list(rr2),p.auto = FALSE)

#to check on overdispersion use quasipoisson
QuasiPoissonReg1 = glm(narr86~inc86,data = crime1,family=quasipoisson())
summary(QuasiPoissonReg1)
#The dispersion here is 1.59, so using the regular poisson wouldn't make much
#of a difference

QuasiPoissonReg2 = glm(narr86~inc86+inc86sq+qemp86+durat+black+hispan+tottime,
                       data = crime1,family=quasipoisson())
summary(QuasiPoissonReg2)
#again relatively small dispersion, 1.50, so can using regular poisson would
#be fine.