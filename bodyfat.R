library(isdals)
data(bodyfat)
attach(bodyfat)
cor(cbind(Fat,Triceps,Thigh,Midarm))

#lm way
fat.hat <- predict(lm(Fat~Thigh))
triceps.hat <- predict(lm(Triceps~Thigh))
cor((Fat-fat.hat),(Triceps-triceps.hat))

#ppcor way
library(ppcor)
pcor(cbind(Fat, Triceps, Thigh))

#-------------------------------------------
fat.hat <- predict(lm(Fat~Thigh+Midarm))
tri.hat <- predict(lm(Triceps~Thigh+Midarm))
cor((Fat-fat.hat),(Triceps-tri.hat))

 