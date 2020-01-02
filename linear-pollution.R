pol.data <- read.csv('pollution.csv')

mort.fullmodel <- lm(MORT ~ PRECIP + EDUC + NONWHITE + 
                   logNOX + logSOX, data = pol.data)
f <- summary(mort.fullmodel)$fstatistic
pf(f[1], f[2], f[3], lower=F) ## 1 - Yes
summary(mort.fullmodel)$adj.r.squared ## 2 - 65%
coef(mort.fullmodel) ## 3 - here is the line
summary(mort.fullmodel)$coefficients[,4] ## 4

mort.polmodel <- lm(MORT ~ logNOX + logSOX, data = pol.data)
fp <- summary(mort.polmodel)$fstatistic
pf(fp[1], fp[2], fp[3], lower=F) ## 5 - Yes
summary(mort.polmodel)
anova(mort.polmodel, mort.fullmodel)

newdata <- data.frame(PRECIP=20, EDUC=12,NONWHITE=33,
                      logNOX = 0, logSOX = 0)

predict(mort.fullmodel, newdata, interval='confidence', level = 0.95) ## 6a
predict(mort.fullmodel, newdata, interval='prediction', level = 0.99) ## 6b

mort.demogmodel <- lm(MORT ~ EDUC + NONWHITE, data = pol.data)
fd <- summary(mort.demogmodel)$fstatistic
pf(fd[1], fd[2], fd[3], lower=F) ## 7 - Yes
summary(mort.demogmodel)

repol.data <- within(pol.data, Region <- relevel(pol.data$Region,"West"))
mort.reggmodel <- lm(MORT ~ Region, data = repol.data)
fg <- summary(mort.reggmodel)$fstatistic
pf(fg[1], fg[2], fg[3], lower=F) ## 7 - Yes
summary(mort.reggmodel)

mort.nonpolmodel <- lm(MORT ~ PRECIP + EDUC + NONWHITE, data = pol.data)
fp <- summary(mort.nonpolmodel)$fstatistic
pf(fp[1], fp[2], fp[3], lower=F) ## 5 - Yes
summary(mort.nonpolmodel)

repol.data <- within(pol.data, Region <- relevel(pol.data$Region,"West"))
mort.fullmodelwReg <- lm(MORT ~ PRECIP + EDUC + NONWHITE + 
                           logNOX + logSOX + Region, data = repol.data)
summary(mort.fullmodelwReg)

newdatawReg <- data.frame(PRECIP=10, EDUC=12, NONWHITE=10,
                      logNOX = 1, logSOX = 1, Region="West")

predict(mort.fullmodelwReg, newdatawReg, interval='confidence')
predict(mort.fullmodelwReg, newdatawReg, interval='prediction', level = .99)

