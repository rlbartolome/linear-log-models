library(tidyverse)

##categorical not included????

waterprices_all <- read.csv('data_BaBe.csv')
waterprices <- waterprices_all[1:250,-1] %>% as_tibble() ##-c(1:3,9:14,16:17,19)
waterprices_test <- waterprices_all[251:300,-1] %>% as_tibble()

full_mod <- lm(wd_rate ~ . -REGION -WD.Area, data = waterprices)
int_mod <- lm(wd_rate ~ 1, data = waterprices)

back_elim <- step(object = full_mod, scope = list(lower = int_mod), direction = "backward")
forward_elim <- step(object = int_mod, scope = list(upper = full_mod), direction = "forward")
step_sel <- step(object = int_mod, scope = list(upper = full_mod), direction = "both")

fitstat <- function(x) {
  xsum <- summary(x)
  resid <- x$residuals
  fit <- x$fitted.values
  return(c(R2 = xsum$r.squared,
           R2Adj = xsum$adj.r.squared,
           AIC = AIC(x),
           BIC = BIC(x),
           MSE = mean(resid^2),
           MAPE = mean(abs(resid/fit))
  ))}

sapply(list('Full Model' = full_mod,
            'Forward Elimination' = forward_elim,
            'Backward Elimination' = back_elim,
            'Stepwise Selection' = step_sel), fitstat)

final <- lm(wd_rate ~ nrwpcent + sprw, data = waterprices)



prediction <- predict(final,waterprices_test)
(prediction -  mean(waterprices_test$wd_rate)) %>% 
  abs() %>% sum() %>% sqrt() / mean(waterprices_test$wd_rate)

#### logit