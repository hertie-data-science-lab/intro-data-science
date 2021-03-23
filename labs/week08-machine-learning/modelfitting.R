library(tidyverse)
library(BART)

electric <- read_csv("data/electric-company-long.csv") %>%
  mutate(grade = factor(grade),                      # make grade a factor
         treatment = ifelse(treatment == "T", 1, 0)) # make treatment an indicator

mod0 <- lm(post.score ~ treatment, data = electric)
summary(mod0)

## a carefully constructed linear model

mod_flm <- lm(post.score ~ treatment + grade + treatment:grade +
              pre.score + pre.score:grade +
              supp + supp:grade + supp:grade + city,
                  data = electric)

## a not so carefully constructed bayesian regression tree model

X <- model.matrix(~ treatment + city + grade + pre.score + supp,
                  data = electric)
dfX <- select(as_tibble(X), -`(Intercept)`)

mod_bart <- wbart(as.matrix(dfX), electric$post.score)
names(mod_bart)

# predictions for class 1
hist(mod_bart$yhat.train[,1], breaks = 60)

# predictions for whether class 1 is better than class 2
hist(mod_bart$yhat.train[,1] - mod_bart$yhat.train[,2], breaks = 60)

# how about than class 13 vs 16? (chosen to be quite close
prop.table(table((mod_bart$yhat.train[,13] > mod_bart$yhat.train[,16])))

## do they agree?
plot(fitted(mod_flm), mod_bart$yhat.train.mean)

## slightly unfair because we ignore prediction uncertainty
mean((mod_bart$yhat.train.mean - electric$post.score)^2)
mean((fitted(mod_flm) - electric$post.score)^2)

## causal inference is out of sample inference (actually
## out of world inference...)

# linear model 'predictions'

flm_Y1 <- predict(mod_flm, newdata = mutate(electric, treatment = 1))
flm_Y0 <- predict(mod_flm, newdata = mutate(electric, treatment = 0))

## average treatment effect
mean(flm_Y1 - flm_Y0)

# ATEs for each grade from linear model
mean(flm_Y1[electric$grade == 1] - flm_Y0[electric$grade == 1])
mean(flm_Y1[electric$grade == 2] - flm_Y0[electric$grade == 2])
mean(flm_Y1[electric$grade == 3] - flm_Y0[electric$grade == 3])
mean(flm_Y1[electric$grade == 4] - flm_Y0[electric$grade == 4])

# these are 1000 by nrow(electric) matrices of draws
bart_Y1 <- predict(mod_bart, as.matrix(mutate(dfX, treatment = 1)))
bart_Y0 <- predict(mod_bart, as.matrix(mutate(dfX, treatment = 0)))

# construct the differences in advance
res <- t(bart_Y1 - bart_Y0) # flip to cases are rows

## 'individual effects'
vv <- rowMeans(res)

## make up the average treatment effect
cat("Effect:", round(mean(vv), 3), "SE:", round(sd(vv), 3))

# ATEs for each grade from bart model
mean(vv[electric$grade == 1])
mean(vv[electric$grade == 2])
mean(vv[electric$grade == 3])
mean(vv[electric$grade == 4])

