df = read.csv("mlbteamsdata.csv")
df = as_tibble(df)
dim(df)

df$lsalary = log(df$salary)
require(dplyr)
df = df %>% rename("WLpct" = W.L., 
                   "DoublePlay" = DP)

df.train.all = df[31:dim(df)[1],]
df.train.all = select(df.train.all, -c("TeamName", "W", "L"))
df.test.all = df[1:30,] 
dim(df.train.all)

variable.names(df)
df.train.off = df.train.all[,c("WLpct",
                               "H.1",
                               "R.G",
                               "RBI",
                               "SB",
                               "SO.1",
                               "BA",
                               "OBP",
                               "SLG",
                               "LOB",
                               "salary",
                               "OPS",
                               "MVP",
                               "lsalary")]

df.train.def = df.train.all[,c("WLpct",
                               "DefEff",
                               "ER",
                               "RA.G",
                               "tSho",
                               "H",
                               "HR",
                               "E",
                               "BB",
                               "DoublePlay",
                               "SO",
                               "salary",
                               "MVP",
                               "CYYOUNG",
                               "lsalary")]

logit.off = glm(WLpct ~ ., family = "quasibinomial", data = df.train.off)
summary(logit.off)

logit.def = glm(WLpct ~ ., family = "quasibinomial", data = df.train.def)
summary(logit.def)

probit.off = glm(WLpct ~ ., family = quasibinomial(link = "probit"), data = df.train.off)
summary(probit.off)

probit.def = glm(WLpct ~ ., family = quasibinomial(link = "probit"), data = df.train.def)
summary(probit.def) 

probit.all = glm(WLpct ~ ., family = quasibinomial(link = "probit"), data = df.train.all)
summary(probit.all)

logit.off.preds = predict.glm(logit.off, newdata = df.test.all)
logit.def.preds = predict.glm(logit.def, newdata = df.test.all)
probit.off.preds = predict.glm(probit.off, newdata = df.test.all)
probit.def.preds = predict.glm(probit.def, newdata = df.test.all) 
probit.all.preds = predict.glm(probit.all, newdata = df.test.all) 
logit.all.preds = predict.glm(logit.all, newdata = df.test.all)

par(mfrow = c(2,2))
hist(logit.off.preds)
hist(logit.def.preds)
hist(probit.off.preds)
hist(probit.def.preds)

logit.off.preds.mean = mean(abs(logit.off.preds))
logit.def.preds.mean = mean(abs(logit.def.preds))
logit.all.preds.mean = mean(abs(logit.all.preds))
probit.off.preds.mean = mean(abs(probit.off.preds))
probit.def.preds.mean = mean(abs(probit.def.preds))
probit.all.preds.mean = mean(abs(probit.all.preds))


df.test.all$pythag = 1/(1+((df.test.all$ER/df.test.all$R)^2))
pythag.acc = mean(abs(df.test.all$WLpct - df.test.all$pythag)) 
1-pythag.acc

logit.off.preds.mean*162
logit.def.preds.mean*162 
probit.off.preds.mean*162
probit.def.preds.mean*162
probit.all.preds.mean*162 


logit.all = glm(WLpct~., data = df.train.all, family = "quasibinomial")
summary(logit.all)


require(knitr)
require(xtable)
require(car)
require(broom)
n = nrow(df.train.all)
k = ncol(df.train.all)
J = ncol(df.train.all) - ncol(df.train.def)
fcr = qf(1-.05,J,n-k)

Hnull = c()
for(i in 1:dim(df.train.def)[2]){
  v = variable.names(df.train.def)[i]
  c = paste0(v,"=0")
  Hnull[i] = c
}
Hnull
linearHypothesis(logit.all, Hnull[2:length(Hnull)])

smod.all = summary(logit.all)
kable(tidy(logit.all), caption = "Defensive Regression Output")
glance(logit.all)$deviance

