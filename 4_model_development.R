library(data.table)
library(ggplot2)
library(xgboost)
library(randomForest)
library(Matrix)
rm(list=ls())

df <- fread("C:/Users/kcnf898/Box Sync/CTLA4_pop PK simulation_1_10mpk/sim_data/processed_df_downsample_time.csv")
df$log.conc <- log10(df$conc + 1)

train.subj <- unique(df$subject)
set.seed(123)
train.subj <- train.subj[sample(length(train.subj), 0.6 * length(train.subj))]
train.df <- subset(df, subject %in% train.subj)
test.df <- subset(df, !subject %in% train.subj)

write.csv(data.frame(subject=train.subj), 
          file = "C:/Users/kcnf898/Box Sync/CTLA4_pop PK simulation_1_10mpk/sim_data/train_subject_downsample_time.csv",
          quote=FALSE, row.names = FALSE)

########################################################################
########################## Linear regression ###########################
########################################################################
lm.fit <- lm(log.conc ~ wt + regimen + dose + cycle + posttime, data = train.df)
summary(lm.fit)

plot.idx <- sample(nrow(train.df), 2e3)
plot(train.df$log.conc[plot.idx], lm.fit$fitted.values[plot.idx])

y.pred <- predict(lm.fit, test.df)
plot.idx <- sample(nrow(test.df), 2e3)
plot(test.df$log.conc[plot.idx], y.pred[plot.idx])

test.df$logconc_pred_lm <- y.pred

# MSE
mean((test.df$logconc_pred_lm - test.df$log.conc)^2)

########################################################################
######################## Boosting Tree xgboost #########################
########################################################################
formula <- as.formula(log.conc ~ wt + regimen + dose + cycle + posttime)

train.data <- sparse.model.matrix(formula, data=train.df)
dtrain <- xgb.DMatrix(data = train.data, label = train.df$log.conc)

test.data <- sparse.model.matrix(formula, data=test.df)
dtest <- xgb.DMatrix(data = test.data, label = test.df$log.conc)

# Cross validation to find the optimal parameters
bst.cv <- xgb.cv(data = dtrain, nrounds = 100, maxdepth = 3, base_score = 1,
                 params = list(eta=0.5), eval_metric = "rmse", min_child_weight = 0.05,
                 nfold=4,
                 early_stopping_rounds = 4)

xgb.fit <- xgb.train(params = bst.cv$params, dtrain, nrounds = 25)

save(xgb.fit, file="C:/Users/kcnf898/Box Sync/CTLA4_pop PK simulation_1_10mpk/model/xgboost_model_051818.RData")
load(file="C:/Users/kcnf898/Box Sync/CTLA4_pop PK simulation_1_10mpk/model/xgboost_model_051818.RData")

y.pred <- predict(xgb.fit, dtrain)
plot.idx <- sample(nrow(train.df), 2e3)
plot(train.df$log.conc[plot.idx], y.pred[plot.idx])

y.pred <- predict(xgb.fit, dtest)
plot.idx <- sample(nrow(test.df), 2e3)
plot(test.df$log.conc[plot.idx], y.pred[plot.idx])

test.df$logconc_pred_xgb <- y.pred

# MSE
mean((test.df$logconc_pred_xgb - test.df$log.conc)^2)

write.csv(test.df, file = "C:/Users/kcnf898/Box Sync/CTLA4_pop PK simulation_1_10mpk/results/lm_xgb_prediction_downsample_timepoints.csv",
          row.names = FALSE, quote = FALSE)

