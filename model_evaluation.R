library(ggplot2)
library(data.table)
rm(list=ls())

ann.df <- fread("D:/CTLA4_pop PK simulation_1_10mpk/sim_data/ann_prediction.csv")
rf.df <- fread("D:/CTLA4_pop PK simulation_1_10mpk/sim_data/rf_prediction.csv")
lm_xgb.df <- fread("D:/CTLA4_pop PK simulation_1_10mpk/sim_data/lm_xgb_prediction.csv")

## Merge all the predictions
df <- merge(ann.df, rf.df)
remove.idx <- which(colnames(df) == "regimen")
df <- df[,-remove.idx,with=FALSE]
df <- merge(df, lm_xgb.df)
rm(ann.df, rf.df, lm_xgb.df)

## Mean square error
# Linear regression
mean((df$log.conc - df$logconc_pred_lm)^2)
# Xgboost
mean((df$log.conc - df$logconc_pred_xgb)^2)
# Random Forest
mean((df$log.conc - df$logconc_pred_rf)^2)
# Random Forest
mean((df$log.conc - df$logconc_pred_ann)^2)

sample.idx <- sample(nrow(df), 5e3)

## algorithm definition
algo.df <- data.frame(algorithm = c("logconc_pred_lm", "logconc_pred_xgb", 
                                    "logconc_pred_rf", "logconc_pred_ann"),
                      algorithm1 = c("Linear Regression", "Boosting tree (xgboost)", 
                                     "Random Forest (scikit-learn)", "Neural network (tensorflow)"))

temp.df <- df[sample.idx,]
temp.df <- melt(temp.df, id.vars = c("subject", "time", "wt", "dose.x", "regimen", "log.conc"), 
                measure.vars = c("logconc_pred_lm", "logconc_pred_xgb", "logconc_pred_rf", "logconc_pred_ann"),
                value.name = "log.conc.prediction",
                variable.name = "algorithm")
temp.df <- merge(temp.df, algo.df)
temp.df$algorithm1 <- factor(temp.df$algorithm1, levels = c("Linear Regression", 
                                                            "Boosting tree (xgboost)", 
                                                            "Random Forest (scikit-learn)", 
                                                            "Neural network (tensorflow)"))

ggplot(temp.df, aes(x=log.conc, y=log.conc.prediction)) + geom_point(alpha=0.2) +
    facet_wrap(~ algorithm1) + ylab("Predicted conc (log10)") + xlab("True conc (log10)")



temp.df <- subset(df, dose.x == 8 & regimen == "Q3W")
## True value
kinetics.df <- temp.df[,.(mean.logconc=mean(log.conc),
                          quan90.logconc = quantile(log.conc, 0.9),
                          quan10.logconc = quantile(log.conc, 0.1)),by=list(time)]
kinetics.df$mean.conc <- 10^kinetics.df$mean.logconc - 1
kinetics.df$lower <- 10^kinetics.df$quan10.logconc - 1
kinetics.df$upper <- 10^kinetics.df$quan90.logconc - 1
   
ggplot(kinetics.df, aes(x=time, y = mean.conc)) + geom_line() +
    geom_ribbon(aes(ymin=kinetics.df$lower, ymax=kinetics.df$upper), linetype=2, alpha=0.2) +
    ylab("Concentration") + xlab("Time (Day)")

## Xgboost
kinetics.df <- temp.df[,.(mean.logconc=mean(logconc_pred_lm),
                          quan90.logconc = quantile(logconc_pred_lm, 0.9),
                          quan10.logconc = quantile(logconc_pred_lm, 0.1)),by=list(time)]
kinetics.df$mean.conc <- 10^kinetics.df$mean.logconc - 1
kinetics.df$lower <- 10^kinetics.df$quan10.logconc - 1
kinetics.df$upper <- 10^kinetics.df$quan90.logconc - 1

ggplot(kinetics.df, aes(x=time, y = mean.conc)) + geom_line() +
    geom_ribbon(aes(ymin=kinetics.df$lower, ymax=kinetics.df$upper), linetype=2, alpha=0.2) +
    ylab("Concentration") + xlab("Time (Day)")

## Xgboost
kinetics.df <- temp.df[,.(mean.logconc=mean(logconc_pred_xgb),
                          quan90.logconc = quantile(logconc_pred_xgb, 0.9),
                          quan10.logconc = quantile(logconc_pred_xgb, 0.1)),by=list(time)]
kinetics.df$mean.conc <- 10^kinetics.df$mean.logconc - 1
kinetics.df$lower <- 10^kinetics.df$quan10.logconc - 1
kinetics.df$upper <- 10^kinetics.df$quan90.logconc - 1

ggplot(kinetics.df, aes(x=time, y = mean.conc)) + geom_line() +
    geom_ribbon(aes(ymin=kinetics.df$lower, ymax=kinetics.df$upper), linetype=2, alpha=0.2) +
    ylab("Concentration") + xlab("Time (Day)") + ylim(0, 300)

## Random Forest
kinetics.df <- temp.df[,.(mean.logconc=mean(logconc_pred_rf),
                          quan90.logconc = quantile(logconc_pred_rf, 0.9),
                          quan10.logconc = quantile(logconc_pred_rf, 0.1)),by=list(time)]
kinetics.df$mean.conc <- 10^kinetics.df$mean.logconc - 1
kinetics.df$lower <- 10^kinetics.df$quan10.logconc - 1
kinetics.df$upper <- 10^kinetics.df$quan90.logconc - 1

ggplot(kinetics.df, aes(x=time, y = mean.conc)) + geom_line() +
    geom_ribbon(aes(ymin=kinetics.df$lower, ymax=kinetics.df$upper), linetype=2, alpha=0.2) +
    ylab("Concentration") + xlab("Time (Day)") + ylim(0, 300)

## Random Forest
kinetics.df <- temp.df[,.(mean.logconc=mean(logconc_pred_ann),
                          quan90.logconc = quantile(logconc_pred_ann, 0.9),
                          quan10.logconc = quantile(logconc_pred_ann, 0.1)),by=list(time)]
kinetics.df$mean.conc <- 10^kinetics.df$mean.logconc - 1
kinetics.df$lower <- 10^kinetics.df$quan10.logconc - 1
kinetics.df$upper <- 10^kinetics.df$quan90.logconc - 1

ggplot(kinetics.df, aes(x=time, y = mean.conc)) + geom_line() +
    geom_ribbon(aes(ymin=kinetics.df$lower, ymax=kinetics.df$upper), linetype=2, alpha=0.2) +
    ylab("Concentration") + xlab("Time (Day)")
