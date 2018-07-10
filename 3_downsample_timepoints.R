library(data.table)
library(ggplot2)
library(xgboost)
library(randomForest)
library(Matrix)
library(plyr)
rm(list=ls())

## Downsample timepoints
df <- fread("D:/CTLA4_pop PK simulation_1_10mpk/sim_data/processed_df.csv")
time.df <- fread("D:/CTLA4_pop PK simulation_1_10mpk/sim_data/time_points.csv")

df <- subset(df, posttime %in% time.df$posttime)

fwrite(df, "D:/CTLA4_pop PK simulation_1_10mpk/sim_data/processed_df_downsample_time.csv")

## Downsample patient number
meta.df <- df[,c("regimen", "dose", "subject")]
meta.df <- meta.df[!duplicated(meta.df),]
count(meta.df, vars=c("regimen", "dose"))


## Downsample to 100 patient per cohort
meta.df$subject.num <- as.integer(sapply(meta.df$subject, function(x) strsplit(x, "_")[[1]][3]))
meta.df1 <- subset(meta.df, subject.num <= 100)
df1 <- merge(df, meta.df1)

fwrite(df1, "D:/CTLA4_pop PK simulation_1_10mpk/sim_data/processed_df_downsample_time_cohort100.csv")


## Downsample to 100 patient per cohort
meta.df$subject.num <- as.integer(sapply(meta.df$subject, function(x) strsplit(x, "_")[[1]][3]))
meta.df2 <- subset(meta.df, subject.num <= 50)
df2 <- merge(df, meta.df2)

fwrite(df2, "D:/CTLA4_pop PK simulation_1_10mpk/sim_data/processed_df_downsample_time_cohort50.csv")

## Downsample to 20 patient per cohort
meta.df$subject.num <- as.integer(sapply(meta.df$subject, function(x) strsplit(x, "_")[[1]][3]))
meta.df2 <- subset(meta.df, subject.num <= 20)
df2 <- merge(df, meta.df2)

fwrite(df2, "D:/CTLA4_pop PK simulation_1_10mpk/sim_data/processed_df_downsample_time_cohort20.csv")
