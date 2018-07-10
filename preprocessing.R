library(data.table)
library(ggplot2)

infiles <- list.files("C:/Users/kcnf898/Box Sync/CTLA4_pop PK simulation_1_10mpk/updated datasets/Treme Population PK simulation/",
                      pattern = "*.csv", full.names = T)

df <- list()
for(i in 1:length(infiles)) {
    fname <- infiles[i]
    # print(fname)

    df.temp <- fread(fname)
    sname <- basename(fname)
    sname <- strsplit(sname, "\\.c")[[1]][1]
    sname <- gsub("BM_simTreme_", "", sname)
    reg1 <- strsplit(sname, "_")[[1]][2]
    print(sname)
    ## Extract weight, dose and regimen information
    wt.df <- df.temp[1,2:501]
    wt.df <- data.frame(wt = t(wt.df))
    wt.df$subject <- paste(sname, seq(1,500), sep="_")
    wt.df$regimen <- reg1
    wt.df$dose <- gsub("mpk.*", "", wt.df$subject)
    
    conc.df <- df.temp[,c(1, 502:1001)]
    colnames(conc.df) <- c("time", wt.df$subject)
    conc.df <- melt(conc.df, id.vars = "time")
    
    ## Subset useful time points
    ## Keep the timepoints within 1 day and after one day, keep it increment by 0.1
    qual.time <- unique(conc.df$time)
    temp.time <- seq(0,1,by = 0.05)
    temp.time <- c(temp.time, seq(1.5, 200, by = 0.5))
    qual.time <- qual.time[qual.time %in%  temp.time]
    conc.df <- subset(conc.df, time %in% qual.time)
    colnames(conc.df) <- c("time", "subject", "conc")
    conc.df <- merge(conc.df, wt.df)
    
    reg2 <- as.integer(gsub("[A-Z]","",reg1)) * 7
    
    conc.df$cycle <- as.integer(conc.df$time / reg2)
    conc.df$posttime <- conc.df$time %% reg2
    
    df[[i]] <- conc.df
}

df <- do.call(rbind, df)
fwrite(df, "C:/Users/kcnf898/Box Sync/CTLA4_pop PK simulation_1_10mpk/sim_data/processed_df.csv")

