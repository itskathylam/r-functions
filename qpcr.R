# Author:   Kathy N Lam
# Purpose:  R functions to be used with dual-labeled probe data from Bio-Rad CFX384
#           Fluors: FAM, HEX, Cy5, Texas Red            


#libraries
try(library(tidyverse), silent=FALSE)


### make geom_tile plot of end-point flourescence to represent 384-well plate
### required: filename prefix, e.g. "data/csv/2018-02-05 -  End Point Results"
plot_endpoint_fluorescence = function(prefix) {
    
    #read the files
    try((fam = read.csv(paste0(prefix, "_FAM.csv"), header=TRUE)), silent=TRUE)
    try((hex = read.csv(paste0(prefix, "_HEX.csv"), header=TRUE)), silent=TRUE)
    try((cy = read.csv(paste0(prefix, "_Cy5.csv"), header=TRUE)), silent=TRUE)
    try((tr = read.csv(paste0(prefix, "_Texas Red.csv"), header=TRUE)), silent=TRUE)
    
    #make empty df to merge with
    ep=data.frame(X=character(), Well=character(), Fluor=character(), Target=character(), 
                  Content=character(), Sample=character(), End.RFU=numeric(), Call=character(), 
                  Sample.Type=character(), CallType=character(), IsControl=character())
    
    #check if individual dfs exists and merge
    df_names = c("fam", "hex", "cy", "tr")
    for (df in df_names){
        if (exists(df)) {  #nb object passed to exists require quotes
            ep = bind_rows(ep, get(df))
        }
    }

    #make new vars for Plate Row and Column
    ep$Row = gsub("[0-9]*", "", ep$Well)
    ep$Column = gsub("[A-Z]", "", ep$Well)
    
    #factor Row to alphabetize
    ep$Row = factor(ep$Row, levels = rev(levels(as.factor(ep$Row))))
    
    #plot the plate and facet by fluor
    ggplot(ep, aes(x=Column, y=Row)) +
        geom_tile(aes(fill = End.RFU)) +
        scale_fill_gradient(low = "black", high = "yellow") +
        theme_linedraw(14) +
        xlab("") +
        ylab("") +
        facet_wrap(~Fluor, ncol=1) +
        theme(legend.position="none")
}


### make geom_line plot of flourescence vs cycle 
### required: filename prefix, e.g. "data/csv/2018-02-05 -  Quantification Amplification Results"
plot_cycle_fluorescence = function(prefix) {

    #read the files
    try((FAM = read.csv(paste0(prefix, "_FAM.csv"), header=TRUE)), silent=TRUE)
    try((HEX = read.csv(paste0(prefix, "_HEX.csv"), header=TRUE)), silent=TRUE)
    try((Cy5 = read.csv(paste0(prefix, "_Cy5.csv"), header=TRUE)), silent=TRUE)
    try((Texas_Red = read.csv(paste0(prefix, "_Texas Red.csv"), header=TRUE)), silent=TRUE)
    
    #make empty df to merge with
    qa = data.frame(Cycle = numeric(), Well = character(), RFU = numeric(), Fluor=character())
    
    #check if individual dfs exists and merge
    df_names = c("FAM", "HEX", "Cy5", "Texas_Red")
    for (df in df_names){
        if (exists(df)) {  #nb object passed to exists require quotes
            fluor = df
            df = select(get(df), -X)
            df = melt(df, id.vars="Cycle")
            df$Fluor = rep(fluor, length(df$Cycle)) 
            names(df) = c("Cycle", "Well", "RFU", "Fluor")
            qa = bind_rows(qa, df)
        }
    }
    
    #plot the fluorescence over cycles, facet by fluor
    ggplot(qa, aes(x=Cycle, y=RFU, group=interaction(Well, Fluor), colour=Fluor)) +
        geom_line() +
        facet_wrap(~Fluor) +
        theme_linedraw(14) +
        theme(legend.position="none")
}


### plot standards - quantity vs cq; generate lm 
### required: data frame 


    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
