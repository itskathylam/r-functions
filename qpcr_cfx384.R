# Author:   Kathy N Lam
# Purpose:  R functions to be used with dual-labeled probe data from Bio-Rad CFX384
#           Fluors: FAM, HEX, Cy5, Texas Red            


### make geom_tile plot of end-point flourescence from filename prefix
plot_endpoint_fluorescence = function(prefix) {
    
    #libraries
    try(library(dplyr), silent=FALSE)
    
    #read the files
    try((fam = read.csv(paste0(prefix, "_FAM.csv"), header=TRUE)), silent=TRUE)
    try((hex = read.csv(paste0(prefix, "_HEX.csv"), header=TRUE)), silent=TRUE)
    try((cy = read.csv(paste0(prefix, "_Cy5.csv"), header=TRUE)), silent=TRUE)
    try((tr = read.csv(paste0(prefix, "_Texas Red.csv"), header=TRUE)), silent=TRUE)
    
    #check if individual dfs exists and combine into single df
    ep=data.frame(X=character(), Well=character(), Fluor=character(), Target=character(), 
                  Content=character(), Sample=character(), End.RFU=numeric(), Call=character(), 
                  Sample.Type=character(), CallType=character(), IsControl=character())
    df_names = c("fam", "hex", "cy", "tr")
    for (df in df_names){
        if (exists(df)) {
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
        theme(legend.position="right", legend.title = element_blank())
}

