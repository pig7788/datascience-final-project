# should install hash, ggcorrplot
library(hash)
library(tibble)

# define three part columns
index_column <- c("id")
answer_column <- c("heart_disease")
first_part <- c(index_column, "age", "sex", "chest_pain", "resting_bp", "cholestoral")
second_part <- c(index_column, "cholestoral", "high_sugar", "max_rate", "exercise_angina", "st_depression")
third_part <- c(index_column, "slope", "vessels", "thalium_scan")

# define new added column, make sure the length can divided by 2
first_added <- c("age", "resting_bp", "cholestoral")
second_added <- c("cholestoral", "max_rate", "st_depression")
third_added <- c("thalium_scan")

# define plot information, make sure the length is as same as part
plot_dir <- "/plots"
plot_kind <- list(a=c("box", "Range"), b=c("bar", "Count"))
first_plot <- c(0, 2, 1, 1, 2, 2)
second_plot <- c(0, 2, 1, 1, 2, 2)
third_plot <- c(0, 2, 2,2)

# define new csv file name
first_csv <- "/data/first_part_processed_data_"
second_csv <- "/data/second_part_processed_data_"
third_csv <- "/data/third_part_processed_data_"

# hash dictionary
params <- hash()
params[["1"]] <- list(id="first", csv=first_csv, part=first_part, added=first_added, answer=answer_column, plot=first_plot, ranger=list(a=c(17, 40, 65), b=c(120, 139), c=c(129, 200, 239)))
params[["2"]] <- list(id="second", csv=second_csv, part=second_part, added=second_added, answer=answer_column, plot=second_plot, ranger=list())
params[["3"]] <- list(id="third", missing_index=c(3,4), missing_value=list(a=c(123,147,220,228), b=c(63,201)), csv=third_csv, part=third_part, added=third_added, answer=answer_column, plot=third_plot, ranger=list())

#    define some function
save2img <- function(target, file_name, plot_type, main_title, x_title, y_title) {
    png(filename = paste0(getwd(), file_name))
    
    if (plot_type == "bar") {
        data_table <- table(target)
        barplot(data_table, main = main_title, xlab = x_title, ylab = y_title) 
    } else if (plot_type == "box") {
        boxplot(target, main = main_title, xlab = x_title, ylab = y_title)
    } else if (plot_type == "corr") {
        if (!require(ggcorrplot)) {
            install.packages(ggcorrplot)
        }
        library(ggcorrplot)
        corrplot <- ggcorrplot(target, hc.order = TRUE, type = "lower", lab = TRUE)
        print(corrplot)
    }
    
    dev.off()
}

do_corr_process <- function(data, headers, id) {
    corr_processed_data <- data
    names(corr_processed_data) <- headers
    corr_matrix <- cor(corr_processed_data)
    save2img(corr_matrix, paste(plot_dir, "/", id, "_part_corr_plot.png", sep=""), "corr", "", "", "")
}

get_headers <- function(data, params, mode, headers){
    for (i in seq(1, length(params$part), by=1)) {
        plot_num <- params$plot[[i]]
        index_bf <- which(colnames(data)==params$part[[i]])
        if (i==length(params$part)){
            if (plot_num > 1){
                index_af <- index_bf + 3
            }else {
                index_af <- index_bf
            }
        }else {
            index_af <- which(colnames(data)==params$part[[i+1]])
        }
        if (plot_num > 0){
            for (k in seq(index_bf, index_af, by=2)){
                for (j in 1:plot_num){
                    colname <- names(data)[[k]]
                    headers <- c(headers, colname)
                    if (mode == "train") {
                        type <- plot_kind[[j]][[1]]
                        main_title <- paste(colname, type, "plot", sep="")
                        x_title <- colname
                        y_title <- plot_kind[[j]][[2]]
                        file_name = paste(plot_dir, "/", colname, "_", type, "plot.png", sep="")
                        save2img(data[[colname]], file_name, type, main_title, x_title, y_title)
                    }
                }
            }
        }
    }
    headers <- unique(headers)
    return(headers)
}

doProcessing <- function(data, mode, params, part) {
  
    add_postfix <- c("_without_label", "_with_label")
    headers <- c(index_column)
    rearrange_ranger <- c()
        
    # use kmeans to cut bins
    if(length(params$ranger) == 0){
        message("ranger not implemented !")
    # use self defined bins to cut bins
    }else{
        message("ranger has been defined !")
    }
    
    # missing value
    if (length(params$missing_value > 0)){
        for (i in seq(1, length(params$missing_value), by=1)){
            index <- params$missing_index[i]
            name <- params$part[[index]]
            current_data <- data[,name]
            data[params$missing_value[[i]], name] <- median(current_data[which(!is.na(current_data))])
        }
    }
    
        
    # go through each numeric data by defined
    for (i in seq(1, length(params$added), by=1)) {
                
        # append min and max to each ranger
        # define rearrange data
        name = params$added[[i]]
        current_data <- data[[name]]
        
        if (part != 3) {
            rearrange_ranger <- c(min(current_data)-1)
            rearrange_ranger <- c(rearrange_ranger, params$ranger[[i]])
            rearrange_ranger <- c(rearrange_ranger, max(current_data)+1)
        }
        
        # rename added columns
        for(j in 1:2) {
            
            # get new colname
            current_postfix <- add_postfix[[j]]
            add_colname <- paste(name, current_postfix, sep="")
            
            if (part == 3) {
                # revalue
                current_coldata <- sapply(current_data, function(x) {ifelse(x==3, 1,x)})
            }else {
                # cut bins
                current_coldata <- cut(current_data, rearrange_ranger)
            }
            

            # add new data to specific place
            if (j==1){
                data <- add_column(data, !!(add_colname):=as.numeric(current_coldata), .after = grep(name, colnames(data)))
            }else{
                data <- add_column(data, !!(add_colname):=as.numeric(current_coldata), .after = grep(paste(name, add_postfix[[1]], sep=""), colnames(data)))
            }
            
        }
         
    }
    
    
    # plot 
    if (mode == "train") {
        
        # get headers and plot
        headers <- get_headers(data, params, mode, headers)
        headers <- c(headers, answer_column)

        # do correlation
        do_corr_process(data[headers], headers, params$id)
    } else {
        
        # get headers
        headers <- get_headers(data, params, mode, headers)
    }
    
    # ready to save new csv
    first_part_processed_data <- data.frame(data[headers])
    names(first_part_processed_data) <- headers
    write.table(first_part_processed_data,
                file = paste0(getwd(), params$csv, mode, ".csv"),
                quote = T,
                sep = ",",
                row.names = F)
}

# =====================================================================================
# define which part
part <- 1

# log info
message("Start Processing")

# process training data
data <- read.csv(paste0(getwd(), "/data/train.csv"))
doProcessing(data, "train", params[[as.character(part)]], part)

# process testing data
data <- read.csv(paste0(getwd(), "/data/test.csv"))
doProcessing(data, "test", params[[as.character(part)]], part)

# log info
message("Finish Processing")