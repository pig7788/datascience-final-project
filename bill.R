# should install hash, ggcorrplot
library(hash)
library(tibble)
library(infotheo)
library(stringr)

# define three part columns
index_column <- c("id")
answer_column <- c("heart_disease")
first_part <- c(index_column, "age", "sex", "chest_pain", "resting_bp", "cholestoral")
second_part <- c(index_column, "cholestoral", "high_sugar", "ecg", "max_rate", "exercise_angina", "st_depression")
third_part <- c(index_column, "slope", "vessels", "thalium_scan")

# define new added column, make sure the length can divided by 2 (numeric data)
first_added <- c("age", "resting_bp", "cholestoral")
second_added <- c("cholestoral", "max_rate", "st_depression")
third_added <- c("thalium_scan")

# define hypothesis column (categorical data)
alpha <- 0.05
first_hypothesis <- c()
second_hypothesis <- c("high_sugar", "ecg", "exercise_angina")
third_hypothesis <- c()

# define plot information, make sure the length is as same as part
plot_kind <- list(a=c("Box", "Box Plot", "Range"), b=c("Bar", "", "Count"))

# define new csv file name
first_csv <- "/data/first_part/first_part_processed_data_"
second_csv <- "/data/second_part/second_part_processed_data_"
third_csv <- "/data/third_part/third_part_processed_data_"

# hash dictionary
params <- hash()
params[["1"]] <- list(id="first", csv=first_csv, part=first_part, added=first_added, answer=answer_column, ranger=list(a=c(17, 40, 65), b=c(120, 139), c=c(129, 200, 239)))
params[["2"]] <- list(id="second", hypothesis=second_hypothesis, csv=second_csv, part=second_part, added=second_added, answer=answer_column, ranger=list())
params[["3"]] <- list(id="third",  csv=third_csv, part=third_part, added=third_added, answer=answer_column, ranger=list())

# define some function
CapStr <- function(y) {
    c <- strsplit(y, " ")[[1]]
    string <- paste(toupper(substring(c, 1,1)), substring(c, 2), sep="", collapse=" ")
    return(string)
}

save2img <- function(target, file_name, plot_type, main_title, x_title, y_title) {
    png(filename = paste0(getwd(), file_name))
    
    if (plot_type == "Bar") {
        data_table <- table(target)
        barplot(data_table, main = main_title, xlab = x_title, ylab = y_title) 
    } else if (plot_type == "Box") {
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
    total_col <- names(data)
    for (i in seq(2, length(total_col), by=1)) {
        colname <- total_col[[i]]
        current_data <- data[[colname]]
        if (!str_detect(colname, "without_label")){
            headers <- c(headers, colname)
        }
        for (j in seq(1, length(plot_kind), by=1)){
            colname <- str_replace_all(colname, "_", " ")
            Cap_colname <- CapStr(colname)
            if (mode == "train") {
                type <- plot_kind[[j]][[1]]
                main_title <- paste(Cap_colname, type, "Plot", sep=" ")
                x_title <- ""
                if (str_detect(Cap_colname, "With Label")){
                    if (j==2){
                        x_title <- str_replace_all(Cap_colname, "With Label", "Range Interval")
                    } else {
                        x_title <- paste(Cap_colname, plot_kind[[j]][[2]], "", sep=" ")
                    }
                }else if (str_detect(Cap_colname, "Without Label")){
                    if (j==2){
                        x_title <- str_replace_all(Cap_colname, "Without Label", "Label")
                    }else {
                        x_title <- paste(Cap_colname, plot_kind[[j]][[2]], sep=" ")
                    }
                }else {
                    x_title <- paste(Cap_colname, plot_kind[[j]][[2]], sep=" ")
                }
                y_title <- plot_kind[[j]][[3]]
                file_name = paste(plot_dir, "/", colname, "_", type, "plot.png", sep="")
                save2img(current_data, file_name, type, main_title, x_title, y_title)
            }
        }
    }
    headers <- unique(headers)
    return(headers)
}

remove_outliers <- function(col_data){
    rge <- 0.5
    Q <- quantile(col_data, probs=c(.25, .75), na.rm = FALSE)
    iqr <- IQR(col_data)
    up <-  Q[2] + rge*iqr # Upper Range  
    low <- Q[1] - rge*iqr # Lower Range
    
    # get index
    lower_idx <- col_data < low
    upper_idx <- col_data > up
    inlier_idx <- !(lower_idx & col_data > upper_idx)
    
    # get inliers
    min_inlier <- min(col_data[inlier_idx])
    max_inlier <- max(col_data[inlier_idx])
    
    # reassign to min and max value
    col_data[lower_idx] <- min_inlier
    col_data[upper_idx] <- max_inlier
    
    # all_outlier idx
    lower_idx <- which(lower_idx)
    upper_idx <- which(upper_idx)
    all_outlier <- c(lower_idx, upper_idx)

    return(list(data=col_data, outliers=all_outlier))
}

print_hypothesis <- function(hypothesis_data, colname){
    hypothesis_result <- chisq.test(hypothesis_data) 
    
    if (hypothesis_result$p.value <= alpha){
        cat('Variables', colname, ': are associated (reject H0)\n')
    }else {
        cat('Variables', colname, ': are not associated(fail to reject H0)\n')
    }
}

kmeans_ranger <- function(data, kmeans_ranger){
    ranger <- list()
    Sum_of_squared_thre = 0.5
    for (i in kmeans_ranger){
        current_data <- data[i]
        best_k <- 0
        last_dst <- 0
        center <- c()
        for (j in 1:15){
            kmeans.cluster <- kmeans(current_data, centers=j) 
            
            # 用組內平方和去看距離
            distance <- kmeans.cluster$tot.withinss
            if (j == 1){
                last_dst <- distance
                best_k <- j
                center <- kmeans.cluster$centers
            }else {
                if(abs(last_dst - distance)/last_dst >= Sum_of_squared_thre && last_dst - distance > 0) {
                    best_k <- j
                    center <- kmeans.cluster$centers
                }
                last_dst <- distance
            }
        }
        center <- sort(center)
        ranger[[i]] <- center
    }
    return(ranger)
}

doProcessing <- function(data, mode, params, part) {
  
    add_postfix <- c("_with_label", "_without_label")
    headers <- c()
    current_header <- c()
    rearrange_ranger <- c()
    
    # define current header
    for (i in params$part) {
        if (i != "id") {
            if (i %in% params$added){
                current_header <- c(current_header, i)
                current_header <- c(current_header, paste(i, add_postfix[[1]], sep=""))
                current_header <- c(current_header, paste(i, add_postfix[[2]], sep=""))
            }else {
                current_header <- c(current_header, i)
            }
        }
    }
    
    # missing value
    for (i in params$part){
        current_data <- data[,i]
        na_index <- which(is.na(current_data))
        data[na_index, i] <- median(current_data[which(!is.na(current_data))])
    }
    
    outliers <- c()
    
    # detect and remove outlier
    for (i in params$added){
        current_data <- data[[i]]
        result <- remove_outliers(current_data)
        data[[i]] <- result$data
        outliers <- c(outliers, result$outliers)
    }
    
    # remove common outliers' row
    
    n_occur <- data.frame(table(outliers))
    outliers_common_idx <- n_occur$Freq > 1
    if (length(n_occur[outliers_common_idx,])!=0){
        outliers_common <- n_occur[outliers_common_idx,]$outliers
        outliers_common <- as.numeric(as.character(outliers_common))
        data<- data[-outliers_common, ]
    }
    
    # use kmeans to cut bins
    if(length(params$ranger) == 0){
        if (part==2){
            cat("ranger not implemented, use Kmeans to define bins !\n")
            params$ranger <- kmeans_ranger(data, params$added)
            cat("Bins are defined as \n")
            print(params$ranger)
        }else {
            cat("ranger do not need to be defined !\n\n")
        }
        # use self defined bins to cut bins
    }else{
        cat("ranger has been defined !\n\n")
    }
    
    # hypothesis
    if (length(params$hypothesis > 0) && mode == "train"){
        
        cat("Check relationship between each categorical data and answer... \n\n")
        for (i in params$hypothesis){
            params$hypothesis <- c(i, answer_column)
            hypothesis_data <- data[params$hypothesis]
            hypothesis_table <- data.frame(table(hypothesis_data))
            hypothesis_freq <- as.numeric(as.character((hypothesis_table$Freq)))
            hypothesis_matrix <- matrix(hypothesis_freq, nrow=2)
            
            # chi square
            print_hypothesis(hypothesis_matrix, i)
            
            # mutual information
            MI <- mutinformation(hypothesis_data[1], hypothesis_data[2])
            cat("Variable", i, ": Mutual Information Value is ", MI, "\n\n")
            
        }
        cat("Finish checking !\n\n")
    }
        
    # go through each numeric data by defined
    for (i in seq(1, length(params$added), by=1)) {
                
        # append min and max to each ranger
        # define rearrange data
        name = params$added[[i]]
        current_data <- data[[name]]
        
        # part three data are not using cut bins
        if (part != 3) {
            ranger <- params$ranger[[i]]
            rearrange_ranger <- c(min(current_data)-1)
            rearrange_ranger <- c(rearrange_ranger, ranger)
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
        headers <- get_headers(data[current_header], params, mode, headers)
        headers <- c(headers, answer_column)

        # do correlation
        do_corr_process(data[headers], headers, params$id)
    } else {
        
        # get headers
        headers <- get_headers(data[current_header], params, mode, headers)
    }
    
    # ready to save new csv
    # add index column
    headers <- c(index_column, headers)
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
part <- 3
plot_dir <- ""
if (part==1){
    plot_dir <- "/plots/first_part"
}else if (part==2){
    plot_dir <- "/plots/second_part"
}else {
    plot_dir <- "/plots/third_part"
}

# log info
cat("Start Processing Training\n\n")

# process training data
data <- read.csv(paste0(getwd(), "/data/train.csv"))
doProcessing(data, "train", params[[as.character(part)]], part)

cat("Finish Processing Training\n\n")
cat("Start Processing Testing\n\n")

# process testing data
data <- read.csv(paste0(getwd(), "/data/test.csv"))
doProcessing(data, "test", params[[as.character(part)]], part)

# log info
cat("Finish Processing Testing")