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

do_corr_process <- function(data) {
  corr_processed_data <- data.frame(data$id,
                                    data$age,
                                    data$sex,
                                    data$chest_pain,
                                    data$resting_bp,
                                    data$cholestoral,
                                    data$heart_disease)
  headers <- c("id",
               "age",
               "sex",
               "chest_pain",
               "resting_bp",
               "cholestoral",
               "heart_disease")

  names(corr_processed_data) <- headers
  
  corr_matrix <- cor(corr_processed_data)
  
  save2img(corr_matrix, "/plots/first_part_corr_plot.png", "corr", "", "", "")
}

doProcessing <- function(data, mode) {
  age_cut <- cut(data$age, c(0, 17, 40, 65, max(data$age)))
  data$age_interval_without_label <- as.numeric(age_cut)
  data$age_interval_with_label <- age_cut
  
  resting_bp_cut <- cut(data$resting_bp, c(0, 120, 139, max(data$resting_bp)))
  data$resting_bp_interval_without_label <- as.numeric(resting_bp_cut)
  data$resting_bp_interval_with_label <- resting_bp_cut
  
  cholestoral_cut <- cut(data$cholestoral, c(0, 129, 200, 239, max(data$cholestoral)))
  data$cholestoral_interval_without_label <- as.numeric(cholestoral_cut)
  data$cholestoral_interval_with_label <- cholestoral_cut
  
  headers <- c("id",
               "age",
               "age_interval_without_label",
               "age_interval_with_label",
               "sex",
               "chest_pain",
               "resting_bp",
               "resting_bp_interval_without_label",
               "resting_bp_interval_with_label",
               "cholestoral",
               "cholestoral_interval_without_label",
               "cholestoral_interval_with_label")
  if (mode == "train") {
    # age plots
    save2img(data$age_interval_with_label, "/plots/age_interval_barplot.png", "bar", "Age Bar Plot", "Age Interval", "Count")
    save2img(data$age, "/plots/age_boxplot.png", "box", "Age Box Plot", "Age", "Range")
    
    # sex plot
    save2img(data$sex, "/plots/sex_barplot.png", "bar", "Sex Bar Plot", "Sex", "Count")
    
    # chest pain plot
    save2img(data$chest_pain, "/plots/chest_pain_barplot.png", "bar", "Chest Pain Bar Plot", "Chest Pain", "Count")
    
    # resting bp plots
    save2img(data$resting_bp_interval_with_label, "/plots/resting_bp_interval_barplot.png", "bar", "Resting BP Bar Plot", "Resting BP Interval", "Count")
    save2img(data$resting_bp, "/plots/resting_bp_boxplot.png", "box", "Resting BP Box Plot", "Resting BP", "Range")
    
    # cholestoral plots
    save2img(data$cholestoral_interval_with_label, "/plots/cholestoral_interval_barplot.png", "bar", "Cholestoral Bar Plot", "Cholestoral Interval", "Count")
    save2img(data$cholestoral, "/plots/cholestoral_boxplot.png", "box", "Cholestoral Box Plot", "Cholestoral", "Range") 
    
    do_corr_process(data)
    
    headers <- c(headers, "heart_disease")
    first_part_processed_data <- data.frame(data$id,
                                            data$age,
                                            data$age_interval_without_label,
                                            data$age_interval_with_label,
                                            data$sex,
                                            data$chest_pain,
                                            data$resting_bp,
                                            data$resting_bp_interval_without_label,
                                            data$resting_bp_interval_with_label,
                                            data$cholestoral,
                                            data$cholestoral_interval_without_label,
                                            data$cholestoral_interval_with_label,
                                            data$heart_disease)
  } else {
    first_part_processed_data <- data.frame(data$id,
                                            data$age,
                                            data$age_interval_without_label,
                                            data$age_interval_with_label,
                                            data$sex,
                                            data$chest_pain,
                                            data$resting_bp,
                                            data$resting_bp_interval_without_label,
                                            data$resting_bp_interval_with_label,
                                            data$cholestoral,
                                            data$cholestoral_interval_without_label,
                                            data$cholestoral_interval_with_label)
  }
  
  
  
  names(first_part_processed_data) <- headers
  
  write.table(first_part_processed_data,
              file = paste0(getwd(), "/data/first_part_processed_data_", mode, ".csv"),
              quote = F,
              sep = ",",
              row.names = F)
}


data <- read.csv(paste0(getwd(), "/data/train.csv"))
doProcessing(data, "train")

data <- read.csv(paste0(getwd(), "/data/test.csv"))
doProcessing(data, "test")
