library(ggplot2)
library(dplyr)


bt1 <- read.csv(file = "C:\\Users\\GoldsteinGroup\\Desktop\\Research\\GoAmazon_filters\\blob_tables\\GCxGC_20181128_0739.h5_img01_Blob_Table.csv")

make_file_name <- function(date_string) {
  file_start <- "C:\\Users\\GoldsteinGroup\\Desktop\\Research\\GoAmazon_filters\\blob_tables\\GCxGC_"
  file_end <- ".h5_img01_Blob_Table.csv"
  full_name <- paste(file_start,date_string,file_end, sep = "")
  return(full_name)
  
}

make_file_name("20181128_0739")

read_bt_files <- function(date_string) {
  
  fi_name <- make_file_name(date_string)
  run_date <- strptime(as.character(gsub("_", "", date_string)), format = "%Y%m%d%H%M")

  temp_bt <<- read.csv(file = fi_name)
  temp_bt$RunDate = run_date
  temp_bt <<- temp_bt
  return(temp_bt)
}



read_bt_files("20181128_0739")

summary_table <- read.csv("C:\\Users\\GoldsteinGroup\\Desktop\\Research\\GoAmazon_filters\\blob_tables\\Blob_table_summary.csv")
sum_table <- read.csv("C:\\Users\\GoldsteinGroup\\Desktop\\Research\\GoAmazon_filters\\blob_tables\\Blob_table_summary.csv")


make_massive_table <- function(summary_table){
 
  M <- read_bt_files(as.character(summary_table$File_num[1]))
  M$Category <- as.character(summary_table$Category[1])
  M$Filter_num <- as.integer(summary_table$Filter_num[1])
  M$Filter_punches <- as.integer(summary_table$Filter_punches[1])
  M$IOP <- as.integer(summary_table$IOP[1])
  # need to sort out date conversion later this is a pain
  #M$AMZ_date = as.Date(as.character(summary_table$AMZ_date[1]), format = '%m/%d/%Y %h:%m')
  M$AMZ_date <- as.character(summary_table$AMZ_date[1])
  

  for(i in 2:length(summary_table$File_num)){
    t <- read_bt_files(as.character(summary_table$File_num[i]))
    t$Category <- as.character(summary_table$Category[i])
    t$Filter_num <- as.integer(summary_table$Filter_num[i])
    t$Filter_punches <- as.integer(summary_table$Filter_punches[i])
    t$IOP = as.integer(summary_table$IOP[i])
    # need to sort out date conversion later this is a pain
    #M$AMZ_date = as.Date(as.character(summary_table$AMZ_date[1]), format = '%m/%d/%Y %h:%m')
    t$AMZ_date <- as.character(summary_table$AMZ_date[i])
    Mnew <- rbind(M, t)
    M <- Mnew
    print(i)
  }
  M_t <<- M
  
}

make_massive_table(sum_table)
