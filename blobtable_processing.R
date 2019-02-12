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
  run_date <- as.POSIXct(strptime(as.character(gsub("_", "", date_string)), format = "%Y%m%d%H%M"))

  temp_bt <<- read.csv(file = fi_name)
  temp_bt$RunDate = run_date
  temp_bt <<- temp_bt
  return(temp_bt)
}

#for testing
#date_string <- "20181128_0739"
#ttt <- as.POSIXct(strptime(as.character(gsub("_", "", date_string)), format = "%Y%m%d%H%M"))

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
  M_tempvol <- M %>% 
    filter(Library.Name != "amzi0503_b") %>% 
    filter(Library.Name != "fb_1130_1618")
  mtempvol <- sum(M_tempvol$Volume)
  M$Loading <- mtempvol
  

  for(i in 2:length(summary_table$File_num)){
    t <- read_bt_files(as.character(summary_table$File_num[i]))
    t$Category <- as.character(summary_table$Category[i])
    t$Filter_num <- as.integer(summary_table$Filter_num[i])
    t$Filter_punches <- as.integer(summary_table$Filter_punches[i])
    t$IOP = as.integer(summary_table$IOP[i])
    # need to sort out date conversion later this is a pain
    #M$AMZ_date = as.Date(as.character(summary_table$AMZ_date[1]), format = '%m/%d/%Y %h:%m')
    t$AMZ_date <- as.character(summary_table$AMZ_date[i])
    t_tempvol <- t %>% 
      filter(Library.Name != "amzi0503_b") %>% 
      filter(Library.Name != "fb_1130_1618")
    tempvol <- sum(t_tempvol$Volume)
    t$Loading <- tempvol
      
    Mnew <- rbind(M, t)
    M <- Mnew
    print(i)
  }
  M_t <<- M
  
}

make_massive_table(sum_table)

M_t_IS_2punch <- M_t %>% 
  filter(Library.Name == "amzi0503_b", Filter_punches == 2)

ggplot(M_t_IS_2punch, aes(x = Compound.Name, y = Volume))+
  geom_boxplot() +
  coord_flip()

ggplot(M_t_IS_2punch, aes(x = total_loading, y = Volume, color = Compound.Name))+
  geom_line()

# Determine_loading <- function(Blobtable_a, new_btname) {
#   temp <- Blobtable_a %>% 
#     filter(Library != "amzi0503_b") %>% 
#     filter(Library != "fb_") %>% 
#     
#   }


Only_matches <- function(raw_bt, bt_onlymatch, min_match_factor, min_vol) {
  onlymatch_bt_a <- raw_bt %>%
    filter(Volume > min_vol, Library.Match.Factor > min_match_factor) %>% 
    arrange(desc(Library.Match.Factor)) %>% 
    arrange(desc(Compound.Name)) %>% 
    arrange(desc(RunDate)) %>% 
    distinct(Compound.Name, RunDate, .keep_all = TRUE)
  
  assign(bt_onlymatch, onlymatch_bt_a, envir = globalenv())
  return(bt_onlymatch)
}

bt_402 <- M_t %>% 
  filter(Filter_num == 402)

Only_matches(bt_402, "bt_402_onlymatch", 750, 150)

bt_400 <- M_t %>% 
  filter(Filter_num == 400)

Only_matches(bt_400, "bt_400_onlymatch", 750, 150)


# min_vol <- 150
# min_match_factor <- 750
# 
# tt <- bt_402 %>%
#   filter(Volume > min_vol, Library.Match.Factor > min_match_factor) %>% 
#   arrange(desc(Library.Match.Factor)) %>% 
#   arrange(desc(Compound.Name)) %>% 
#   arrange(desc(RunDate)) %>% 
#   distinct(Compound.Name, .keep_all = TRUE)
# 
# mtcars
# 
# subset_high_hp <- function(full_table, name_only) {
#   only_highHP_a <- full_table %>% 
#     filter(hp > 200)
#   
#   assign(name_only, only_highHP_a, envir = globalenv())
#   
# }
# 
# subset_high_hp(mtcars, "mtcars_highhp")
# 
# 
#   