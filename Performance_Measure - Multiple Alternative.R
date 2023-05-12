install.packages("tidycensus")
install.packages("dplyr")
install.packages("stringr")
install.packages("purrr")
install.packages("tidyr")
install.packages("readxl")
install.packages("writexl")
install.packages("data.table")
install.packages("parallel")

library("tidycensus")
library("dplyr")
library("stringr")
library("purrr")
library("tidyr")
library("readxl")
library("writexl")
library("data.table")
library("parallel")

###pathway
### Number of alternatives

read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- excel_sheets(filename)
  x <- parallel::mclapply(sheets, function(X) read_excel(filename, sheet = X, col_types = "numeric"))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  return(x)
}

setwd("~/Downloads/Lochmueller WFH/Parag")
all_file_paths <- list.files(path = "10-R/", full.names = T)
all_file_names <- list.files(path = "10-R/" )
trips <- lapply(all_file_paths, read_excel_allsheets)
names(trips) <- str_sub(all_file_names, end = -6)


### Name convention for multiple alternatives specify trip indexes using c(trips[[1]], trips[[6]])
trips_raw <- trips
trips <- trips_raw
number_of_alts <- 2
length_time <- c(trips[[5]])
length_time[[1]] <- NULL
trips <- length_time
trips[[5]] <- NULL

county_12_tazs <- read.csv(file.choose())
tazs <- sort(as.vector(county_12_tazs$TAZID))
print(tazs)

####  Analysing Distribution
trip_groups <- function(list_name){
  
  i_i <- function(d_frame) {
    i_i <- d_frame[d_frame$Rows %in% tazs, ]
    i_i_f <- i_i[ , colnames(i_i) %in% tazs]
    i_i_ff <- cbind(i_i[,1], i_i_f)
    i_i_sum <- colSums(i_i_ff, na.rm = T)
    i_i_sum.final <- sum(i_i_sum[-1])
    return(i_i_sum.final)
  }
  
  i_e <- function(d_frame){
    i_e <- d_frame[d_frame$Rows %in% tazs, ]
    i_e_f <- i_e[ , !(colnames(i_e) %in% tazs)]
    i_e_sum <- colSums(i_e_f, na.rm = T)
    i_e_sum.final <- sum(i_e_sum[-1])
    return(i_e_sum.final)
  }
  
  e_i <- function(d_frame) {
    e_i <- d_frame[!(d_frame$Rows %in% tazs), ]
    e_i_f <- e_i[ , colnames(e_i) %in% tazs]
    e_i_ff <- cbind(e_i[, 1], e_i_f)
    e_i_sum <- colSums(e_i_ff, na.rm = T)
    e_i_sum.final <- sum(e_i_sum[-1])
    return(e_i_sum.final)
  }
  
  e_e <- function(d_frame){
    e_e <- d_frame[!(d_frame$Rows %in% tazs), ]
    e_e_f <- e_e[ , !(colnames(e_e) %in% tazs)]
    e_e_f_sum <- colSums(e_e_f, na.rm = T)
    e_e_f_sum.final <- sum(e_e_f_sum[-1])
    return(e_e_f_sum.final)
  }
  
  x <- trips     ##making an empty list
  
  for( i in 1:length(list_name)){
    print(i)
    for(j in 1:2) {
      ii = i_i(as.data.frame(list_name[[i]][[j]] ))
      ie = i_e(as.data.frame(list_name[[i]][[j]]))
      ei = e_i(as.data.frame(list_name[[i]][[j]]))
      ee = e_e(as.data.frame(list_name[[i]][[j]]))
      x[[i]][[j]] <- data.frame(i.i = ii, i.e = ie, e.i = ei, e.e = ee, all = sum(ii,ie,ei,ee))
    }
  }
  names(x) <- names(list_name)
  return(x)
  
}

#### cleaning data
cleaning_list <- function(list_name) {
    list_name_df <- as.data.frame(list_name)
    list_name_df <- gather(list_name_df, "Variable_Name", "Value")
    list_name_df$Zone <- rep(c("I_I", "I_E", "E_I", "E_E", "All"), times = nrow(list_name_df)/5 )
    list_name_df$Mode <- rep(c("Auto", "MUT"), each = 5, times = nrow(list_name_df)/ 10 )
   list_name_df$TOD <- rep(c("AM", "MD", "OP", "PM"), each = 10, times = nrow(list_name_df)/40)
   list_name_df$Alternate_Name <- str_sub(list_name_df$Variable_Name, start = 1, end = 3)
   list_name_spread <- spread(list_name_df[,-1], key = Zone, value = Value)
   auto_mode <- list_name_spread[list_name_spread$Mode == "Auto", c(3,2,1,8,7,6,5,4)]
   mut_mode <- list_name_spread[list_name_spread$Mode == "MUT", c(3,2,1,8,7,6,5,4)]
    return(list(Auto = auto_mode, MUT = mut_mode))
  }

## converting trips into matrix
for( i in 1:length(trips)){
  for(j in 1:2){
    trips[[i]][[j]] <- as.matrix(trips[[i]][[j]])
  }
}

#### calculating vmt ########
calculating_vmt <- function(trips, number_of_alternatives = number_of_alts){
  vmt <- trips
  index_length <- rep(c(1:number_of_alternatives), each = 4)
  for(i in 1: length(vmt)) {
    print(i)
    for(j in 1:2) {
      vmt[[i]][[j]][ , 2:ncol(vmt[[i]][[j]] )] <- as.matrix( trips[[i]][[j]][ , -1 ] ) * 
        as.matrix( length_time[[ index_length[i] ]][ ,-1] )
      
    }
  }
  return(vmt)
}

### calculating vht #################
calculating_vht <- function(length_time_f, trips_f, number_of_alternatives = number_of_alts){
  index_length_i <- 1:(4*number_of_alternatives)
  vht <- trips_f
  k <- 0
    for(j in 2:5) {
      k <- k + 1
      vht[[index_length_i[k] ]][[1]][, 2:ncol(vht[[ index_length_i[k] ]] [[1]] )] <- 
        as.matrix( trips_f[[ index_length_i[k] ]] [[1]] [ , -1 ] ) * as.matrix(length_time_f[[j]][ , -1]) /60
      vht[[index_length_i[k] ]][[2]][, 2:ncol(vht[[ index_length_i[k] ]] [[2]] )] <-
        as.matrix( trips_f[[ index_length_i[k] ]] [[2]] [ , -1 ] ) * as.matrix(length_time_f[[j]][ , -1] ) / 60
  }
  return(vht)
}

#### calculating e_e_f ###########
calculating_e_e_f <- function(vht, length_time_f, number_of_alternatives = number_of_alts){
  index_length_i <- 1:(4 * number_of_alternatives)
  e_e_f <- vht
  k <- 0
    for(j in 2:5) {
      k <- k + 1
      e_e_f[[ index_length_i[k] ]][[1]][ , 2:ncol(e_e_f[[ index_length_i[k] ]] [[1]] )] <- 
        as.matrix( vht[[ index_length_i[k] ]] [[1]] [ , -1 ] ) * 60 / as.matrix(length_time_f[[j]][ , -1]) 
      e_e_f[[ index_length_i[k] ]][[2]][ , 2:ncol(e_e_f[[ index_length_i[k] ]] [[2]] )] <- 
        as.matrix( vht[[ index_length_i[k] ]] [[2]] [ , -1 ] ) * 60 / as.matrix(length_time_f[[j]][ , -1] )
  }
  return(e_e_f)
}

###calls
trips_ff <- cleaning_list(trip_groups(trips))
vmt_f <- cleaning_list(trip_groups(calculating_vmt(trips)))
vht_f <- cleaning_list(trip_groups(calculating_vht(length_time, trips)))
vht_input <- calculating_vht(length_time, trips)
e_e_f_refined <- cleaning_list(trip_groups(calculating_e_e_f(vht_input, length_time)))

#Write excel
write_xlsx(trips_ff, "Trips.xlsx")
write_xlsx(vmt_f, "VMT.xlsx")
write_xlsx(vht_f, "VHT.xlsx")
write_xlsx(e_e_f_refined, "EEF.xlsx")

