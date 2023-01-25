# ------------------------------------------------------------------------------
# Function Archive                                                           ---
# ------------------------------------------------------------------------------

#...............................................................................
fun_achive                  <- function() {
#...............................................................................
  dt_archive <- 
    dt_file_table[name_char %like% 'W',] %>%
    split(., by = c("name_num", "name")) %>%
    map(., fun_0000_archive_main)
#...............................................................................
}
# ------------------------------------------------------------------------------

#...............................................................................
fun_0000_archive_main       <- function(nm){
#...............................................................................
  fun_1000_download_zip(nm)
  fun_2000_archive_mungle(ALLNEW)
  fun_3000_strike_processing(dx_blob)
  fun_4000_bfly_main(dx_blob)
#...............................................................................
}
#...............................................................................

# ------------------------------------------------------------------------------
fun_1000_download_zip       <- function(nm){
# ------------------------------------------------------------------------------
  
#...............................................................................
#  browser()
#...............................................................................

# ------------------------------------------------------------------------------
#  download zip file(s)
#  zip_file_path <- paste0(here::here("zip//"))    
# ------------------------------------------------------------------------------
  cd$download_file(paste0(dir_ms365_zip, nm[,1]),
                   paste0(zip_file_path, nm[,1]),
                   overwrite = TRUE)
# ------------------------------------------------------------------------------
# chatGPT extract the csv files from the zip file and return a list of file name
# ------------------------------------------------------------------------------
  utils::unzip(
    paste0(zip_file_path, nm[,1]),
    list      = FALSE,
    overwrite = TRUE,
    exdir     = here::here("data")
#   exdir     = valueline_path_data,
  )
# ------------------------------------------------------------------------------
  dt_unzipped_files <<- utils::unzip(
    paste0(zip_file_path, nm[,1]),
    list      = TRUE,
    overwrite = TRUE,
    exdir     = here::here("data")
#   exdir     = valueline_path_data,
  )$Name
# ------------------------------------------------------------------------------
#  return( dt_unzipped_files )
# ------------------------------------------------------------------------------
# dt_file_table[nrow(dt_file_table) + 1, "file_name"] <- file

#...............................................................................
#  browser()
#...............................................................................

  file.remove(here::here('zip', nm[,1]))  

  # g[[paste0(nm[,2])]] <- read_csv(here::here("data", dt_unzipped_files))
  g[[paste0(nm[,2])]] <<- fread(here::here("data", dt_unzipped_files))
  
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# loop through the list of file names and read each csv file into a data.table
# ------------------------------------------------------------------------------
# dt_list <- lapply(csv_files, function(x) {
#   fread(unzip(zip_file, files = x))
# })
# 
# date_run <- read.csv(paste0(here::here("ml//"), str_extract(nm, "[a-zA-Z]+"), ".CSV"))[3,2]
# dt_file_table[name == nm, date_run := date_run]
# ------------------------------------------------------------------------------
# file_table[nrow(file_table) + 1, "date_run"] <- date_run
# test <- setDT(data.table::fread(paste0(here::here("ml//"), str_extract(nm, "[a-zA-Z]+"), ".CSV")))[3,2]
# test <- setDT(data.table::fread(paste0(here::here("ml//"), str_extract(nm, "[a-zA-Z]+"), ".CSV")))
# ------------------------------------------------------------------------------  
}
# ------------------------------------------------------------------------------

#...............................................................................
fun_2000_archive_mungle     <- function(ALLNEW){
#...............................................................................

# ------------------------------------------------------------------------------
## Step 00.00 Processing                                                     ---
# ------------------------------------------------------------------------------

################################################################################
## Step 00.01 dynamically create dataframe      https://tinyurl.com/y3adrqwa ###
## grep with multiple patterns                  https://tinyurl.com/8kpnktnm ###
################################################################################

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
#  create a template to rbind the dataframes
# ------------------------------------------------------------------------------
  date_run <- ALLNEW[4,2] 
# ------------------------------------------------------------------------------
  dx_blob  <- ALLNEW[12,]      %>%  row_to_names(row_number = 1)
  ALLNEW   <- tail(ALLNEW,-11) %>%  row_to_names(row_number = 1)
  dx_blob  <- rbind(dx_blob, ALLNEW)
# ------------------------------------------------------------------------------
  dx_blob$date_run        <- as.Date(date_run$V2, format('%m/%d/%Y'))
# ------------------------------------------------------------------------------
  dx_blob[,c(2,6,9,24:25,27:28,32:35,37:46)] <-
    lapply(dx_blob[,c(2,6,9,24:25,27:28,32:35,37:46)],
      function(x) parse_number(x))
# ------------------------------------------------------------------------------
  dx_blob[,c(3:4,8,12,14,16:23,26,29:31,36,47:54)] <-
    lapply(dx_blob[,c(3:4,8,12,14,16:23,26,29:31,36,47:54)],
      function(x) parse_number(x))
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# remove redundant entries                                                   ---
# ------------------------------------------------------------------------------
dx_blob <<- setorder(
  unique(dx_blob[,-"Record.Number"]), OPTKR)[
    , Record.Number := .I][
      ,c(1:13,55,14:54)]
# ------------------------------------------------------------------------------
  dx_blob$EXPDAY        <- as.Date(dx_blob$EXPDAY, format('%m/%d/%Y'))
  dx_blob               <<- setorder(dx_blob, OPTKR)
# ------------------------------------------------------------------------------

################################################################################
# Step 00.02 table creation                                                  ###
################################################################################
dx_company              <- as.data.table(distinct(dx_blob[,1]))
# ------------------------------------------------------------------------------
dx_date_exp             <- data.table::setorder(as.data.table(distinct(dx_blob[,15])),EXPDAY)
# dx_date_exp$day         <- weekdays(dx_date_exp$EXPDAY)
dx_date_exp$EXPDAY      <- as.Date(dx_date_exp$EXPDAY, format('%m/%d/%Y'))
dx_date_exp$day         <- data.table::wday(dx_date_exp$EXPDAY)
dx_date_exp$week        <- data.table::week(dx_date_exp$EXPDAY)
# dx_date_exp$diff        <- dx_date_exp$EXPDAY - Sys.Date()
dx_date_exp$diff        <- dx_date_exp$EXPDAY - as.Date(date_run$V2, format('%m/%d/%Y'))
dx_date_exp             <<- setorder(dx_date_exp, EXPDAY)
# ------------------------------------------------------------------------------
dx_date_exp_mth         <- data.table::setorder(
  as.data.table(dx_date_exp[day == 6 & week == 3, ][
    , day_diff := diff - shift(diff)], EXPDAY)
)
# ------------------------------------------------------------------------------
dt_time                 <- timeSequence(from = "2020-01-01",
                                        to = "2030-12-31",
                                        by = "month")
dt_date_exp_mth         <- as.data.table(
  timeNthNdayInMonth(dt_time,
                     nday = 5,
                     nth = 3,
                     format = "%Y-%m-%d")
)
# ------------------------------------------------------------------------------
names(dt_date_exp_mth)[1]   <- "EXPDAY"
dt_date_exp_mth$EXPDAY      <- as.Date(dt_date_exp_mth$EXPDAY)
dt_date_exp_mth$day         <- data.table::wday(dt_date_exp_mth$EXPDAY)
dt_date_exp_mth$week        <- data.table::week(dt_date_exp_mth$EXPDAY)
dt_date_exp_mth$diff        <- dt_date_exp_mth$EXPDAY - as.Date(date_run$V2, format('%m/%d/%Y'))
dt_date_exp_mth[, next_exp := shift(EXPDAY, type = "lead")]
dt_date_exp_mth             <<- setorder(dt_date_exp_mth, EXPDAY)
# ------------------------------------------------------------------------------

#...............................................................................
browser()
#...............................................................................

# ------------------------------------------------------------------------------
setkey(dx_blob,          EXPDAY)
setkey(dx_date_exp,      EXPDAY)
setkey(dx_date_exp_mth,  EXPDAY)
# ------------------------------------------------------------------------------

################################################################################
# Step 00.03 Week number of the month           https://tinyurl.com/2zejur23 ###
################################################################################
x <- ymd(dx_date_exp$EXPDAY)
week(x) - week(floor_date(x, unit = "months")) + 1
# ------------------------------------------------------------------------------
DT <- data.table(days = seq(as.Date("0-01-01"), Sys.Date(), "days"))
# compute the week of the month and account for the '5th week' case
DT[, week := ifelse( ceiling(mday(days)/7) == 5, 4, ceiling(mday(days)/7) )]
dx_future <- as.data.table(seq(as.Date(Sys.Date()),by = "day", length.out = 3650))
names(dx_future)[1] <- "date"
setkey(dx_future,date)
# ------------------------------------------------------------------------------
dx_exp                  <- as.data.table(inner_join(dx_date_exp, dx_future, by = c("EXPDAY" = "date")))
# ------------------------------------------------------------------------------
dx_industry             <<- data.table::setorder(as.data.table(distinct(dx_blob[,7])),INDUST)
dx_tech_rank            <<- data.table::setorder(as.data.table(distinct(dx_blob[,4])),TechRank)
dx_strike               <<- data.table::setorder(as.data.table(distinct(dx_blob[,12])),STRIKE)
dx_ticker               <<- as.data.table(distinct(dx_blob[,c(10,8)]))
################################################################################
# Step 00.05 R: How to get the Week number of the month                      ###
#                                               https://tinyurl.com/3bk8x28h ###
################################################################################
weekdays(dx_date_exp$EXPDAY)
w <- dx_date_exp[, wk := isoweek(EXPDAY)]
a <- dx_date_exp[, week := ifelse( ceiling(mday(EXPDAY)/7) == 5, 4, ceiling(mday(EXPDAY)/7) )]
################################################################################
## Step 00.99: VERSION HISTORY                                               ###
################################################################################
a00.version             <- "1.0.0"
a00.ModDate             <- as.Date("2023-01-20")
# ------------------------------------------------------------------------------
# 2020.07.10 - v.1.0.0
#  1st release
# ------------------------------------------------------------------------------
}
#...............................................................................

#...............................................................................
fun_3000_strike_processing  <- function(dx_blob){
#...............................................................................
  
#...............................................................................
# browser()
#...............................................................................
  
# ------------------------------------------------------------------------------
  dx_tkr_stk             <- dx_blob[dt_date_exp_mth[between(diff, 30,59),][1,1]]
# ------------------------------------------------------------------------------
  setkey(dx_tkr_stk,    TKR)
  setkey(dx_ticker,     TKR)
# ------------------------------------------------------------------------------
  s_minus_0              <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N],     by = .(TKR)]
  s_minus_1              <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N - 1], by = .(TKR)]
  s_plus_0               <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, head(.SD, 1),   by = .(TKR)]
  s_plus_1               <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, .SD[2],         by = .(TKR)]
# ------------------------------------------------------------------------------
  dx_s_minus_0           <<- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N],     by = .(TKR)]
  dx_s_minus_1           <<- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N - 1], by = .(TKR)]
  dx_s_minus_2           <<- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N - 2], by = .(TKR)]
# ------------------------------------------------------------------------------
  dx_s_plus_0            <<- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, head(.SD, 1),   by = .(TKR)][,c(1,3)]
  dx_s_plus_1            <<- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, .SD[2],         by = .(TKR)][,c(1,3)]
  dx_s_plus_2            <<- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, .SD[3],         by = .(TKR)][,c(1,3)]
# ------------------------------------------------------------------------------
  
################################################################################
# Step 01.01 clean the tables                                               ###
################################################################################
  dx_s_list <- grep("^dx_s_", ls(), value = TRUE)
# ------------------------------------------------------------------------------
#  lapply(dx_s_list, setnames, old = "V1", new = "STRIKE")
# ------------------------------------------------------------------------------
  lapply(dx_s_list, function(nm) {
    df  <- get(nm)
#   g[[paste0("dt", nm, "_")]] <- nm
    setnames(df, old = "V1", new = "STRIKE")
    setkey(df, "TKR")
    }
  )
# ------------------------------------------------------------------------------
  browser()
# ------------------------------------------------------------------------------
  names(dx_s_minus_0)  [1:2]  <- c("TKR", "STRIKE")
  names(dx_s_minus_1)  [1:2]  <- c("TKR", "STRIKE")
  names(dx_s_minus_2)  [1:2]  <- c("TKR", "STRIKE")
# ------------------------------------------------------------------------------
  names(dx_s_plus_0)   [1:2]  <- c("TKR", "STRIKE")
  names(dx_s_plus_1)   [1:2]  <- c("TKR", "STRIKE")
  names(dx_s_plus_2)   [1:2]  <- c("TKR", "STRIKE")
# ------------------------------------------------------------------------------
  setkey(dx_s_minus_0, TKR)
  setkey(dx_s_minus_1, TKR)
  setkey(dx_s_minus_2, TKR)
# ------------------------------------------------------------------------------
  setkey(dx_s_plus_0,  TKR)
  setkey(dx_s_plus_1,  TKR)
  setkey(dx_s_plus_2,  TKR)
# ------------------------------------------------------------------------------
  setkey(dx_ticker,    TKR)
# ------------------------------------------------------------------------------
  
#...............................................................................
browser()
#...............................................................................
  
}
#...............................................................................

#...............................................................................
fun_4000_bfly_main    <- function(dx_blob) {

#...............................................................................
# browser()
#...............................................................................

  butterfly <- dx_blob[
    #  dx_date_exp[day == 6 & week == 3, ][1, 1]
    dt_date_exp_mth[between(diff, 30, 60)][1,1]  
    #  dx_date_exp_mth[2,1]
  ][
    order(OPTKR, -date_run),
  ] %>%
    split(., by = c("TKR")) %>%
    map(., fun_4100_bfly_processing)
  
#...............................................................................
# browser()
#...............................................................................

#...............................................................................
}
#...............................................................................

fun_4100_bfly_processing    <- function(dx_blob) {

# ------------------------------------------------------------------------------
# Function Butterfly                                                         ---
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# create a stub template dataframe (initial 1 time setup)
# https://tinyurl.com/wda2yyuh
# ------------------------------------------------------------------------------
if (z == TRUE) {
# g[[paste0("dt_bfly")]] <<- dx_blob[.0, 0]
  g[[paste0("dt_bfly")]]  <<- blob[.0,0]               # stub template dataframe 
# g[[paste0("dt_bfly")]]  <<- blob[.0]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, TKR        := unique(blob[, "TKR"])]
  g[[paste0("dt_bfly")]][, CMRK       := unique(blob[, "CMRK"])]
  g[[paste0("dt_bfly")]][, TechRank   := unique(blob[, "TechRank"])]
# g[[paste0("dt_bfly")]][, X.vCM      := unique(blob[, "X.vCM"])]
  g[[paste0("dt_bfly")]][, X.vCM      := unique(blob[, 5])]                     # `^vCM`
  g[[paste0("dt_bfly")]][, CMPRICE    := unique(blob[, "CMPRICE"])]  
  g[[paste0("dt_bfly")]][, HISTVO     := unique(blob[, "HISTVO"])] 
  g[[paste0("dt_bfly")]][, VOLF       := unique(blob[, "VOLF"])]
  g[[paste0("dt_bfly")]][, vol_diff   := ((dt_bfly[.N,7] - dt_bfly[.N,6])/dt_bfly[.N,6])*100]
  g[[paste0("dt_bfly")]][, EXPDAY     := unique(blob[, "EXPDAY"])]
# ------------------------------------------------------------------------------
# reference by column number instead of column name
# ------------------------------------------------------------------------------  
  g[[paste0("dt_bfly")]][, id_minus_1 := blob[blob[[11]] == 'P'][dx_s_minus_1, on = .c("TKR", "STRIKE" ), nomatch = 0][,13]]  # OPTKR
#  g[[paste0("dt_bfly")]][, id_minus_1 := blob[C.P == "P", ][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]] # OPTKR
  g[[paste0("dt_bfly")]][, id_minus_0 := blob[C.P == "C", ][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]  
  g[[paste0("dt_bfly")]][, id_plus_0  := blob[C.P == "P", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]
  g[[paste0("dt_bfly")]][, id_plus_1  := blob[C.P == "C", ][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, cost       := 
   -blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19] + # ASK
    blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] + # BID
    blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] -
    blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]]
# ------------------------------------------------------------------------------
# Individual components of cost
# ------------------------------------------------------------------------------
  # g[[paste0("dt_bfly")]][, s_minus_1  := unique(blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19])]
  # g[[paste0("dt_bfly")]][, s_minus_0  := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18])]
  # g[[paste0("dt_bfly")]][, s_plus_0   := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18])]
  # g[[paste0("dt_bfly")]][, s_plus_1   := unique(blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19])]
#...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_oi      := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"])]
  g[[paste0("dt_bfly")]][, s_plus_0_oi       := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"])]
  #...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_iotm    := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"])]
  g[[paste0("dt_bfly")]][, s_plus_0_iotm     := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"])]
#...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_un_ov   := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"])]
  g[[paste0("dt_bfly")]][, s_plus_0_un_ov    := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"])]
# #...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_prb_stk := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"])]
  g[[paste0("dt_bfly")]][, s_plus_0_prb_stk  := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"])]
  # #...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_pct_dbl := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"])]
  g[[paste0("dt_bfly")]][, s_plus_0_pct_dbl  := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"])]  
# #...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_rowrt   := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"])]
  g[[paste0("dt_bfly")]][, s_plus_0_rowrt    := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"])]
# #...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_delta   := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"])]
  g[[paste0("dt_bfly")]][, s_plus_0_delta    := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"])]
  
  g[[paste0("dt_bfly")]][, s_minus_0_gamma   := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"])]
  g[[paste0("dt_bfly")]][, s_plus_0_gamma    := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"])]  
  
  g[[paste0("dt_bfly")]][, s_minus_0_rho     := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"])]
  g[[paste0("dt_bfly")]][, s_plus_0_rho      := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"])]  
  
  g[[paste0("dt_bfly")]][, s_minus_0_theta   := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"])]
  g[[paste0("dt_bfly")]][, s_plus_0_theta    := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"])]
  
  g[[paste0("dt_bfly")]][, s_minus_0_vega    := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"])]
  g[[paste0("dt_bfly")]][, s_plus_0_vega     := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"])]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, date_run   := unique(blob[, "date_run"])]
# g[[paste0("dt_bfly")]][, id         := .I]
# ------------------------------------------------------------------------------
  z <<- FALSE
# ------------------------------------------------------------------------------
} else {
#...............................................................................  
# browser()
#...............................................................................  
  g[[paste0("dt_bfly")]] <<- rbind(g[[paste0("dt_bfly")]],
  data.table(
# ------------------------------------------------------------------------------
  unique(blob[, "TKR"]),
  unique(blob[, "CMRK"]),
  unique(blob[, "TechRank"]),
# unique(blob[, "X.vCM"]),
  unique(blob[, 5]),                                                            # `^vCM`
  unique(blob[, "CMPRICE"]),
  unique(blob[, "HISTVO"]), 
  unique(blob[, "VOLF"]),
  # ((dt_bfly[.N,7] - dt_bfly[.N,6])/dt_bfly[.N,6])*100,
  ((unique(blob[, "VOLF"]) - unique(blob[, "HISTVO"]))/unique(blob[, "HISTVO"]))*100,
  unique(blob[, "EXPDAY"]),
# ------------------------------------------------------------------------------
  unique(blob[C.P == "C", ][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]), # OPTKR
  unique(blob[C.P == "P", ][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
  unique(blob[C.P == "P", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
  unique(blob[C.P == "C", ][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
  # ------------------------------------------------------------------------------
sum(
 -unique(blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]), # ASK
  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]), # BID
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]),
 -unique(blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]),
  na.rm = TRUE
  ),
# ------------------------------------------------------------------------------
# Individual components of cost
# ------------------------------------------------------------------------------
  # unique(blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]),
  # unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]),
  # unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]),
  # unique(blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]),
#...............................................................................
  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"]),
#...............................................................................
  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"]),
#...............................................................................
  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"]),
#...............................................................................
  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"]),
#...............................................................................
  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"]),
# #...............................................................................
  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"]),
# #............................................................................... 
  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"]),

  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"]),

  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"]),

  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"]),

  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"]),
  unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"]),
# ------------------------------------------------------------------------------
  unique(blob[, "date_run"])
#  dt_bfly[, .I]
  ),
# ------------------------------------------------------------------------------
  use.names = FALSE)
  }

#...............................................................................
# browser()
#...............................................................................

#...............................................................................
}
#...............................................................................
