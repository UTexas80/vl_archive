# ------------------------------------------------------------------------------
# Function Archive                                                           ---
# ------------------------------------------------------------------------------

#...............................................................................
fun_0000_archive_main       <- function() {
#...............................................................................

#...............................................................................
# browser()
#...............................................................................

  dt_archive <-                                        # ALLNEW by DATE
#    dt_file_table[name_char %like% 'W',]          %>% # Find all the ALLNEW.CSV
#    dt_file_table[name %like% '221101ALLNEw*',]   %>%
    dt_file_table[name_char %like% 'W',][N == 1,]  %>% # Find all the ALLNEW.CSV
    split(., by = c("name_num", "name"))           %>% # name_num -> yymmdd
    map(., fun_0000_archive_processing)                # name -> yymmddAllNEw.zip
#...............................................................................
}
# ------------------------------------------------------------------------------``

#...............................................................................
fun_0000_archive_processing <- function(nm){

#...............................................................................
# browser()
#...............................................................................

#...............................................................................
  fun_1000_download_zip(nm)
  fun_2000_archive_mungle()
  # fun_3000_strike_processing(dx_blob)
  fun_3000_strike_main()
# fun_4000_bfly_main(dx_blob)
#...............................................................................
}
#...............................................................................

# ------------------------------------------------------------------------------
fun_1000_download_zip       <- function(nm){
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# Step 1000.00 download zip files                                            ---
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

#...............................................................................
# browser()
#...............................................................................

  file.remove(here::here('zip', nm[,1]))
  g[[paste0(nm[,2])]] <<- fread(here::here("data", dt_unzipped_files))
# ------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

#...............................................................................
fun_2000_archive_mungle     <- function(){
#...............................................................................

# ------------------------------------------------------------------------------
# Step 2000.00 Processing                                                    ---
# ------------------------------------------------------------------------------

################################################################################
## Step 00.01 dynamically create dataframe      https://tinyurl.com/y3adrqwa ###
## grep with multiple patterns                  https://tinyurl.com/8kpnktnm ###
################################################################################

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
## create a template to rbind the dataframes
# ------------------------------------------------------------------------------
# dx_blob                 <- ALLNEW[11,] %>%  row_to_names(row_number = 1)
# data.table::setnames(dx_blob, 1:49,  names(Top200CallsBuy)[1:49])
# data.table::setnames(dx_blob,50:54,  as.character(ALLNEW[11,50:54]))
# dx_blob$date_run        <- ALLNEW[4,2] 
# dx_blob$date_run        <- as.Date(dx_blob$date_run, format('%m/%d/%Y'))


# ------------------------------------------------------------------------------
# Step 2000.01.a create a template to rbind the dataframes
# ------------------------------------------------------------------------------
  date_run <<- ALLNEW[4,2]
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
# Step 2000.01.b remove redundant entries                                    ---
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
# Step 2000.02 monthly expiration date creation                              ###
################################################################################

# ------------------------------------------------------------------------------
dx_date_exp             <- data.table::setorder(
                            as.data.table(
                              distinct(dx_blob[,15])),
                            EXPDAY)
# dx_date_exp$day       <- weekdays(dx_date_exp$EXPDAY)
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
                                        to = "2050-12-31",
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
dt_date_exp_mth$diff_today  <- dt_date_exp_mth$EXPDAY - as.Date(date_run$V2, format('%m/%d/%Y'))
dt_date_exp_mth[, next_exp  := shift(EXPDAY, type = "lead")]
dt_date_exp_mth[, diff_exp  := as.numeric(next_exp - shift(next_exp))]
dt_date_exp_mth             <<- setorder(dt_date_exp_mth, EXPDAY)
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

################################################################################
# Step 00.03 run date processing                                             ###
################################################################################

# ------------------------------------------------------------------------------
dt_date_run             <<- date_run
names(dt_date_run)[1]   <<- "EXPDAY"
dt_date_run$EXPDAY      <<- as.Date(dt_date_run$EXPDAY, format('%m/%d/%Y'))
# ------------------------------------------------------------------------------
setkey(dx_blob,         EXPDAY)
setkey(dt_date_run,     EXPDAY)
setkey(dt_date_exp_mth, EXPDAY)
# ------------------------------------------------------------------------------

################################################################################
# Step 2000.04 exp date processing - next three (3) expiration dates.        ###
################################################################################

# ------------------------------------------------------------------------------
dt_top_3_exp            <<- head(
                              dt_date_run[
                                dt_date_exp_mth, on = .(EXPDAY), ,
                                 roll = Inf, nomatch = 0][
                                  , 1],
                              3)
dx_blob                 <- dx_blob[dt_top_3_exp, on = .(EXPDAY)]
# ------------------------------------------------------------------------------

################################################################################
# Step 2000.05 company specific variables                                    ###
################################################################################

# ------------------------------------------------------------------------------
dx_company              <<- data.table::setorder(as.data.table(unique(
                              dx_blob[,1])),Company)
dx_industry             <<- data.table::setorder(as.data.table(unique(
                              dx_blob[,7])),INDUST)
dx_tech_rank            <<- data.table::setorder(as.data.table(unique(
                              dx_blob[,4])),TechRank)
dx_strike               <<- data.table::setorder(as.data.table(unique(
                              dx_blob[,12])),STRIKE)
dx_strike               <<- dx_strike[STRIKE %% 5 == 2.5 | STRIKE %% 5 == 0]
# ------------------------------------------------------------------------------
dx_ticker               <<- as.data.table(distinct(
                              dx_blob[,c(10,8)]))
dx_ticker               <<- data.table::setorder(
                              dx_ticker, "TKR", "CMPRICE")
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# [10] - TKR      <chr>
# [11] - `C/P`    <chr>
# [12] - STRIKE   <dbl>
# [13] - OPTKR    <chr>
# [15] - EXPDAY   <date>
# ------------------------------------------------------------------------------
# [03] - CMRK     <dbl>
# [04] - TechRank <dbl>
# [05] - X.vCM    <chr>
# [06] - HISTVO   <dbl>
# [08] - CMPRICE  <dbl>
# [09] - VOLF     <dbl>
# [##] - vol_diff <falkulated>
# [55] - date_run <date>
# ------------------------------------------------------------------------------
# option strike specific fields
# ------------------------------------------------------------------------------
# [18] - BID     <dbl>
# [19] - ASK     <dbl>
# ------------------------------------------------------------------------------

dx_tkr_stk <- data.table::setorder(
  unique(
    dx_blob[,c(10:13,15)],
    #dx_blob[, c(10:13, 15, 3:6, 8:9, 55)]
  ),
  OPTKR
)

nearest_strike <-  merge(dx_tkr_stk,dx_ticker, by = "TKR")[, .SD[which.min(abs(STRIKE - CMPRICE))], by = TKR]
# ------------------------------------------------------------------------------
dx_tkr_stk     <<-  dx_tkr_stk[dx_strike, on = .(STRIKE), nomatch = 0]
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

#...............................................................................
}
#...............................................................................

################################################################################
# Step 3000.06 strike specific variables                                     ###
################################################################################
#...............................................................................
fun_3000_strike_main        <- function(){
#...............................................................................

#...............................................................................
# browser()
#...............................................................................

# date_run  <<- as.Date(date_run, format = "%y%m%d")

# ------------------------------------------------------------------------------
# g <<- dx_blob[.0,0]                               # stub template dataframe---
g[[paste0("dt_gf")]] <<- dx_blob[.0,0]
g[[paste0("dx_condor_strike")]] <<- dx_blob[.0,0]
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# dx_stk
# ------------------------------------------------------------------------------
# TKR    <chr>
# `C/P`  <chr>
# STRIKE <dbl>
# OPTKR  <chr>
# EXPDAY <dat
# ------------------------------------------------------------------------------

dx_stk  <- data.table::setorder(
  rbind(
    rbind(dx_ticker[dx_tkr_stk, on = .(TKR), roll = Inf, nomatch = 0][
      , by = .(TKR, dx_tkr_stk[[2]], EXPDAY), .SD[.N]][
        , c("STRIKE", "OPTKR") := .(CMPRICE, "")][
          ,c(1,5:7,3)]),
    dx_tkr_stk),
  TKR, EXPDAY, 'C/P', STRIKE
)

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------230526
dx_stk <- dx_stk[dt_date_exp_mth[data.table::between(diff_today, 27,63),][1,1], on = .(EXPDAY)]
# ------------------------------------------------------------------------------
dx_stk  %>%
  split(., by = c("TKR", "EXPDAY", 'C/P')) %>%
  map(., fun_3010_strike_processing)

#...............................................................................
# browser()
#...............................................................................

dt_gf[LEN(OPTKR)>0 & LEN(lag_1_optkr)>0 & LEN(lag_0_optkr)>0 & LEN(lead_0_optkr)>0 & LEN(lead_1_optkr)>0,]
dx_condor_strike[LEN(OPTKR)>0 & LEN(lag_1_optkr)>0 & LEN(lag_0_optkr)>0 & LEN(lead_0_optkr)>0 & LEN(lead_1_optkr)>0,]

# ------------------------------------------------------------------------------
dt_gf <-            na.omit(dt_gf[OPTKR == "",])
dx_condor_strike <- na.omit(dx_condor_strike[OPTKR == "",])

#...............................................................................
# browser()
#...............................................................................

dt_gf <-  dt_gf[dt_gf[[2]]=='C',][
          dt_gf[dt_gf[[2]]=='P',],
            on = .(TKR, EXPDAY),
            nomatch=0][
            , id := .I]

dx_condor_strike_disp <-  dx_condor_strike[dx_condor_strike[[2]]=='C',][
          dx_condor_strike[dx_condor_strike[[2]]=='P',],
            on = .(TKR, EXPDAY),
            nomatch=0][
            , lead_0_stk_cmp_diff:= i.lead_0_stk - STRIKE][
            , cmp   := STRIKE][
            , mid      := lag_0_stk + ((i.lead_0_stk - lag_0_stk))/2][
            , diff     := (STRIKE -(lag_0_stk + ((i.lead_0_stk - lag_0_stk))/2))][
            , pct_diff := (STRIKE -(lag_0_stk + ((i.lead_0_stk - lag_0_stk))/2))/STRIKE][
            , id := .I]

# ------------------------------------------------------------------------------
# dt_condor_strike
# ------------------------------------------------------------------------------
# 01 TKR              <chr>
# ------------------------------------------------------------------------------
# CALLS
# ------------------------------------------------------------------------------
# 02 `C/P`            <chr>
# 03 STRIKE           <dbl>
# 04 OPTKR            <chr>
# 05 EXPDAY           <date
# 06 lag_1_optkr      <chr>
# 07 lag_1_stk        <dbl>
# 08 lag_1_pct_chg    <dbl>
# 09 lag_0_optkr      <chr>
# 10 lag_0_stk        <dbl>
# 11 lag_0_pct_chg    <dbl>
# 12 lead_0_optkr     <chr>
# 13 lead_0_stk       <dbl>
# 14 lead_0_pct_chg   <dbl>
# 15 lead_1_optkr     <chr>
# 16 lead_1_stk       <dbl>
# 17 lead_1_pct_chg   <dbl>
# ------------------------------------------------------------------------------
# PUTS
# ------------------------------------------------------------------------------
# 18 `i.C/P`          <chr>
# 19 i.STRIKE         <dbl>
# 20 i.OPTKR          <chr>
# 21 i.lag_1_optkr    <chr>
# 22 i.lag_1_stk      <dbl>
# 23 i.lag_1_pct_chg  <dbl>
# 24 i.lag_0_optkr    <chr>
# 25 i.lag_0_stk      <dbl>
# 26 i.lag_0_pct_chg  <dbl>
# 27 i.lead_0_optkr   <chr>
# 28 i.lead_0_stk     <dbl>
# 29 i.lead_0_pct_chg <dbl>
# 30 i.lead_1_optkr   <chr>
# 31 i.lead_1_stk     <dbl>
# 32 i.lead_1_pct_chg <dbl>
# 33 lead_0_stk_diff  <dbl>
# 34 id               <int>

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
dx_condor_key <- data.table::setorder(
  data.table::melt(dt_gf,
    id = 1:3,                              # TKR, 'C/P' = 'C', STRIKE
    measure = c(21, 9, 27, 15)             # i.lag_1_optkr, lag_0_optkr, i.lead_0_optkr, lead_1_optkr
  ), # bid/ask
  "TKR"
)[
  , c(1, 3, 5)
]
# ------------------------------------------------------------------------------
dx_condor_key[, id_strike:= rleid(value), by = .(TKR, rleid(TKR))]
# ------------------------------------------------------------------------------
dx_condor_key[, `:=`(id_strike = rleid(value)),
#                      date_run  = as.Date(date_run, format = "%y%m%d")),
                 by = .(TKR, rleid(TKR))]
# ------------------------------------------------------------------------------
dx_condor_key <- cbind(dx_condor_key, dt_date_run[,1])
names(dx_condor_key)[c(2:3,5)] <- c("CMPRICE", "OPTKR", "date_run")
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# Define the cost for the condor
# ------------------------------------------------------------------------------
setkey(dx_condor_key, OPTKR)
setkey(dx_blob, OPTKR)
# ------------------------------------------------------------------------------
dx_condor_max_profit <<- dx_condor_key[!(id_strike > 1 & id_strike < 4), ][
  dx_blob,
  nomatch = 0
][
  , c(1:4, 22, 5)
][
  , BID := BID * -1
]
# ------------------------------------------------------------------------------
dx_condor_max_profit <<- data.table::setorder(
  rbind(
    dx_condor_max_profit,
    dx_condor_key[id_strike > 1 & id_strike < 4, ][
      dx_blob,
      nomatch = 0
    ][
      , c(1:4, 23, 5)
    ],
    use.names = FALSE
  )
)
# ------------------------------------------------------------------------------
dx_condor_max_profit <<- dx_condor_max_profit[, profit := sum(BID), by = TKR]
dx_condor_max_profit <<- unique(
  dx_condor_max_profit[
    , profit := sum(BID),
    by = TKR
  ][
    , c(1, 7, 6)
  ]
)

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# odd id_strike, i.e., puts
# ------------------------------------------------------------------------------
dx_condor_max_loss <<- data.table::setorder(
  na.omit(
    rbind(
      dx_condor_key[id_strike %% 2 == 1, ][
        dx_blob,
        nomatch = 0
      ][
        , c(1:4, 17)
      ][
        , diff := STRIKE - shift(STRIKE),
        by = TKR
      ],
      dx_condor_key[id_strike %% 2 != 1, ][
        dx_blob,
        nomatch = 0
      ][
        , c(1:4, 17)
      ][
        , diff := STRIKE - shift(STRIKE),
        by = TKR
      ]
    )
  ), TKR, id_strike
)
# ------------------------------------------------------------------------------
dx_condor_max_loss <<-
  unique(
    dx_condor_max_loss[,
      loss := sum(diff),
      by = TKR
    ][
      , c(1, 7)
    ]
  )
#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
dx_condor_roi <-
  cbind(
    dx_condor_max_profit[
      dx_condor_max_loss,
      on = .(TKR)
    ][
      , roi := scales::percent(profit / loss)
    ],
#    as.Date(date_run, format = "%y%m%d"),
    date_run,
    as.Date(mid(dx_condor_key[, 3], 10, 6), format = "%y%m%d")
  )[
    , setnames(.SD, c("V2", "V3"), c("date_run", "date_exp"))
  ][, .(TKR, profit, loss, roi, date_exp, date_run)]
# ------------------------------------------------------------------------------
# Probability of Profit
# TKR Company CMPRICE VOLF date_run EXPDAY diff
# ------------------------------------------------------------------------------
setkey(dx_condor_roi, TKR, date_exp)
setkey(dx_blob, TKR, EXPDAY)
# ------------------------------------------------------------------------------
dx_condor_pop <<- 
  cbind(
    dx_blob[dx_condor_roi, nomatch = 0][
            , .SD[1], by = TKR, .SDcols = c(1,8:9,55,15)][
            dx_date_exp, on = .(EXPDAY), nomatch = 0][
            ,-c(7:8)][,
            date_exp_diff_yr := diff/365],
    int_rate)

#...............................................................................
browser()
#...............................................................................

if (z == TRUE) {
  g[["dx_condor_key"]] <<- dx_condor_key
  g[["dx_condor_roi"]] <<- dx_condor_roi
# ------------------------------------------------------------------------------
  z <<- FALSE
# ------------------------------------------------------------------------------
} else {

# ...............................................................................
# browser()
# ...............................................................................

  g[["dx_condor_key"]] <<- rbind(dx_condor_key, g[["dx_condor_key"]], fill=TRUE)
  g[["dx_condor_roi"]] <<- rbind(dx_condor_roi, g[["dx_condor_roi"]])

}

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# dt_gf[, cost_x :=
#   dt_gf[dx_blob, on = .(lag_1_optkr = OPTKR), nomatch = 0 ][
#   ,'ASK'] -
#   dt_gf[dx_blob, on = .(lag_0_optkr = OPTKR), nomatch = 0 ][
#   ,'BID']
#   ]

# dt_gf[, c(
# ------------------------------------------------------------------------------common rank
  # "cmrk") :=
# ------------------------------------------------------------------------------common rank
 # .(unique(dt_gf[dx_blob, on = .(lag_1_optkr = OPTKR), nomatch = 0 ][,'CMRK']))
# ------------------------------------------------------------------------------
 # ]
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

#...............................................................................
}
#...............................................................................

fun_3010_strike_processing  <- function(l) {

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------lag_1_c
# TKR
#`C/P`
# STRIKE
# OPTKR
# EXPDAY
# ------------------------------------------------------------------------------lag_1_c

#  l[l[[2]]=='C',][, c(
new_records <-
  l[, c(
# ------------------------------------------------------------------------------lag_1_c
    "lag_1_optkr",
    "lag_1_stk",
    "lag_1_pct_chg",
# ------------------------------------------------------------------------------lag_0_c
    "lag_0_optkr",
    "lag_0_stk",
    "lag_0_pct_chg",
# ------------------------------------------------------------------------------lead_0_c
    "lead_0_optkr",
    "lead_0_stk",
    "lead_0_pct_chg",
# ------------------------------------------------------------------------------lead_1_c
    "lead_1_optkr",
    "lead_1_stk",
    "lead_1_pct_chg") :=
# ------------------------------------------------------------------------------lag_1_c
      .(shift(OPTKR,    type = 'lag', n = 2),
        shift(STRIKE,   type = 'lag', n = 2),
        (STRIKE - shift(STRIKE, type = "lag", n = 2)) /
          shift(STRIKE, type = "lag", n = 2) * 100,
# ------------------------------------------------------------------------------lag_0_c
        shift(OPTKR,    type = 'lag', n = 1),
        shift(STRIKE,   type = 'lag', n = 1),
        (STRIKE - shift(STRIKE, type = "lag", n = 1)) /
          shift(STRIKE, type = "lag", n = 1) * 100,
# ------------------------------------------------------------------------------lead_0_c
        shift(OPTKR,    type = 'lead', n = 1),
        shift(STRIKE,   type = 'lead', n = 1),
        (STRIKE  - shift(STRIKE, type = "lead", n = 1)) /
          shift(STRIKE, type = "lead", n = 1) * 100,
# ------------------------------------------------------------------------------lead_1_c
        shift(OPTKR,    type = "lead", n = 2),
        shift(STRIKE,   type = "lead", n = 2),
        (STRIKE - shift(STRIKE, type = "lead", n = 2)) /
          shift(STRIKE, type = "lead", n = 2) * 100
      )]
#      )][
#        TKR=='AA' & OPTKR == "",]
# ------------------------------------------------------------------------------
g[[paste0("dt_gf")]] <<- rbind(g[[paste0("dt_gf")]], new_records)
g[[paste0("dx_condor_strike")]] <<- rbind(g[[paste0("dx_condor_strike")]], new_records)
#...............................................................................
# browser()
#...............................................................................

dt_gf[complete.cases(dt_gf), ][
  (OPTKR        == "" &
     !lag_0_optkr  == "" &
     !lag_1_optkr  == "" &
     !lead_0_optkr == "" &
     !lead_1_optkr == ""),
]

dx_condor_strike[complete.cases(dx_condor_strike), ][
  (OPTKR           == "" &
     !lag_0_optkr  == "" &
     !lag_1_optkr  == "" &
     !lead_0_optkr == "" &
     !lead_1_optkr == ""),
]

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------230516
# chat_gpt: filter data.table all columns contain a value using r code
# check this out - add the rep function and .N to recycle rows
# ------------------------------------------------------------------------------

# na.omit(dt_gf[rep(sapply(dt_gf, function(x) all(is.character(x) | is.numeric(x))), length = .N)])
# dt_gf[rep(!mapply(function(x) any(is.na(x)), dt_gf),length = .N)]

# ------------------------------------------------------------------------------

# dt_gf[complete.cases(dt_gf), ]

# ------------------------------------------------------------------------------
# Remove rows with blank values in one particular column

# df[!(is.na(df$start_pc) | df$start_pc==""), ]

# Remove rows with blank values in multiple columns

# df[!(is.na(df$start_pc) | df$start_pc=="" | is.na(df$end_pc) | df$end_pc==""), ]

# Remove rows with blank values in all columns

# df[complete.cases(df), ]

#...............................................................................
}
#...............................................................................

#...............................................................................
fun_3000_strike_processing  <- function(dx_blob){
#...............................................................................

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
  dx_tkr_stk             <- dx_blob[
                              dt_date_exp_mth[
                                data.table::between(diff_today, 27,63),][1,1]
                              ]
# ------------------------------------------------------------------------------
  setkey(dx_tkr_stk,    TKR)
  setkey(dx_ticker,     TKR)
# ------------------------------------------------------------------------------
  dx_tkr_stk             <- data.table::setorder(dx_tkr_stk, "TKR", "`C/P`", "STRIKE")
# ------------------------------------------------------------------------------
  s_minus_0              <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N],     by = .(TKR)]
  s_minus_1              <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N - 1], by = .(TKR)]
  s_plus_0               <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, head(.SD, 1),   by = .(TKR)]
  s_plus_1               <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, .SD[2],         by = .(TKR)]
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------230518
#  dx_s_minus_0           <<- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N],     by = .(TKR)]
#  dx_s_minus_1           <<- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N - 1], by = .(TKR)]
#  dx_s_minus_2           <<- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N - 2], by = .(TKR)]
# ------------------------------------------------------------------------------
  dx_s_minus_0            <<- dx_tkr_stk[dx_tkr_stk[[11]] == 'C' & CMPRICE >= STRIKE, .SD[1], by = .(TKR)][,c(1,12)]
#  dx_s_minus_0           <<- dx_tkr_stk[dx_tkr_stk[[11]] == 'C' & CMPRICE >= STRIKE, .SD[2], by = .(TKR)][,c(1,12)]
  dx_s_minus_1           <<- dx_tkr_stk[dx_tkr_stk[[11]] == 'P' & CMPRICE >= STRIKE, .SD[2], by = .(TKR)][,c(1,12)]
  dx_s_minus_2           <<- dx_tkr_stk[dx_tkr_stk[[11]] == 'P' & CMPRICE >= STRIKE, .SD[3], by = .(TKR)][,c(1,12)]
# ------------------------------------------------------------------------------
# dx_s_minus_0           <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N],     by = .(TKR)]
# dx_s_minus_1           <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N - 1], by = .(TKR)]
# dx_s_minus_2           <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N - 2], by = .(TKR)]
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
  dx_s_plus_0            <<- dx_tkr_stk[dx_tkr_stk[[11]] == 'P' & CMPRICE <= STRIKE, .SD[1], by = .(TKR)][,c(1,12)]
  dx_s_plus_1            <<- dx_tkr_stk[dx_tkr_stk[[11]] == 'C' & CMPRICE <= STRIKE, .SD[2], by = .(TKR)][,c(1,12)]
  dx_s_plus_2            <<- dx_tkr_stk[dx_tkr_stk[[11]] == 'C' & CMPRICE <= STRIKE, .SD[3], by = .(TKR)][,c(1,12)]
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# dx_s_plus_0            <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, head(.SD, 1),   by = .(TKR)][,c(1,3)]
# dx_s_plus_1            <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, .SD[2],         by = .(TKR)][,c(1,3)]
# dx_s_plus_2            <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, .SD[3],         by = .(TKR)][,c(1,3)]
# ------------------------------------------------------------------------------
  na.omit(dx_s_minus_1[dx_s_minus_0][dx_s_plus_0][dx_s_plus_1])
  dx_tkr_stk[dx_s_minus_1]

#...............................................................................
# browser()
#...............................................................................

################################################################################
# Step 01.01 clean the tables                                               ###
################################################################################
  # dx_s_list <- grep("^dx_s_", ls(envir = .GlobalEnv), value = TRUE)
# ------------------------------------------------------------------------------
  # lapply(dx_s_list, function(nm) {
  #   df  <- get(nm)
  #   # g[[paste0("dx", nm, "_")]] <<- nm
  #   # setnames(df, c("TKR", "V1"), c("TKR", "STRIKE"))
  #   setkey(df, "TKR")
  #   }
  # )

#...............................................................................
# browser()
#...............................................................................

  # dx_s_minus_1[dx_blob, nomatch = 0][C.P == "P" & EXPDAY %like% '2023-06-16',][,13]
  # dx_s_minus_0[dx_blob, nomatch = 0][C.P == "C" & EXPDAY %like% '2023-06-16',][,13]
  # dx_s_plus_0[dx_blob,  nomatch = 0][C.P == "P" & EXPDAY %like% '2023-06-16',][,13]
  # dx_s_plus_1[dx_blob,  nomatch = 0][C.P == "C" & EXPDAY %like% '2023-06-16',][,13]
# ------------------------------------------------------------------------------230524
  dx_s_minus_1           <<- dx_tkr_stk[dx_tkr_stk[[11]] == 'P' & CMPRICE >= STRIKE, .SD[2], by = .(TKR)][,c(1,11,9,12:13, 19)] # ASK
  dx_s_minus_0           <<- dx_tkr_stk[dx_tkr_stk[[11]] == 'C' & CMPRICE >= STRIKE, .SD[1], by = .(TKR)][,c(1,11,9,12:13, 18)] # BID
  dx_s_plus_0            <<- dx_tkr_stk[dx_tkr_stk[[11]] == 'P' & CMPRICE <= STRIKE, .SD[1], by = .(TKR)][,c(1,11,9,12:13, 18)] # BID
  dx_s_plus_1            <<- dx_tkr_stk[dx_tkr_stk[[11]] == 'C' & CMPRICE <= STRIKE, .SD[2], by = .(TKR)][,c(1,11,9,12:13, 19)] # ASK

#...............................................................................
# browser()
#...............................................................................

dx_condor_strike <- na.omit(rbind(dx_s_minus_1,  dx_s_minus_0,  dx_s_plus_0,  dx_s_plus_1, use.names=FALSE))
dx_condor_count    <- dx_condor_strike[, .N, by = TKR][N == 4]

# ------------------------------------------------------------------------------
  dx_s_minus_0 <<- names(dx_s_minus_0)  [1:2]  <- c("TKR", "STRIKE")
  dx_s_minus_1 <<- names(dx_s_minus_1)  [1:2]  <- c("TKR", "STRIKE")
  dx_s_minus_2 <<- names(dx_s_minus_2)  [1:2]  <- c("TKR", "STRIKE")
# ------------------------------------------------------------------------------
  dx_s_plus_0  <<- names(dx_s_plus_0)   [1:2]  <- c("TKR", "STRIKE")
  dx_s_plus_1  <<- names(dx_s_plus_1)   [1:2]  <- c("TKR", "STRIKE")
  dx_s_plus_2  <<- names(dx_s_plus_2)   [1:2]  <- c("TKR", "STRIKE")
# ------------------------------------------------------------------------------
  g[[paste0("dx_s_minus_0")]] <<- dx_s_minus_0
  g[[paste0("dx_s_minus_1")]] <<- dx_s_minus_1
  g[[paste0("dx_s_minus_2")]] <<- dx_s_minus_2
# ------------------------------------------------------------------------------
  g[[paste0("dx_s_plus_0")]]  <<- dx_s_plus_0
  g[[paste0("dx_s_plus_1")]]  <<- dx_s_plus_1
  g[[paste0("dx_s_plus_2")]]  <<- dx_s_plus_2
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
  data.table::setkey(dx_s_minus_0, TKR)
  data.table::setkey(dx_s_minus_1, TKR)
  data.table::setkey(dx_s_minus_2, TKR)
# ------------------------------------------------------------------------------
  data.table::setkey(dx_s_plus_0,  TKR)
  data.table::setkey(dx_s_plus_1,  TKR)
  data.table::setkey(dx_s_plus_2,  TKR)
# ------------------------------------------------------------------------------
  data.table::setkey(dx_ticker,    TKR)
# ------------------------------------------------------------------------------

#...............................................................................
browser()
#...............................................................................

#...............................................................................
}
#...............................................................................

#...............................................................................
fun_4000_bfly_main          <- function(dx_blob) {
#...............................................................................

#...............................................................................
# browser()
#...............................................................................

  butterfly <- dx_blob[
#   dx_date_exp[day == 6 & week == 3, ][1, 1]
#   dt_date_exp_mth[between(diff_today, 0, 63)][1,1]
    tail(dt_date_exp_mth[data.table::between(diff_today, 28, 62)],1)
    #  dx_date_exp_mth[2,1]
  ][
    order(OPTKR, -date_run),
  ] %>%
    split(., by = c("TKR", "EXPDAY")) %>%
    map(., fun_4100_bfly_processing)

#...............................................................................
# browser()
#...............................................................................

#...............................................................................
}
#...............................................................................

#...............................................................................
fun_4100_bfly_processing    <- function(blob) {
#...............................................................................

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

#...............................................................................
# browser()
#...............................................................................

  g[[paste0("dt_bfly")]][, id_minus_1 := blob[blob[[11]] == 'P'][dx_s_minus_1, on = .c("TKR", "STRIKE" ), nomatch = 0][,13]]  # OPTKR
  g[[paste0("dt_bfly")]][, id_minus_0 := blob[blob[[11]] == 'C'][dx_s_minus_0, on = .c("TKR", "STRIKE" ), nomatch = 0][,13]]
  g[[paste0("dt_bfly")]][, id_plus_0  := blob[blob[[11]] == 'P'][dx_s_plus_0,  on = .c("TKR", "STRIKE" ), nomatch = 0][,13]]
  g[[paste0("dt_bfly")]][, id_plus_1  := blob[blob[[11]] == 'C'][dx_s_plus_1,  on = .c("TKR", "STRIKE" ), nomatch = 0][,13]]

# ------------------------------------------------------------------------------
# g[[paste0("dt_bfly")]][, id_minus_1 := blob[C.P == "P", ][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]] # OPTKR
# g[[paste0("dt_bfly")]][, id_minus_0 := blob[C.P == "C", ][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]
# g[[paste0("dt_bfly")]][, id_plus_0  := blob[C.P == "P", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]
# g[[paste0("dt_bfly")]][, id_plus_1  := blob[C.P == "C", ][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]

#...............................................................................
browser()
#...............................................................................

# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, cost       :=
   -blob[blob[[11]] == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19] + # ASK
    blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] + # BID
    blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] -
    blob[blob[[11]] == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]]
# ------------------------------------------------------------------------------
# g[[paste0("dt_bfly")]][, cost       :=
#  -blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19] + # ASK
#   blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] + # BID
#   blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] -
#   blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]]
# ------------------------------------------------------------------------------
# Individual components of cost
# ------------------------------------------------------------------------------
  # g[[paste0("dt_bfly")]][, s_minus_1  := unique(blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19])]
  # g[[paste0("dt_bfly")]][, s_minus_0  := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18])]
  # g[[paste0("dt_bfly")]][, s_plus_0   := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18])]
  # g[[paste0("dt_bfly")]][, s_plus_1   := unique(blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19])]
#...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_oi      := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"])]
  g[[paste0("dt_bfly")]][, s_plus_0_oi       := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"])]
# ------------------------------------------------------------------------------
#  g[[paste0("dt_bfly")]][, s_minus_0_oi      := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"])]
#  g[[paste0("dt_bfly")]][, s_plus_0_oi       := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"])]
# ------------------------------------------------------------------------------
#  g[[paste0("dt_bfly")]][, s_minus_0_iotm    := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"])]
#  g[[paste0("dt_bfly")]][, s_plus_0_iotm     := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"])]
#...............................................................................
#  g[[paste0("dt_bfly")]][, s_minus_0_prb_stk := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"])]
#  g[[paste0("dt_bfly")]][, s_plus_0_prb_stk  := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"])]
#...............................................................................
#  g[[paste0("dt_bfly")]][, s_minus_0_prb_stk := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"])]
#  g[[paste0("dt_bfly")]][, s_plus_0_prb_stk  := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"])]
#...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_iotm    := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 35])] # "I.OTM"
  g[[paste0("dt_bfly")]][, s_plus_0_iotm     := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 35])] # "I.OTM"
#...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_un_ov   := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 24])] # "Bid.UN.OV"
  g[[paste0("dt_bfly")]][, s_plus_0_un_ov    := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 24])] # "Bid.UN.OV"
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, s_minus_0_prb_stk := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 40])] # "PrtbStrk.VOLF"
  g[[paste0("dt_bfly")]][, s_plus_0_prb_stk  := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 40])] # "PrtbStrk.VOLF"
#...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_pct_dbl := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"])]
  g[[paste0("dt_bfly")]][, s_plus_0_pct_dbl  := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"])]
#...............................................................................
#  g[[paste0("dt_bfly")]][, s_minus_0_rowrt   := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"])]
#  g[[paste0("dt_bfly")]][, s_plus_0_rowrt    := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"])]
#...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_rowrt   := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 48])] # "ROWRT"
  g[[paste0("dt_bfly")]][, s_plus_0_rowrt    := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 48])] # "ROWRT"
#...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_delta   := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"])]
  g[[paste0("dt_bfly")]][, s_plus_0_delta    := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"])]

  g[[paste0("dt_bfly")]][, s_minus_0_gamma   := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"])]
  g[[paste0("dt_bfly")]][, s_plus_0_gamma    := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"])]

  g[[paste0("dt_bfly")]][, s_minus_0_rho     := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"])]
  g[[paste0("dt_bfly")]][, s_plus_0_rho      := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"])]

  g[[paste0("dt_bfly")]][, s_minus_0_theta   := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"])]
  g[[paste0("dt_bfly")]][, s_plus_0_theta    := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"])]

  g[[paste0("dt_bfly")]][, s_minus_0_vega    := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"])]
  g[[paste0("dt_bfly")]][, s_plus_0_vega     := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"])]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, date_run   := unique(blob[, "date_run"])]
# g[[paste0("dt_bfly")]][, id         := .I]
# ------------------------------------------------------------------------------
  z <<- FALSE
# ------------------------------------------------------------------------------
} else {

#...............................................................................
browser()
#...............................................................................

g[[paste0("dt_bfly")]] <<- rbind(
    setDT(g[[paste0("dt_bfly")]]),
# g[[paste0("dt_bfly")]] <<- rbind(list(g[[paste0("dt_bfly")]],
# g[[paste0("dt_bfly")]] <<- purrr::map2_df(g[[paste0("dt_bfly")]],
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
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]), # OPTKR
  unique(blob[blob[[11]] == "P", ][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
  unique(blob[blob[[11]] == "C", ][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
  # ------------------------------------------------------------------------------
sum(
 -unique(blob[blob[[11]] == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]), # ASK
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]), # BID
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]),
 -unique(blob[blob[[11]] == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]),
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
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"]),
#...............................................................................
#  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"]),
#  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"]),
#...............................................................................
#  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"]),
#  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"]),
#...............................................................................
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 35]), # "I.OTM"
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 35]), # "I.OTM"
#...............................................................................
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 24]), # "Bid.UN.OV"
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 24]), # "Bid.UN.OV"
#...............................................................................
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 40]), # "PrtbStrk.VOLF"
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 40]), # "PrtbStrk.VOLF"
#...............................................................................
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"]),
#...............................................................................
#  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"]),
#  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"]),
#...............................................................................
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 48]), # "ROWRT"
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 48]), # "ROWRT"
#...............................................................................
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"]),

  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"]),

  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"]),

  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"]),

  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"]),
# ------------------------------------------------------------------------------
  unique(blob[, "date_run"])
#  dt_bfly[, .I]
  ),
# ------------------------------------------------------------------------------
    use.names = FALSE
#    fill = TRUE
#      )
    )
  }
#...............................................................................
# browser()
#...............................................................................

#...............................................................................
}
#...............................................................................

# dx_condor        <- merge(dx_tkr_stk, dx_ticker, by = "TKR")
# dx_condor_strike <- dx_condor[dx_condor[[2]]=='P', .SD[which.min(abs(STRIKE - CMPRICE))], by = c("TKR", "C/P")]
# dx_condor_strike_c <- dx_condor[dx_condor[[2]]=='C', .SD[which.min(abs(STRIKE - CMPRICE))], by = c("TKR", "C/P")]
# dx_condor_strike_p <- dx_condor[dx_condor[[2]]=='P', .SD[which.min(abs(STRIKE - CMPRICE))], by = c("TKR", "C/P")]
# dx_condor_index  <- dx_condor[dx_condor[[2]]=='P', .I[ which.min(abs(STRIKE - CMPRICE))], by = c("TKR", "C/P")]

# na.omit(as.data.table(shift(dx_condor[order(TKR)][dx_condor_strike, roll = Inf, on = .(TKR, STRIKE = CMPRICE)][order(TKR)], type = "lead", n = -1)))
# na.omit(as.data.table(shift(dx_condor[order(TKR)][dx_condor_strike, roll = -Inf, on = .(TKR, STRIKE = CMPRICE)][order(TKR)], type = "lag", n = 1)))

# dx_s_minus_1[dx_blob, nomatch = 0][C.P == "P" & EXPDAY %like% '2023-06-16',][,13]
# dx_s_minus_0[dx_blob, nomatch = 0][C.P == "C" & EXPDAY %like% '2023-06-16',][,13]
# dx_s_plus_0[dx_blob, nomatch = 0][C.P == "P" & EXPDAY %like% '2023-06-16',][,13]
# dx_s_plus_1[dx_blob, nomatch = 0][C.P == "C" & EXPDAY %like% '2023-06-16',][,13]

# dx_condor_strike <- na.omit(rbind(dx_s_minus_1,  dx_s_minus_0,  dx_s_plus_0,  dx_s_plus_1))
# dx_condor_key    <- dx_condor_strike[, .N, by = TKR][N == 4]

# options prices
# quantmod::getOptionChain("AAPL", "2023-06-16,")

# 120 P - buy   / long
# 125 C - write / short
# 130 P - write / short
# 135 C - buy   / long

