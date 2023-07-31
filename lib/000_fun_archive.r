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
    dt_file_table[name %like% '230727ALLNEw*',]   %>%
#    dt_file_table[name_char %like% 'W',][N == 1,]  %>% # Find all the ALLNEW.CSV
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
#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# Step 2000.01.a create a template to rbind the dataframes
# ------------------------------------------------------------------------------
  date_run                <<- ALLNEW[4,2]
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
  dx_blob                 <- ALLNEW[12,]      %>%  row_to_names(row_number = 1)
  ALLNEW                  <- tail(ALLNEW,-11) %>%  row_to_names(row_number = 1)
  dx_blob                 <- rbind(dx_blob, ALLNEW)
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
  dx_blob$EXPDAY        <-  as.Date(dx_blob$EXPDAY, format('%m/%d/%Y'))
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
g[[paste0("dx_condor")]]        <<- dx_blob[.0,0]
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

# -----------------------------------------------------------------------------230526
dx_stk <- dx_stk[dt_date_exp_mth[data.table::between(diff_today, 27,63),][1,1], on = .(EXPDAY)]
# ------------------------------------------------------------------------------
dx_stk  %>%
  split(., by = c("TKR", "EXPDAY", 'C/P')) %>%
  map(., fun_3010_strike_processing)

#...............................................................................
# browser()
#...............................................................................

dx_condor[LEN(OPTKR)>0 & LEN(lag_1_optkr)>0 & LEN(lag_0_optkr)>0 & LEN(lead_0_optkr)>0 & LEN(lead_1_optkr)>0,]
dx_condor_strike[LEN(OPTKR)>0 & LEN(lag_1_optkr)>0 & LEN(lag_0_optkr)>0 & LEN(lead_0_optkr)>0 & LEN(lead_1_optkr)>0,]

# ------------------------------------------------------------------------------
dx_condor        <- na.omit(dx_condor[OPTKR == "",])
dx_condor_strike <- na.omit(dx_condor_strike[OPTKR == "",])
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
dx_condor <-
  dx_condor[dx_condor[[2]] == "C",
  ][
  dx_condor[dx_condor[[2]] == "P", ],
  on = .(TKR, EXPDAY),
  nomatch = 0
  ][
  , id := .I
  ]
# ------------------------------------------------------------------------------
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
  data.table::melt(dx_condor,
    id = 1:3,                      # TKR, 'C/P' = 'C', STRIKE
    measure = c(21, 9, 27, 15)
#    measure = c("lag_1_optkr", "lag_0_optkr", "lead_0_optkr", "lead_1_optkr")
  ), # bid/ask
  "TKR"
)[
  , c(1, 3, 5)
#  , c("TKR", "STRIKE", "VALUE")
]

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
dx_condor_key[, id_strike:= rleid(value), by = .(TKR, rleid(TKR))]
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................
names(dt_date_run)[1]  <- "date_run"
names(dx_condor_key)[c(2:3)] <- c("CMPRICE", "OPTKR")
dx_condor_key[, strike := as.numeric(substr(OPTKR, nchar(OPTKR) - 7, nchar(OPTKR))) / 1000]
# dx_condor_key[, .(strike    = as.numeric(substr(OPTKR, nchar(OPTKR) - 7, nchar(OPTKR))) / 1000),
#                  by = .(TKR)]
# ------------------------------------------------------------------------------
dx_condor_key  <- cbind(dx_condor_key, dt_date_run[,1])
# ------------------------------------------------------------------------------
dx_condor_date <-
  data.table(run = unique(dt_date_run$date_run),
             exp = unique(dx_stk$EXPDAY))         %>%
    .[, diff    := exp - run]                     %>%
    .[, diff_yr := diff / 365]
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# set the key of the data table and add the key dates
# ------------------------------------------------------------------------------
data.table::setkey(dx_condor_key, OPTKR)
data.table::setkey(dx_blob, OPTKR)
# ------------------------------------------------------------------------------
# dx_condor_key <-
#   dx_condor_key[dx_blob, nomatch=0
#                ][,c("TKR", "CMPRICE", "OPTKR", "id_strike", "strike", "EXPDAY")
#                ][, date_run := dt_date_run[,1]
#                ]
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# Define the cost for the condor
# ------------------------------------------------------------------------------
dx_condor_max_profit <<-
  dx_condor_key[!(id_strike > 1 & id_strike < 4),
    ][
    dx_blob,
    nomatch = 0
    ][
#  , c(1:4, 24, 5)
  , c("TKR", "CMPRICE", "OPTKR", "id_strike", "ASK", "strike")
    ][
  , ASK := ASK * -1
]

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# Total maximum potential profit =
#   Net premium from the call spread + Net premium from the put spread 
# https://tinyurl.com/v4wvb3ke
# ------------------------------------------------------------------------------
dx_condor_max_profit <<- data.table::setorder(
  rbind(
    dx_condor_max_profit,
    dx_condor_key[id_strike > 1 & id_strike < 4,
    ][
      dx_blob,
      nomatch = 0
    ][
#     , c(1:4, 23, 5)
      , c("TKR", "CMPRICE", "OPTKR", "id_strike", "BID", "strike")
    ],
    use.names = FALSE
  ), TKR, id_strike
)

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
dx_condor_cost <-
  data.table::setorder(
    na.omit(
      rbind(
        dx_condor_max_profit[
          !id_strike %% 2 == 1
          ][, .(c_p    = 'c',
                diff   =  strike - shift(strike),
                profit = sum(ASK),
                loss   = (strike - shift(strike)) - sum(ASK)
                ),
            by = TKR
            ],
        dx_condor_max_profit[
          id_strike %% 2 == 1
          ][, .(c_p    = 'p',
                diff   =  strike - shift(strike),
                profit = sum(ASK),
                loss   = (strike - shift(strike)) - sum(ASK)
                ),
            by = TKR
            ]
      )
    ), TKR
  )
# ------------------------------------------------------------------------------
data.table::merge.data.table(
  dcast(dx_condor_max_profit,
        TKR + CMPRICE ~ id_strike,
        value.var = "ASK",
        sep = "_"),
  dx_condor_cost[, max(diff), by = .(TKR)]
)
# ------------------------------------------------------------------------------
dx_condor_strike_diff <-
  as.data.table(
    dcast(dx_condor_key,
      TKR + CMPRICE ~ id_strike,
      value.var = "strike",
      sep = "_"
    )
  )
# ------------------------------------------------------------------------------
colnames(dx_condor_strike_diff) <- c("TKR", "CMPRICE", "strike_1", "strike_2", "strike_3", "strike_4")
# ------------------------------------------------------------------------------
dx_condor_strike_diff$strike_diff_inner <-
  dx_condor_strike_diff$strike_3 - dx_condor_strike_diff$strike_2
dx_condor_strike_diff$strike_diff_c     <-
  dx_condor_strike_diff$strike_4 - dx_condor_strike_diff$strike_2
dx_condor_strike_diff$strike_diff_p     <-
  dx_condor_strike_diff$strike_3 - dx_condor_strike_diff$strike_1
# ------------------------------------------------------------------------------
dcast(dx_condor_max_profit,
      TKR + CMPRICE ~ id_strike,
      value.var = "ASK",
      sep = "_")

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# dx_condor_strike_cost <-
#   as.data.table(
#     dcast(
#       dx_condor_max_profit,
#       TKR + CMPRICE ~ id_strike,
#       value.var = "ASK",
#       sep = "_"
#     )
#   )[, tot_cost   := rowSums(.SD), .SDcols = c(3:6)
#   ][dx_condor_strike_diff, on = .(TKR)
#   ][, .(max_risk = strike_diff_c - tot_cost,
#         max_ret  = tot_cost - strike_diff_inner,
#         be_lower = strike_2 - (tot_cost -strike_diff_inner),
#         be_upper = strike_3 + (tot_cost -strike_diff_inner)), 
#     by = TKR
#   ][dx_condor_strike_cost, on = .(TKR)
#   ][,c(1,6:11,2:5)
#   ][, max_ret_on_risk := percent(max_ret / max_risk, accuracy = 0.1)]
# ------------------------------------------------------------------------------
dx_condor_strike_cost <-
  data.table::merge.data.table(as.data.table(
    dcast(
      dx_condor_max_profit,
      TKR + CMPRICE ~ id_strike,
      value.var = "ASK",
      sep = "_"
    )
  )[, tot_cost := rowSums(.SD), .SDcols = c(3:6)], dx_condor_strike_diff)   %>%
  .[, `:=`(
    max_risk = strike_diff_c - tot_cost,
    max_ret  = tot_cost - strike_diff_inner,
    be_lower = strike_2 - (tot_cost - strike_diff_inner),
    be_upper = strike_3 + (tot_cost - strike_diff_inner)
  ),
  by = "TKR"
  ]                                                                         %>%
  .[, max_ret_on_risk     := percent(max_ret / max_risk, accuracy = 0.1)]   %>%
  .[, max_ret_on_risk_ann := scales::percent(
                                (1/as.double(dx_condor_date[,4])) * 
                                (max_ret/max_risk),
                              accuracy = 2, big.mark = ",")]                %>%
  data.table::setnames(3:6, c("s1_cost", "s2_cost", "s3_cost", "s4_cost"))  %>%
  .[, max_loss :=
      (strike_diff_c -  (s2_cost + s4_cost)) +
      (strike_diff_p -  (s3_cost + s1_cost))
    ]
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
dx_condor_max_loss <- dx_condor_cost[, .(loss = sum(loss)),by = TKR]
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# dx_condor_max_profit <<- dx_condor_max_profit[, profit := sum(ASK), by = TKR]
# ------------------------------------------------------------------------------
dx_condor_max_profit <-
  dx_condor_max_profit[
    , profit := sum(ASK),
    by = TKR
  ][
    , c(1, 7)
  ][
    , .SD[1],
    by = TKR
  ]
#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# odd id_strike, i.e., puts id_strike = 1,3 / 2,4
# ------------------------------------------------------------------------------
# dx_condor_max_loss <<- data.table::setorder(
#   na.omit(
#     rbind(
#       dx_condor_key[id_strike %% 2 == 1,
#       ][
#         dx_blob,
#         nomatch = 0
#       ][
#         , c(1:4, 18)
#       ][
#         , diff := STRIKE - shift(STRIKE),
#         by = TKR
#       ],
#       dx_condor_key[id_strike %% 2 != 1,
#       ][
#         dx_blob,
#         nomatch = 0
#       ][
#         , c(1:4, 18)
#       ][
#         , diff := STRIKE - shift(STRIKE),
#         by = TKR
#       ]
#     )
#   ), TKR, id_strike
# )
# ------------------------------------------------------------------------------
# dx_condor_max_loss <<-
#   unique(
#     dx_condor_max_loss[,
#       loss := sum(diff),
#       by = TKR
#       ][
#       , c(1, 7)
#       ]
#   )
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
dx_condor_roi <-
  unique(
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
  )
# ------------------------------------------------------------------------------
# dx_condor_roi <-
#   merge(dx_condor_roi,
#         dcast(dx_condor_key[id_strike == 2 | id_strike == 3],
#               TKR + CMPRICE + date_run ~ id_strike,
#               value.var = "strike",
#               sep = "_"),
#         by = c("TKR"))[,-8]
# ------------------------------------------------------------------------------
#  TKR    profit  loss  roi     date_exp   date_run CMPRICE  2     3
# <char>  <num> <num> <char>     <Date>     <char>   <num> <num> <num>
# 1:    AMD   8.65 11.35  76.2% 2023-08-18 07/14/2023  115.90 115.0   120
# 2:    DIS   7.49 12.51  59.9% 2023-08-18 07/14/2023   88.62  85.0    90
# 3:   MSFT  11.45 13.55  84.5% 2023-08-18 07/14/2023  344.75 340.0   350
# 4:    OXY   3.96  6.04  65.6% 2023-08-18 07/14/2023   59.36  57.5    60
# 5:   RIVN   4.01  5.99  66.9% 2023-08-18 07/14/2023   24.87  22.5    25
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Probability of Profit
# TKR Company CMPRICE VOLF date_run EXPDAY diff
# ------------------------------------------------------------------------------
setkey(dx_blob, TKR, EXPDAY)
setkey(dx_condor_key, TKR)
setkey(dx_condor_roi, TKR, date_exp)

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
dx_condor_pop <-
  unique(
    cbind(
      dx_blob[dx_condor_roi, nomatch = 0][
        , .SD[1],
        by = TKR, .SDcols = c(1, 8:9, 55, 15)
      ][
        dx_date_exp,
        on = .(EXPDAY), nomatch = 0
      ][
        , -c(7:8)
      ][
        ,
        date_exp_diff_yr := diff / 365
      ][
        ,
        sigma := (VOLF / 100) * sqrt(as.double.difftime(date_exp_diff_yr))
      ][
        dx_condor_max_profit,
        nomatch = 0
      ][
        ,
        `:=`(
          breakeven_upper = CMPRICE + profit,
          breakeven_lower = CMPRICE - profit
        )
        #        breakeven_upper :=  CMPRICE + profit
      ],
      #          dx_condor_strike_disp, nomatch = 0],
      dx_int
    )
  )
# ------------------------------------------------------------------------------

#...............................................................................
browser()
#...............................................................................

# ------------------------------------------------------------------------------
S0  <- 90.49
vol <- 0.261
r   <- 0.0549
T   <- 37/252
P   <- 7.67
# ------------------------------------------------------------------------------
# Create a data table with the inputs
dt <- data.table(S0 = S0, vol = vol, r = r, T = T)

# Calculate the annualized standard deviation
dt[, sigma := vol * sqrt(T)]

BE_lower <- 90 - P
BE_upper <- 95 + P

dt[, prob := 
stats::pnorm((log(BE_upper/S0) + (r + (sigma ^ 2)/2) * T) / sigma * base::sqrt(T)) -
stats::pnorm((log(BE_lower/S0) + (r + (sigma ^ 2)/2) * T) / sigma * base::sqrt(T))]


#  7:    DIS   90.09 DIS   230818P00085000         1 -1.46     85
#  5:    DIS   90.09 DIS   230818C00090000         2  3.85     90
#  8:    DIS   90.09 DIS   230818P00095000         3  6.15     95
#  6:    DIS   90.09 DIS   230818C00100000         4 -0.87    100


# Given the following iron condor option position parameters

# current_stock_price <- 90.49
# upper_call_strike   <- 100
# lower_call_strike   <- 90
# lower_put_strike    <- 85
# upper_put_strike    <- 95
# implied_volatility  <- 0.261
# risk_free_rate      <- 0.0543
# days_to_expiration  <- 30

# How do I calculate the Maximum Potential Profit, Maximum Potential Loss and ROI using data.table with r code?


# Define the parameters
current_stock_price <- 90.49                                                    # dx_condor_key[,2]
upper_call_strike   <- 100                                                      # dx_condor_key[id_strike == 4, 5]
lower_call_strike   <- 90                                                       # dx_condor_key[id_strike == 2, 5]
lower_put_strike    <- 85                                                       # dx_condor_key[id_strike == 1, 5]
upper_put_strike    <- 95                                                       # dx_condor_key[id_strike == 3, 5]
net_credit          <- 7.35                                                     # dx_condor_roi[, 2]
num_simulations     <- 10000
implied_volatility  <- 0.216                                                    # dx_condor_pop[,4]/100
risk_free_rate      <- 0.0543                                                   # dx_int[,1]
days_to_expiration  <- 37

# Calculate time to expiration in years
time_to_expiration  <- days_to_expiration / 365                                 # dx_condor_pop[1,8]

# Generate random stock price scenarios
set.seed(123) # For reproducibility
z <- rnorm(num_simulations)
simulated_stock_prices <- current_stock_price * exp((risk_free_rate - 0.5 * implied_volatility^2) * time_to_expiration + implied_volatility * sqrt(time_to_expiration) * z)

# Calculate the payoff for each scenario
payoffs <- numeric(num_simulations)
for (i in 1:num_simulations) {
  payoff <- ifelse(
    simulated_stock_prices[i] < lower_put_strike,
    net_credit - (lower_put_strike - simulated_stock_prices[i]),
    ifelse(
      simulated_stock_prices[i] >= lower_put_strike & simulated_stock_prices[i] <= upper_put_strike,
      net_credit,
      ifelse(
        simulated_stock_prices[i] > upper_put_strike & simulated_stock_prices[i] < lower_call_strike,
        0,
        ifelse(
          simulated_stock_prices[i] >= lower_call_strike & simulated_stock_prices[i] <= upper_call_strike,
          net_credit,
          net_credit - (simulated_stock_prices[i] - upper_call_strike)
        )
      )
    )
  )
  payoffs[i] <- payoff
}

# Calculate the probability of profitability
probability_of_profit <- sum(payoffs > 0) / num_simulations

# Print the result
cat("Probability of Profitability:", probability_of_profit, "\n")

#...............................................................................
browser()
#...............................................................................

sigma <- vol * sqrt(T)

dt[,`:=`(avg=mean(mpg), med=median(mpg), min=min(mpg)), by=cyl]


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
# dx_condor[, cost_x :=
#   dx_condor[dx_blob, on = .(lag_1_optkr = OPTKR), nomatch = 0 ][
#   ,'ASK'] -
#   dx_condor[dx_blob, on = .(lag_0_optkr = OPTKR), nomatch = 0 ][
#   ,'BID']
#   ]

# dx_condor[, c(
# ------------------------------------------------------------------------------common rank
  # "cmrk") :=
# ------------------------------------------------------------------------------common rank
 # .(unique(dx_condor[dx_blob, on = .(lag_1_optkr = OPTKR), nomatch = 0 ][,'CMRK']))
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
g[[paste0("dx_condor")]]            <<- rbind(g[[paste0("dx_condor")]], new_records)
g[[paste0("dx_condor_strike")]] <<- rbind(g[[paste0("dx_condor_strike")]], new_records)
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

dx_condor[complete.cases(dx_condor), ][
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


# Load the required library
library(data.table)

# Given parameters
current_stock_price <- 88.62
upper_call_strike   <- 95
lower_call_strike   <- 85
upper_put_strike    <- 90
lower_put_strike    <- 80
implied_volatility  <- 0.259
risk_free_rate      <- 0.0543
days_to_expiration  <- 22
days_to_expiry      <- 22
call_price_long     <- 1.47
call_price_short    <- 5.75
put_price_long      <- 0.74
put_price_short     <- 3.95

# Given parameters
current_stock_price <- 47.85
upper_call_strike   <- 52.5
lower_call_strike   <- 47.5
upper_put_strike    <- 50
lower_put_strike    <- 45
implied_volatility  <- 0.296
risk_free_rate      <- 0.0543
days_to_expiration  <- 50
days_to_expiry      <- 50
put_price_long      <- 0.61
call_price_short    <- 1.58
put_price_short     <- 2.92
call_price_long     <- 0.22

# Given parameters
# current_stock_price <- dx_condor_key / dx_condor_pop    / dx_condor_strike_cost / dx_condor_strike_diff / dx_condor_strike_disp
# upper_call_strike   <- dx_condor_key / dx_condor_strike / dx_condor_strike_cost / dx_condor_strike_diff / dx_condor_strike_disp
# lower_call_strike   <- dx_condor_key / dx_condor_strike / dx_condor_strike_cost / dx_condor_strike_diff / dx_condor_strike_disp
# upper_put_strike    <- dx_condor_key / dx_condor_strike / dx_condor_strike_cost / dx_condor_strike_diff / dx_condor_strike_disp
# lower_put_strike    <- dx_condor_key / dx_condor_strike / dx_condor_strike_cost / dx_condor_strike_diff / dx_condor_strike_disp
# implied_volatility  <- dx_condor_pop
# risk_free_rate      <- dx_condor_pop
# days_to_expiration  <- dx_condor_date / dx_condor_pop / dx_condor_roi
# days_to_expiry      <- dx_condor_date / dx_condor_pop / dx_condor_roi
# put_price_long      <- dx_condor_strike_cost / dx_condor_strike_diff
# call_price_short    <- dx_condor_strike_cost / dx_condor_strike_diff
# put_price_short     <- dx_condor_strike_cost / dx_condor_strike_diff
# call_price_long     <- dx_condor_strike_cost / dx_condor_strike_diff


# ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ # Function to calculate option value using Black-Scholes formula ---------
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
option_value <- function(S, K, r, T, sigma, option_type) {

#...............................................................................
# browser()
#...............................................................................

  d1 <- (log(S / K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)

  if (option_type == "call") {
    option_price <- S * pnorm(d1) - K * exp(-r * T) * pnorm(d2)
  } else if (option_type == "put") {
    option_price <- K * exp(-r * T) * pnorm(-d2) - S * pnorm(-d1)
  } else {
    stop("Invalid option type")
  }
  return(option_price)
}
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#............Create a data.table to store the results...........................
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dx_condor_profit_loss <- data.table(
  Stock_Price = rep(seq(lower_put_strike, upper_call_strike, by = 0.50), days_to_expiration + 1),
  Date        = rep(seq(Sys.Date(), Sys.Date() + days_to_expiration, by = "days"), each = 31),
  PnL         = NA
)
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##  ~ Calculate P&L using mapply for each stock price and date combination -----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dx_condor_profit_loss$PnL <- mapply(
  function(stock_price, date) {
#...............................................................................
# browser()
#...............................................................................

    days_to_expiry   <- as.numeric(date - Sys.Date())
    call_long_value  <- option_value(stock_price, upper_call_strike, risk_free_rate, days_to_expiry / 365, implied_volatility, "call") * -1
    call_short_value <- option_value(stock_price, lower_call_strike, risk_free_rate, days_to_expiry / 365, implied_volatility, "call")
    put_long_value   <- option_value(stock_price, upper_put_strike,  risk_free_rate, days_to_expiry / 365, implied_volatility, "put") * -1
    put_short_value  <- option_value(stock_price, lower_put_strike,  risk_free_rate, days_to_expiry / 365, implied_volatility, "put")
    pnl              <- call_long_value + call_short_value + put_long_value + put_short_value
    return(pnl)
  },
  dx_condor_profit_loss$Stock_Price, dx_condor_profit_loss$Date
)

# Print the data.table
print(dx_condor_profit_loss)

dx_condor_pnl <<- data.table::dcast(dx_condor_profit_loss,
       Stock_Price ~ Date,
       value.var = "PnL",
       sep = "_")


dx_condor_pnl <<- data.table::dcast(dx_condor_profit_loss, 
                  ... ~ Date,
                  fun.aggregate = sum,
                  value.var     ="PnL")
