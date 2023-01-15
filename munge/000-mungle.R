################################################################################
## Step 00.00 Processing Start Time - start the timer                        ###
################################################################################
start.time              <- Sys.time()
started.at              <- proc.time()
################################################################################
## Step 00.01 create object table               https://tinyurl.com/y3adrqwa ###
## Check existence of directory and create if doesn't exist                  ###
################################################################################
dirCheck(mainDir, subDir)
################################################################################
## Step 00.02 dynamically create dataframe      https://tinyurl.com/y3adrqwa ###
## grep with multiple patterns                  https://tinyurl.com/8kpnktnm ###
################################################################################
## create a template to rbind the dataframes
# ------------------------------------------------------------------------------
dx_blob                 <- ALLNEW[11,] %>%  row_to_names(row_number = 1)
data.table::setnames(dx_blob, 1:49,  names(Top200CallsBuy)[1:49])
data.table::setnames(dx_blob,50:54,  as.character(ALLNEW[11,50:54]))
dx_blob$date_run        <- ALLNEW[4,2] 
dx_blob$date_run        <- as.Date(dx_blob$date_run, format('%m/%d/%Y'))
# dx_blob$EXPDAY          <- as.Date(dx_blob$EXPDAY,   format('%m/%d/%Y'))
dx_blob[,c(2,6,9,24:25,27:28,32:35,37:46)] <-
        lapply(dx_blob[,c(2,6,9,24:25,27:28,32:35,37:46)],
        function(x) parse_number(x))
dx_blob[,c(3:4,8,12,14,16:23,26,29:31,36,47:54)] <-
        lapply(dx_blob[,c(3:4,8,12,14,16:23,26,29:31,36,47:54)],
        function(x) parse_number(x))
# ------------------------------------------------------------------------------
# x00                   <- grep(pattern = 'CALL|PUT', ls(), value = TRUE)
x00                     <- grep(pattern = 'AL*|UT', ls(), value = TRUE)
# ------------------------------------------------------------------------------
lapply(x00, function(nm) {
    df <- get(nm)
    g[[paste0("dx", "_", nm)]] <- tail(df,-11)
#    data.table::setDT(g[[paste0("dx", "_", nm)]], keep.rownames = TRUE)
    data.table::setDT(g[[paste0("dx", "_", nm)]])
#    g[[paste0("dx", "_", nm)]] %>% row_to_names(row_number = 1) 
    data.table::setnames(g[[paste0("dx", "_", nm)]], 1:49,  names(Top200CallsBuy)[1:49])
    data.table::setnames(g[[paste0("dx", "_", nm)]], 50:54, as.character(ALLNEW[11,50:54]))
    g[[paste0("dx", "_", nm)]]$date_run <- df[3,2] 
    g[[paste0("dx", "_", nm)]]$date_run <- as.Date(g[[paste0("dx", "_", nm)]]$date_run, format('%m/%d/%Y'))
    data.table::setkey(g[[paste0("dx", "_", nm)]], "OPTKR", "C.P")
# ------------------------------------------------------------------------------pct to dbl
    g[[paste0("dx", "_", nm)]][,c(2,6,9,24:25,27:28,32:35,37:46)] <-
        lapply(g[[paste0("dx", "_", nm)]][,c(2,6,9,24:25,27:28,32:35,37:46)],
        function(x) parse_number(x))
# ------------------------------------------------------------------------------chr to dbl
    g[[paste0("dx", "_", nm)]][,c(3:4,8,12,14,16:23,26,29:31,36,47:54)] <-
        lapply(g[[paste0("dx", "_", nm)]][,c(3:4,8,12,14,16:23,26,29:31,36,47:54)],
        function(x) parse_number(x))
# ------------------------------------------------------------------------------
    dx_blob             <<-      rbind(dx_blob, g[[paste0("dx", "_", nm)]] )
# ------------------------------------------------------------------------------
    }
)
# ------------------------------------------------------------------------------
# remove redundant entries                                                   ---
# ------------------------------------------------------------------------------
dx_blob <<- setorder(
                unique(dx_blob[,-"Record.Number"]), OPTKR)[
            , Record.Number := .I][
            ,c(1:13,55,14:54)]
################################################################################
# table creation                                                             ###
################################################################################
dx_company              <- as.data.table(distinct(dx_blob[,1]))
# ------------------------------------------------------------------------------
dx_date_exp             <- data.table::setorder(as.data.table(distinct(dx_blob[,15])),EXPDAY)
# dx_date_exp$day         <- weekdays(dx_date_exp$EXPDAY)
dx_date_exp$EXPDAY      <- as.Date(dx_date_exp$EXPDAY, format('%m/%d/%Y'))
dx_date_exp$day         <- data.table::wday(dx_date_exp$EXPDAY)
dx_date_exp$week        <- data.table::week(dx_date_exp$EXPDAY)
dx_date_exp$diff        <- dx_date_exp$EXPDAY - Sys.Date()
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
names(dt_date_exp_mth)[1] <- "EXPDAY"
dt_date_exp_mth$EXPDAY      <- as.Date(dt_date_exp_mth$EXPDAY)
dt_date_exp_mth$day         <- data.table::wday(dt_date_exp_mth$EXPDAY)
dt_date_exp_mth$week        <- data.table::week(dt_date_exp_mth$EXPDAY)
dt_date_exp_mth$diff        <- dt_date_exp_mth$EXPDAY - Sys.Date()
dt_date_exp_mth             <<- setorder(dt_date_exp_mth, EXPDAY)
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# dx_date_exp             <- dx_date_exp[,c(1:2,4:5,3)]
setkey(dx_date_exp,EXPDAY)
# ------------------------------------------------------------------------------
# dx_date_exp[week == 3 & between(diff, 30, 100),]
################################################################################
# Week number of the month                      https://tinyurl.com/2zejur23 ###
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
dx_industry             <- data.table::setorder(as.data.table(distinct(dx_blob[,7])),INDUST)
dx_tech_rank            <- data.table::setorder(as.data.table(distinct(dx_blob[,4])),TechRank)
dx_strike               <- data.table::setorder(as.data.table(distinct(dx_blob[,12])),STRIKE)
dx_ticker               <- as.data.table(distinct(dx_blob[,c(10,8)]))
################################################################################
# use first row data as column names in r         https://tinyurl.com/2eyyyb7b
################################################################################
dt_allnew               <- ALLNEW %>%  row_to_names(row_number = 11)
# Date Conversion mm/dd/yyy hh:mm:ss to yyyy-mm-dd in R [closed]                https://tinyurl.com/3byrbxzp
dt_allnew$EXPDAY        <- as.Date(dt_allnew$EXPDAY,   format('%m/%d/%Y'))
dt_allnew$date_run      <- ALLNEW[3,2]
dt_allnew$date_run      <- as.Date(dt_allnew$date_run, format('%m/%d/%Y'))
# ------------------------------------------------------------------------------
# format following character percent columns as numeric double
# ------------------------------------------------------------------------------
# `DIV'D,HISTVO,VOLF,`BID UN/OV`,`ASK UN/OV`,`10%`,`-10%',CCPWPA,CCPWBE,CCPWMAX,
# `I/OTM`,`BID IMPLIE`,`ASK IMPLIE`,VOLFADJ,`Forecast PrtbStrk`,`Bid Prbstrk`,
# `Ask Prbstrk`,PctDble, Margin, YLDMGN, MXMGN
# ------------------------------------------------------------------------------pct to dbl
dt_allnew[,c(2,6,9,24:25,27:28,32:35,37:46)] <-
    lapply(dt_allnew[,c(2,6,9,24:25,27:28,32:35,37:46)], function(x) parse_number(x))
# ------------------------------------------------------------------------------chr to dbl
dt_allnew[,c(3:4,8,12,14,16:23,26,29:31,36,47:54)] <-
        lapply(dt_allnew[,c(3:4,8,12,14,16:23,26,29:31,36,47:54)], function(x) parse_number(x))
# ------------------------------------------------------------------------------
dt_calls                <- rbind(dx_CALLS1,dx_CALLS2,dx_CALLS3)
dt_puts                 <- rbind(dx_PUTS1,dx_PUTS2,dx_PUTS3)
################################################################################
# R: How to get the Week number of the month        https://tinyurl.com/3bk8x28h
################################################################################
weekdays(dx_date_exp$EXPDAY)
w <- dx_date_exp[, wk := isoweek(EXPDAY)]
a <- dx_date_exp[, week := ifelse( ceiling(mday(EXPDAY)/7) == 5, 4, ceiling(mday(EXPDAY)/7) )]
################################################################################
## Step 00.99: VERSION HISTORY                                               ###
################################################################################
a00.version             <- "1.0.0"
a00.ModDate             <- as.Date("2020-07-10")
# ------------------------------------------------------------------------------
# 2020.07.10 - v.1.0.0
#  1st release
# ------------------------------------------------------------------------------
