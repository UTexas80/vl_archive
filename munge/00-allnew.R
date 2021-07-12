################################################################################
## Step 00.00 Processing Start Time - start the timer                        ###
################################################################################
start.time <- Sys.time()
started.at <- proc.time()
################################################################################
## Step 00.01 create object table               https://tinyurl.com/y3adrqwa ###
## Check existence of directory and create if doesn't exist                  ###
################################################################################
dirCheck(mainDir, subDir)
################################################################################
## Step 00.02 dynamically create dataframe      https://tinyurl.com/y3adrqwa ###
## grep with multiple patterns                  https://tinyurl.com/8kpnktnm ###
################################################################################
x00 <- grep("^ALLNEW" | "CALLS*" | "PUTS*" | "Top200*", ls(), value = TRUE)
# ------------------------------------------------------------------------------
lapply(x00, function(nm) {
    df  <- get(nm)
    g[[paste0("dx", "_", nm)]] <- df
    data.table::setDT(paste0("dx", "_", nm)])
    }
)
################################################################################
# use first row data as column names in r         https://tinyurl.com/2eyyyb7b
################################################################################
dt_allnew               <- ALLNEW %>%  row_to_names(row_number = 11)
# Date Conversion mm/dd/yyy hh:mm:ss to yyyy-mm-dd in R [closed]                https://tinyurl.com/3byrbxzp
dt_allnew$EXPDAY        <- as.Date(dt_allnew$EXPDAY, format('%m/%d/%Y'))
dt_allnew$date_run      <- ALLNEW[3,2]
dt_allnew$date_run      <- as.Date(dt_allnew$date_run,  format('%m/%d/%Y'))
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
################################################################################
## Step 00.99: VERSION HISTORY                                               ###
################################################################################
a00.version             <- "1.0.0"
a00.ModDate             <- as.Date("2020-07-10")
# ------------------------------------------------------------------------------
# 2020.07.10 - v.1.0.0
#  1st release
# ------------------------------------------------------------------------------
