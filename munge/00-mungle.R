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
dx_blob$date_run        <- ALLNEW[3,2] 
dx_blob$date_run        <- as.Date(dx_blob$date_run, format('%m/%d/%Y'))
dx_blob[,c(2,6,9,24:25,27:28,32:35,37:46)] <-
        lapply(dx_blob[,c(2,6,9,24:25,27:28,32:35,37:46)],
        function(x) parse_number(x))
dx_blob[,c(3:4,8,12,14,16:23,26,29:31,36,47:54)] <-
        lapply(dx_blob[,c(3:4,8,12,14,16:23,26,29:31,36,47:54)],
        function(x) parse_number(x))
# ------------------------------------------------------------------------------
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
################################################################################
# table creation                                                             ###
################################################################################
dx_company              <- as.data.table(distinct(dx_blob[,1]))
dx_date_exp             <- as.data.table(distinct(dx_blob[,15]))
dx_industry             <- as.data.table(distinct(dx_blob[,7]))
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
################################################################################
## Step 00.99: VERSION HISTORY                                               ###
################################################################################
a00.version             <- "1.0.0"
a00.ModDate             <- as.Date("2020-07-10")
# ------------------------------------------------------------------------------
# 2020.07.10 - v.1.0.0
#  1st release
# ------------------------------------------------------------------------------
