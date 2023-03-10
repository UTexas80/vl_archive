# ------------------------------------------------------------------------------
# 003. stealth curve parameters                                              ---
# ------------------------------------------------------------------------------
################################################################################
## Step 000.00 Processing Start Time - start the timer                       ###
################################################################################
wmt$diff_lo <- wmt$Close - wmt$Stealth_Curve_Low
wmt$diff_hi <- wmt$Close - wmt$Stealth_Curve_High
# ------------------------------------------------------------------------------
dt_wmt_stealth <- rbind(wmt[abs(diff_lo) <= 0.1,], wmt[abs(diff_hi) <= 0.1,])
# ------------------------------------------------------------------------------
# start.time     = Sys.time()
# started.at     <- proc.time()
################################################################################
## Step 000.01 create object table              https://tinyurl.com/y3adrqwa ###
## Check existence of directory and create if doesn't exist                  ###
################################################################################
# dirCheck(mainDir, subDir)
################################################################################
## Step 000.02 dynamically clean the tables                                  ###
################################################################################
# x00 <- grep("^dt_vol\\.", ls(), value = TRUE)
# ------------------------------------------------------------------------------
# lapply(x00, function(nm) {
#   df  <- get(nm)
#   setDT(df)
#   setkey(df, "id")
#   setorder(df, id)
#   }
# )
################################################################################
## Step 000.99: VERSION HISTORY                                              ###
################################################################################
a000.version = "1.0.0"
a000.ModDate = as.Date("2022-06-14")
# ------------------------------------------------------------------------------
# 2022.06.14 - v.1.0.0
#  1st release
# ------------------------------------------------------------------------------
