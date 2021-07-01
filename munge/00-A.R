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
## How to do login on website using R and to check login success?            ### https://tinyurl.com/4deeeey
################################################################################
# url <- "https://lgloz050.lss.emc.com:58443/APG/"
# dn_url <- "https://lgloz050.lss.emc.com:58443/APG/lookup/Report%20Library/Amazon%20S3/Inventory/Accounts/report.csv"
# url <- "https://investors.valueline.com/Users/Account/LogOn?"
# dn_url <- "https://www3.valueline.com/secure/options/ALLNEW.CSV"
# session <-  html_session(url)
# form <- html_form(session)[[1]]
# fl_fm <- html_form_set()(form,
#                         J_username = "GlenCFalk",
#                         j_password = key_get("ValueLIne", "GlenCFalk"))
## Step 00.02: clean dataframes with Janitor                                 ###
## Use fread on zipped files                        https://tinyurl.com/2nphb6cd
## You can import a zipped file without unzipping it first.
## fread can import gz and bz2 files directly, such as mydt <- fread("myfile.gz")
## If you need to import a zip file, you can unzip it with the unzip system
## command within fread, using the syntax mydt <- fread(cmd = 'unzip -cq myfile.zip').
################################################################################
# use first row data as column names in r         https://tinyurl.com/2eyyyb7b
################################################################################
dt_allnew               <- ALLNEW %>%  row_to_names(row_number = 11)
# Date Conversion mm/dd/yyy hh:mm:ss to yyyy-mm-dd in R [closed]                https://tinyurl.com/3byrbxzp
dt_allnew$EXPDAY        <- as.Date(dt_allnew$EXPDAY, format('%m/%d/%Y'))
# ------------------------------------------------------------------------------
Top200CallsBuy          <- setorder(Top200CallsBuy, CMRK, TechRank, -X.vCM)
Top200CallsBuy$EXPDAY   <- as.Date(Top200CallsBuy$EXPDAY, format('%m/%d/%Y'))
dt_CallsBuy             <- Top200CallsBuy[CMRK == 1 & TechRank == 1,]
# ------------------------------------------------------------------------------
Top200CallsWrite        <- setorder(Top200CallsWrite, -CMRK, -TechRank)
Top200CallsWrite$EXPDAY <- as.Date(Top200CallsWrite$EXPDAY, format('%m/%d/%Y'))
dt_CallsWrite           <- Top200CallsWrite[CMRK == 5 & TechRank == 5,]
# ------------------------------------------------------------------------------
Top200PutsBuy           <- setorder(Top200PutsBuy, -CMRK, -TechRank)
Top200PutsBuy$EXPDAY    <- as.Date(Top200PutsBuy$EXPDAY, format('%m/%d/%Y'))
dt_PutsBuy              <- Top200PutsBuy[CMRK == 5 & TechRank == 5,]
# ------------------------------------------------------------------------------
Top200PutsWrite         <- setorder(Top200PutsWrite, CMRK, TechRank)
Top200PutsWrite$EXPDAY  <- as.Date(Top200PutsWrite$EXPDAY, format('%m/%d/%Y'))
dt_PutsWrite            <- Top200PutsWrite[CMRK == 1 & TechRank == 1,]
################################################################################
## Step 00.99: VERSION HISTORY                                               ###
################################################################################
a00.version             <- "1.0.0"
a00.ModDate             <- as.Date("2020-07-10")
# ------------------------------------------------------------------------------
# 2020.07.10 - v.1.0.0
#  1st release
# ------------------------------------------------------------------------------
