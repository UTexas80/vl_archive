################################################################################
## Step 00.00 Processing Start Time - start the timer                        ###
################################################################################
start.time = Sys.time()
started.at <- proc.time()
################################################################################
## Step 00.01 create object table               https://tinyurl.com/y3adrqwa ###
## Check existence of directory and create if doesn't exist                  ###
################################################################################
dirCheck(mainDir, subDir)
################################################################################
## Step 00.02: clean dataframes with Janitor                                 ###
## Use fread on zipped files                        https://tinyurl.com/2nphb6cd
## You can import a zipped file without unzipping it first.
## fread can import gz and bz2 files directly, such as mydt <- fread("myfile.gz")
## If you need to import a zip file, you can unzip it with the unzip system
## command within fread, using the syntax mydt <- fread(cmd = 'unzip -cq myfile.zip').
################################################################################
url <- "http://www3.valueline.com/secure/options/ALLNEW.CSV"
DT <- data.table::fread(url, fill = TRUE, skip=11, verbose = T)
################################################################################
## Step 00.99: VERSION HISTORY                                               ###
################################################################################
a00.version = "1.0.0"
a00.ModDate = as.Date("2020-07-10")
# ------------------------------------------------------------------------------
# 2020.07.10 - v.1.0.0
#  1st release
# ------------------------------------------------------------------------------
