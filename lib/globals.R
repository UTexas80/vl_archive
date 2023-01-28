# Add any project specific configuration here.
# Date Formats in R    https://tinyurl.com/r-date-format
add.config(
  apply.override  = FALSE,
  currentYr       = as.numeric(format(Sys.Date(), format = "%y")),
  currentYr4      = as.numeric(format(Sys.Date(), format = "%Y")),
  lastYr          = as.numeric(format(Sys.Date(), format = "%y")) - 1,
  LastYr4         = as.numeric(format(Sys.Date(), format = "%Y")) - 1,
  currentAY       = as.numeric(paste(as.numeric(format(Sys.Date(), format = "%y")) - 1, as.numeric(format(Sys.Date(), format = "%y")), sep = "")),
  header          = "ValueLine" # header in reports
)
# ----------------------------------dat--------------------------------------------
# Date Configuration
# ------------------------------------------------------------------------------
date_curr_mo      <- as.numeric(format(Sys.Date(), format = "%m"))
date_curr_yr      <- as.numeric(format(Sys.Date(), format = "%y"))
date_curr_yr4     <- as.numeric(format(Sys.Date(), format = "%Y"))
date_prefix       <- as.character(today(), format = '%Y%m%d')
# ------------------------------------------------------------------------------
# Directory Configuration
# ------------------------------------------------------------------------------
dir_downloads     <- "C:/Users/glen.falk/Downloads/"
dir_git_data      <- "Documents/GitHub/ValueLine/data"
dir_git_ml        <- "Documents/GitHub/ValueLine/ml"
dir_ms365_top_200 <- "Options/ValueLine/Top200"
dir_ms365_zip     <- "Options/ValueLine/Zip/"
# ------------------------------------------------------------------------------
vl_path           <- file.path(
  "C:/Users/glen.falk/OneDrive - IHS Markit/Documents/github/vl//"
)
# ------------------------------------------------------------------------------
valueline_path_data      <- file.path(
  "C:/Users/glen.falk/OneDrive - IHS Markit/Documents/github/ValueLine/data/"
)

# ------------------------------------------------------------------------------
valueline_path           <- file.path(
  "C:/Users/glen.falk/OneDrive - IHS Markit/Documents/github/ValueLine/"
)
vl_source         <- "/ValueLine.r"
# ------------------------------------------------------------------------------  
zip_file_path <- paste0(here::here("zip//"))
# ------------------------------------------------------------------------------
# create global environment to dynamically name data frames                     ### https://tinyurl.com/y3adrqwa ###
# ------------------------------------------------------------------------------
g                 <- globalenv()             # https://tinyurl.com/r3yrspv   ###
z                 <- TRUE                  # template switch create dx_blob
################################################################################
# Hmisc package
################################################################################
# getRs('reptools.r')
# getRs('movStats.r')
################################################################################
## Repo Package: data management to build centralized metadata repository       ### https://github.com/franapoli/repo
## Check existence of directory and create if doesn't exist                     ### https://tinyurl.com/y3adrqwa
################################################################################
mainDir           <- (".")
subDir            <- ("repo")
rp_path           <- file.path(mainDir, subDir)
# ------------------------------------------------------------------------------
# Add project specific configuration that can be overridden from load.project()
add.config(
  apply.override  = TRUE
)
