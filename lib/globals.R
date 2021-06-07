# Add any project specific configuration here.
# Date Formats in R    https://tinyurl.com/r-date-format
add.config(
  apply.override = FALSE,
  currentYr = as.numeric(format(Sys.Date(), format="%y")),
  currentYr4 = as.numeric(format(Sys.Date(), format="%Y")),
  lastYr = as.numeric(format(Sys.Date(), format="%y")) - 1,
  LastYr4 = as.numeric(format(Sys.Date(), format="%Y"))-1,
  currentAY = as.numeric(paste(as.numeric(format(Sys.Date(), format="%y")) - 1, as.numeric(format(Sys.Date(), format="%y")), sep = "")),
  header = "Project Template" # header in reports
)

################################################################################
## Repo Package: data management to build centralized metadata repository       ### https://github.com/franapoli/repo
## Check existence of directory and create if doesn't exist                     ### https://tinyurl.com/y3adrqwa
################################################################################
mainDir <- (".")
subDir  <- ("repo")
rp_path <- file.path(mainDir, subDir)

# Add project specific configuration that can be overridden from load.project()
add.config(
  apply.override = TRUE
)
