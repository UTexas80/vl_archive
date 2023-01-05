## Step 00.0: load ALLNEW.CSV                                                ###
################################################################################
# ------------------------------------------------------------------------------
cd <- Microsoft365R::get_personal_onedrive()
# ------------------------------------------------------------------------------
dt_download  <- cbind(
  as.data.table(
    cd$list_items(
      dir_git_data))[
        name %like% ".csv|.CSV",][,1],
  as.data.table(
    cd$list_items(
      dir_git_data, full_names = T))[
        name %like% ".csv|.CSV",][,1])[,2:1]
# ------------------------------------------------------------------------------
cd$download_file("Documents/GitHub/ValueLine/data/ALLNEW.CSV",
                 paste0(here::here("data", "ALLNEW.CSV")),
                 overwrite = TRUE)
# ------------------------------------------------------------------------------
cd$download_file("Documents/GitHub/ValueLine/data/Top200CallsBuy.csv",
                 paste0(here::here("data", "Top200CallsBuy.csv")),
                 overwrite = TRUE)
# ------------------------------------------------------------------------------
cd$download_file("Documents/GitHub/ValueLine/data/Top200CallsWrite.csv",
                 paste0(here::here("data", "Top200CallsWrite.csv")),
                 overwrite = TRUE)
# ------------------------------------------------------------------------------
cd$download_file("Documents/GitHub/ValueLine/data/Top200PutsBuy.csv",
                 paste0(here::here("data", "Top200PutsBuy.csv")),
                 overwrite = TRUE)
# ------------------------------------------------------------------------------
cd$download_file("Documents/GitHub/ValueLine/data/Top200PutsWrite.csv",
                 paste0(here::here("data", "Top200PutsWrite.csv")),
                 overwrite = TRUE)
# ------------------------------------------------------------------------------
cd$download_file("Documents/GitHub/ValueLine/zip/CALLS.zip",
                 paste0(here::here("zip", "CALLS.zip")),
                 overwrite = TRUE)
# ------------------------------------------------------------------------------
cd$download_file("Documents/GitHub/ValueLine/zip/PUTS.zip",
                 paste0(here::here("zip", "PUTS.zip")),
                 overwrite = TRUE)
# ------------------------------------------------------------------------------
utils::unzip(
  paste0(here::here("zip", "CALLS.zip")),
  list      = FALSE,
  overwrite = TRUE,
  exdir     = here::here("data")
)
# ------------------------------------------------------------------------------
utils::unzip(
  paste0(here::here("zip", "PUTS.zip")),
  list      = FALSE,
  overwrite = TRUE,
  exdir     = here::here("data")
  )
# ------------------------------------------------------------------------------
