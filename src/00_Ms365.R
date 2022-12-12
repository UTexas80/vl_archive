# ------------------------------------------------------------------------------
# 99. valueline                                                              ---
# ------------------------------------------------------------------------------
################################################################################
## Step 99.01 connect to OneDrive                                            ###
################################################################################
# ------------------------------------------------------------------------------
# Microsoft365R                                                              ---
#...............................................................................
browser()
#...............................................................................
od         <- Microsoft365R::get_personal_onedrive()
dt_top_200 <- as.data.table(cd$list_files(dir_ms365_top_200))
#...............................................................................
# download files
#...............................................................................
cd$download_file(
  paste0(dir_ms365_top_200,  "/211208", "Top200CallsBuy.csv"), 
  here::here("data", "Top200CallsBuy.csv"),
  overwrite = TRUE
  )
dt_top_200_calls_buy <- fread(here::here("data", "Top200CallsBuy.csv"), 
                              fill = TRUE)
#...............................................................................
od$download_file(
  paste0(dir_ms365_top_200,  date_prefix, "Top200CallsBuy.csv"), 
  paste0(dir_ms365_top_200,  date_prefix, "Top200CallsWrit.csv")
)  
#...............................................................................
od$upload_file(
  paste0(dir_download,  date_prefix, "Top200CallsWrite.csv"), 
  paste0(dir_ms365_zip, date_prefix, "Top200CallsWrite.csv")
)  
#...............................................................................
od$upload_file(
  paste0(dir_download,  date_prefix, "Top200PutsBuy.csv"), 
  paste0(dir_ms365_zip, date_prefix, "Top200PutsBuy.csv")
)
#...............................................................................
od$upload_file(
  paste0(dir_download,  date_prefix, "Top200PutsWrite.csv"), 
  paste0(dir_ms365_zip, date_prefix, "Top200PutsWrite.csv")
)  
#...............................................................................
od$upload_file(
  paste0(dir_download,  date_prefix, "ALLNEW.ZIP"), 
  paste0(dir_ms365_zip, date_prefix, "ALLNEW.ZIP")
)  
#...............................................................................
od$upload_file(
  paste0(dir_download,  date_prefix, "CALLS.ZIP"), 
  paste0(dir_ms365_zip, date_prefix, "CALLS.ZIP")
)  
#...............................................................................
od$upload_file(
  paste0(dir_download,  date_prefix, "PUTS.ZIP"), 
  paste0(dir_ms365_zip, date_prefix, "PUTS.ZIP")
)  
#...............................................................................
