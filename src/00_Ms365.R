# ------------------------------------------------------------------------------
# 99. valueline                                                              ---
# ------------------------------------------------------------------------------
################################################################################
## Step 99.01 connect to OneDrive                                            ###
################################################################################
# ------------------------------------------------------------------------------
# Microsoft365R                                                              ---
# ------------------------------------------------------------------------------

#...............................................................................
browser()
#...............................................................................

# ------------------------------------------------------------------------------
cd             <- Microsoft365R::get_personal_onedrive()
# ------------------------------------------------------------------------------
dt_x           <- cbind(
  as.data.table(
    cd$list_items(
      dir_git_data))[
        name %like% ".csv|.CSV",][,1],
  as.data.table(
    cd$list_items(
      dir_git_data, full_names = T))[
        name %like% ".csv|.CSV",][,1])[,2:1]
# ------------------------------------------------------------------------------
names(dt_x)[1] <- "full_name"
names(dt_x)    <- tolower(names(dt_x))
# ------------------------------------------------------------------------------
# dt_x$full_name <- tolower(dt_x$full_name)
# dt_x$name <- tolower(dt_x$name)
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
                 paste0(here::here("data", "Top200CallsBuy.csv")),
                 overwrite = TRUE)
# ------------------------------------------------------------------------------
cd$download_file("Documents/GitHub/ValueLine/data/Top200PutsWrite.csv", 
                 paste0(here::here("data", "Top200CallsWrite.csv")),
                 overwrite = TRUE)
# ------------------------------------------------------------------------------
cd$download_file("Documents/GitHub/ValueLine/data/PUTS1.CSV", overwrite = TRUE)
cd$download_file("Documents/GitHub/ValueLine/data/Top200CallsBuy.csv", overwrite = TRUE)
cd$download_file("Documents/GitHub/ValueLine/zip/CALLS.ZIP", overwrite = TRUE)
cd$download_file("Documents/GitHub/ValueLine/zip/PUTS.ZIP", overwrite = TRUE)
cd$download_file("Documents/GitHub/ValueLine/zip/ALLNEW.ZIP", overwrite = TRUE)
# ------------------------------------------------------------------------------
for (i in 1:nrow(dt_x[name %like% "Top200", ])) {
  cd$download_file(
    dt_x[i, 1],
    paste0(here::here("data", dt_x[i, 2])),
    overwrite = TRUE
  )
}
# ------------------------------------------------------------------------------




cbind(as.data.table(cd$list_items(dir_github_data))[name %like% ".csv|.CSV",][,1], as.data.table(cd$list_items(dir_github_data, full_names = T))[name %like% ".csv|.CSV",][,1])
cbind(as.data.table(cd$list_items(dir_github_data))[name %like% ".csv|.CSV",][,1], as.data.table(cd$list_items(dir_github_data, full_names = T))[name %like% ".csv|.CSV",][,1])
#...............................................................................
od          <- Microsoft365R::get_personal_onedrive()
dt_git_data <- as.data.table(
  cbind(
    cd$list_items(dir_github_data)[name %like% ".csv|.CSV",][,1],
    cd$list_items(dir_github_data, full_names=TRUE))[name %like% ".csv|.CSV",][,1]
  )
dt_top_200  <- as.data.table(cd$list_files(dir_ms365_top_200))
#...............................................................................
# download files
#...............................................................................

    cd$download_file(
        dt_x[1,1],
        dt_x[1,2],
        overwrite = T
    )


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
