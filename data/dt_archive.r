################################################################################
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
dt_zip_files  <- setDT(cd$list_files(dir_ms365_zip))
file_table    <- data.table(file_name = character(0))
# ------------------------------------------------------------------------------

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
dt_file_table <- as.data.table(sapply(dt_zip_files[,1], function(x) x))
# ------------------------------------------------------------------------------
# data.table - create multiple column names
dt_file_table <-
  dt_file_table[
    , .(
      name      = name,
      name_char = str_extract(name, "[a-zA-Z]+"),
      name_num  = as.character(gsub("[^0-9]", "", name))
      )
    ]
# ------------------------------------------------------------------------------
# dt_file_table[, name_char := str_extract(name, "[a-zA-Z]+")]
# dt_file_table$name_num <- as.character(gsub("[^0-9]", "", dt_file_table$name))
# ------------------------------------------------------------------------------
dt_file_table[, N :=.N, by = name_num]
dt_file_table <-
  rbind(
    dt_file_table[stringr::str_ends(name_char, "W") & N == 1, ],
    dt_file_table[stringr::str_ends(name_char, "W") & N == 2, ],
    dt_file_table[stringr::str_ends(name_char, "W") & N == 3, ]    
#    dt_file_table[stringr::str_ends(name_char, "S") & N == 3, ]
  )
dt_file_table <- data.table::setorder(dt_file_table,name)
# ------------------------------------------------------------------------------

