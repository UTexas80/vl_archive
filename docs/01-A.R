# ------------------------------------------------------------------------------
# 01. Project Name                                                           ---
# ------------------------------------------------------------------------------
################################################################################
## Step 01.01 clean the tables                                               ###
################################################################################
x01 <- grep("^X01", ls(), value = TRUE)
# ------------------------------------------------------------------------------
lapply(x01, function(nm) {
  df  <- get(nm)
  g[[paste0("dt", nm, "_")]] <- nm
  setDT(df)
  setkey(df, "id")
  setorder(df, id)
  }
)
# ------------------------------------------------------------------------------
# How to do login on website using R and to check login success?
# https://tinyurl.com/z48hzdyr
# ------------------------------------------------------------------------------
url     <- "https://investors.valueline.com/Users/Account/LogOn?"
dn_url  <- "http://www3.valueline.com/secure/options/ALLNEW.ZIP"
session <- session(url)
form    <- html_form(session)[[1]]
fl_fm   <- html_form_set(form,
                        j_username = "GlenCFalk",
                        j_password = key_get("valueline", "GlenCFalk")
                        ) # "test password"
main_page <- submit_form(session, fl_fm)
downlaod <- jump_to(main_page,dn_url)
writeBin(downlaod$response$content, basename(dn_url))
################################################################################
## Step 00.99: VERSION HISTORY                                               ###
################################################################################
a00.version = "1.0.0"
a00.ModDate = as.Date("2020-07-10")
# ------------------------------------------------------------------------------
# 2020.07.16 - v.1.0.0
#  1st release
# ------------------------------------------------------------------------------
