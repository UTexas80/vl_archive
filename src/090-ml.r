#................................................................
# browser()
#................................................................
dx_date_exp_mth
# ------------------------------------------------------------------------------
dx_blob$EXPDAY        <- as.Date(dx_blob$EXPDAY, format('%m/%d/%Y'))
dx_blob               <- setorder(dx_blob, OPTKR)
# ------------------------------------------------------------------------------
setkey(dx_blob,         EXPDAY)
setkey(dx_date_exp,     EXPDAY)
setkey(dx_date_exp_mth, EXPDAY)
# ------------------------------------------------------------------------------
# dx_blob[dx_date_exp[ day == 6 & week == 3, ][1,1]][,c(10,12)]
dx_tkr_stk             <- dx_blob[dt_date_exp_mth[between(diff, 30,59),][1,1]][,c(10,12)]
# dx_tkr_stk           <- dx_blob[dx_date_exp[day == 6 & week == 3, ][1,1]][,c(10,12)]
# ------------------------------------------------------------------------------
setkey(dx_tkr_stk,    TKR)
setkey(dx_ticker,     TKR)
# ------------------------------------------------------------------------------
s_minus_0              <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N],     by = .(TKR)]
s_minus_1              <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N - 1], by = .(TKR)]
s_plus_0               <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, head(.SD, 1),   by = .(TKR)]
s_plus_1               <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, .SD[2],         by = .(TKR)]
# ------------------------------------------------------------------------------
dx_s_minus_0           <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N],     by = .(TKR)]
dx_s_minus_1           <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N - 1], by = .(TKR)]
dx_s_minus_2           <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE >= STRIKE, STRIKE[.N - 2], by = .(TKR)]
# ------------------------------------------------------------------------------
dx_s_plus_0            <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, head(.SD, 1),   by = .(TKR)][,c(1,3)]
dx_s_plus_1            <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, .SD[2],         by = .(TKR)][,c(1,3)]
dx_s_plus_2            <- unique(dx_ticker[dx_tkr_stk, allow.cartesian=TRUE])[CMPRICE <= STRIKE, .SD[3],         by = .(TKR)][,c(1,3)]
# ------------------------------------------------------------------------------
names(dx_s_minus_0)  [1:2]  <- c("TKR", "STRIKE")
names(dx_s_minus_1)  [1:2]  <- c("TKR", "STRIKE")
names(dx_s_minus_2)  [1:2]  <- c("TKR", "STRIKE")
# ------------------------------------------------------------------------------
names(dx_s_plus_0)   [1:2]  <- c("TKR", "STRIKE")
names(dx_s_plus_1)   [1:2]  <- c("TKR", "STRIKE")
names(dx_s_plus_2)   [1:2]  <- c("TKR", "STRIKE")
# ------------------------------------------------------------------------------
setkey(dx_s_minus_0, TKR)
setkey(dx_s_minus_1, TKR)
setkey(dx_s_minus_2, TKR)
# ------------------------------------------------------------------------------
setkey(dx_s_plus_0,  TKR)
setkey(dx_s_plus_1,  TKR)
setkey(dx_s_plus_2,  TKR)
# ------------------------------------------------------------------------------
setkey(dx_ticker,    TKR)
# ------------------------------------------------------------------------------
fun_butterfly <- function(blob){
  
  #...............................................................................
  # browser()
  #...............................................................................
  
  # ------------------------------------------------------------------------------
  # create a stub template dataframe (initial 1 time setup)
  # https://tinyurl.com/wda2yyuh
  # ------------------------------------------------------------------------------
  if (z == TRUE) {
    #  g[[paste0("dt_bfly")]] <<- dx_blob[.0, 0]
    g[[paste0("dt_bfly")]]  <<- blob[.0,0]               # stub template dataframe 
    #  g[[paste0("dt_bfly")]]  <<- blob[.0]
    # ------------------------------------------------------------------------------
    g[[paste0("dt_bfly")]][, TKR        := unique(blob[, "TKR"])]
    g[[paste0("dt_bfly")]][, CMRK       := unique(blob[, "CMRK"])]
    g[[paste0("dt_bfly")]][, TechRank   := unique(blob[, "TechRank"])]
    g[[paste0("dt_bfly")]][, X.vCM      := unique(blob[, "X.vCM"])]
    g[[paste0("dt_bfly")]][, CMPRICE    := unique(blob[, "CMPRICE"])]  
    g[[paste0("dt_bfly")]][, HISTVO     := unique(blob[, "HISTVO"])] 
    g[[paste0("dt_bfly")]][, VOLF       := unique(blob[, "VOLF"])]
    g[[paste0("dt_bfly")]][, vol_diff   := ((dt_bfly[.N,7] - dt_bfly[.N,6])/dt_bfly[.N,6])*100]
    g[[paste0("dt_bfly")]][, EXPDAY     := unique(blob[, "EXPDAY"])]
    # ------------------------------------------------------------------------------
    g[[paste0("dt_bfly")]][, id_minus_1 := blob[C.P == "P", ][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]] # OPTKR
    g[[paste0("dt_bfly")]][, id_minus_0 := blob[C.P == "C", ][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]  
    g[[paste0("dt_bfly")]][, id_plus_0  := blob[C.P == "P", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]
    g[[paste0("dt_bfly")]][, id_plus_1  := blob[C.P == "C", ][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]
    # ------------------------------------------------------------------------------
    g[[paste0("dt_bfly")]][, cost       := 
                             -blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19] + # ASK
                             blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] + # BID
                             blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] -
                             blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]]
    # ------------------------------------------------------------------------------
    # Individual components of cost
    # ------------------------------------------------------------------------------
    # g[[paste0("dt_bfly")]][, s_minus_1  := unique(blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19])]
    # g[[paste0("dt_bfly")]][, s_minus_0  := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18])]
    # g[[paste0("dt_bfly")]][, s_plus_0   := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18])]
    # g[[paste0("dt_bfly")]][, s_plus_1   := unique(blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19])]
    #...............................................................................
    g[[paste0("dt_bfly")]][, s_minus_0_oi      := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"])]
    g[[paste0("dt_bfly")]][, s_plus_0_oi       := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"])]
    #...............................................................................
    g[[paste0("dt_bfly")]][, s_minus_0_iotm    := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"])]
    g[[paste0("dt_bfly")]][, s_plus_0_iotm     := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"])]
    #...............................................................................
    g[[paste0("dt_bfly")]][, s_minus_0_un_ov   := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"])]
    g[[paste0("dt_bfly")]][, s_plus_0_un_ov    := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"])]
    # #...............................................................................
    g[[paste0("dt_bfly")]][, s_minus_0_prb_stk := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"])]
    g[[paste0("dt_bfly")]][, s_plus_0_prb_stk  := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"])]
    # #...............................................................................
    g[[paste0("dt_bfly")]][, s_minus_0_pct_dbl := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"])]
    g[[paste0("dt_bfly")]][, s_plus_0_pct_dbl  := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"])]  
    # #...............................................................................
    g[[paste0("dt_bfly")]][, s_minus_0_rowrt   := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"])]
    g[[paste0("dt_bfly")]][, s_plus_0_rowrt    := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"])]
    # #...............................................................................
    g[[paste0("dt_bfly")]][, s_minus_0_delta   := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"])]
    g[[paste0("dt_bfly")]][, s_plus_0_delta    := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"])]
    
    g[[paste0("dt_bfly")]][, s_minus_0_gamma   := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"])]
    g[[paste0("dt_bfly")]][, s_plus_0_gamma    := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"])]

    g[[paste0("dt_bfly")]][, s_minus_0_gamma   := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"])]
    g[[paste0("dt_bfly")]][, s_plus_0_gamma    := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"])]
    
    g[[paste0("dt_bfly")]][, s_minus_0_theta   := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"])]
    g[[paste0("dt_bfly")]][, s_plus_0_theta    := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"])]
    
    g[[paste0("dt_bfly")]][, s_minus_0_vega    := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"])]
    g[[paste0("dt_bfly")]][, s_plus_0_vega     := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"])]
    # ------------------------------------------------------------------------------
    # CMPRICE diff from nearest +/- strike
    # ------------------------------------------------------------------------------  
    # g[[paste0("dt_bfly")]][, id_minus_0_stk
    #                        := unique(dt_bfly[,"dx_s_minus_0"][blob, on = .(id_minus_0 = OPTKR), nomatch = 0])[,"STRIKE"]]
    # g[[paste0("dt_bfly")]][, id_plus_0_stk
    #                        := unique(dt_bfly[,"dx_s_plus_0"] [blob, on = .(id_minus_0 = OPTKR), nomatch = 0])[,"STRIKE"]]
    # ------------------------------------------------------------------------------
    # IOTM Falkulation 
    # ------------------------------------------------------------------------------
    # g[[paste0("dt_bfly")]][, id_minus_0_stk_diff
    #                        := ((CMPRICE -
    #                               unique(dt_bfly[,"id_minus_0"][blob, on = .(id_minus_0 = OPTKR), nomatch = 0])[,"STRIKE"])
    #                            / CMPRICE)*100]
    # g[[paste0("dt_bfly")]][, id_plus_0_stk_diff
    #                        := ((CMPRICE - 
    #                               unique(dt_bfly[,"id_plus_0"] [blob, on = .(id_plus_0 = OPTKR), nomatch = 0])[,"STRIKE"])
    #                            / CMPRICE)*100]
    # ------------------------------------------------------------------------------
    g[[paste0("dt_bfly")]][, date_run   := unique(blob[, "date_run"])]
    # g[[paste0("dt_bfly")]][, id         := .I]
    # ------------------------------------------------------------------------------
    z <<- FALSE
    # ------------------------------------------------------------------------------
  } else {
    #...............................................................................  
    # browser()
    #...............................................................................  
    g[[paste0("dt_bfly")]] <<- rbind(g[[paste0("dt_bfly")]],
                                     data.table(
                                       # ------------------------------------------------------------------------------
                                       unique(blob[, "TKR"]),
                                       unique(blob[, "CMRK"]),
                                       unique(blob[, "TechRank"]),
                                       unique(blob[, "X.vCM"]),
                                       unique(blob[, "CMPRICE"]),
                                       unique(blob[, "HISTVO"]), 
                                       unique(blob[, "VOLF"]),
                                       # ((dt_bfly[.N,7] - dt_bfly[.N,6])/dt_bfly[.N,6])*100,
                                       ((unique(blob[, "VOLF"]) - unique(blob[, "HISTVO"]))/unique(blob[, "HISTVO"]))*100,
                                       unique(blob[, "EXPDAY"]),
                                       # ------------------------------------------------------------------------------
                                       unique(blob[C.P == "C", ][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]), # OPTKR
                                       unique(blob[C.P == "P", ][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
                                       unique(blob[C.P == "P", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
                                       unique(blob[C.P == "C", ][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
                                       # ------------------------------------------------------------------------------
                                       sum(
                                         -unique(blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]), # ASK
                                         unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]), # BID
                                         unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]),
                                         -unique(blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]),
                                         na.rm = TRUE
                                       ),
                                       # ------------------------------------------------------------------------------
                                       # Individual components of cost
                                       # ------------------------------------------------------------------------------
                                       # unique(blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]),
                                       # unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]),
                                       # unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]),
                                       # unique(blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]),
                                       #...............................................................................
                                       unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"]),
                                       unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"]),
                                       #...............................................................................
                                       unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"]),
                                       unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"]),
                                       #...............................................................................
                                       unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"]),
                                       unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"]),
                                       #...............................................................................
                                       unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"]),
                                       unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"]),
                                       #...............................................................................
                                       unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"]),
                                       unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"]),
                                       # #...............................................................................
                                       unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"]),
                                       unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"]),
                                       # #............................................................................... 
                                       unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"]),
                                       unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"]),
                                       
                                       unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"]),
                                       unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"]),

                                       unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"]),
                                       unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"]),                                       
                                       
                                       unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"]),
                                       unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"]),
                                       
                                       unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"]),
                                       unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"]),
                                       # ------------------------------------------------------------------------------
                                       # CMPRICE diff from nearest +/- strike
                                       # ------------------------------------------------------------------------------
                                       # as.double(((unique(blob$CMPRICE) - unique(dt_bfly[.N,"id_minus_0"][blob, on = .(id_minus_0 = OPTKR), nomatch = 0])[,"STRIKE"])/unique(blob$CMPRICE))*100),
                                       # as.double(((unique(blob$CMPRICE) - unique(dt_bfly[.N,"id_plus_0"][blob,  on = .(id_plus_0  = OPTKR), nomatch = 0])[,"STRIKE"])/unique(blob$CMPRICE))*100),
                                       # ------------------------------------------------------------------------------
                                       # IOTM Falkulation 
                                       # ------------------------------------------------------------------------------
                                       # (sum(
                                       #  unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 8]),
                                       #  -unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 12]),
                                       #   na.rm = TRUE))/unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 12]),
                                       # (sum(
                                       #  unique(blob[C.P == "C", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 8]),
                                       #  -unique(blob[C.P == "C", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 12]),
                                       #   na.rm = TRUE))/unique(blob[C.P == "C", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 12]),
                                       # ------------------------------------------------------------------------------
                                       unique(blob[, "date_run"])
                                       #  dt_bfly[, .I]
                                     ),
                                     # ------------------------------------------------------------------------------
                                     use.names = FALSE)
    # browser()
  }
}
# ------------------------------------------------------------------------------
# dx_blob[TKR %like% 'AA', ][dt_date_exp_mth[between(diff, 30, 60)][1,1]][
#   order(OPTKR, -date_run),
# ] %>%
#   split(., by = c("TKR")) %>%
#   map(., fun_butterfly)

butterfly <- dx_blob[
  #  dx_date_exp[day == 6 & week == 3, ][1, 1]
  dt_date_exp_mth[between(diff, 30, 60)][1,1]  
  #  dx_date_exp_mth[2,1]
][
  order(OPTKR, -date_run),
] %>%
  split(., by = c("TKR")) %>%
  map(., fun_butterfly)

# ------------------------------------------------------------------------------
dt_zip_files  <- setDT(cd$list_files("Options/ValueLine/zip"))
file_table    <- data.table(file_name = character(0))
# ------------------------------------------------------------------------------

#...............................................................................
browser()
#...............................................................................

# ------------------------------------------------------------------------------
dt_file_table <- as.data.table(sapply(dt_zip_files[,1], function(x) x))
dt_file_table[, name_char := str_extract(name, "[a-zA-Z]+")]
dt_file_table$name_num <- as.character(gsub("[^0-9]", "", dt_file_table$name))
dt_file_table <- data.table::setorder(dt_file_table,name_num)
dt_file_table[, N :=.N, by = name_num]
# ------------------------------------------------------------------------------
# dt_zip_files[name %like% 'S',]
# dt_zip_files[name %like% 'LS|UT',]
# dt_zip_files[1,1] %>% map_df(~cd$download_file(paste0("Options/ValueLine/zip/", .), paste0(here::here("ml", .)), overwrite = TRUE))
# ------------------------------------------------------------------------------
# sapply(dt_zip_files[name %like% 'S',1], function(nm) {

#...............................................................................
# browser()
#...............................................................................

# ------------------------------------------------------------------------------
# cd$download_file(paste0("Options/ValueLine/zip/",  nm),
#                paste0(here::here("zip", nm)),
#                overwrite = TRUE)
# ------------------------------------------------------------------------------
# utils::unzip(
#   paste0(here::here("zip",nm)),
#   list      = FALSE,
#   overwrite = TRUE,
#   exdir     = here::here("ml")
# )
# ------------------------------------------------------------------------------
# dt_file_table[nrow(dt_file_table) + 1, "file_name"] <- file
# browser()
# date_run <- read.csv(paste0(here::here("ml//"), str_extract(nm, "[a-zA-Z]+"), ".CSV"))[3,2]
# dt_file_table[name == nm, date_run := date_run]

# file_table[nrow(file_table) + 1, "date_run"] <- date_run
# test <- setDT(data.table::fread(paste0(here::here("ml//"), str_extract(nm, "[a-zA-Z]+"), ".CSV")))[3,2]
# test <- setDT(data.table::fread(paste0(here::here("ml//"), str_extract(nm, "[a-zA-Z]+"), ".CSV")))
#  }
# )


#...............................................................................
fun_archive <- function(nm){
#...............................................................................
  browser()
#...............................................................................
  
# ------------------------------------------------------------------------------
#  specify the path to the zip file
# ------------------------------------------------------------------------------
  vl_data_path      <- file.path(
    "C:/Users/glen.falk/OneDrive - IHS Markit/Documents/github/ValueLine/data/"
  )  
# ------------------------------------------------------------------------------  
  zip_file_path <- paste0(here::here("zip", nm))
# ------------------------------------------------------------------------------
  cd$download_file(paste0("Options/ValueLine/zip/",  nm),
                   zip_file_path,
                   overwrite = TRUE)
# ------------------------------------------------------------------------------
# chatGPT extract the csv files from the zip file and return a list of file name
# ------------------------------------------------------------------------------
  csv_files <- utils::unzip(
    zip_file_path,
    list      = TRUE,
    overwrite = TRUE,
#   exdir     = here::here("temp")
    exdir     = vl_data_path,
  )$Name
# ------------------------------------------------------------------------------
# dt_file_table[nrow(dt_file_table) + 1, "file_name"] <- file

#...............................................................................
  browser()
#...............................................................................  
  
  # loop through the list of file names and read each csv file into a data.table
  dt_list <- lapply(csv_files, function(x) {
    fread(unzip(zip_file, files = x))
  })
  
  date_run <- read.csv(paste0(here::here("ml//"), str_extract(nm, "[a-zA-Z]+"), ".CSV"))[3,2]
  dt_file_table[name == nm, date_run := date_run]
  
# file_table[nrow(file_table) + 1, "date_run"] <- date_run
# test <- setDT(data.table::fread(paste0(here::here("ml//"), str_extract(nm, "[a-zA-Z]+"), ".CSV")))[3,2]
# test <- setDT(data.table::fread(paste0(here::here("ml//"), str_extract(nm, "[a-zA-Z]+"), ".CSV")))
}
#...............................................................................
dt_zip_files[name %like% 'S',][,1]  %>%
  split(., by = c("name")) %>%
  map(., fun_archive)
#...............................................................................
dt_zip_files[name %like% 'S',] %>% 
  map(., fun_archive)
