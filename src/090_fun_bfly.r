# ------------------------------------------------------------------------------
# Function Butterfly                                                         ---
# ------------------------------------------------------------------------------
fun_butterfly <- function(blob){

#...............................................................................
browser()
#...............................................................................

# ------------------------------------------------------------------------------
# create a stub template dataframe (initial 1 time setup)
# https://tinyurl.com/wda2yyuh
# ------------------------------------------------------------------------------
if (z == TRUE) {
# g[[paste0("dt_bfly")]] <<- dx_blob[.0, 0]
  g[[paste0("dt_bfly")]]  <<- blob[.0,0]               # stub template dataframe 
# g[[paste0("dt_bfly")]]  <<- blob[.0]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, TKR        := unique(blob[, "TKR"])]
  g[[paste0("dt_bfly")]][, CMRK       := unique(blob[, "CMRK"])]
  g[[paste0("dt_bfly")]][, TechRank   := unique(blob[, "TechRank"])]
# g[[paste0("dt_bfly")]][, X.vCM      := unique(blob[, "X.vCM"])]
  g[[paste0("dt_bfly")]][, X.vCM      := unique(blob[, 5])]                     # `^vCM`
  g[[paste0("dt_bfly")]][, CMPRICE    := unique(blob[, "CMPRICE"])]  
  g[[paste0("dt_bfly")]][, HISTVO     := unique(blob[, "HISTVO"])] 
  g[[paste0("dt_bfly")]][, VOLF       := unique(blob[, "VOLF"])]
  g[[paste0("dt_bfly")]][, vol_diff   := ((dt_bfly[.N,7] - dt_bfly[.N,6])/dt_bfly[.N,6])*100]
  g[[paste0("dt_bfly")]][, EXPDAY     := unique(blob[, "EXPDAY"])]
# ------------------------------------------------------------------------------
# reference by column number instead of column name
# ------------------------------------------------------------------------------  
  g[[paste0("dt_bfly")]][, id_minus_1 := blob[blob[[11]] == 'P'][dx_s_minus_1, on = .c("TKR", "STRIKE" ), nomatch = 0][,13]]  # OPTKR
  g[[paste0("dt_bfly")]][, id_minus_0 := blob[blob[[11]] == 'C'][dx_s_minus_0, on = .c("TKR", "STRIKE" ), nomatch = 0][,13]]
  g[[paste0("dt_bfly")]][, id_plus_0  := blob[blob[[11]] == 'P'][dx_s_plus_0,  on = .c("TKR", "STRIKE" ), nomatch = 0][,13]]  
  g[[paste0("dt_bfly")]][, id_plus_1  := blob[blob[[11]] == 'C'][dx_s_plus_1,  on = .c("TKR", "STRIKE" ), nomatch = 0][,13]]    
# ------------------------------------------------------------------------------
# g[[paste0("dt_bfly")]][, id_minus_1 := blob[C.P == "P", ][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]] # OPTKR
# g[[paste0("dt_bfly")]][, id_minus_0 := blob[C.P == "C", ][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]  
# g[[paste0("dt_bfly")]][, id_plus_0  := blob[C.P == "P", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]
# g[[paste0("dt_bfly")]][, id_plus_1  := blob[C.P == "C", ][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, cost       := 
   -blob[blob[[11]] == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19] + # ASK
    blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] + # BID
    blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] -
    blob[blob[[11]] == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]]
# ------------------------------------------------------------------------------
  # g[[paste0("dt_bfly")]][, cost       := 
  #  -blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19] + # ASK
  #   blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] + # BID
  #   blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18] -
  #   blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]]
# ------------------------------------------------------------------------------
# Individual components of cost
# ------------------------------------------------------------------------------
  # g[[paste0("dt_bfly")]][, s_minus_1  := unique(blob[C.P == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19])]
  # g[[paste0("dt_bfly")]][, s_minus_0  := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18])]
  # g[[paste0("dt_bfly")]][, s_plus_0   := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18])]
  # g[[paste0("dt_bfly")]][, s_plus_1   := unique(blob[C.P == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19])]
#...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_oi      := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"])]
  g[[paste0("dt_bfly")]][, s_plus_0_oi       := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"])]
# ------------------------------------------------------------------------------
#  g[[paste0("dt_bfly")]][, s_minus_0_oi      := unique(blob[C.P == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"])]
#  g[[paste0("dt_bfly")]][, s_plus_0_oi       := unique(blob[C.P == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"])]  
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, s_minus_0_iotm    := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"])]
  g[[paste0("dt_bfly")]][, s_plus_0_iotm     := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"])]
#...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_iotm    := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"])]
  g[[paste0("dt_bfly")]][, s_plus_0_iotm     := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"])]
# ------------------------------------------------------------------------------
  g[[paste0("dt_bfly")]][, s_minus_0_un_ov   := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"])]
  g[[paste0("dt_bfly")]][, s_plus_0_un_ov    := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"])]
# #...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_prb_stk := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"])]
  g[[paste0("dt_bfly")]][, s_plus_0_prb_stk  := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"])]
  # #...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_pct_dbl := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"])]
  g[[paste0("dt_bfly")]][, s_plus_0_pct_dbl  := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"])]  
# #...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_rowrt   := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"])]
  g[[paste0("dt_bfly")]][, s_plus_0_rowrt    := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"])]
# #...............................................................................
  g[[paste0("dt_bfly")]][, s_minus_0_delta   := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"])]
  g[[paste0("dt_bfly")]][, s_plus_0_delta    := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"])]
  
  g[[paste0("dt_bfly")]][, s_minus_0_gamma   := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"])]
  g[[paste0("dt_bfly")]][, s_plus_0_gamma    := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"])]  
  
  g[[paste0("dt_bfly")]][, s_minus_0_rho     := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"])]
  g[[paste0("dt_bfly")]][, s_plus_0_rho      := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"])]  
  
  g[[paste0("dt_bfly")]][, s_minus_0_theta   := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"])]
  g[[paste0("dt_bfly")]][, s_plus_0_theta    := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"])]
  
  g[[paste0("dt_bfly")]][, s_minus_0_vega    := unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"])]
  g[[paste0("dt_bfly")]][, s_plus_0_vega     := unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"])]
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
# unique(blob[, "X.vCM"]),
  unique(blob[, 5]),                                                            # `^vCM`
  unique(blob[, "CMPRICE"]),
  unique(blob[, "HISTVO"]), 
  unique(blob[, "VOLF"]),
  # ((dt_bfly[.N,7] - dt_bfly[.N,6])/dt_bfly[.N,6])*100,
  ((unique(blob[, "VOLF"]) - unique(blob[, "HISTVO"]))/unique(blob[, "HISTVO"]))*100,
  unique(blob[, "EXPDAY"]),
# ------------------------------------------------------------------------------
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]), # OPTKR
  unique(blob[blob[[11]] == "P", ][dx_s_minus_1, on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
  unique(blob[blob[[11]] == "C", ][dx_s_plus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 13]),
  # ------------------------------------------------------------------------------
sum(
 -unique(blob[blob[[11]] == "P", ][dx_s_minus_1,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]), # ASK
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]), # BID
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 18]),
 -unique(blob[blob[[11]] == "C", ][dx_s_plus_1,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, 19]),
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
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "OI"]),
#...............................................................................
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "I.OTM"]),
#...............................................................................
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "Bid.UN.OV"]),
#...............................................................................
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PrtbStrk.VOLF"]),
#...............................................................................
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "PctDble"]),
# #...............................................................................
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "ROWRT"]),
# #............................................................................... 
  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "DLTA"]),

  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "GAMMA"]),

  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "RHO"]),

  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "THETA"]),

  unique(blob[blob[[11]] == "C", ][dx_s_minus_0,  on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"]),
  unique(blob[blob[[11]] == "P", ][dx_s_plus_0,   on = c(TKR = "TKR", STRIKE = "STRIKE"), nomatch = 0][, "VEGA"]),
# ------------------------------------------------------------------------------
  unique(blob[, "date_run"])
#  dt_bfly[, .I]
  ),
# ------------------------------------------------------------------------------
  use.names = FALSE)
  }
 
#...............................................................................      
browser()
#...............................................................................      
 
}
