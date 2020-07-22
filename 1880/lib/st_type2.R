type <- function(x) {
  type <- "ST"
  case_when(
   str_detect(x, " PL") == T  ~ "PL",
   str_detect(x, " CT") == T  ~ "CT",
   str_detect(x, " PARK") == T  ~ "PARK",
   str_detect(x, " DR") == T  ~ "DR",
   str_detect(x, " SQ") == T  ~ "SQ",
   str_detect(x, " PLZ") == T  ~ "PLZ",
   str_detect(x, " PL") == T  ~ "PL",
   str_detect(x, " CIR") == T  ~ "CIR",
   str_detect(x, " PKWY") == T  ~ "PKWY",
   str_detect(x, " WAY") == T  ~ "WAY",
   str_detect(x, " ALY") == T  ~ "ALY",
   str_detect(x, " PIER") == T  ~ "PIER",
   str_detect(x, "PIER ") == T  ~ "PIER",
   str_detect(x, " SLIP") == T  ~ "SLIP",
   str_detect(x, " ROW") == T  ~ "ROW",
   str_detect(x, " APPROACH") == T  ~ "APPROACH",
   str_detect(x, " LN") == T  ~ "LN",
   str_detect(x, " TER") == T  ~ "TER",
   str_detect(x, " HTS") == T  ~ "HTS",
   str_detect(x, " BLVD") == T  ~ "BLVD",
   str_detect(x, " BRG") == T  ~ "BRG",
   str_detect(x, " HL") == T  ~ "HL",
   str_detect(x, " AVE") == T  ~ "AVE",
   str_detect(x, "AVE ") == T  ~ "AVE",
   str_detect(x, "BROADWAY") == T  ~ "BROADWAY",
   TRUE ~ as.character(x)
  )
  return(type)
}
