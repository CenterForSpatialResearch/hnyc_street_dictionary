street_type_builder = function(df1) {
  df1$street_type = case_when(
    str_detect(df1$best_match, pattern = " ST") ~ "ST",
    str_detect(df1$best_match, pattern = " DR") ~ "DR",
    str_detect(df1$best_match, pattern = " CIR") ~ "CIR",
    str_detect(df1$best_match, pattern = " AVE") ~ "AVE",
    str_detect(df1$best_match, pattern = " PL") ~ "PL",
    str_detect(df1$best_match, pattern = " CT") ~ "CT",
    str_detect(df1$best_match, pattern = " PARK") ~ "PARK",
    str_detect(df1$best_match, pattern = " PLZ") ~ "PLZ",
    str_detect(df1$best_match, pattern = " PKWY") ~ "PKWY",
    str_detect(df1$best_match, pattern = " WAY") ~ "WAY",
    str_detect(df1$best_match, pattern = " ALY") ~ "ALY",
    str_detect(df1$best_match, pattern = " PIER") ~ "PIER",
    str_detect(df1$best_match, pattern = "PIER") ~ "PIER",
    str_detect(df1$best_match, pattern = " SLIP") ~ "SLIP",
    str_detect(df1$best_match, pattern = " ROW") ~ "ROW",
    str_detect(df1$best_match, pattern = " APPROACH") ~ "APPROACH",
    str_detect(df1$best_match, pattern = " LN") ~ "LN",
    str_detect(df1$best_match, pattern = " TER") ~ "TER",
    str_detect(df1$best_match, pattern = " HTS") ~ "HTS",
    str_detect(df1$best_match, pattern = " BLVD") ~ "BLVD",
    str_detect(df1$best_match, pattern = " BRG") ~ "BRG",
    str_detect(df1$best_match, pattern = " HL") ~ "HL",
    str_detect(df1$best_match, pattern = "AVE") ~ "AVE",
    str_detect(df1$best_match, pattern = "BROADWAY") ~ "AVE",
    TRUE ~ "ST")
  return(df)
}
