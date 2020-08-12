street_type_builder = function(df1) {
  df1$street_type = case_when(
    str_detect(df1$Name, pattern = " ST") ~ "ST",
    str_detect(df1$Name, pattern = " DR") ~ "DR",
    str_detect(df1$Name, pattern = " CIR") ~ "CIR",
    str_detect(df1$Name, pattern = " AVE") ~ "AVE",
    str_detect(df1$Name, pattern = " PL") ~ "PL",
    str_detect(df1$Name, pattern = " CT") ~ "CT",
    str_detect(df1$Name, pattern = " PARK") ~ "PARK",
    str_detect(df1$Name, pattern = " PLZ") ~ "PLZ",
    str_detect(df1$Name, pattern = " PKWY") ~ "PKWY",
    str_detect(df1$Name, pattern = " WAY") ~ "WAY",
    str_detect(df1$Name, pattern = " ALY") ~ "ALY",
    str_detect(df1$Name, pattern = " PIER") ~ "PIER",
    str_detect(df1$Name, pattern = "PIER") ~ "PIER",
    str_detect(df1$Name, pattern = " SLIP") ~ "SLIP",
    str_detect(df1$Name, pattern = " ROW") ~ "ROW",
    str_detect(df1$Name, pattern = " APPROACH") ~ "APPROACH",
    str_detect(df1$Name, pattern = " LN") ~ "LN",
    str_detect(df1$Name, pattern = " TER") ~ "TER",
    str_detect(df1$Name, pattern = " HTS") ~ "HTS",
    str_detect(df1$Name, pattern = " BLVD") ~ "BLVD",
    str_detect(df1$Name, pattern = " BRG") ~ "BRG",
    str_detect(df1$Name, pattern = " HL") ~ "HL",
    str_detect(df1$Name, pattern = "AVE") ~ "AVE",
    str_detect(df1$Name, pattern = "BROADWAY") ~ "AVE",
    TRUE ~ "ST")
  return(df)
}



