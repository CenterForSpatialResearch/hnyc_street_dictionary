# Instructions

### Before running the scripts 
+ Create a new folder "Data" under this "script" folder.  
+ __All__ dataset inputs and outputs should be placed under the "Data" folder. 
+ Latest versions of datasets could be find in the shared google drive folder.

### Raw dataset inputs
* Morse dictionary  
  + MN_MORSE_EDstreet_dict_1880.csv
  + BK_MORSE_EDstreet_dict_18800.csv
* Geo-coder segment info
  + mn_segments_1880export.csv
  + bk_segments_1880export.csv
  
### Clean function
__clean_function.R__ under "lib" folder
  
### Scripts (in accordance with the pipeline construction)
* __clean_morse.rmd__ clean morse dictionary
* __clean_segment_create_geo_dict.rmd__ clean segment data, create geo_dict
* Dictionary
  + __full_dict.rmd__ merge morse dict and geo_dict
  + __missing_EDs.rmd__ check missing EDs in geo_dict from Morse
* segment info
  + __combine_morse_segment.rmd__ merge morse ED and segment info into segment datasets
  + __house_range_segment_flags.rmd__ summarize house range by per ED per segment, flag inconsistent and incomplete house ranges
  + nested data also available in google drive; nest combined morse and segment data by ED

### Output datasets (available in google drive)
https://drive.google.com/drive/folders/1N7S7l0XX8y4eYUTmI3AZlwO6XpCPhAXn

* morse dict
  + morse_mn1880.csv
  + morse_bk1880.csv
* cleaned segment data
  + segment_mn.csv
  + segment_bk.csv
* geo_dict
  + geo_dict_mn.csv
  + geo_dict_bk.csv
* full_dict
  + full_dict_mn.csv
  + full_dict_bk.csv
* missing EDs
  + missing_EDs_mn.csv
  + missing_EDs_bk.csv
* combined segment info with morse
  + combine_mn.csv
  + combine_bk.csv
* exported nested data
  + mn_nest.rds
  + bk_nest.rds
* segment house range
  + mn_seg_add_range.csv
  + bk_seg_add_range.csv
* flag out error house numbers
  + mn_seg_flag.csv
  + bk_seg_flag.csv
