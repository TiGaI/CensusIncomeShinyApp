library(dplyr)
library(stringr)

# Clean numerical
to_num <- function(x){
  gsub(",", "", x) %>% as.numeric()
}

# Date Cleaning
to_date_with_fmt <- function(date_str, fmt){
  as.Date(date_str, format = fmt)
}

get_date <- function(date_str){
  if( grepl("^\\d{1,2}\\b/\\d{1,2}\\b/\\d{4}\\b", date_str[1]) ){
    return(to_date_with_fmt(date_str, "%m/%d/%Y"))
  } else if ( grepl("^\\d{1,2}\\b/\\d{1,2}\\b/\\d{2}\\b", date_str[1]) ){
    return(to_date_with_fmt(date_str, "%m/%d/%y"))
  } else if( grepl("^\\d{1,2}\\b-\\d{1,2}\\b-\\d{4}\\b", date_str[1]) ){
    return(to_date_with_fmt(date_str, "%m-%d-%Y"))
  } else if ( grepl("^\\d{1,2}\\b-\\d{1,2}\\b-\\d{2}\\b", date_str[1]) ){
    return(to_date_with_fmt(date_str, "%m-%d-%y"))
  } else if( grepl("^\\d{4}\\b-\\d{1,2}\\b-\\d{1,2}\\b", date_str[1]) ){
    return(to_date_with_fmt(date_str, "%Y-%m-%d"))
  }
}

remove_empty_date <- function(date_str){
  date_str = trimws(date_str)
  return(ifelse(date_str=="", NA, date_str))
}

# OFFICE Cleaning
duplicate_recog <- function(str_vec){
  patn = "\\d{4}"
  str_vec = sub(patn, "", str_vec)

  patn = "FISCAL YEAR"
  str_vec = sub(patn, "", str_vec)

  str_vec = trimws(str_vec)

  return(str_vec)
}

replacee = c(
  "CAMPUS VOICE NETWORK ENHANCE", "CHIEF ADMINISTRATIVE OFFICER",
  "COMM ON SCIENCE  SPACE & TECH", "COMM ON SCIENCE AND TECHNOLOGY"
)
replacer = c(
  "CAMPUS VOICE NETWORK ENHANCMNT", "CHIEF ADMIN OFCR OF THE HOUSE",
  "COMM ON SCIENCE, SPACE & TECH", "COMM ON SCIENCE, SPACE & TECH"
)
replace_match = data.frame(replacee, replacer, stringsAsFactors = F)

clean_office <- function(df){
  df %>%
    mutate(OFFICE = duplicate_recog(OFFICE)) %>%
    left_join(replace_match, by=c("OFFICE" = "replacee")) %>%
    mutate(
      OFFICE=ifelse(is.na(replacer), OFFICE, replacer))
  }

# Cleaning Functions Together
clean_single_df <- function(df){
  df %>%
    clean_office() %>%
    transmute(
      START.DATE = get_date(remove_empty_date(START.DATE)),
      END.DATE = get_date(remove_empty_date(END.DATE)),
      COVERAGE.PERIOD = END.DATE - START.DATE,
      AMOUNT = to_num(AMOUNT),
      OFFICE = OFFICE,
      PURPOSE = PURPOSE
    )
}



