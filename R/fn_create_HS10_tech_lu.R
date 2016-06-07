fn_create_HS10_tech_lu <- function(p_df_HS10_anzsic) {
  source("R/fn_obtain_HS10_codes.R")
  
  # the following is a 24 row data.frame. It maps categories of high tech / medium tech
  # to ANZSIC numbers.  These ANZSIC numbers are two, three, and four digit numbers.
  df_tech_lu <- read.csv("concordances/tech_lookups.csv", header = TRUE)
  
  # p_df_HS10_anzsic is a HS10 to ANZSIC mapping (two columns = H10, ANZSIC06)
  
  # following iterates through the rows of df_tech_lu (i.e 1:24)
  lst_df <- lapply(seq_len(nrow(df_tech_lu)), 
                   function(x) fn_obtain_HS10_codes(p_df_HS10_anzsic, df_tech_lu, x))
  
  df_lu <- do.call(bind_rows, lst_df) %>% select(-c(main_anzsic, sub_anzsic))
  
  return(df_lu)
}