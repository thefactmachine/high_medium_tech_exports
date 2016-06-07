fn_create_df_other <- function(a_df_HS10_anzic, a_df_HS10_tech_lu) {
  # create a data.frame called df_other_lu  this will be all the items
  # in the HS10_anzsic lookup (i.e. a_df_HS10_anzic) that were not
  # matched in the tech_lookups.csv file (i.e a_df_HS10_tech_lu)
  
  # create columns with ""tech_type", "main", "sub" and set all their
  # values to "Other"
  
  df_not_in_tech <- anti_join(a_df_HS10_anzic, a_df_HS10_tech_lu,   by = c("HS10" = "HS10"))
  df_lu <- data.frame(HS10 = df_not_in_tech$HS10)
  # hard code three columns to "Other"
  df_lu[, c("tech_type", "main", "sub")] <-  rep("Other", nrow(df_not_in_tech))
  # hard code the sort columns to "30"
  df_lu$sort <- rep(30, nrow(df_not_in_tech))
  return(df_lu)

}