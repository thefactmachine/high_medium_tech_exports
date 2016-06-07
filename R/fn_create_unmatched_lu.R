fn_create_unmatched_lu <- function(a_df_me, a_df_hs10_anzsic) {
  # this function creates a data.frame that is a list
  # of unique HS10 codes that are not in the original:
  # "Concordance_HS_ANZSIC.csv"
  
  # 15977 (for raw data 2015.12.31)
  vct_unique_hs10_me <- unique(a_df_me$HS10)
  
  # 14232 (for extract from Andrew McCallum )
  vct_unique_hs10_anzlic <- a_df_hs10_anzsic$HS10
  
  # true = 14232; false = 1745. Total: 15977
  vct_lgcl_in_hs10_anzlic <- vct_unique_hs10_me %in% vct_unique_hs10_anzlic
  
  vct_hs10_not_in_hs10_anzlic <- vct_unique_hs10_me[!vct_lgcl_in_hs10_anzlic]
  
  df_lu <- data.frame(HS10 = vct_hs10_not_in_hs10_anzlic)
  df_lu[, c("tech_type", "main", "sub")] <-  rep("Unmatched", nrow(df_lu))
  df_lu$sort <- rep(40, nrow(df_lu))
  return(df_lu)

}