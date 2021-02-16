if(!require("jtools")){install.packages("jtools")}


fct_model <- function(data){
  lm = lm(log_exp ~ log_pop_sup_comp, data = data)
  jb_test = tseries::jarque.bera.test(lm$residuals)
  sw_test_res = shapiro.test(lm$residuals)
  lm_summary = summary(lm)
  pp_lm_summary = jtools::summ(lm, digits = 5)
  
  return(
    list(
      lm=lm,
      jb_test=jb_test,
      sw_test_res=sw_test_res,
      lm_summary=lm_summary,
      pp_lm_summary=pp_lm_summary
    )
  )
}

