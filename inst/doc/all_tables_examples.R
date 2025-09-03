## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
	collapse = TRUE,
	comment = "#>"
)

## ----eval=FALSE---------------------------------------------------------------
# install.packages("finalfit")

## -----------------------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
  summary_factorlist(dependent, explanatory) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r"))

## ----message=FALSE, warning=FALSE---------------------------------------------
library(finalfit)
library(dplyr)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	mutate(
		sex.factor = ff_label(sex.factor, "Gender")
	) %>% 
  summary_factorlist(dependent, explanatory) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r"))

## -----------------------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
  summary_factorlist(dependent, explanatory, p = TRUE, p_cat = "fisher") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r"))

## -----------------------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
  summary_factorlist(dependent, explanatory, p = TRUE, p_cont_para = "t.test") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "nodes", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE, cont = "median") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "nodes", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE, cont_nonpara = c(2)) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE, 
										 na_to_p = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
										 column = FALSE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE, cont = "median", na_include = TRUE,
										 column = TRUE, total_col = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r"))

## ----message=FALSE, warning=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
										 total_col = TRUE,
										 add_row_total = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r"))

## ----message=FALSE, warning=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
										 total_col = TRUE,
										 add_row_total = TRUE,
										 include_row_missing_col = FALSE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r"))

## ----message=FALSE, warning=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE,
										 total_col = TRUE,
										 add_row_total = TRUE,
										 row_totals_colname = "N (total)",
										 row_missing_colname = "N (missing)") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("extent.factor")
dependent = "perfor.factor"
colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE, cont = "median", na_include = TRUE,
										 column = TRUE, total_col = TRUE, orderbytotal = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
explanatory = c("age.factor", "sex.factor")
dependent = "rx.factor"

colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE,
										 add_col_totals = TRUE)	-> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "l", "l", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
explanatory = c("age.factor", "sex.factor")
dependent = "rx.factor"

colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE,
										 add_col_totals = TRUE,
										 include_col_totals_percent = FALSE)	-> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "l", "l", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
explanatory = c("age.factor", "sex.factor")
dependent = "rx.factor"

colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE,
										 add_col_totals = TRUE,
										 include_col_totals_percent = FALSE,
										 col_totals_rowname = "",
										 col_totals_prefix = "N=")	-> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "l", "l", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	summary_factorlist(dependent, explanatory, p = TRUE, cont = "median", na_include = TRUE,
										 column = TRUE, total_col = TRUE, add_dependent_label = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
  dplyr::mutate(
    perfor.factor = ff_label(perfor.factor, "Perforated cancer")
	) %>% 
  summary_factorlist(dependent, explanatory, p = TRUE, cont = "median", na_include = TRUE,
    column = TRUE, total_col = TRUE, add_dependent_label = TRUE, dependent_label_prefix = "") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "extent.factor"
colon_s %>%
  dplyr::mutate(
    perfor.factor = ff_label(perfor.factor, "Perforated cancer")
  ) %>% 
  summary_factorlist(dependent, explanatory, p = TRUE, cont = "median", na_include = TRUE,
    column = TRUE, total_col = TRUE, add_dependent_label = TRUE, dependent_label_prefix = "") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## -----------------------------------------------------------------------------
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "mort_5yr"
colon_s %>% 
	ff_glimpse(dependent, explanatory)

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "mort_5yr"
colon_s %>%
  summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
  									 total_col = TRUE,
  									 add_col_totals = TRUE, add_row_totals = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "mort_5yr"
colon_s %>%
	mutate(
		mort_5yr = forcats::fct_na_value_to_level(mort_5yr, level = "(Missing)")
	) %>% 
  summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
  									 total_col = TRUE,
  									 add_col_totals = TRUE, add_row_totals = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "mort_5yr"
colon_s %>%
  summary_factorlist(dependent, explanatory, p = TRUE, 
  									 na_include = TRUE, na_include_dependent = TRUE,
  									 total_col = TRUE,
  									 add_col_totals = TRUE, add_row_totals = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "mort_5yr"
colon_s %>%
  summary_factorlist(dependent, explanatory, p = TRUE, 
  									 na_complete_cases = TRUE,
  									 total_col = TRUE,
  									 add_col_totals = TRUE, add_row_totals = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(finalfit)
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "mort_5yr"
vlabels = colon_s %>% 
	extract_variable_label()

colon_s %>%
	select(dependent, explanatory) %>% 
	tidyr::drop_na() %>% # Silently removes attributes
	ff_relabel(vlabels) %>% # Relabel
  summary_factorlist(dependent, explanatory, p = TRUE, na_include = TRUE,
  									 total_col = TRUE,
  									 add_col_totals = TRUE, add_row_totals = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----message=FALSE, warning=FALSE---------------------------------------------
library(finalfit)

# Here, `extent` is a continuous variable with 4 distinct values. 
# Any continuous variable with 5 or fewer unique values is converted silently to factor 
# e.g.
explanatory = c("extent")
dependent = "mort_5yr"
colon_s %>%
  summary_factorlist(dependent, explanatory) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----message=FALSE, warning=FALSE---------------------------------------------
library(finalfit)

# Here, `extent` is a continuous variable with 4 distinct values. 
# Any continuous variable with 5 or fewer unique values is converted silently to factor 
# e.g.
explanatory = c("extent")
dependent = "mort_5yr"
colon_s %>%
  summary_factorlist(dependent, explanatory, 
  									 cont_cut = 0) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
library(dplyr)
explanatory = c("age.factor", "sex.factor")
dependent = "perfor.factor"

# Pick option below
split = "rx.factor"
split = c("rx.factor", "node4.factor")

# Piped function to generate stratified crosstabs table
colon_s %>%
  group_by(!!! syms(split)) %>% # Looks awkward, but avoids unquoted var names
  group_modify(~ summary_factorlist(.x, dependent, explanatory)) %>%
  ff_stratify_helper(colon_s) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "l", "l", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
  summary_factorlist(dependent, explanatory, p = TRUE, digits = c(1,2,3,4,0)) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE------------------------------------------------------------
explanatory = c("age", "age.factor", "sex.factor", "obstruct.factor")
dependent = "perfor.factor"
colon_s %>%
	mutate(my_weights = runif(929, 0, 1)) %>% # Random just to demonstrate
	summary_factorlist(dependent, explanatory, weights = "my_weights", digits = c(1, 1, 3, 1, 1))-> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
colon_s %>%
	finalfit(dependent, explanatory) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age", "sex.factor")
dependent = "mort_5yr"
colon_s %>%
	finalfit(dependent, explanatory, add_dependent_label = FALSE) %>% 
	ff_remove_ref() %>% 
	dependent_label(colon_s, dependent)-> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
colon_s %>%
	finalfit(dependent, explanatory, metrics = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t[[1]], row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))
kable(t[[2]], row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"), col.names = "")

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
glm(mort_5yr ~ age.factor + sex.factor + obstruct.factor + perfor.factor, data = colon_s, family = "binomial") %>% 
	ff_metrics() -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"), col.names = "")

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory_multi = c("age.factor", "obstruct.factor")
dependent = "mort_5yr"
colon_s %>%
	finalfit(dependent, explanatory, explanatory_multi) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
explanatory_multi = c("age.factor", "obstruct.factor")
dependent = "mort_5yr"
colon_s %>%
	finalfit(dependent, explanatory, explanatory_multi, metrics = TRUE, keep_models = TRUE) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t[[1]], row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))
kable(t[[2]], row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"), col.names = "")

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor*sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
colon_s %>%
	finalfit(dependent, explanatory) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
#explanatory = c("age.factor*sex.factor", "obstruct.factor", "perfor.factor")
explanatory = c("obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
colon_s %>%
	ff_interaction(age.factor, sex.factor) %>% 
	finalfit(dependent, c(explanatory, "age.factor_sex.factor")) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
colon_s %>%
	dplyr::mutate(
		mort_5yr = ff_label(mort_5yr, "5-year mortality")
	) %>% 
	finalfit(dependent, explanatory, dependent_label_prefix = "",
					 dependent_label_suffix = " (full model)") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
colon_s %>%
	finalfit(dependent, explanatory, estimate_name = "Odds ratio") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
colon_s %>%
	finalfit(dependent, explanatory, digits = c(3,3,4)) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
colon_s %>%
	finalfit(dependent, explanatory, confint_type = "default") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
colon_s %>%
	finalfit(dependent, explanatory, confint_level = 0.90) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
colon_s %>%
	finalfit(dependent, explanatory, confint_sep = " to ") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
# explanatory = c("age", "sex.factor")
# dependent = 'mort_5yr'
# 
# # Standard finalfit regression table
# t1 = colon_s %>%
#   finalfit(dependent, explanatory, keep_fit_id = TRUE)
# 
# # GLM with Stata-like robust standard errors
# t2 = colon_s %>%
#   glmmulti(dependent, explanatory) %>%
#   lmtest::coeftest(., vcov = sandwich::vcovHC(., "HC1")) %>%
#   broom::tidy(conf.int = TRUE) %>%
#   remove_intercept() %>%
#   select(term, estimate, conf.low, conf.high, p.value) %>%
#   mutate(across(c(estimate, conf.low, conf.high), exp)) %>% # or mutate_at(vars())
#   as.data.frame() %>%
#   condense_fit(estimate_name = "OR (multivariable robust SE)")
# 
# ff_merge(t1, t2, last_merge = TRUE)

## ----echo=FALSE---------------------------------------------------------------
library(finalfit)
explanatory = c("age", "sex.factor")
dependent = 'mort_5yr'
t1 = colon_s %>%
  finalfit(dependent, explanatory, keep_fit_id = TRUE)

t2 = colon_s %>% 
  glmmulti(dependent, explanatory) %>% 
  lmtest::coeftest(., vcov = sandwich::vcovHC(., "HC1")) %>% 
  broom::tidy(conf.int = TRUE) %>% 
  remove_intercept() %>% 
  dplyr::select(term, estimate, conf.low, conf.high, p.value) %>% 
  dplyr::mutate_at(dplyr::vars(estimate, conf.low, conf.high), exp) %>% 
  as.data.frame() %>% 
  condense_fit(estimate_name = "OR (multivariable robust SE)")

ff_merge(t1, t2, last_merge = TRUE) %>% 
	knitr::kable(row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
colon_s %>%
	finalfit(dependent, explanatory) %>% 
	ff_remove_p() -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
random_effect = "hospital"
colon_s %>%
	finalfit(dependent, explanatory, random_effect = random_effect,
					 dependent_label_suffix = " (random intercept)") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
random_effect = "hospital"

colon_s %>%
	finalfit(dependent, explanatory, random_effect = random_effect, keep_fit_id = TRUE) %>% 
	ff_merge(
		explanatory %>% 
			purrr::map_df(~ glmmixed(colon_s, dependent, .x, random_effect = random_effect) %>% 
											fit2df(estimate_suffix = " (univariable with RE)")), 
		last_merge = TRUE
	) %>% 
	dplyr::relocate(7, .before = 6) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
random_effect = "(age.factor | hospital)"
colon_s %>%
	finalfit(dependent, explanatory, random_effect = random_effect,
					 dependent_label_suffix = " (random slope: age)") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----eval=FALSE, message=FALSE, warning=FALSE, include=TRUE-------------------
# library(finalfit)
# explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
# dependent = "mort_5yr"
# random_effect = "(age.factor | hospital)"
# colon_s %>%
# 	glmmixed(dependent, explanatory, random_effect = random_effect) %>%
# 	broom.mixed::tidy() -> t

## ----echo=FALSE, eval=FALSE---------------------------------------------------
# library(knitr)
# kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
	dplyr::select(explanatory, dependent) %>%
	tidyr::drop_na() %>%
	finalfit(dependent, explanatory) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'nodes'
colon_s %>%
	finalfit(dependent, explanatory) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "nodes"
random_effect = "hospital"
colon_s %>%
	finalfit(dependent, explanatory, random_effect = random_effect,
					 dependent_label_suffix = " (random intercept)") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "nodes"
random_effect = "(age.factor | hospital)"
colon_s %>%
	finalfit(dependent, explanatory, random_effect = random_effect,
					 dependent_label_suffix = " (random slope: age)") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"
colon_s %>%
	finalfit(dependent, explanatory) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"
colon_s %>%
	finalfit(dependent, explanatory, add_dependent_label = FALSE) %>% 
	dplyr::rename("Overall survival" = label) %>% 
	dplyr::rename(" " = levels)	-> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
library(dplyr)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"

## Crosstable
colon_s %>%
	summary_factorlist(dependent, explanatory, fit_id=TRUE) -> table_1

## Univariable
colon_s %>%
	glmuni(dependent, explanatory) %>%
	fit2df(estimate_suffix=" (univariable)") -> table_2

## Merge

table_1 %>% 
	ff_merge(table_2) %>% 
	select(-c(fit_id, index)) %>% 
	dependent_label(colon_s, dependent)-> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
library(dplyr)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
random_effect = "hospital"
dependent = "mort_5yr"

# All in one pipe

colon_s %>%
	## Crosstable
	summary_factorlist(dependent, explanatory, fit_id=TRUE)  %>% 
	
	## Add univariable
	ff_merge(
		glmuni(colon_s, dependent, explanatory) %>%
			fit2df(estimate_suffix=" (univariable)")
	) %>% 
	
	## Add multivariable
	ff_merge(
		glmmulti(colon_s, dependent, explanatory) %>%
			fit2df(estimate_suffix=" (multivariable)")
	) %>% 
	
	## Add mixed effects
	ff_merge(
		glmmixed(colon_s, dependent, explanatory, random_effect) %>%
			fit2df(estimate_suffix=" (multilevel)"),
		last_merge = TRUE
	) %>% 
	dependent_label(colon_s, dependent) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
library(dplyr)

## Dobson (1990) Page 93: Randomized Controlled Trial :
counts = c(18,17,15,20,10,20,25,13,12)
outcome = gl(3,1,9)
treatment = gl(3,3)
d.AD <- data.frame(treatment, outcome, counts)

dependent = "counts"
explanatory = c("outcome", "treatment")

fit_uni = d.AD %>% 
	glmuni(dependent, explanatory, family = poisson) %>% 
	fit2df(estimate_name = "Rate ratio (univariable)")

fit_multi = d.AD %>% 
	glmmulti(dependent, explanatory, family = poisson) %>% 
	fit2df(estimate_name = "Rate ratio (multivariable)")

# All in one pipe
d.AD %>%
	## Crosstable
	summary_factorlist(dependent, explanatory, cont = "median", fit_id=TRUE)  %>% 
	
	## Add univariable
	ff_merge(fit_uni, estimate_name = "Rate ratio") %>% 
	
	## Add multivariable
	ff_merge(fit_multi, estimate_name = "Rate ratio",
					 last_merge = TRUE) %>% 
	dependent_label(d.AD, dependent) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
library(dplyr)

# A Gamma example, from McCullagh & Nelder (1989, pp. 300-2)
clotting <- data.frame(
    u = c(5,10,15,20,30,40,60,80,100),
    lot1 = c(118,58,42,35,27,25,21,19,18),
    lot2 = c(69,35,26,21,18,16,13,12,12))

dependent = "lot1"
explanatory = "log(u)"

fit_uni = clotting %>% 
	glmuni(dependent, explanatory, family = Gamma) %>% 
	fit2df(estimate_name = "Coefficient", exp = FALSE, digits = c(3,3,4))

# All in one pipe
clotting %>%
	## Crosstable
	summary_factorlist(dependent, explanatory, cont = "median", fit_id=TRUE)  %>% 
	
	## Add fit
	ff_merge(fit_uni, last_merge = TRUE) %>% 
	dependent_label(colon_s, dependent) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
library(dplyr)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"

colon_s %>%
	mutate(myweights = runif(dim(colon_s)[1])) %>% # random just for example
	finalfit(dependent, explanatory, weights = "myweights") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
library(dplyr)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"

# All in one pipe

colon_s %>%
	## Crosstable
	summary_factorlist(dependent, explanatory, fit_id=TRUE)  %>% 
	
	## Add univariable
	ff_merge(
		glmuni(colon_s, dependent, explanatory) %>%
			fit2df(estimate_suffix=" (univariable)")
	) %>% 
	
	## Add multivariable
	ff_merge(
		glm(
			ff_formula(dependent, explanatory), data = colon_s, family = "binomial", weights = NULL
		) %>%
			fit2df(estimate_suffix=" (multivariable)"),
		last_merge = TRUE
	) %>% 
	dependent_label(colon_s, dependent) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
library(dplyr)
explanatory = c("age.factor*sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"

# Run model for term test
fit <- glm(
	ff_formula(dependent, explanatory), 
	data=colon_s, family = binomial
)

# Not run
#term_test <- survey::regTermTest(fit, "age.factor:sex.factor")

# Run final table with results of term test
colon_s %>%
	finalfit(dependent, explanatory) %>%
	rbind(c(
		"age.factor:sex.factor (overall)",
		"Interaction",
		"-",
		"-",
		"-",
		paste0("p = 0.775")
	))-> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(dplyr)
mydata = colon_s
explanatory_base = c("age.factor", "sex.factor")
explanatory_permute = c("obstruct.factor", "perfor.factor", "node4.factor")
dependent = "Surv(time, status)"

mydata %>% 
	ff_permute(dependent, explanatory_base, explanatory_permute) %>% 
	rename("Overall survival" = `Dependent: Surv(time, status)`, # optional tidying
				 `n (%)` = "all") -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(finalfit)
library(dplyr)

mydata = colon_s
base_explanatory = c("age.factor", "sex.factor")
explanatory = c("obstruct.factor", "perfor.factor", "node4.factor")
dependent = "Surv(time, status)"

mydata %>%
	# Counts
	summary_factorlist(dependent, c(base_explanatory,
																	explanatory),
										 column = TRUE,
										 fit_id = TRUE) %>% 
	
	# Univariable
	ff_merge(
		coxphuni(mydata, dependent, c(base_explanatory, explanatory)) %>% 
			fit2df(estimate_suffix = " (Univariable)")
	) %>% 
	
	# Base
	ff_merge(
		coxphmulti(mydata, dependent, base_explanatory) %>% 
			fit2df(estimate_suffix = " (Base model)")
	) %>% 
	
	# Model 1
	ff_merge(
		coxphmulti(mydata, dependent, c(base_explanatory, explanatory[1])) %>% 
			fit2df(estimate_suffix = " (Model 1)")
	) %>% 
	
	# Model 2
	ff_merge(
		coxphmulti(mydata, dependent, c(base_explanatory, explanatory[2])) %>% 
			fit2df(estimate_suffix = " (Model 2)")
	) %>% 
	
	# Model 3
	ff_merge(
		coxphmulti(mydata, dependent, c(base_explanatory, explanatory[3])) %>% 
			fit2df(estimate_suffix = " (Model 3)")
	) %>% 
	
	# Full
	ff_merge(
		coxphmulti(mydata, dependent, c(base_explanatory, explanatory)) %>% 
			fit2df(estimate_suffix = " (Full)"),
		last_merge = TRUE
	) %>% 
	
	# Tidy-up
	rename("Overall survival" = label) %>% 
	rename(" " = levels) %>% 
	rename(`n (%)` = all) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(survey)
library(dplyr)

data(api)
dependent = "api00"
explanatory = c("ell", "meals", "mobility")

# Label data frame
apistrat = apistrat %>%
  mutate(
  api00 = ff_label(api00, "API in 2000 (api00)"),
  ell = ff_label(ell, "English language learners (percent)(ell)"),
  meals = ff_label(meals, "Meals eligible (percent)(meals)"),
  mobility = ff_label(mobility, "First year at the school (percent)(mobility)"),
  sch.wide = ff_label(sch.wide, "School-wide target met (sch.wide)")
  )

# Linear example
dependent = "api00"
explanatory = c("ell", "meals", "mobility")

# Stratified design
dstrat = svydesign(id=~1,strata=~stype, weights=~pw, data=apistrat, fpc=~fpc)

# Univariable fit
fit_uni = dstrat %>%
  svyglmuni(dependent, explanatory) %>%
  fit2df(estimate_suffix = " (univariable)")

# Multivariable fit
fit_multi = dstrat %>%
  svyglmmulti(dependent, explanatory) %>%
  fit2df(estimate_suffix = " (multivariable)")

# Pipe together
apistrat %>%
  summary_factorlist(dependent, explanatory, fit_id = TRUE) %>%
  ff_merge(fit_uni) %>%
  ff_merge(fit_multi, last_merge = TRUE) %>%
  dependent_label(apistrat, dependent) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r", "r"))

## ----warning=FALSE, message=FALSE---------------------------------------------
library(survey)
library(dplyr)

data(api)
dependent = "sch.wide"
explanatory = c("ell", "meals", "mobility")

# Label data frame
apistrat = apistrat %>%
  mutate(
  api00 = ff_label(api00, "API in 2000 (api00)"),
  ell = ff_label(ell, "English language learners (percent)(ell)"),
  meals = ff_label(meals, "Meals eligible (percent)(meals)"),
  mobility = ff_label(mobility, "First year at the school (percent)(mobility)"),
  sch.wide = ff_label(sch.wide, "School-wide target met (sch.wide)")
  )
  
# Univariable fit
fit_uni = dstrat %>%
  svyglmuni(dependent, explanatory, family = "quasibinomial") %>%
  fit2df(exp = TRUE, estimate_name = "OR", estimate_suffix = " (univariable)")

# Multivariable fit
fit_multi = dstrat %>%
  svyglmmulti(dependent, explanatory, family = "quasibinomial") %>%
  fit2df(exp = TRUE, estimate_name = "OR", estimate_suffix = " (multivariable)")

# Pipe together
apistrat %>%
  summary_factorlist(dependent, explanatory, fit_id = TRUE) %>%
  ff_merge(fit_uni) %>%
  ff_merge(fit_multi, last_merge = TRUE) %>%
  dependent_label(apistrat, dependent) -> t

## ----echo=FALSE---------------------------------------------------------------
library(knitr)
kable(t, row.names=FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r", "r"))

