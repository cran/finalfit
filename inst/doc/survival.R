## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=FALSE---------------------------------------------------------
#  # Make sure finalfit is up-to-date
#  install.packages("finalfit")

## ------------------------------------------------------------------------
# For this vignette only, pre-specify table output
mykable = function(x){
	knitr::kable(x, row.names = FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"))
}

## ------------------------------------------------------------------------
library(finalfit)
melanoma = boot::melanoma #F1 here for help page with data dictionary
ff_glimpse(melanoma)

## ------------------------------------------------------------------------
library(dplyr)
library(forcats)
melanoma = melanoma %>%
  mutate(
    # Overall survival
    status_os = ifelse(status == 2, 0, # "still alive"
    									 1), # "died of melanoma" or "died of other causes"
    
    # Diease-specific survival
    status_dss = ifelse(status == 2, 0, # "still alive"
      ifelse(status == 1, 1, # "died of melanoma"
        0)), # "died of other causes is censored"

    # Competing risks regression
    status_crr = ifelse(status == 2, 0, # "still alive"
      ifelse(status == 1, 1, # "died of melanoma"
        2)), # "died of other causes"
    
    # Label and recode other variables
    age = ff_label(age, "Age (years)"), # ff_label to make table friendly var labels
    thickness = ff_label(thickness, "Tumour thickness (mm)"), # ff_label to make table friendly var labels
    sex = factor(sex) %>% 
    	fct_recode("Male" = "1", 
    						 "Female" = "0") %>% 
    	ff_label("Sex"),
    ulcer = factor(ulcer) %>% 
    	fct_recode("No" = "0",
    						 "Yes" = "1") %>% 
    	ff_label("Ulcerated tumour")
  )

## ------------------------------------------------------------------------
library(survival)

survival_object = melanoma %$% 
	Surv(time, status_os)

# Explore:
head(survival_object) # + marks censoring, in this case "Alive"

# Expressing time in years
survival_object = melanoma %$% 
	Surv(time/365, status_os)

## ------------------------------------------------------------------------
# Overall survival in whole cohort
my_survfit = survfit(survival_object ~ 1, data = melanoma)
my_survfit # 205 patients, 71 events

## ------------------------------------------------------------------------
summary(my_survfit, times = c(0, 1, 2, 3, 4, 5))
# 5 year overall survival is 73%

## ---- fig.width = 5, fig.height = 4--------------------------------------
dependent_os = "Surv(time/365, status_os)"
explanatory = c("ulcer")

melanoma %>% 
	surv_plot(dependent_os, explanatory, pval = TRUE)

## ------------------------------------------------------------------------
dependent_os = "Surv(time, status_os)"
dependent_dss = "Surv(time, status_dss)"
dependent_crr = "Surv(time, status_crr)"
explanatory = c("age", "sex", "thickness", "ulcer")

melanoma %>% 
	finalfit(dependent_os, explanatory) %>% 
	mykable() # for vignette only

## ------------------------------------------------------------------------
melanoma %>% 
	finalfit(dependent_os, explanatory, add_dependent_label = FALSE) %>% 
	rename("Overall survival" = label) %>% 
	rename(" " = levels) %>% 
	rename(" " = all) %>% 
	mykable()

## ------------------------------------------------------------------------
explanatory_multi = c("age", "thickness", "ulcer")
melanoma %>% 
	finalfit(dependent_os, explanatory, explanatory_multi, keep_models = TRUE) %>% 
	mykable()

## ---- fig.width = 5, fig.height = 4--------------------------------------
explanatory = c("age", "sex", "thickness", "ulcer", "year")
melanoma %>% 
	coxphmulti(dependent_os, explanatory) %>% 
	cox.zph() %>% 
	{zph_result <<- .} %>% 
	plot(var=5)
zph_result

## ------------------------------------------------------------------------
explanatory= c("age", "sex", "ulcer", "thickness", "strata(year)")
melanoma %>% 
	finalfit(dependent_os, explanatory) %>% 
	mykable()

## ------------------------------------------------------------------------
# Simulate random hospital identifier
melanoma = melanoma %>% 
	mutate(hospital_id = c(rep(1:10, 20), rep(11, 5)))

# Cluster model
explanatory = c("age", "sex", "thickness", "ulcer", "cluster(hospital_id)")
melanoma %>% 
	finalfit(dependent_os, explanatory) %>% 
	mykable()

## ------------------------------------------------------------------------
# Frailty model
explanatory = c("age", "sex", "thickness", "ulcer", "frailty(hospital_id)")
melanoma %>% 
	finalfit(dependent_os, explanatory) %>% 
	mykable()

## ----eval=FALSE----------------------------------------------------------
#  melanoma %>%
#  	hr_plot(dependent_os, explanatory)

## ----echo=FALSE, fig.height=3, fig.width=7-------------------------------
library(ggplot2)
melanoma %>% 
	hr_plot(dependent_os, explanatory, table_text_size = 3.5,
					 title_text_size = 16,
					plot_opts=list(xlab("HR, 95% CI"), theme(axis.title = element_text(size=12))))

## ------------------------------------------------------------------------
explanatory = c("age", "sex", "thickness", "ulcer")
dependent_dss = "Surv(time, status_dss)"
dependent_crr = "Surv(time, status_crr)"

melanoma %>%
	
	# Summary table
  summary_factorlist(dependent_dss, explanatory, column = TRUE, fit_id = TRUE) %>%
	
	# CPH univariable
	  ff_merge(
    melanoma %>%
      coxphmulti(dependent_dss, explanatory) %>%
      fit2df(estimate_suffix = " (DSS CPH univariable)")
    ) %>%
	
	# CPH multivariable
  ff_merge(
    melanoma %>%
      coxphmulti(dependent_dss, explanatory) %>%
      fit2df(estimate_suffix = " (DSS CPH multivariable)")
    ) %>%
	
	# Fine and Gray competing risks regression
  ff_merge(
    melanoma %>%
      crrmulti(dependent_crr, explanatory) %>%
      fit2df(estimate_suffix = " (competing risks multivariable)")
    ) %>%
	

  select(-fit_id, -index) %>%
  dependent_label(melanoma, "Survival") %>% 
	mykable()

