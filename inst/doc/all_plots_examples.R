## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
	message = FALSE,
	warning = FALSE,
	collapse = TRUE,
	comment = "#>"
)

## -----------------------------------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  finalfit(dependent, explanatory) %>% 
	knitr::kable(row.names=FALSE) # This line only needed for formatting. 

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or_plot(dependent, explanatory)
# Note this example uses fig.height=3, fig.width=9

## -----------------------------------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
random_effect = "hospital"
colon_s %>%
  finalfit(dependent, explanatory, random_effect = random_effect)%>% 
	knitr::kable(row.names=FALSE) # This line only needed for formatting.

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
random_effect = "hospital"
colon_s %>%
  or_plot(dependent, explanatory, random_effect = random_effect)
# Note this example uses fig.height=3, fig.width=9

## ----fig.height=2, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'

# Run summary_factorlist for variables you wish to include
## Include total_col = TRUE and fit_id = TRUE
factorlist  = colon_s %>% 
	summary_factorlist(dependent, "age.factor", total_col = TRUE, fit_id = TRUE)

# Run full model including factorlist
colon_s %>%
  or_plot(dependent, explanatory, factorlist = factorlist)
# Note this example uses fig.height=2, fig.width=9

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
random_effect = "hospital"

fit = colon_s %>% 
	glmmixed(dependent, explanatory, random_effect)

# Equivalent to:
fit = colon_s %>% 
	lme4::glmer(mort_5yr ~ age.factor + sex.factor + obstruct.factor + perfor.factor + (1 | hospital), 
							family="binomial", data = .)

# Which is incidentally equivalent to:
fit = colon_s %>% 
	lme4::glmer(ff_formula(dependent, explanatory, random_effect),
							family="binomial", data = .)

# Plot
system.time(colon_s %>%
  or_plot(dependent, explanatory, random_effect = random_effect, glmfit = fit)
)
# Note this example uses fig.height=3, fig.width=9

## ----fig.height=6, fig.width=9------------------------------------------------
library(finalfit)

explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"

fit_uni = colon_s %>% 
	glmuni(dependent, explanatory)

p_uni = colon_s %>%
	or_plot(dependent, explanatory, suffix = " (univariable)", remove_ref = TRUE, 
					glmfit = fit_uni)
p_multi = colon_s %>%
	or_plot(dependent, explanatory, suffix = " (multivariable)", remove_ref = TRUE)

cowplot::plot_grid(p_uni, p_multi, ncol = 1)


## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or_plot(dependent, explanatory, confint_type = "default")

# Note this example uses fig.height=3, fig.width=9

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or_plot(dependent, explanatory, remove_ref = TRUE)

# Note this example uses fig.height=3, fig.width=9

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or_plot(dependent, explanatory, breaks = c(0.4, 0.6, 0.8, 1.0, 1.2, 1.4, 1.8, 2.4))

# Note this example uses fig.height=3, fig.width=9

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or_plot(dependent, explanatory, column_space = c(-0.5, -0.1, 0.5))
# Note this example uses fig.height=3, fig.width=9

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or_plot(dependent, explanatory, dependent_label = "Mortality")
# Note this example uses fig.height=3, fig.width=9

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or_plot(dependent, explanatory, prefix = "Figure 1 - ")
# Note this example uses fig.height=3, fig.width=9

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or_plot(dependent, explanatory, suffix = "")

# Note this example uses fig.height=3, fig.width=9

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or_plot(dependent, explanatory, table_text_size = 3)
# Note this example uses fig.height=4, fig.width=9

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or_plot(dependent, explanatory, title_text_size = 12)
# Note this example uses fig.height=3, fig.width=9

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
library(ggplot2)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
	or_plot(dependent, explanatory,
					plot_opts = list(xlim(0.1, 3),
													 xlab("OR (95% CI, log)"),
													 theme(axis.title = element_text(size=10))
					)
	)
# Note this example uses fig.height=3, fig.width=9

## ----fig.height=3, fig.width=10-----------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = 'mort_5yr'
colon_s %>%
  or_plot(dependent, explanatory,
  				digits = c(3,3,3), confint_sep = " to ", column_space = c(-0.5, -0.1, 0.5))
# Note this example uses fig.height=3, fig.width=10

## -----------------------------------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"
colon_s %>%
  finalfit(dependent, explanatory) %>% 
	knitr::kable(row.names=FALSE) # This line only needed for formatting. 

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "Surv(time, status)"
colon_s %>%
  hr_plot(dependent, explanatory)
# Note this example uses fig.height=3, fig.width=9

## -----------------------------------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "nodes"
colon_s %>%
  finalfit(dependent, explanatory) %>% 
	knitr::kable(row.names=FALSE) # This line only needed for formatting. 

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "nodes"
colon_s %>%
  coefficient_plot(dependent, explanatory)
# Note this example uses fig.height=3, fig.width=9

## ----fig.height=3, fig.width=9------------------------------------------------
library(finalfit)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "nodes"
colon_s %>%
  ff_plot(dependent, explanatory)
# Note this example uses fig.height=3, fig.width=9

## ----fig.height=6, fig.width=9------------------------------------------------
library(finalfit)
explanatory = "perfor.factor"
dependent = "Surv(time, status)"
colon_s %>%
	surv_plot(dependent, explanatory)

## ----fig.height=6, fig.width=9------------------------------------------------
library(finalfit)
explanatory = "perfor.factor"
dependent = "Surv(time, status)"
colon_s %>%
	surv_plot(dependent, explanatory, xlab="Time (days)", pval=TRUE, legend="none")

