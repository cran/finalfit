#' Mixed effects binomial logistic regression models: \code{finalfit} model wrapper
#'
#' Using \code{finalfit} conventions, produces mixed effects binomial logistic
#'   regression models for a set of explanatory variables against a binary dependent.
#'
#' Uses \code{lme4::\link[lme4]{glmer}} with \code{finalfit} modelling conventions. Output can be
#'   passed to \code{\link{fit2df}}. This is only currently set-up to take a single random effect
#'   as a random intercept. Can be updated in future to allow multiple random intercepts,
#'   random gradients and interactions on random effects if there is a need
#'
#' @param .data Dataframe.
#' @param dependent Character vector of length 1, name of depdendent variable (must have 2 levels).
#' @param explanatory Character vector of any length: name(s) of explanatory variables.
#' @param random_effect Character vector of length 1, either, (1) name of random
#'   intercept variable, e.g. "var1", (automatically convered to "(1 | var1)");
#'   or, (2) the full \code{lme4} specification, e.g. "(var1 | var2)". Note
#'   parenthesis MUST be included in (2) but NOT included in (1).
#' @param ... Other arguments to pass to \code{lme4::\link[lme4]{glmer}}.
#' @return A list of multivariable \code{lme4::\link[lme4]{glmer}} fitted model outputs.
#'   Output is of class \code{glmerMod}.
#'
#' @seealso \code{\link{fit2df}, \link{finalfit_merge}}
#' @family finalfit model wrappers
#' @export
#'
#' @examples
#' library(finalfit)
#' library(dplyr)
#'
#' explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
#' random_effect = "hospital"
#' dependent = "mort_5yr"
#'
#' colon_s %>%
#'   glmmixed(dependent, explanatory, random_effect) %>%
#' 	 fit2df(estimate_suffix=" (multilevel)")

glmmixed <- function(.data, dependent, explanatory, random_effect, ...){
	# If single term random effect, default to random intercept model
	if(!grepl("\\|", random_effect)) random_effect = paste0("(1 | ", random_effect, ")")
  lme4::glmer(paste0(dependent, "~", paste(explanatory, collapse="+"), " + ", random_effect),
              data=.data, family="binomial", control=lme4::glmerControl(optimizer="bobyqa",
                                                                        optCtrl=list(maxfun=200000)), ...)
}
