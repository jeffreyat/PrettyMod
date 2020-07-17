#' Checks if a number is whole
#'
#' Returns whether a number is whole
#'
#' @param num (numeric): number to check
#' @param tol (numeric): how close the number needs to be to whole
#' @export
#'
is_wholenumber <- function(num, tol = .Machine$double.eps^0.5)  abs(num - round(num)) < tol

#' Convert number to scientific notation
#'
#' Returns a string version of number in scientific notation.
#'
#' @param num (numeric): number to convert
#' @param digits (integer): precision of result
#' @export
#'
get_sci = function(num, digits = 2) {
  return(formatC(num, format = 'e', digits = digits))
}

#' Format confidence intervals
#'
#' Returns vector of strings with formatted confidence intervals
#'
#' @param cis (data.frame): CIs with low in first column high in second
#' @param digits (integer): precision of result
#' @export
#'
format_cis = function(cis, digits = 2) {
  low = stringr::str_pad(round(cis[,1], digits),
                nchar(round(cis[,1])) + digits, 'right', '0')
  high = stringr::str_pad(round(cis[,2], digits),
                 nchar(round(cis[,2])) + digits, 'right', '0')
  cis = paste0('[', low, ',', high, ']')
  return(cis)
}

#' Pretty format results from models
#'
#' Returns list with data.frame or data.frame and flextable
#'
#' @param mod: a model object
#' @param type (character): the type of model
#' @param digits (integer): precision of result
#' @param flex_caption (character): caption for flex table
#' @export
#'
pretty_mod = function(mod,
                      type = 'binomial',
                      digits = 2,
                      flex_caption = NULL) {

  if(type == 'binomial' || type == 'negbin') {
    # Depending on model type, set effect label
    effect_lab = ifelse(type == 'binomial', 'OR', 'Estimate')

    # Put coefficients from model in data.frame
    mod_res = data.frame(summary(mod)$coefficients)

    # We just need the estimate of the effect and the p-value for each
    # variable, ditching the intercept
    mod_res = mod_res[-1,c(1,4)]
    colnames(mod_res) = c(effect_lab, 'pvalue')

    # Exponentiate the effect
    mod_res[,1] = exp(mod_res[,1])

    # Retrieve confidence intervals
    cis = data.frame(exp(confint(mod)))
    cis = cis[-1,] # ditch the intercept

    # Convert CIs to string and add to results
    mod_res$CI = format_cis(cis, digits = digits)

    # Reorder the results
    mod_res = mod_res[,c(effect_lab, 'CI', 'pvalue')]

    # Save the p-values in case needed
    p = mod_res$pvalue

    # Reformat the p-values for consistent scientific notation
    mod_res$pvalue = get_sci(mod_res$pvalue, digits = digits)

    # Format the ORs
    ors = mod_res[,effect_lab]
    if(any(is_wholenumber(round(mod_res[,effect_lab], digits)))) {
      whole = is_wholenumber(round(mod_res[,effect_lab], digits))
      ors[whole] = round(ors[whole])
      ors[whole] = stringr::str_pad(paste0(ors[whole], '.'),
                                    nchar(ors[whole]) + 1 + digits, 'right', '0')
      ors[!whole] = stringr::str_pad(round(mod_res[,effect_lab][!whole], digits),
                                    nchar(round(mod_res[,effect_lab][!whole])) + digits, 'right', '0')
      mod_res[,effect_lab] = ors
    } else {
      mod_res[,effect_lab] = stringr::str_pad(round(mod_res[,effect_lab], digits),
                                    nchar(round(mod_res[,effect_lab])) + digits, 'right', '0')
    }

    # If there is a flex table caption, create a flex table
    if(!is.null(flex_caption)) {
      mod_tbl = mod_res

      # Add variable names
      mod_tbl$Variable = rownames(mod_tbl)

      # Reorder the results
      mod_tbl = mod_tbl[,c('Variable', effect_lab, 'CI', 'pvalue')]

      # Create flex table
      mod_tbl = flextable::flextable(mod_tbl)
      mod_tbl = flextable::set_header_labels(mod_tbl,
                                  pvalue = 'p-value',
                                  CI = '95% CI')
      mod_tbl = flextable::set_caption(mod_tbl, flex_caption)
      return(list(flex_table = mod_tbl, df = mod_res))
    } else {
      return(list(df = mod_res))
    }
  } else if(type == 'lm') {

    # Put coefficients from model in data.frame
    mod_res = data.frame(summary(mod)$coefficients)

    # We just need the estimate of the effect and the p-value for each
    # variable, ditching the intercept
    mod_res = mod_res[-1,c(1,4)]
    colnames(mod_res) = c('Estimate', 'pvalue')

    # Retrieve confidence intervals
    cis = data.frame(confint(mod))
    cis = cis[-1,] # ditch the intercept

    # Convert CIs to string and add to results
    mod_res$CI = format_cis(cis, digits = digits)

    # Reorder the results
    mod_res = mod_res[,c('Estimate', 'CI', 'pvalue')]

    # Save the p-values in case needed
    p = mod_res$pvalue

    # Reformat the p-values for consistent scientific notation
    mod_res$pvalue = get_sci(mod_res$pvalue, digits = digits)

    # Format the estimates
    mod_res$Estimate = stringr::str_pad(round(mod_res$Estimate, digits),
                               nchar(round(mod_res$Estimate)) + digits,
                               'right', '0')

    # If there is a flex table caption, create a flex table
    if(!is.null(flex_caption)) {
      mod_tbl = mod_res

      # Add variable names
      mod_tbl$Variable = rownames(mod_tbl)

      # Reorder the results
      mod_tbl = mod_tbl[,c('Variable', 'Estimate', 'CI', 'pvalue')]

      # Create flex table
      mod_tbl = flextable::flextable(mod_tbl)
      mod_tbl = flextable::set_header_labels(mod_tbl,
                                  pvalue = 'p-value',
                                  CI = '95% CI')
      mod_tbl = flextable::set_caption(mod_tbl, flex_caption)
      return(list(flex_table = mod_tbl, df = mod_res))
    } else {
      return(list(df = mod_res))
    }
  } # end if type

} # end pretty_mod
