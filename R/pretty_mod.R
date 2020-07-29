#' Checks if a number is whole
#'
#' @return (boolean): is the number whole?
#' @param num (numeric): number to check
#' @param tol (numeric): how close the number needs to be to whole
#' @export
#'
is_wholenumber <- function(num, tol = .Machine$double.eps^0.5)  abs(num - round(num)) < tol

#' Convert number to scientific notation
#'
#' @return (character): number in scientific notation
#' @param num (numeric): number to convert
#' @param digits (integer): precision of result
#' @export
#'
get_sci = function(num, digits = 2) {
  return(formatC(num, format = 'e', digits = digits))
}

#' Format number
#'
#' @return (vector): strings of fixed width formatted numbers
#' @param num (vector): vectors of numbers to format
#' @param digits (integer): precision of result
#' @export
#'
format_num = function(num, digits = 2) {
  num_temp = num

  whole = is_wholenumber(round(num, digits))
  nas = is.na(num)
  num_temp[whole & !nas] = round(num[whole & !nas])
  num_temp[whole & !nas] = stringr::str_pad(paste0(num_temp[whole & !nas], '.'),
                                              nchar(num_temp[whole & !nas]) + 1 + digits, 'right', '0')
  num_temp[!whole & !nas] = stringr::str_pad(round(num[!whole & !nas], digits),
                                               nchar(round(num[!whole & !nas])) + 1 + digits, 'right', '0')
  num = num_temp

  return(num)
}


#' Format confidence intervals
#'
#' @return (vector): strings with formatted confidence intevals
#' @param cis (data.frame): CIs with low in first column high in second
#' @param digits (integer): precision of result
#' @export
#'
format_cis = function(cis, digits = 2) {
  low = format_num(cis[,1], digits = digits)
  high = format_num(cis[,2], digits = digits)

  cis = paste0('[', low, ',', high, ']')
  return(cis)
}

#' Pretty format results from models
#'
#' @return (list): data.frame of results or df and flextable
#' @param mod (object): a model object
#' @param type (character): the type of model
#' @param digits (integer): precision of result
#' @param flex_caption (character): caption for flex table
#' @export
#'
pretty_mod = function(mod,
                      type = 'binomial',
                      digits = 2,
                      flex_caption = NULL,
                      expo = TRUE) {

  if(type %in% c('binomial', 'negbin', 'surv')) {
    # Depending on model type, set effect label
    if(type == 'negbin') {
      effect_lab = 'Estimate'
    } else if(type == 'binomial' & expo) {
      effect_lab = 'OR'
    } else if(type == 'binomial'){
      effect_lab = 'ln(OR)'
    } else if(type == 'surv' & expo) {
      effect_lab = 'HR'
    } else if(type == 'surv') {
      effect_lab = 'ln(HR)'
    }

    # Put coefficients from model in data.frame
    mod_res = data.frame(summary(mod)$coefficients)

    # We just need the estimate of the effect and the p-value for each
    # variable, ditching the intercept
    if(type != 'surv') {
      mod_res = mod_res[-1,c(1,4)]
    } else {
      mod_res = mod_res[,c(1,4)]
    }

    colnames(mod_res) = c(effect_lab, 'pvalue')

    if(expo) {
      # Exponentiate the effect
      mod_res[,1] = exp(mod_res[,1])

      # Retrieve confidence intervals
      cis = data.frame(exp(confint(mod)))
    } else {
      cis = data.frame(confint(mod))
    }

    # ditch the intercept
    if(type != 'surv') {
      cis = cis[-1,]
    }

    # Convert CIs to string and add to results
    mod_res$CI = format_cis(cis, digits = digits)

    # Reorder the results
    mod_res = mod_res[,c(effect_lab, 'CI', 'pvalue')]

    # Save the p-values in case needed
    p = mod_res$pvalue

    # Reformat the p-values for consistent scientific notation
    mod_res$pvalue = get_sci(mod_res$pvalue, digits = digits)

    # Format the ORs
    mod_res[,effect_lab] = format_num(mod_res[,effect_lab], digits = digits)

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
    mod_res$Estimate = format_num(mod_res$Estimate, digitis = digits)

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
