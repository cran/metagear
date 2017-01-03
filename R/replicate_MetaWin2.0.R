#' Replicate meta-analysis results and summaries from MetaWin 2.0.
#'
#' Replicate meta-analysis results and summaries from Rosenberg's et al. (2000)
#' software 'MetaWin' 2.0.  Currently only replicates moderator analyses and not
#' meta-regressions.  
#'
#' @param model A two-sided linear formula object describing the model, with the
#'    response (effect sizes) on the left of a ~ operator and the moderator 
#'    variables, separated by +, :, * operators, on the right.  NOTE:  
#'    MetaWin was limited to analyses with a single moderator variable.  
#'    This function currently supports only categorical moderators.
#' @param weights A vector of effect size variances that will be used as 
#'    weights for the meta-analysis.
#' @param effects_model The default is \code{"random"}, which specifies a 
#'    random-effects meta-analysis.  Other options include \code{"fixed"} 
#'    which presents fixed-effect analyses.
#' @param data An optional data frame containing the variables named in model
#'    and weights.   
#' @param bootstraps The number of bootstraps used to estimate confidence 
#'    intervals.  As with 'MetaWin' 2.0, the default is 999.
#'
#' @return NULL
#'
#' @references Rosenberg, M.S., Adams, D.C., and Gurevitch, J. 2000. 
#'    MetaWin: Statistical Software for Meta-Analysis. Sinauer Associates 
#'    Sunderland, Massachusetts. 
#'
#' @importFrom metafor rma
#' @importFrom stats anova lm as.formula pchisq qt quantile coef
#' @export replicate_MetaWin2.0


replicate_MetaWin2.0 <- function(model, 
                                 weights, 
                                 effects_model = "random", 
                                 data, 
                                 bootstraps = 999) {
  
  theCall <- match.call()
  weights <- eval(theCall[[match("weights", names(theCall))]], 
                  data, 
                  enclos = sys.frame(sys.parent()))
  
  effects_model <- switch(effects_model,
                          random = "DL",
                          fixed = "FE")
  
  
  cat("\n=== START of Rosenberg et al. (2002) MetaWin 2.0 output ===\n\n")


  if(length(all.vars(model)) == 1) {
    
    # analyses without predictor variable
    rma_pooled <- rma(model, vi = weights, data = data, method = effects_model)
    rma_random <- rma(model, vi = weights, data = data, method = "DL")
    
    effect.pooled <- anova(do.call("lm", 
                  list(model, data, NULL, (1.0/(weights + rma_pooled$tau2)))))
    
    if(effects_model == "DL") {
      cat(paste0("\nEstimate of pooled variance: ", 
                 round(rma_pooled$tau2, 6), "\n"))
    }
    
    SUMMARY.RESULTS(rma_pooled, effect.pooled, rma_random, bootstraps)
    
  } else if (is.factor(data[, all.vars(model)[2]]) == TRUE) {
    
    # start: analyses with categorical predictor variable
    
    # get tau and pooled effects from model
    fixedModel <- as.formula(paste(all.vars(model)[1], 
                                   "~", 
                                   all.vars(model)[2], 
                                   "-1"))
    rma_results <- rma(fixedModel, vi = weights, data = data, method = effects_model)
    rma_random <- rma(fixedModel, vi = weights, data = data, method = "DL")
    
    rma_pooled <- rma(as.formula(paste(all.vars(model)[1], "~ 1")), 
                      tau2 = rma_results$tau2, vi = weights, data = data, 
                      method = effects_model)
  
    # get model sums of squares from lm based on metafor's tau square
    effects.results <- anova(do.call("lm", list(model, 
                                                data, 
                                                NULL, 
                                                (1.0/(weights + rma_results$tau2)))))
    effect.pooled <- anova(do.call("lm", list(as.formula(paste(all.vars(model)[1], "~ 1")), 
                                              data, 
                                              NULL, 
                                              (1.0/(weights + rma_results$tau2)))))
    
    cat(paste0("\nEstimate of pooled variance: ", round(rma_results$tau2, 6), "\n"))
    CATEGORICAL.RESULTS(rma_results, effects.results, data, model, weights, effects_model, bootstraps)
    SUMMARY.RESULTS(rma_pooled, effect.pooled, rma_random, bootstraps)
  }
  
  cat("\n\n=== END of Rosenberg et al. (2002) MetaWin 2.0 output ===\n")
  return(NULL)
}


# a few replicate_MetaWin2.0 helpers
SUMMARY.RESULTS <- function (rma.results, effects.results, random.results, bootstraps) {
  cat("\nSUMMARY RESULTS\n\n")
  cat(sprintf(c("%-20s", "%-5s", "%-10s\n"), c("Heterogeneity", "df", "Prob(Chi-Square)")))
  cat("------------------------------------------------\n")
  cat(sprintf(c("%-20s", "%-5s", "%-10s\n\n"), 
              c(
                paste0("Qtotal  ", round(effects.results[1,2], 6)), 
                effects.results[1,1], 
                round(1.0 - pchisq(effects.results[1,2], df=effects.results[1,1]), 5))
  )
  )
  theBootCI <- getBootCI(rma.results, bootstraps)
  cat(sprintf(c("%-23s", "%-23s", "%-23s", "%-23s\n"), c("Mean Effect Size", "95% CI", "Bootstrap CI", "Bias CI")))
  cat("--------------------------------------------------------------------------------------------\n")
  cat(sprintf(c("%-23s", "%-23s", "%-23s", "%-23s\n\n"), 
              c(
                paste0("E++  ", round(rma.results$b, 6)), 
                paste0(round(t_lCI(rma.results$b, rma.results$se, effects.results[1,1]), 6), " to ", round(t_uCI(rma.results$b, rma.results$se, effects.results[1,1]), 6)),
                paste0(round(theBootCI[1], 6), " to ", round(theBootCI[2], 6)),
                paste0("unknown", " to ", "unknown")
              )
  ))
  cat(paste0("Sqrt Pooled Variance = ", round(sqrt(random.results$tau2), 6), "\n"))
  cat(sprintf(c("%-25s", "%-20s\n"), 
              c(
                paste0(" Mean Study Variance = ", round(mean(rma.results$vi), 6)),
                paste0(" Ratio = ", round((sqrt(random.results$tau2))/mean(rma.results$vi), 6))
              )
  )
  )
  cat("\n------------------------------------------------\n")
}


CATEGORICAL.RESULTS <- function (rma.results, effects.results, theData, model, weights, effects_model, bootstraps) {
  cat("\nCATEGORICAL RESULTS\n\n")
  
  cat("--Heterogeneity--\n")
  if(effects_model == "DL") {

    cat(sprintf(c("%-10s", "%-10s", "%-10s\n"), c("Class", "#Studies", "PooledVar")))
    cat("-------------------------------\n")
    parsedMA <- split(theData, theData[, all.vars(model)[2]])
    parsedWeights <- split(weights, theData[, all.vars(model)[2]])
    fixedModel <- as.formula(paste(all.vars(model)[1], "~ 1"))
    for(i in length(parsedMA):1) {
      theRes <- rma(fixedModel, vi = parsedWeights[[i]], data = parsedMA[[i]], method = effects_model)
      cat(sprintf(c("%-10s", "%-10s", "%-10s\n"), 
                  c(names(parsedMA)[i], round(theRes$k, 6), round(theRes$tau2, 6))
      ))
    }
    cat("\n")
    
  } else {
    cat(sprintf(c("%-10s", "%-10s", "%-15s", "%-20s", "%-20s\n"), c("Class", "#Studies", "Qw", "df", "Prob(Chi-Square)")))
    cat("---------------------------------------------------------------------\n")
    parsedMA <- split(theData, theData[, all.vars(model)[2]])
    parsedWeights <- split(weights, theData[, all.vars(model)[2]])
    fixedModel <- as.formula(paste(all.vars(model)[1], "~ 1"))
    for(i in length(parsedMA):1) {
      theRes <- rma(fixedModel, vi = parsedWeights[[i]], data = parsedMA[[i]], method = "FE")
      cat(sprintf(c("%-10s", "%-10s", "%-15s", "%-20s", "%-20s\n"), 
                  c(names(parsedMA)[i], 
                    round(theRes$k, 6), 
                    round(theRes[[18]], 6),
                    theRes$k - 1,
                    round(1.0 - pchisq(theRes[[18]], df=(theRes$k - 1)), 5)
      )))
    }
    cat("\n") 
  }
  
  
  cat(sprintf(c("%-10s", "%-10s", "%-15s", "%-20s", "%-20s\n"), c("Model", "df", "Q", "Prob(Chi-Square)", "Prob(Rand)")))
  cat("---------------------------------------------------------------------\n")
  effectsRange <- nrow(effects.results) 
  cat(sprintf(c("%-10s", "%-10s", "%-15s", "%-20s", "%-20s\n"), 
              c("Between",
                effects.results[1,1],
                round(effects.results[1,2], 6),
                round(1.0 - pchisq(effects.results[1,2], df=effects.results[1,1]), 5),
                round(getRandomizationTest(rma.results, bootstraps, effects.results[1,2]), 5)
              )
  )
  )
  cat(sprintf(c("%-10s", "%-10s","%-15s","%-15s", "%-10s\n"), 
              c("Within",
                effects.results[2,1],
                round(effects.results[2,2], 6),
                round(1.0 - pchisq(effects.results[2,2], df=effects.results[2,1]), 5),
                ""
              )
  )
  )
  cat("---------------------------------------------------------------------\n")
  cat(sprintf(c("%-10s", "%-10s","%-15s","%-15s", "%-10s\n"), 
              c("Total",
                sum(effects.results[1:effectsRange,1]),
                round(sum(effects.results[1:effectsRange,2]), 6),
                round(1.0 - pchisq(sum(effects.results[1:effectsRange,2]), df=sum(effects.results[1:effectsRange,1])), 5),
                ""
              )
  )
  ) 
  
  cat("\n\n--Mean Effect Sizes--\n")
  cat(sprintf(c("%-10s", "%-10s","%-10s","%-5s", "%-25s", "%-25s", "%-25s\n"), c("Class", "#Studies", "E+", "df", "95% CI", "Bootstrap CI",  "Bias CI")))
  cat("----------------------------------------------------------------------------------------------------------------\n")
  
  thePred <- all.vars(model)[2]
  for(i in length(levels(theData[, thePred])):1) {
    cat(sprintf(c("%-10s", "%-10s","%-10s","%-5s", "%-25s", "%-25s", "%-25s\n"), 
                c(
                  levels(theData[, thePred])[i], 
                  table(theData[, thePred])[i],
                  round(rma.results$b[i], 6),
                  table(theData[, thePred])[i] - 1,
                  paste0(round(t_lCI(rma.results$b[i], rma.results$se[i], table(theData[, thePred])[i] - 1), 6), " to ", round(t_uCI(rma.results$b[i], rma.results$se[i], table(theData[, thePred])[i] - 1), 6)),
                  paste0("unknown", " to ", "unknown"),
                  paste0("unknown", " to ", "unknown")
                )
    ))
  }
  
}

t_lCI <- function(E, var, df) {
  return(E - qt(1.0 - 0.024991, df) * var)
}

t_uCI <- function(E, var, df) {
  return(E + qt(1.0 - 0.024991, df) * var)
}

getRandomizationTest <- function(rma.results, bootstraps, theQM) {
  
  rma.randomizationTest <- function(observed) {
    aSample <- sample(length(observed$yi), length(observed$yi), replace = TRUE)
    sampleRES <- suppressWarnings(try(rma(yi = observed$yi[aSample], 
                                          mods = observed$X, 
                                          vi = observed$vi[aSample], 
                                          method = observed$method),  
                                      silent = TRUE))
    return(sampleRES)
  }
  
  randomRMA <- replicate(bootstraps, rma.randomizationTest(rma.results))
  return(mean(unlist(randomRMA["QM",]) >= theQM))
}

getBootCI <- function(rma.results, bootstraps) {
  
  rma.randomizationTest <- function(observed) {
    aSample <- sample(length(observed$yi), length(observed$yi), replace = TRUE)
    sampleRES <- suppressWarnings(try(rma(yi = observed$yi[aSample], 
                                          mods = observed$X, 
                                          vi = observed$vi[aSample], 
                                          method = observed$method, 
                                          intercept = FALSE),  
                                      silent = TRUE))
    return(coef(sampleRES))
  }
  
  randomRMA <- replicate(bootstraps, rma.randomizationTest(rma.results))
  return(quantile(sort(randomRMA),  c(0.025, 0.975)))
}
