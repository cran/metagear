#' Replicate phylogeneic meta-analysis results and summaries from phyloMeta 1.3.
#'
#' Replicate phylogenetic meta-analysis results and summaries from Lajeunesse
#' (2011) software 'phyloMeta' 1.3.  Currently does not fully replicate all
#' functionality.
#'
#' @param model A two-sided linear formula object describing the model, with the
#'    response (effect sizes) on the left of a ~ operator and the moderator
#'    variables, separated by +, :, * operators, on the right.  NOTE:
#'    phyloMeta was limited to analyses with a single moderator variable.
#'    This function currently supports only numerical categorical moderators.
#' @param weights A vector of effect size variances that will be used as
#'    weights for the meta-analysis.
#' @param data A data frame containing the variables named in model
#'    and weights and species names (names must be exact as specified in
#'    phylogeny).
#' @param phylogenyFile A text file containing a NEWICK phylogeny. The number of
#'    species must be same as (k) number of effect sizes in data.
#'
#' @return NULL
#'
#' @references Lajeunesse, M.J. (2011) phyloMeta: a program for phylogenetic
#'    comparative analyses with meta-analysis. Bioinformatics 27, 2603-2604.
#'
#' @import Matrix
#' @importFrom metafor rma rma.mv
#' @importFrom stats as.formula pchisq model.matrix
#' @export replicate_MetaWin2.0


replicate_phyloMeta1.3 <- function(model,
                                   weights,
                                   data,
                                   phylogenyFile) {

  phylogeny <- ape::read.tree(file = "phylogeny_example.txt")

  # allign species names with phylogeny
  data$species <- factor(data$species, levels = data$species)
  data <- data[order(factor(data$species, levels = c(phylogeny$tip.label))), ]

  # to do douwble check if only one moderator
  # phyloMeta idiosyncrasy: convert moderator number to factor

  #if(all.vars(model) > 2) {}

  #data[, all.vars(model)[2]] <- as.factor(data[, all.vars(model)[2]])

  #weightsLabel <- weights
  theCall <- match.call()
  weights <- eval(theCall[[match("weights", names(theCall))]],
                  data,
                  enclos = sys.frame(sys.parent()))


  cat("=== START of Lajeunesse (2011) phyloMeta 1.3 output ===\n\n\n")

  # phylogeny section

  number_polytomies <- sum(tapply(phylogeny$edge[, 2], phylogeny$edge[, 1], length) > 2)

  cat("PHYLOGENY\n")
  cat("================================================================================\n\n")
  cat("NOTE: text phylogeny omitted\n\n")
  cat("  PHYLOGENY SUMMARY.\n\n")
  cat(sprintf(c("%20s", "%-1s", "%-10s\n"), c("# of tips", "=", length(phylogeny$tip.label))))
  cat(sprintf(c("%20s", "%-1s", "%-10s\n"), c("ultrametric", "=", ape::is.ultrametric(phylogeny))))
  cat(sprintf(c("%20s", "%-1s", "%-10s\n\n\n"), c("# of polytomies", "=", number_polytomies)))


  ##############START: all studies analyses

  # get 'all studies' pooled effects
  rma_fixed <- rma(as.formula(paste(all.vars(model)[1], "~ 1")), vi = weights, data = data, method = "FE")
  rma_random <- rma(as.formula(paste(all.vars(model)[1], "~ 1")), vi = weights, data = data, method = "DL")

  message(paste(all.vars(model)[1], "~ 1"))

  # weighting matrix for fixed and random effects model
  W_fixed <- diag(rma_fixed$vi)
  W_random <- diag(rma_fixed$vi) + diag(rma_random$tau2, nrow(W_fixed), ncol(W_fixed))

  # extract phylogenetic correlations from phylogeny
  P <- cov2cor(ape::vcv(phylogeny))

  # weighted by variance matrix and phylogenetic correlations for fixed effect
  WP_fixed <- sqrt(W_fixed) %*% P %*% sqrt(W_fixed)

  # weighted by variance matrix and phylogenetic correlations for random effects,
  # with tau square conditional on non-phylogenetic model
  WP_random <- WP_fixed + diag(rma_random$tau2, nrow(WP_fixed), ncol(WP_fixed))

  # get 'all studies' pooled phylogenetic effects (model assumes that sampling error has phylogenetic structure)
  rma_fixed_phylo <- rma.mv(as.formula(paste(all.vars(model)[1], "~ 1")), V = WP_fixed, data = data, method = "FE")
  rma_random_phylo <- rma.mv(as.formula(paste(all.vars(model)[1], "~ 1")), V = WP_random, data = data, method = "FE")

  ############## END: all studies analyses


  if(length(all.vars(model)) == 1) {

    # traditional meta-analysis section

    cat("RESULTS SECTION A. Traditional meta-analysis.\n")
    cat("================================================================================\n\n")


    # effects table
    cat("\n  TABLE 1. Summary of fit statistics.\n")
    cat("  ----------------------------------------------\n")
    cat(sprintf(c("    %17-s", "%7-s", "%4-s", "%6-s\n"), c("Source", "Q", "df", "p")))
    cat("  ----------------------------------------------\n")
    cat(sprintf(c("    %17-s", "%7-s", "%4-s", "%6-s\n"), c("Within groups", round(rma_random$QE, 2) , rma_random$k - 1, round(rma_random$QEp, 4) )))
    cat("  ----------------------------------------------\n\n")

    Z_fixed <- getZtests(rma_fixed$yi, diag(rma_fixed$vi), matrix(1, length(rma_fixed$yi)))
    Z_random <- getZtests(rma_fixed$yi, diag(rma_fixed$vi + rma_random$tau2), matrix(1, length(rma_fixed$yi)))

    # pooled effects table
    cat("\n  TABLE 2. Summary of pooled effect sizes (pooled) and variances (var).\n")
    cat("  ------------------------------------------------------------------------------\n")
    cat("                                                               Is non-zero?    \n")
    cat("                                                           ---------------------\n")
    cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c("Group", "k", "pooled", "var", "95%CI", "Z", "df", "p")))
    cat("  ------------------------------------------------------------------------------\n")
    cat("  Fixed-effects\n\n")
    cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c("All studies",
                                                                                             rma_fixed$k,
                                                                                             round(rma_fixed$b, 3),
                                                                                             round((rma_fixed$se)^2, 4),
                                                                                             paste0("(",round(rma_fixed$ci.lb,3),",",round(rma_fixed$ci.ub,3),")"),
                                                                                             round(Z_fixed$Z_test, 2),
                                                                                             Z_fixed$Z_df,
                                                                                             format(round(Z_fixed$Z_p, 4), scientific = FALSE))))
    cat(paste0("\n  Random-effects (between-study var = ", round(rma_random$tau2, 5), ")\n\n"))
    cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c("All studies",
                                                                                             rma_fixed$k,
                                                                                             round(rma_random$b, 3),
                                                                                             round((rma_random$se)^2, 4),
                                                                                             paste0("(",round(rma_random$ci.lb,3),",",round(rma_random$ci.ub,3),")"),
                                                                                             round(Z_random$Z_test, 2),
                                                                                             Z_random$Z_df,
                                                                                             format(round(Z_random$Z_p, 4), scientific = FALSE))))
    cat("  ------------------------------------------------------------------------------\n\n\n\n")



    cat("RESULTS SECTION B. Phylogenetically-independent meta-analysis.\n")
    cat("================================================================================\n\n")


    # effects table
    cat("\n  TABLE 1. Summary of fit statistics.\n")
    cat("  ----------------------------------------------------------\n")
    cat("                                              Adjusted via\n")
    cat("                                              # polytomies\n")
    cat("                                          -------------------\n")
    cat(sprintf(c("    %17-s", "%7-s", "%4-s", "%9-s", "%4-s", "%6-s\n"), c("Source", "Q", "df", "p", "df", "p")))
    cat("  -----------------------------------------------------------\n")
    cat(sprintf(c("    %17-s", "%7-s", "%4-s", "%9-s", "%4-s", "%6-s\n"), c("Within groups", round(rma_fixed_phylo$QE, 2) , rma_fixed_phylo$k - 1, round(rma_fixed_phylo$QEp,4), rma_fixed_phylo$k -  number_polytomies - 1, round(1 - pchisq(rma_fixed_phylo$QE, df = rma_fixed_phylo$k -  number_polytomies - 1), 4) )))
    cat("  -----------------------------------------------------------\n\n")


    # pooled effects table
    cat("\n  TABLE 2. Summary of pooled effect sizes (pooled) and variances (var).\n")
    cat("  ------------------------------------------------------------------------------\n")
    cat("                                                               Is non-zero?    \n")
    cat("                                                           ---------------------\n")
    cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c("Group", "k", "pooled", "var", "95%CI", "Z", "df", "p")))
    cat("  ------------------------------------------------------------------------------\n")
    cat("  Fixed-effects\n\n")
    cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c("All studies",
                                                                                             rma_fixed_phylo$k,
                                                                                             round(rma_fixed_phylo$b, 3),
                                                                                             round((rma_fixed_phylo$se)^2, 4),
                                                                                             paste0("(",round(rma_fixed_phylo$ci.lb,3),",",round(rma_fixed_phylo$ci.ub,3),")"),
                                                                                             round(Z_fixed$Z_test, 2),
                                                                                             Z_fixed$Z_df,
                                                                                             format(round(Z_fixed$Z_p, 4), scientific = FALSE))))
    cat(paste0("\n  Random-effects (between-study var = ", round(rma_random$tau2, 5), ")\n\n"))
    cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c("All studies",
                                                                                             rma_fixed$k,
                                                                                             round(rma_random_phylo$b, 3),
                                                                                             round((rma_random_phylo$se)^2, 4),
                                                                                             paste0("(",round(rma_random_phylo$ci.lb,3),",",round(rma_random_phylo$ci.ub,3),")"),
                                                                                             round(Z_random$Z_test, 2),
                                                                                             Z_random$Z_df,
                                                                                             format(round(Z_random$Z_p, 4), scientific = FALSE))))
    cat("  ------------------------------------------------------------------------------\n\n\n\n")



    cat("RESULTS SECTION C. Traditional vs. phylogenetically-independent meta-analysis.\n")
    cat("================================================================================\n\n")


    # pooled effects table
    cat("\n  TABLE 1. Summary of model fit.\n")
    cat("  ------------------------------------------------------------------------------\n")
    cat("    Meta-analysis                               AIC          -2(likelihood)  \n")
    cat("                                         -----------------  ----------------\n")
    cat(sprintf(c("    %37-s", "%8-s", "%8-s", "%8-s", "%8-s\n"), c("",
                                                                    "fixed",
                                                                    "random",
                                                                    "fixed",
                                                                    "random" )))
    cat("  ------------------------------------------------------------------------------\n")
    cat(sprintf(c("    %37-s", "%8-s", "%8-s", "%8-s", "%8-s\n"), c("Traditional",
                                                                    round(AIC_via_RSS(rma_fixed$yi, W_fixed, matrix(1, length(rma_fixed$yi)), FALSE), 2),
                                                                    round(AIC_via_RSS(rma_fixed$yi, W_random, matrix(1, length(rma_fixed$yi)), FALSE), 2),
                                                                    "NA",
                                                                    "NA" )))
    cat(sprintf(c("    %37-s", "%8-s", "%8-s", "%8-s", "%8-s\n"), c("Phylogenetically-independent",
                                                                    round(AIC_via_RSS(rma_fixed$yi, WP_fixed, matrix(1, length(rma_fixed$yi)), TRUE), 2),
                                                                    round(AIC_via_RSS(rma_fixed$yi, WP_random, matrix(1, length(rma_fixed$yi)), TRUE), 2),
                                                                    "NA",
                                                                    "NA" )))
    cat("  ------------------------------------------------------------------------------\n")
    cat("  Note. Lowest AIC is best fit, and -2(likelihood) not yet calculated in R.\n\n\n")

  } else {

    ##############START: by group analyses

    data[, all.vars(model)[2]] <- as.factor(data[, all.vars(model)[2]])

    # get 'all studies' pooled effects

    rma_fixed_mod <- rma(model, vi = weights, data = data, method = "FE")
    rma_random_mod <- rma(model, vi = weights, data = data, method = "DL")
    rma_fixed_pooled <- rma(update(model, ~ . - 1), vi = weights, data = data, method = "FE")
    rma_random_pooled <- rma(update(model, ~ . - 1), vi = weights, data = data, method = "DL")

    theGroups <- model.matrix(as.formula(paste("~ as.factor(", all.vars(model)[2], ") - 1")), data = data)
    groupNames <- gsub("..*)", "", colnames(theGroups))
    rma_fixed_groups <- groupMeta(theGroups, as.vector(rma_fixed_mod$yi), W_fixed)

    W_random <- diag(rma_fixed$vi) + diag(rma_random_mod$tau2, nrow(W_fixed), ncol(W_fixed))
    WP_random <- WP_fixed + diag(rma_random_mod$tau2, nrow(WP_fixed), ncol(WP_fixed))

    rma_fixed_mod_phylo <- rma.mv(model, V = WP_fixed, data = data, method = "FE") # Qb fixed
    rma_random_mod_phylo <- rma.mv(model, V = WP_random, data = data, method = "FE") # Qb random
    rma_fixed_pooled_phylo <- rma.mv(update(model, ~ . - 1), V = WP_fixed, data = data, method = "FE")
    rma_random_pooled_phylo <- rma.mv(update(model, ~ . - 1), V = WP_random, data = data, method = "FE")

    rma_fixed_groups_phylo <- groupMeta(theGroups, as.vector(rma_fixed_mod$yi), WP_fixed)



    ############## END: by group analyses


    # traditional meta-analysis section

    cat("RESULTS SECTION A. Traditional meta-analysis.\n")
    cat("================================================================================\n\n")


    # effects table
    cat("\n  TABLE 1. Summary of fit statistics.\n")
    cat("  ----------------------------------------------\n")
    cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%6-s\n"), c("Source", "Q", "df", "p")))
    cat("  ----------------------------------------------\n")
    cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%6-s\n"), c("Between groups", round(rma_fixed_mod$QM, 2) , rma_fixed_mod$m, round(rma_fixed_mod$QMp, 4) )))
    cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%6-s\n"), c("Within groups", round(rma_fixed_mod$QE, 2) , rma_fixed_mod$k - 1, round(rma_fixed_mod$QEp, 4) )))
    for(i in 1:ncol(rma_fixed_groups))
      cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%6-s\n"), c(paste("  Within group ", groupNames[i]), round(rma_fixed_groups[["QE", i]], 2) , rma_fixed_groups[["k", i]] - 1, round(rma_fixed_groups[["QEp", i]], 4) )))
    cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%6-s\n\n"), c("Total", round(rma_fixed_mod$QM, 2) + round(rma_fixed_mod$QE, 2), "", "" )))
    cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%6-s\n"), c("Between groups", "" , "", "")))
    cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%6-s\n"), c("  random-effects", round(rma_random_mod$QM, 2) , rma_random_mod$m, round(rma_random_mod$QMp, 4) )))
    cat("  ----------------------------------------------\n\n\n")


    Z_fixed <- getZtests(rma_fixed$yi, diag(rma_fixed$vi), matrix(1, length(rma_fixed$yi)))
    Z_random <- getZtests(rma_fixed$yi, diag(rma_fixed$vi + rma_random$tau2), matrix(1, length(rma_fixed$yi)))
    Z_fixed_group <- getZtests(rma_fixed_pooled$yi, diag(rma_fixed_pooled$vi), theGroups)
    Z_random_group <- getZtests(rma_random_pooled$yi, diag(rma_random_pooled$vi + rma_random_pooled$tau2), theGroups)


    # pooled effects table
    cat("\n  TABLE 2. Summary of pooled effect sizes (pooled) and variances (var).\n")
    cat("  ------------------------------------------------------------------------------\n")
    cat("                                                               Is non-zero?    \n")
    cat("                                                           ---------------------\n")
    cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c("Group", "k", "pooled", "var", "95%CI", "Z", "df", "p")))
    cat("  ------------------------------------------------------------------------------\n")
    cat("  Fixed-effects\n\n")
    cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c("All studies",
                                                                                             rma_fixed$k,
                                                                                             round(rma_fixed$b, 3),
                                                                                             round((rma_fixed$se)^2, 4),
                                                                                             paste0("(",round(rma_fixed$ci.lb,3),",",round(rma_fixed$ci.ub,3),")"),
                                                                                             round(Z_fixed$Z_test, 2),
                                                                                             Z_fixed$Z_df,
                                                                                             format(round(Z_fixed$Z_p, 4), scientific = FALSE))))
    for(i in 1:ncol(rma_fixed_groups))
      cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c(paste("  Group ", groupNames[i]),
                                                                                               rma_fixed_groups[["k", i]],
                                                                                               round(rma_fixed_pooled$b[i], 3),
                                                                                               round((rma_fixed_pooled$se[i])^2, 4),
                                                                                               paste0("(",round(rma_fixed_pooled$ci.lb[i],3),",",round(rma_fixed_pooled$ci.ub[i],3),")"),
                                                                                               round(Z_fixed_group$Z_test[i], 2),
                                                                                               Z_fixed_group$Z_df[i],
                                                                                               format(round(Z_fixed_group$Z_p[i], 4), scientific = FALSE))))



    cat(paste0("\n  Random-effects (between-study var = ", round(rma_random_pooled$tau2, 5), ")\n\n"))
    cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c("All studies",
                                                                                             rma_fixed$k,
                                                                                             round(rma_random$b, 3),
                                                                                             round((rma_random$se)^2, 4),
                                                                                             paste0("(",round(rma_random$ci.lb,3),",",round(rma_random$ci.ub,3),")"),
                                                                                             round(Z_random$Z_test, 2),
                                                                                             Z_random$Z_df,
                                                                                             format(round(Z_random$Z_p, 4), scientific = FALSE))))
    for(i in 1:ncol(rma_fixed_groups))
      cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c(paste("  Group ", groupNames[i]),
                                                                                               rma_fixed_groups[["k", i]],
                                                                                               round(rma_random_pooled$b[i], 3),
                                                                                               round((rma_random_pooled$se[i])^2, 4),
                                                                                               paste0("(",round(rma_random_pooled$ci.lb[i],3),",",round(rma_random_pooled$ci.ub[i],3),")"),
                                                                                               round(Z_random_group$Z_test[i], 2),
                                                                                               Z_random_group$Z_df[i],
                                                                                               format(round(Z_random_group$Z_p[i], 4), scientific = FALSE))))
    cat("  ------------------------------------------------------------------------------\n\n\n\n")



    cat("RESULTS SECTION B. Phylogenetically-independent meta-analysis.\n")
    cat("================================================================================\n\n")


    # effects table
    cat("\n  TABLE 1. Summary of fit statistics.\n")
    cat("  --------------------------------------------------------------\n")
    cat("                                                  Adjusted via\n")
    cat("                                                  # polytomies\n")
    cat("                                                -----------------\n")
    cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%12-s", "%5-s", "%6-s\n"), c("Source", "Q", "df", "p", "df", "p")))
    cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%6-s\n"), c("Between groups", round(rma_fixed_mod_phylo$QM, 2) , rma_fixed_mod_phylo$m, round(rma_fixed_mod_phylo$QMp, 4) )))
    cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%12-s", "%5-s", "%6-s\n"), c("Within groups", round(rma_fixed_mod_phylo$QE, 2) , rma_fixed_mod_phylo$k - 1, round(1 - pchisq(rma_fixed_mod_phylo$QE, df = rma_fixed_mod_phylo$k - 1), 4) , rma_fixed_mod_phylo$k -  number_polytomies - 1, round(1 - pchisq(rma_fixed_mod_phylo$QE, df = rma_fixed_mod_phylo$k - number_polytomies - 1), 4) )))
    for(i in 1:ncol(rma_fixed_groups))
      cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%12-s", "%5-s", "%6-s\n"), c(paste("  Within group ", groupNames[i]), round(rma_fixed_groups_phylo[["QE", i]], 2) , rma_fixed_groups_phylo[["k", i]] - 1, round(rma_fixed_groups_phylo[["QEp", i]], 4), rma_fixed_groups_phylo[["k", i]] -  number_polytomies - 1, round(1 - pchisq(rma_fixed_groups_phylo[["QE", i]], df = rma_fixed_groups_phylo[["k", i]] - number_polytomies - 1), 4) )))
    cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%6-s\n\n"), c("Total", round(rma_fixed_mod_phylo$QM, 2) + round(rma_fixed_mod_phylo$QE, 2), "", "" )))
    cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%6-s\n"), c("Between groups", "" , "", "")))
    cat(sprintf(c("    %20-s", "%7-s", "%4-s", "%6-s\n"), c("  random-effects", round(rma_random_mod_phylo$QM, 2) , rma_random_mod_phylo$m, round(rma_random_mod_phylo$QMp, 4) )))
    cat("  ---------------------------------------------------------------------\n\n\n")



    Z_fixed_phylo <- getZtests(rma_fixed$yi,WP_fixed, matrix(1, length(rma_fixed$yi)))
    Z_random_phylo <- getZtests(rma_fixed$yi, WP_random, matrix(1, length(rma_fixed$yi)))
    Z_fixed_group_phylo <- getZtests(rma_fixed_pooled$yi, WP_fixed, theGroups)
    Z_random_group_phylo <- getZtests(rma_random_pooled$yi, WP_random, theGroups)


    # pooled effects table
    cat("\n  TABLE 2. Summary of pooled effect sizes (pooled) and variances (var).\n")
    cat("  ------------------------------------------------------------------------------\n")
    cat("                                                               Is non-zero?    \n")
    cat("                                                           ---------------------\n")
    cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c("Group", "k", "pooled", "var", "95%CI", "Z", "df", "p")))
    cat("  ------------------------------------------------------------------------------\n")
    cat("  Fixed-effects\n\n")
    cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c("All studies",
                                                                                             rma_fixed_phylo$k,
                                                                                             round(rma_fixed_phylo$b, 3),
                                                                                             round((rma_fixed_phylo$se)^2, 4),
                                                                                             paste0("(",round(rma_fixed_phylo$ci.lb,3),",",round(rma_fixed_phylo$ci.ub,3),")"),
                                                                                             round(Z_fixed_phylo$Z_test, 2),
                                                                                             Z_fixed_phylo$Z_df,
                                                                                             format(round(Z_fixed_phylo$Z_p, 4), scientific = FALSE))))

    for(i in 1:ncol(rma_fixed_groups))
      cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c(paste("  Group ", groupNames[i]),
                                                                                               rma_fixed_groups_phylo[["k", i]],
                                                                                               round(rma_fixed_pooled_phylo$b[i], 3),
                                                                                               round((rma_fixed_pooled_phylo$se[i])^2, 4),
                                                                                               paste0("(",round(rma_fixed_pooled_phylo$ci.lb[i],3),",",round(rma_fixed_pooled_phylo$ci.ub[i],3),")"),
                                                                                               round(Z_fixed_group_phylo$Z_test[i], 2),
                                                                                               Z_fixed_group_phylo$Z_df[i],
                                                                                               format(round(Z_fixed_group_phylo$Z_p[i], 4), scientific = FALSE))))



    cat(paste0("\n  Random-effects (between-study var = ", round(rma_random_phylo$tau2, 5), ")\n\n"))
    cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c("All studies",
                                                                                             rma_fixed$k,
                                                                                             round(rma_random_phylo$b, 3),
                                                                                             round((rma_random_phylo$se)^2, 4),
                                                                                             paste0("(",round(rma_random_phylo$ci.lb,3),",",round(rma_random_phylo$ci.ub,3),")"),
                                                                                             round(Z_random_phylo$Z_test, 2),
                                                                                             Z_random_phylo$Z_df,
                                                                                             format(round(Z_random_phylo$Z_p, 4), scientific = FALSE))))
    for(i in 1:ncol(rma_fixed_groups))
      cat(sprintf(c("    %15-s", "%5-s", "%8-s", "%7-s", "%17-s", "%7-s", "%4-s", "%7-s\n"), c(paste("  Group ", groupNames[i]),
                                                                                               rma_fixed_groups_phylo[["k", i]],
                                                                                               round(rma_random_pooled_phylo$b[i], 3),
                                                                                               round((rma_random_pooled_phylo$se[i])^2, 4),
                                                                                               paste0("(",round(rma_random_pooled_phylo$ci.lb[i],3),",",round(rma_random_pooled_phylo$ci.ub[i],3),")"),
                                                                                               round(Z_random_group_phylo$Z_test[i], 2),
                                                                                               Z_random_group_phylo$Z_df[i],
                                                                                               format(round(Z_random_group_phylo$Z_p[i], 4), scientific = FALSE))))

    cat("  ------------------------------------------------------------------------------\n\n\n\n")





    cat("RESULTS SECTION C. Traditional vs. phylogenetically-independent meta-analysis.\n")
    cat("================================================================================\n\n")


    # pooled effects table
    cat("\n  TABLE 1. Summary of model fit.\n")
    cat("  ------------------------------------------------------------------------------\n")
    cat("    Meta-analysis                               AIC          -2(likelihood)  \n")
    cat("                                         -----------------  ----------------\n")
    cat(sprintf(c("    %37-s", "%8-s", "%8-s", "%8-s", "%8-s\n"), c("",
                                                                    "fixed",
                                                                    "random",
                                                                    "fixed",
                                                                    "random" )))
    cat("  ------------------------------------------------------------------------------\n")
    cat(sprintf(c("    %37-s", "%8-s", "%8-s", "%8-s", "%8-s\n"), c("Traditional",
                                                                    round(AIC_via_RSS(rma_fixed_mod$yi, W_fixed, matrix(1, length(rma_fixed_mod$yi)), FALSE), 2),
                                                                    round(AIC_via_RSS(rma_fixed_mod$yi, W_random, matrix(1, length(rma_fixed_mod$yi)), FALSE), 2),
                                                                    "NA",
                                                                    "NA" )))
    cat(sprintf(c("    %37-s", "%8-s", "%8-s", "%8-s", "%8-s\n"), c("Phylogenetically-independent",
                                                                    round(AIC_via_RSS(rma_fixed_mod$yi, WP_fixed, matrix(1, length(rma_fixed_mod$yi)), TRUE), 2),
                                                                    round(AIC_via_RSS(rma_fixed_mod$yi, WP_random, matrix(1, length(rma_fixed_mod$yi)), TRUE), 2),
                                                                    "NA",
                                                                    "NA" )))
    cat("  ------------------------------------------------------------------------------\n")
    cat("  Note. Lowest AIC is best fit, and -2(likelihood) not yet calculated in R.\n\n\n")




  }


  cat("\n\n=== END of Lajeunesse (2011) phyloMeta 1.3 output ===\n")
  return(NULL)
}

getZtests <- function(E, V, X) {
  Z_test <- as.vector(t(t(E) %*% solve(V)  %*% X  %*% solve(t(X)  %*% solve(V)  %*% X)) * (t(X)  %*% solve(V) %*% E))
  Z_df <- rep(1, length(Z_test))
  Z_p <- 1 - pchisq(Z_test, df = Z_df)
  return(list(Z_test = Z_test, Z_df = Z_df, Z_p = Z_p))
}

AIC_via_RSS <- function(E, V, X, isPhylo) {
  k <- length(E)
  m <- 1
  RSS <- t(E) %*% (solve(V) - solve(V) %*% X %*% solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V)) %*% E
  if(isPhylo == TRUE) m <- 2
  return((2.0 * m) + (k * (log(2.0 * pi * (RSS / k)) + 1.0)))
}

Qw_groups <- function(E, V, X) {
  RSS <- t(E) %*% (solve(V) - solve(V) %*% X %*% solve(t(X) %*% solve(V) %*% X) %*% t(X) %*% solve(V)) %*% E
  return(RSS)
}

getSubgroup <- function(group_Matrix, theData) {
  sapply(split(group_Matrix, c(col(group_Matrix))),
         function(x, y) {
           if(is.vector(y) == TRUE) return(y[which(x != 0)])
           y[which(x != 0), which(x != 0)]
         },
         y = theData)
}

groupMeta <- function(theGroups, theY, theWeights) {
  mapply(function(x, y) rma.mv(yi = x, V = y, method = "FE"),
         x = getSubgroup(theGroups, theY),
         y = getSubgroup(theGroups, theWeights))
}


