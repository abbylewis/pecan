##' Adjust LPJ-GUESS individual's biomass
##' 
##' This function adjusts an LPJ-GUESS individual by calling the LPJ-GUESS allocation function (compiled C++)
##' with a given biomass change.  It updates the individual biomass pools directly, and also returns, in a list further 
##' adjustments to the litter pools.
##' 
##' @param individual A nested list which encapsulates an LPJ-GUESS 'Individual' as read from a binary state file
##' @param biomass.inc A numeric value specifying the biomass increment to be applied.
##' @param sla The SLA (specific leaf area) (per PFT parameter)
##' @param wooddens Wood density (kgC/m^2) (per PFT parameter)
##' @param lifeform An integer code for the lifeform of this individual (cohort): 1 = Tree, 2 = Grass
##' @param k_latosa The leaf area to sapwood area ratio (per PFT parameter)
##' @param k_allom2,k_allom3, Allometry coefficients (per PFT parameters)
##' @param trace Logical; if TRUE, prints details of biomass adjustment process.
##' 
##' The changes in C pools are determined by the allocation.  The changes in the N pools are designed to 
##' maintain the pre-exisiing C:N ratios, so N is just scaled using the updated C with the initial C:N ratio.
##' The N storage pools (nstore_longterm and nstore_labile) don't have pre-existing C:N ratios, so they are 
##' just scaled by the overall biomass change (the 'rel.change' argument to the function).
##' 
##' Note that after this function is called the function \code{allometry} should be used to update the individual
##' and to check that the newly updated individual has a 'valid' allometry. The litter pools should also be updated.
##' This is implemented in the \code{update.state.LPJGUESS} function following the call to this \code{adjust.biomass.LPJGUESS} function. 
##' 
##' 
##' @keywords internal
##' @return the scaled 'individual' (the initial nested list with update values)
##' @author Yinghao Sun
adjust.biomass.scaling.LPJGUESS <- function(individual, biomass.inc, sla, wooddens, lifeform, k_latosa, k_allom2, k_allom3, trace = TRUE){
  
  ## ---------------- 基本量（面积尺度） ----------------
  dens <- suppressWarnings(as.numeric(individual$densindiv))
  if (!is.finite(dens) || dens <= 0) dens <- 1e-6
  
  leaf0   <- as.numeric(individual$cmass_leaf)
  root0   <- as.numeric(individual$cmass_root)
  sap0    <- as.numeric(individual$cmass_sap)
  heart0  <- as.numeric(individual$cmass_heart)
  debt0   <- as.numeric(individual$cmass_debt)
  height  <- suppressWarnings(as.numeric(individual$height))
  ltor0   <- suppressWarnings(as.numeric(individual$ltor))
  
  if (!is.finite(height) || height <= 0) height <- 0.1
  if (!is.finite(ltor0)  || ltor0  <= 0) ltor0  <- 1.0
  
  agb0    <- sap0 + heart0 - debt0               # AbvGrndWood 定义一致 :contentReference[oaicite:3]{index=3}
  totalC0 <- TotalCarbon(individual)             # 旧实现里也用这个来缩放 N :contentReference[oaicite:4]{index=4}
  if (!is.finite(agb0))    agb0    <- 0
  if (!is.finite(totalC0)) totalC0 <- leaf0 + root0 + sap0 + heart0
  
  ## ---------------- 把 biomass.inc 当成 ΔAGB ----------------
  agb_target <- agb0 + biomass.inc
  agb_target <- max(agb_target, 0)               # 不允许负 AGB
  
  if (trace) {
    cat(" ------- STRUCTURAL BIOMASS ADJUSTMENT -------\n")
    cat(" ***** AGB_old =", agb0,
        " dAGB =", biomass.inc,
        " AGB_target =", agb_target, "\n")
  }
  
  ## live wood 总量：sap + heart，debt 保持不变
  Wlive0    <- max(sap0 + heart0, 0)
  Wlive_new <- agb_target + debt0               # 因为 AGB = sap+heart - debt
  
  if (Wlive_new < 0) Wlive_new <- 0
  
  if (Wlive0 > 0 && Wlive_new > 0) {
    frac_sap   <- sap0   / Wlive0
    frac_heart <- heart0 / Wlive0
  } else {
    ## 没有历史信息时，全部放在 sapwood
    frac_sap   <- 1.0
    frac_heart <- 0.0
  }
  
  sap_new   <- Wlive_new * frac_sap
  heart_new <- Wlive_new * frac_heart
  
  ## ---------------- 叶 / 根：维持合理 leaf:wood 比 ----------------
  W_for_ratio   <- max(Wlive_new, 1e-12)
  ratio_lw_now  <- if (Wlive0 > 0) leaf0 / Wlive0 else 0.05   # 当前或一个小缺省
  ratio_lw_now  <- max(ratio_lw_now, 0)
  
  ## 几何上要求的最小叶量（来自 allometry / allocation 推导）
  leaf_geom_min <- 0
  if (lifeform == 1 &&
      is.finite(sla)      && sla      > 0 &&
      is.finite(wooddens) && wooddens > 0) {
    
    leaf_geom_min <- (k_latosa * sap_new) /
      (wooddens * height * sla)
  }
  leaf_geom_min <- max(leaf_geom_min, 0)
  
  ratio_geom   <- if (W_for_ratio > 0) leaf_geom_min / W_for_ratio else 0
  ## 目标 leaf:wood 比：不低于当前，也不低于几何下限
  ratio_target <- max(ratio_lw_now, ratio_geom)
  
  leaf_from_ratio <- ratio_target * W_for_ratio
  
  ## 让叶量随木质“温和”同比增长一遍
  leaf_scaled <- if (Wlive0 > 0) leaf0 * (Wlive_new / Wlive0) else leaf0
  
  leaf_new <- max(leaf_scaled, leaf_geom_min, leaf_from_ratio)
  
  ## 根：保持 ltor
  root_new <- max(leaf_new / ltor0, 0)
  
  ## ---------------- 叶 / 根的 litter 增量 ----------------
  litter_leaf_inc <- max(0, leaf0 - leaf_new) / dens
  litter_root_inc <- max(0, root0 - root_new) / dens
  
  ## ---------------- 把新池写回 individual ----------------
  individual$cmass_leaf  <- leaf_new
  individual$cmass_root  <- root_new
  individual$cmass_sap   <- sap_new
  individual$cmass_heart <- heart_new
  individual$cmass_debt  <- debt0   # 不动 debt
  
  ## ---------------- N 池：尽量维持原有 C:N ----------------
  rescale_CN <- function(c_new, c_old, n_old) {
    if (is.null(n_old)) return(n_old)
    n_old <- as.numeric(n_old)
    if (!is.finite(n_old) || n_old <= 0) return(n_old)
    if (!is.finite(c_old) || c_old <= 0) return(n_old)
    cn <- c_old / n_old
    if (!is.finite(cn) || cn <= 0) return(n_old)
    c_new / cn
  }
  
  individual$nmass_leaf <- rescale_CN(
    leaf_new, leaf0, individual$nmass_leaf)
  
  individual$nmass_root <- rescale_CN(
    root_new, root0, individual$nmass_root)
  
  if (lifeform == 1) {
    individual$nmass_sap <- rescale_CN(
      sap_new, sap0, individual$nmass_sap)
    individual$nmass_heart <- rescale_CN(
      heart_new, heart0, individual$nmass_heart)
  }
  
  ## N 的储存池按总碳比例缩放（沿用原来的思路）
  totalC1 <- TotalCarbon(individual)
  if (!is.finite(totalC1) || totalC1 <= 0) totalC1 <- totalC0
  rel.change <- if (totalC0 > 0) totalC1 / totalC0 else 1
  
  if (!is.null(individual$nstore_labile))
    individual$nstore_labile <- individual$nstore_labile * rel.change
  if (!is.null(individual$nstore_longterm))
    individual$nstore_longterm <- individual$nstore_longterm * rel.change
  
  ## 把被“砍掉”的干物质当成 exceeds_cmass（供上层代码计入木质 litter）
  exceeds_cmass <- (Wlive0 - Wlive_new) / dens
  
  return(list(
    individual      = individual,
    litter_leaf_inc = litter_leaf_inc,
    litter_root_inc = litter_root_inc,
    exceeds_cmass   = exceeds_cmass
  ))

}
