##' @title AbvGrndWood
##'
##' @description Calculates the above-ground woody biomass of an LPJ-GUESS individual.
##'
##' @param individual A nested list representing an LPJ-GUESS individual from a binary state file.
##' @param include.debt Logical; if TRUE, includes carbon mass debt in the calculation.
##'
##' @return A numeric value representing the above-ground woody biomass (kgC/m²).
##' @keywords internal
AbvGrndWood <- function(individual, include.debt = TRUE){
  
  # get total wood
  if(include.debt) total.wood <- individual$cmass_sap + individual$cmass_heart - individual$cmass_debt
  else total.wood <- individual$cmass_sap + individual$cmass_heart
  
  # subtract below ground biomass
  # TODO add better allometry here
  above.ground.wood <- total.wood
  
  return(above.ground.wood)
  
}

##' @title TotalCarbon
##'
##' @description Calculates the total carbon content of an LPJ-GUESS individual.
##'
##' @param individual A nested list representing an LPJ-GUESS individual from a binary state file.
##' @param include.debt Logical; if TRUE, includes carbon mass debt in the calculation.
##'
##' @return A numeric value representing the total carbon content (kgC/m²).
##' @keywords internal
TotalCarbon <- function(individual, include.debt = TRUE){
  
  # get total wood
  if(include.debt) total.carbon <- individual$cmass_sap + individual$cmass_heart + individual$cmass_leaf + individual$cmass_root - individual$cmass_debt
  else total.carbon <- individual$cmass_sap + individual$cmass_heart + individual$cmass_leaf + individual$cmass_root
  
  return(total.carbon)
  
}

##' @title Pick the patch with the largest canopy gap
##'
##' @description Selects a patch index within a stand that has the largest gap
##' (defined as 1 - sum(FPC)). Empty patches are treated as gap = 1 and preferred.
##'
##' @param stand A list representing an LPJ-GUESS stand node from a binary state.
##'
##' @return An integer (1-based) patch index, or NA_integer_ if no patch is available.
##'
##' @details The function reads each patch's individuals' \code{fpc}. If a patch
##' has no individuals, it is assigned a gap of 1. Among finite gaps, the maximum
##' is returned via \code{which.max}.
##'
##' @keywords internal
.pick_patch_for_seeding <- function(stand) {
  np <- stand$npatches
  if (is.null(np) || !is.finite(np) || np <= 0) return(NA_integer_)
  
  gaps <- rep(NA_real_, np)
  for (p in seq_len(np)) {
    patch <- stand$Patch[[p]]
    nind  <- length(patch$Vegetation$Individuals)
    if (nind == 0) { gaps[p] <- 1; next }
    # fpcs <- vapply(
    #   patch$Vegetation$Individuals,
    #   function(ind) { f <- ind$fpc; if (is.null(f) || !is.finite(f)) 0 else f },
    #   FUN.VALUE = 0.0
    # )
    fpcs <- vapply(
      patch$Vegetation$Individuals,
      function(ind) {
        if (isTRUE(ind$alive)) {
          f <- ind$fpc
          if (is.null(f) || !is.finite(f)) 0 else f
        } else 0
      },
      FUN.VALUE = 0.0
    )
    gaps[p] <- max(0, 1 - sum(fpcs, na.rm = TRUE))
  }
  which.max(gaps)
}

##' @title Build a minimal, self-consistent cohort at a given diameter
##'
##' @description Creates a new individual (cohort) for a given PFT using LPJ-GUESS
##' geometric relations (Eq.5/6) at the specified diameter (typically \code{min.diam})
##' and an extremely small density, so that area-based pools are negligible but the
##' cohort is immediately eligible for size-nudge.
##'
##' @param template A list: an existing individual used as a template (fields/slots).
##' @param pft_row A one-row data.frame or a named numeric vector with keys:
##' \code{k_allom1}, \code{k_allom2}, \code{k_allom3}, \code{k_rp},
##' \code{crownarea_max}, \code{wooddens}, \code{sla}, \code{k_latosa}.
##' @param pft_id Integer PFT id to assign to the new individual.
##' @param dens0 Numeric, tiny density (area-scale), default \code{1e-6}.
##' @param ltor_init Numeric, initial leaf:root ratio to back out root mass.
##' @param diam_cm Numeric, stem diameter in cm (use your \code{min.diam}).
##' @param lai_indiv0 Numeric, initial per-individual LAI used to back out leaf mass.
##'
##' @return A list representing the newly seeded individual with key state set
##' (geometry/pools) and derived quantities zeroed to be recomputed by daily/allometry.
##'
##' @details
##' Height is computed as \eqn{h = k\_allom2 * (d\_m)^{k\_allom3}} (Eq.5),
##' crown area as \eqn{\min(k\_allom1 * (d\_m)^{k\_rp}, crownarea\_max}} (Eq.6).
##' Leaf mass per individual is \eqn{(LAI\_{indiv} * crownarea) / SLA}.
##' Sapwood mass per individual follows the LPJ-GUESS proportionality with \code{k_latosa}.
##' Area-scale pools are obtained by multiplying per-individual pools by \code{dens0}.
##'
##' @keywords internal
.seed_cohort_quick <- function(template, pft_row, pft_id,
                               dens0 = 1e-6, ltor_init = 1.0,
                               diam_cm, lai_indiv0 = 0.25) {
  
  stopifnot(is.list(template), is.finite(diam_cm), diam_cm > 0)
  
  # Make sure we can index pft_row with [[ ]] scalars
  getp <- function(nm) {
    if (is.data.frame(pft_row)) return(as.numeric(pft_row[[nm]]))
    as.numeric(pft_row[[nm]])
  }
  
  k_allom1       <- getp("k_allom1")
  k_allom2       <- getp("k_allom2")
  k_allom3       <- getp("k_allom3")
  k_rp           <- getp("k_rp")
  crownarea_max  <- getp("crownarea_max")
  wooddens       <- getp("wooddens")
  sla            <- getp("sla")
  k_latosa       <- getp("k_latosa")
  
  diam_m  <- diam_cm / 100
  height0 <- (diam_m ^ k_allom3) * k_allom2
  crown0  <- min(k_allom1 * (diam_m ^ k_rp), crownarea_max)
  crown0  <- max(crown0, 1e-6)
  
  vol     <- height0 * pi * diam_m * diam_m * 0.25
  hfrac   <- 0.02                         # For heartwood 
  f_sap   <- wooddens * height0 * sla / k_latosa
  lai_min <- (0.9 * wooddens * vol / ((1 + hfrac) * f_sap)) * (sla / crown0)
  
  lai_indiv0 <- max(lai_indiv0, lai_min)  # 允许外部传入，至少不低于 lai_min
  # Per-individual pools
  cmass_leaf_ind <- (lai_indiv0 * crown0) / sla
  cmass_sap_ind  <- (wooddens * height0 * sla / k_latosa) * cmass_leaf_ind
  cmass_heart_ind <- hfrac * cmass_sap_ind
  
  ## 保险起见：再按 allometry 里的公式做一次 LowWoodDensity 检查
  wd_min   <- 0.9 * wooddens
  wood_now <- (cmass_heart_ind + cmass_sap_ind) / vol   # 注意这里是个体尺度, 不含 dens
  
  if (!is.finite(wood_now) || wood_now < wd_min) {
    required_wood <- wd_min * vol
    if (cmass_heart_ind + cmass_sap_ind > 0) {
      scale <- required_wood / (cmass_heart_ind + cmass_sap_ind)
      cmass_heart_ind <- cmass_heart_ind * scale
      cmass_sap_ind   <- cmass_sap_ind   * scale
    } else {
      cmass_heart_ind <- 0
      cmass_sap_ind   <- required_wood
    }
  }
  
  # Area-scale pools (aggregate by tiny density)
  cmass_leaf <- cmass_leaf_ind * dens0
  cmass_sap  <- cmass_sap_ind  * dens0
  cmass_root <- cmass_leaf / ltor_init
  cmass_heart <- cmass_heart_ind * dens0
  
  ind <- template
  
  # Identity & life-cycle
  ind$indiv.pft.id      <- pft_id
  ind$age               <- 1L
  ind$alive             <- TRUE
  ind$last_turnover_day <- -1L
  
  # Geometry & demographic
  ind$densindiv   <- dens0
  ind$height      <- height0
  ind$crownarea   <- crown0
  ind$ltor        <- ltor_init
  
  # Carbon pools (area-scale)
  ind$cmass_leaf  <- cmass_leaf
  ind$cmass_root  <- cmass_root
  ind$cmass_sap   <- cmass_sap
  ind$cmass_heart <- cmass_heart
  ind$cmass_debt  <- 0.0
  # ind$cmass_repro <- 0.0
  
  # Derived to be recomputed by daily/phenology/allometry
  ind$lai        <- 0.0
  ind$lai_indiv  <- lai_indiv0
  ind$fpc        <- 0.0
  ind$deltafpc   <- 0.0
  
  ind$nmass_leaf  <- 0
  ind$nmass_root  <- 0
  ind$nmass_sap   <- 0
  ind$nmass_heart <- 0
  # ind$nmass_debt  <- 0
  ind$nmass_veg   <- 0
  
  if (!is.null(ind$mlai))    ind$mlai[]    <- 0.0
  if (!is.null(ind$greff_5)) ind$greff_5[] <- 0.0
  ind$wstress    <- FALSE
  ind$phen       <- 1.0
  
  ind
}


##' Adjust LPJ-GUESS state
##'
##' @title updateState.LPJGUESS
##'
##' @description Adjust LPJ-GUESS state variables based on input parameters.
##'
##'
##' @param model.state A large multiply-nested list containing the entire LPJ-GUESS state as read by 
##' function \code{readStateBinary.LPJGUESS} 
##' @param pft.params A data.frame containing the parameters for each PFT.  Each row represents one PFT (ordering must be consistent with the vectors below. 
##' The names of the columns describe the per-PFT parameter and must include: 
##' wooddens, crownarea_max, lifeform (1 = tree, 2 = grass), k_latosa, k_rp, k_allom1,  k_allom2, k_allom3, crownarea_max and sla. 
##' wooddens, crownarea_max, lifeform (1 = tree, 2 = grass), k_latosa, k_rp, k_allom1,  k_allom2, k_allom3, crownarea_max and sla. 
##' @param dens.initial A numeric vector of the initial stand-level stem densities (indiv/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
##' using state data assimilation from function XXXXXX.  
##' @param dens.target A numeric vector of the target stand-level stem densities (indiv/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
##' using state data assimilation from function XXXXXX 
##' @param AbvGrndWood.initial A numeric vector of the target stand-level above ground wood (kgC/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
##' using state data assimilation from function XXXXXX 
##' @param AbvGrndWood.target A numeric vector of the target stand-level above ground wood (kgC/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
##' using state data assimilation from function XXXXXX 
##' @param AbvGrndWood.epsilon A single numeric specifying how close the final above ground wood needs to be to the target
##' above ground stem biomass for each individual.  eg. 0.05 requires that the final above ground wood is within 5%
##' of the target above ground wood
##' @param trace Logical; if TRUE, prints detailed adjustment process information.
##' @param min.diam Minimum tree diameter (in cm) for inclusion in adjustments.
##' @param HEIGHT_MAX Maximum allowed height of an individual.  This is the maximum height that a tree
##' can have.  This is hard-coded in LPJ-GUESS to 150 m, but for SDA that might be unrealistically big, 
##' so this argument allows adjustment. 
##' @return  And updated model state (as a big old list o' lists)
##' @export update_state_LPJGUESS 
##' @author Matthew Forrest, Yinghao Sun
update_state_LPJGUESS <- function(model.state, pft.params, dens.initial, dens.target, AbvGrndWood.initial, AbvGrndWood.target, AbvGrndWood.epsilon, trace, min.diam, HEIGHT_MAX = 150) {
  
  # 找 NATURAL 且 frac>0 的 stand（与你后文保持一致）
  nstands <- length(model.state$Stand)
  nat_idx_pre <- integer(0)
  for (s in seq_len(nstands)) {
    st <- model.state$Stand[[s]]
    lc <- st$landcovertype
    fr <- if (!is.null(st$frac)) as.numeric(st$frac) else 0
    if (!is.null(lc) && as.integer(lc) == 4L && fr > 0) nat_idx_pre <- c(nat_idx_pre, s)
  }
  
  # ---- GRIDCELL-LEVEL SEEDING (once per PFT if globally absent) ----
  eps <- .Machine$double.eps
  
  # 先按当前state计算“格子×PFT”的 AGB 初值（不改变state）
  AbvGrndWood.initial.gc <- calculateGridcellVariablePerPFT(
    model.state, "AbvGrndWood", min.diam = min.diam, pft.params = pft.params
  )
  
  # 哪些PFT“全格缺席且观测>0”？(1-based索引)
  need_seed_idx <- which(AbvGrndWood.initial.gc <= eps & AbvGrndWood.target > 0)
  
  if (length(need_seed_idx)) {
    
    # 收集所有 NATURAL 且 frac>0 的stand中，含“死亡个体”的patch作为候选（并计算gap，优先复活gap大的）
    cand <- list()
    for (s in seq_along(model.state$Stand)) {
      st <- model.state$Stand[[s]]
      lc <- st$landcovertype
      fr <- if (!is.null(st$frac)) as.numeric(st$frac) else 0
      if (is.null(lc) || as.integer(lc) != 4L || fr <= 0) next  # 只看 NATURAL
      np <- st$npatches; if (!is.finite(np) || np <= 0) next
      
      for (p in seq_len(np)) {
        pa <- st$Patch[[p]]
        inds <- pa$Vegetation$Individuals
        if (length(inds) == 0) next
        
        dead_idx <- which(!vapply(inds, function(x) isTRUE(x$alive), logical(1)))
        if (length(dead_idx) == 0) next
        
        # 该patch的“活体FPC之和”与gap
        fpc_alive <- vapply(inds, function(ind) if (isTRUE(ind$alive)) as.numeric(ind$fpc) else 0.0, 0.0)
        gap <- max(0, 1 - sum(fpc_alive, na.rm = TRUE))
        
        cand[[length(cand) + 1L]] <- list(stand = s, patch = p, dead_idx = dead_idx, gap = gap)
      }
    }
    
    # 逐个“需要播种”的PFT执行：只播一次；必须复活一个死槽；没有就报错退出
    for (pft.index in need_seed_idx) {
      pft_id_zero_based <- pft.index - 1L
      
      ### Comment for test on 020426
      # if (length(cand) == 0) {
      #   stop(sprintf(
      #     "update_state_LPJGUESS(): PFT %d is absent in the whole gridcell but no dead individual slot is available to reuse. Aborting to avoid restart offset.",
      #     pft_id_zero_based
      #   ))
      # }
      # 
      # # 选 gap 最大的候选（含至少1个死槽）
      # k <- which.max(vapply(cand, `[[`, 0.0, "gap"))
      # s <- cand[[k]]$stand
      # p <- cand[[k]]$patch
      # i_dead <- cand[[k]]$dead_idx[1]           # 复活该死槽（不改变Individuals长度）
      # 
      # st <- model.state$Stand[[s]]
      # pa <- st$Patch[[p]]
      # template <- pa$Vegetation$Individuals[[i_dead]]
      # 
      # # ltor 兜底
      # ltor0 <- suppressWarnings(as.numeric(template$ltor))
      # if (!is.finite(ltor0) || ltor0 <= 0) ltor0 <- 1.0
      # ltor0 <- min(max(ltor0, 0.2), 5.0)
      # 
      # # 生成“自洽”的新个体（极小密度；最小直径；其它池按PFT参数反解）
      # new.ind <- .seed_cohort_quick(
      #   template   = template,
      #   pft_row    = pft.params[pft.index, ],
      #   pft_id     = pft_id_zero_based,
      #   dens0      = 1e-2,         # 很小但非零，便于本轮密度nudge；不会动pos/siz
      #   ltor_init  = ltor0,
      #   diam_cm    = min.diam+1,
      #   lai_indiv0 = 0.25
      # )
      # 
      # # 关键：覆盖死槽，而非append（不改变 Individuals 的长度与offset）
      # model.state$Stand[[s]]$Patch[[p]]$Vegetation$Individuals[[i_dead]] <- new.ind
      # 
      # # 该候选还剩其它死槽？去掉刚用掉的一个，避免给下一个PFT重复用同一位置
      # rest <- cand[[k]]$dead_idx[-1]
      # if (length(rest)) cand[[k]]$dead_idx <- rest else cand <- cand[-k]
      
      
      # 预先找一个“模板 individual”（任何一个都行，只要结构完整）
      template_any <- NULL
      for (s in seq_along(model.state[["Stand"]])) {
        for (p in seq_along(model.state[["Stand"]][[s]][["Patch"]])) {
          inds <- model.state[["Stand"]][[s]][["Patch"]][[p]][["Vegetation"]][["Individuals"]]
          if (length(inds) > 0) { template_any <- inds[[1]]; break }
        }
        if (!is.null(template_any)) break
      }
      if (length(cand) == 0) {
        # 没有死槽：选一个 gap 最大的 patch（你可以复用你前面算 gap 的代码，或者简单选第一个 patch）
        # 这里假设你有 all_patch_gap 列表：list(list(stand=s, patch=p, gap=gap), ...)
        # k <- which.max(vapply(all_patch_gap, `[[`, 0.0, "gap"))
        # s <- all_patch_gap[[k]]$stand
        # p <- all_patch_gap[[k]]$patch
        
        inds <- model.state[["Stand"]][[1]][["Patch"]][[1]][["Vegetation"]][["Individuals"]]
        if (is.null(template_any)) stop("No template individual available anywhere; cannot append a new cohort safely.")
        
        # ltor 兜底
        ltor0 <- suppressWarnings(as.numeric(template_any$ltor))
        if (!is.finite(ltor0) || ltor0 <= 0) ltor0 <- 1.0
        ltor0 <- min(max(ltor0, 0.2), 5.0)
        # 新建一个 cohort（你已有的 .seed_cohort_quick 很合适）
        new_ind <- .seed_cohort_quick(template = template_any,
                                      pft_row  = pft.params[pft.index,],
                                      pft_id   = pft_id_zero_based,
                                      dens0    = 1e-2,
                                      ltor_init= ltor0,
                                      diam_cm  = min.diam+1,
                                      lai_indiv0 = 0.25)
        
        inds[[length(inds) + 1L]] <- new_ind
        model.state[["Stand"]][[s]][["Patch"]][[p]][["Vegetation"]][["Individuals"]] <- inds
        model.state[["Stand"]][[s]][["Patch"]][[p]][["Vegetation"]][["number_of_individuals"]] <- as.integer(length(inds))
        
      }else{
        # 选 gap 最大的候选（含至少1个死槽）
        k <- which.max(vapply(cand, `[[`, 0.0, "gap"))
        s <- cand[[k]]$stand
        p <- cand[[k]]$patch
        i_dead <- cand[[k]]$dead_idx[1]           # 复活该死槽（不改变Individuals长度）

        st <- model.state$Stand[[s]]
        pa <- st$Patch[[p]]
        template <- pa$Vegetation$Individuals[[i_dead]]

        # ltor 兜底
        ltor0 <- suppressWarnings(as.numeric(template$ltor))
        if (!is.finite(ltor0) || ltor0 <= 0) ltor0 <- 1.0
        ltor0 <- min(max(ltor0, 0.2), 5.0)

        # 生成“自洽”的新个体（极小密度；最小直径；其它池按PFT参数反解）
        new.ind <- .seed_cohort_quick(
          template   = template,
          pft_row    = pft.params[pft.index, ],
          pft_id     = pft_id_zero_based,
          dens0      = 1e-2,         # 很小但非零，便于本轮密度nudge；不会动pos/siz
          ltor_init  = ltor0,
          diam_cm    = min.diam+1,
          lai_indiv0 = 0.25
        )

        # 关键：覆盖死槽，而非append（不改变 Individuals 的长度与offset）
        model.state$Stand[[s]]$Patch[[p]]$Vegetation$Individuals[[i_dead]] <- new.ind

        # 该候选还剩其它死槽？去掉刚用掉的一个，避免给下一个PFT重复用同一位置
        rest <- cand[[k]]$dead_idx[-1]
        if (length(rest)) cand[[k]]$dead_idx <- rest else cand <- cand[-k]

      }
      
      
    }
    
    # 播种后再算一次格子级初值（用于后续倍率计算）
    AbvGrndWood.initial.gc <- calculateGridcellVariablePerPFT(
      model.state, "AbvGrndWood", min.diam = min.diam, pft.params = pft.params
    )
  }
  # ---- end GRIDCELL-LEVEL SEEDING ----
  
  
  AbvGrndWood.initial <- calculateGridcellVariablePerPFT(model.state, "AbvGrndWood", min.diam=min.diam, pft.params=pft.params)
  # calculate relative increases to be applied later on (per PFT)
  dens.initial <- calculateGridcellVariablePerPFT(model.state, "densindiv", min.diam=min.diam, pft.params=pft.params)
  dens.target <- dens.initial
  dens.rel.change <- dens.target/dens.initial
  AbvGrndWood.rel.change <- AbvGrndWood.target/AbvGrndWood.initial
  

  # --- NEW: Perform density-individual splitting according to the AGB objective ---
  USE_ALPHA_SPLIT <- TRUE
  if (USE_ALPHA_SPLIT) {
    # 数值工具
    clip <- function(x, lo, hi) pmax(lo, pmin(hi, x))
    # PFT 级别的总比例 R = AGB_tar / AGB_now
    # R <- AbvGrndWood.target / AbvGrndWood.initial
    R_raw <- AbvGrndWood.target / pmax(AbvGrndWood.initial, .Machine$double.eps)
    # ## 初始=0 且 目标>0（引种/新生）时：给一个有限的大R，避免 Inf
    # need_seed <- (AbvGrndWood.initial <= .Machine$double.eps) & (AbvGrndWood.target > 0)
    ## R_eff：把 NaN/Inf 清成 1；对 need_seed 置一个温和的大值（会被步长限幅分步执行）
    R_eff <- R_raw
    R_eff[!is.finite(R_eff)] <- 1
    # R_eff[need_seed] <- 10

    # 自适应 α：保证“每株相对变化”落在 [0.75, 1.30]
    # 说明：当 R≈1 时，令 α=0，避免 0/0
    alpha <- rep(0, length(R_eff))
    idx <- is.finite(R_eff) & (abs(R_eff - 1) > 1e-6)
    alpha[idx] <- 1 - log(clip(R_eff[idx], 0.75, 1.30)) / log(R_eff[idx])
    alpha <- clip(alpha, 0, 1)
    
    # ---- 基于 FPC 的密度软上限（替换原先 n_max=1/crownarea_max 的逻辑） ----
    # 现有 PFT 级 FPC（网格加权求和），与 calculateGridcellVariablePerPFT 用同一汇总口径
    fpc_now <- calculateGridcellVariablePerPFT(model.state, "fpc", pft.params, min.diam = 0.5)
    FPC_CAP <- 0.98  # 可调：0.95~0.99 之间更稳
    
    n_now        <- dens.initial
    n_floor_frac <- 0.2   # 密度地板比例，可调
    n_floor      <- pmax(1e-4, n_floor_frac * pmax(n_now, 1e-6))
    
    # 注意：对 n_now≈0 的 PFT，用 n_floor 参与乘法，避免 0 * Inf -> NaN
    n_now_eff <- pmax(n_now, n_floor)
    
    # 先得到“未约束”的密度目标
    n_tar_raw <- n_now_eff * (R_eff ^ alpha)           # R_eff/alpha 来自你上半段
    lambda_pft <- pmax(n_tar_raw / pmax(n_now, 1e-12), 1e-6)
    
    # 用 FPC 软上限收缩密度倍率 (线性近似：FPC_new ≈ lambda * FPC_now)
    shrink <- rep(1, length(lambda_pft))
    idx <- is.finite(fpc_now) & (fpc_now > 0)
    shrink[idx] <- pmin(1, FPC_CAP / (lambda_pft[idx] * fpc_now[idx]))
    lambda_pft <- lambda_pft * shrink
    
    # 最终密度目标与相对变化
    n_tar <- pmax(lambda_pft * n_now, n_floor)
    dens.rel.change        <- pmax(n_tar / pmax(n_now, n_floor), 1e-6)
    AbvGrndWood.rel.change <- pmax(R_eff ^ (1 - alpha), 1e-6)
    
    

    
    # （可选）把 dens.target 更新成 n_tar，便于记录
    # dens.target[] <- n_tar
  }
  
  
  
  # nstands - should always be 1 but lets make sure
  nstands <- unlist(model.state$nstands)
  # if(nstands != 1) warning("More than one Stand found in LPJ-GUESS state.  This possibly implies that land use has been enabled
  #                          which the PEcAn code might not be robust against.")
  
  if (length(nstands) == 0) nstands <- length(model.state$Stand)
  if (nstands < 1) stop("No Stand found.")
  
  nat_idx  <- integer(0)
  for (s in 1:nstands) {
    st <- model.state$Stand[[s]]
    lc <- st$landcovertype
    fr <- if (!is.null(st$frac)) as.numeric(st$frac) else 0
    if (!is.null(lc) && as.integer(lc) == 4L && fr > 0) {   # 4 == NATURAL
      nat_idx <- c(nat_idx, s)
    }
  }
  if (!length(nat_idx)) {
    if (trace) message("No NATURAL stands with positive frac; nothing to do.")
    return(model.state)
  }
  
  #
  for(stand.counter in nat_idx) {
    
    # get the number of patches
    npatches <- model.state$Stand[[stand.counter]]$npatches
    if(npatches == 0) next
    
    # get list of all the PFTs included in this stand
    active.PFTs <- c()
    for(stand.pft.id in 1:length(model.state$Stand[[stand.counter]]$Standpft$active)) {
      if(model.state$Stand[[stand.counter]]$Standpft$active[[stand.pft.id]]) active.PFTs <- append(active.PFTs, stand.pft.id -1)
    }
    
    # loop through each patch
    for(patch.counter in 1:npatches) {
      
      this.patch <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]
      if(length(this.patch$Vegetation$Individuals) == 0) next
      
      if(trace) {
        print("--------------------------------------------------------------------------------------------------")
        print(paste("-------------------------- STARTING PATCH", patch.counter, "------------------------------------------------------"))
        print(paste("-------------------------- NUMBER OF INDIVIDUALS =", length(this.patch$Vegetation$Individuals), "-------------------------------------------"))
        print("--------------------------------------------------------------------------------------------------")
      }
      
      
      # for each individual
      for(individual.counter in 1:length(this.patch$Vegetation$Individuals)) {
        
        
        # IMPORTANT: note that this is for convenience to *read* variables from the original individual 
        # but it should not be written to.  Instead the 'updated.individual' (defined in the loop below)
        # should be updated and then used to update the main state (model.state)
        original.individual <- this.patch$Vegetation$Individuals[[individual.counter]]
        
        # get the PFT id and check that it is active
        this.pft.id <- original.individual$indiv.pft.id
        pft.index <- this.pft.id + 1
        if(!this.pft.id %in% active.PFTs) stop(paste0("Found individual of PFT id = ",this.pft.id, 
                                                      " but this doesn't seem to be active in the LPJ-GUESS run"))
        
        # calculate its diameter to exclude small trees (converted to cm)
        diam = ((original.individual$height / pft.params[pft.index, "k_allom2"]) ^ (1.0 / pft.params[pft.index, "k_allom3"])) * 100
        
        # don't adjust non-alive individuals as they will soon be removed, 
        # also exclude small trees to help keep the adjustments sensible
        if(original.individual$alive & diam >= min.diam) {
          
          
          # initialise the result code to "FIRST" for the first iteration
          result.code <- "FIRST"
          
          # get the initial, target and changes in Dens and AbvGrndWood
          initial.AbvGrndWood <- AbvGrndWood(original.individual)
          initial.Dens <- original.individual$densindiv          
          target.densindiv.rel.change <- dens.rel.change[pft.index]
          target.AbvGrndWood.rel.change <- AbvGrndWood.rel.change[pft.index]
          target.AbvGrndWood <- initial.AbvGrndWood * target.AbvGrndWood.rel.change
          
          if(trace) {
            print(paste(" * Adjusting individual", individual.counter))
            print(paste(" * PFT ID (zero-indexed) =", this.pft.id))
            print(paste(" * Initial AbvGrndWood value =", initial.AbvGrndWood))
            print(paste(" * Target AbvGrndWood value = ", target.AbvGrndWood))
            print(paste(" * Target AbvGrndWood relative change =", target.AbvGrndWood.rel.change))
          }
          
          
          ####### STEP 0 - 'adjust the adjustment' - if the initial biomass adjustment is too crazy then tone it down here
          
          # if the biomass nudge is less that 0.75 the allocation will probably fail so increase the biomass
          # to 0.75 and increase the stem density accordingly
          if(target.AbvGrndWood.rel.change < 0.75) {
            target.overall.rel.change <- target.AbvGrndWood.rel.change * target.densindiv.rel.change
            current.target.AbvGrndWood.rel.change <- 0.75
            current.target.densindiv.rel.change <- target.overall.rel.change / current.target.AbvGrndWood.rel.change 
            derived.overall.rel.change <- current.target.AbvGrndWood.rel.change * current.target.densindiv.rel.change
            
            if(trace) {
              print(paste(" ***** CHECK INITIAL ADJUSTMENTS"))
              print(paste(" ***** Target AbvGrndWood relative change =", target.AbvGrndWood.rel.change))
              print(paste(" ***** Since Target AbvGrndWood relative change < 0.75, also adjust density"))
              print(paste(" ***** Modified target AbvGrndWood relative change =", current.target.AbvGrndWood.rel.change))
              print(paste(" ***** Modified target density relative change =", current.target.densindiv.rel.change))
              print(paste(" ***** Combined AbvGrndWood relative change =", derived.overall.rel.change))
            } # # if initial AbvGrndWood adjustment very low
          } else if(target.AbvGrndWood.rel.change > 100) {
            target.overall.rel.change <- target.AbvGrndWood.rel.change * target.densindiv.rel.change
            current.target.AbvGrndWood.rel.change <- 100
            current.target.densindiv.rel.change <- target.overall.rel.change / current.target.AbvGrndWood.rel.change 
            derived.overall.rel.change <- current.target.AbvGrndWood.rel.change * current.target.densindiv.rel.change
            
            if(trace) {
              print(paste(" ***** CHECK INITIAL ADJUSTMENTS"))
              print(paste(" ***** Target AbvGrndWood relative change =", target.AbvGrndWood.rel.change))
              print(paste(" ***** Since Target AbvGrndWood relative change > 100, also adjust density"))
              print(paste(" ***** Modified target AbvGrndWood relative change =", current.target.AbvGrndWood.rel.change))
              print(paste(" ***** Modified target density relative change =", current.target.densindiv.rel.change))
              print(paste(" ***** Combined AbvGrndWood relative change =", derived.overall.rel.change))
            } # if initial AbvGrndWood adjustment very high
            
          } else {# AbvGrndWood nudge is safe to try without adjusting density
            current.target.AbvGrndWood.rel.change <- target.AbvGrndWood.rel.change
            current.target.densindiv.rel.change <- target.densindiv.rel.change
            
            if(trace) {
              print(paste(" ***** CHECK INITIAL ADJUSTMENTS"))
              print(paste(" ***** Target AbvGrndWood relative change =", target.AbvGrndWood.rel.change))
              print(paste0(" ***** Initial nudge okay (rel change = ", target.AbvGrndWood.rel.change, "), no need to adjust density"))
            } # if trace
            
          } # if initial AbvGrndWood adjustment is not too crazy
          
          
          
          # STEP 1 - if necessary do an initial nudge density of stems by adjusting the "densindiv" 
          # and also scaling the biomass pools appropriately
          
          if(current.target.densindiv.rel.change != 1) {
            
            if(trace) {
              print(paste(" ------- BEFORE INITIAL DENSITY ADJUSTMENT -------"))
              print(paste(" ***** Density =", original.individual$densindiv))
              print(paste(" ***** AbvGrndWood =", AbvGrndWood(original.individual)))
            }
            
            updated.individual <- adjust.density.LPJGUESS(original.individual, current.target.densindiv.rel.change)
            final.densindiv <- updated.individual$densindiv
            
            # --- NEW: recompute allometry after density change & write back ---
            allr <- allometry(
              lifeform = pft.params[pft.index, "lifeform"],
              cmass_leaf  = updated.individual$cmass_leaf,
              cmass_sap   = updated.individual$cmass_sap,
              cmass_heart = updated.individual$cmass_heart,
              densindiv   = updated.individual$densindiv,
              age         = updated.individual$age,
              fpc         = updated.individual$fpc,
              deltafpc    = updated.individual$deltafpc,
              sla         = pft.params[pft.index, "sla"],
              k_latosa    = pft.params[pft.index, "k_latosa"],
              k_rp        = pft.params[pft.index, "k_rp"],
              k_allom1    = pft.params[pft.index, "k_allom1"],
              k_allom2    = pft.params[pft.index, "k_allom2"],
              k_allom3    = pft.params[pft.index, "k_allom3"],
              wooddens    = pft.params[pft.index, "wooddens"],
              crownarea_max = pft.params[pft.index, "crownarea_max"],
              HEIGHT_MAX  = HEIGHT_MAX
            )
            

            updated.individual$lai_indiv <- allr$lai_indiv
            updated.individual$lai       <- allr$lai
            updated.individual$deltafpc  <- allr$deltafpc
            updated.individual$fpc       <- allr$fpc
            updated.individual$boleht    <- allr$boleht
            
            
            
            if(trace) {
              print(paste(" ------- AFTER INITIAL DENSITY ADJUSTMENT -------"))
              print(paste(" ***** Density =", final.densindiv))
              print(paste(" ***** AbvGrndWood =", AbvGrndWood(updated.individual)))
            }
            if(updated.individual$densindiv != original.individual$densindiv * current.target.densindiv.rel.change) {
              stop(" ***** Density adjustment failed, this is suprising and confusing...")
            }
            
          } else {
            if(trace) {
              print(paste(" ------- NO INITIAL DENSITY ADJUSTMENT REQUIRED -------"))
            } # if trace
            updated.individual <- original.individual  # Make sure the variable is initialized
          } # if no density adjustment required
          
          # STEP 1 结束后、进入 STEP 2 之前
          post_density_baseline <- updated.individual
          post_density_agb      <- AbvGrndWood(post_density_baseline)
          
          

          ## ---------- Step 2 : biomass allocation with line-search + density fallback ----------
          
          ## ---------- Step 2 : biomass allocation (transactional) ----------
          MAX_DENSITY_RETRIES <- 3     # 可调：2–5
          MAX_ATTEMPT_STEP    <- 6     # 线搜索放大次数
          ENLARGE_FACTOR      <- 1.8
          FPC_CAP             <- 0.98  # 软上限
          GEOM_CROWN_MIN      <- 1e-6  # 几何“合理性”阈值（避免 crownarea 极小）
          GEOM_HEIGHT_MIN     <- 0.05  # m，避免高度近 0
          GEOM_LAI_IND_MAX    <- 1e6   # 单株 LAI 过大判为不合理
          
          density_retry <- 0

          repeat {
            # 本轮是否因为特定 error 需要“调参后重来”
            restart_round <- FALSE
            
            # 整体目标倍数（来自你注释）
            target.overall.rel.change <- target.AbvGrndWood.rel.change * target.densindiv.rel.change
            
            # —— 累计 Step-2 期间产生的 litter（面积口径，kgC m-2）
            litter_leaf_accum  <- 0.0
            litter_root_accum  <- 0.0
            exceeds_cmass_sum  <- 0.0
            
            ## 以密度步后的基线定义本轮目标
            pre_round_indiv       <- updated.individual        # <--- 事务起点（回滚用）
            pre_round_agb         <- AbvGrndWood(pre_round_indiv)
            target.AbvGrndWood    <- pre_round_agb * current.target.AbvGrndWood.rel.change
            
            prev_gap <- Inf
            result.code <- "NOTCONVERGED"
            
            for (counter in 1:99) {
              
              current_agb <- AbvGrndWood(updated.individual)
              gap <- target.AbvGrndWood - current_agb
              if (abs(gap) / max(abs(target.AbvGrndWood), .Machine$double.eps) <= AbvGrndWood.epsilon) {
                result.code <- "FIRST"; break
                # result.code <- "OK"; break 应该是OK?
              }
              
              ## ---- 1) 目标导向步长（带结构保底与30%上限） ----
              ltor_val <- updated.individual$ltor
              height_v <- updated.individual$height
              leaf_min <- max(
                0,
                pft.params[pft.index,"k_latosa"] * updated.individual$cmass_sap /
                  (pft.params[pft.index,"wooddens"] * height_v * pft.params[pft.index,"sla"]) -
                  updated.individual$cmass_leaf
              )
              root_min <- max(0, leaf_min / max(ltor_val, 1e-6))
              min_step <- leaf_min + root_min
              
              k_gain   <- 0.6
              step0    <- k_gain * gap
              max_step <- 0.3 * (abs(target.AbvGrndWood) + abs(current_agb))
              # base_inc <- sign(step0) * max(min_step, min(max_step, abs(step0)))
              if (gap > 0) {
                base_inc <- sign(step0) * max(min_step, min(max_step, abs(step0)))
              } else {
                base_inc <- sign(step0) * min(max_step, abs(step0))  # 不用 min_step 约束
              }
              
              ## ---- 2) 线搜索：若落入“叶根兜底”，放大步长再试 ----
              attempt <- 1
              this.biomass.inc <- base_inc
              # 为了“回滚”，保留尝试前的拷贝
              indiv_before <- updated.individual
              
              repeat {
                try.list <- adjust.biomass.scaling.LPJGUESS(
                  individual = updated.individual,
                  biomass.inc = this.biomass.inc,
                  sla       = pft.params[pft.index,"sla"],
                  wooddens  = pft.params[pft.index,"wooddens"],
                  lifeform  = pft.params[pft.index,"lifeform"],
                  k_latosa  = pft.params[pft.index,"k_latosa"],
                  k_allom2  = pft.params[pft.index,"k_allom2"],
                  k_allom3  = pft.params[pft.index,"k_allom3"]
                )
                
                # 用“差分/密度”估算每株木质增量
                sap_inc_pt  <- (try.list$individual$cmass_sap   - updated.individual$cmass_sap)   / updated.individual$densindiv
                heart_inc_pt<- (try.list$individual$cmass_heart - updated.individual$cmass_heart) / updated.individual$densindiv
                
                # 判定是否掉入 leaf-root-only
                if (gap > 0 && abs(sap_inc_pt) < 1e-12 && abs(heart_inc_pt) < 1e-12 && attempt < MAX_ATTEMPT_STEP) {
                  # 放大步长再试
                  this.biomass.inc <- sign(this.biomass.inc) * min(abs(this.biomass.inc) * ENLARGE_FACTOR, max_step)
                  attempt <- attempt + 1
                } else {
                  # 接受这次尝试（暂存），用于几何校验与“是否前进”判断
                  cand.indiv <- try.list$individual
                  # 计算几何
                  cand.allo <- allometry(
                    lifeform = pft.params[pft.index,"lifeform"],
                    cmass_leaf  = cand.indiv$cmass_leaf,
                    cmass_sap   = cand.indiv$cmass_sap,
                    cmass_heart = cand.indiv$cmass_heart,
                    densindiv   = cand.indiv$densindiv,
                    age         = cand.indiv$age,
                    fpc         = cand.indiv$fpc,
                    deltafpc    = cand.indiv$deltafpc,
                    sla         = pft.params[pft.index,"sla"],
                    k_latosa    = pft.params[pft.index,"k_latosa"],
                    k_rp        = pft.params[pft.index,"k_rp"],
                    k_allom1    = pft.params[pft.index,"k_allom1"],
                    k_allom2    = pft.params[pft.index,"k_allom2"],
                    k_allom3    = pft.params[pft.index,"k_allom3"],
                    wooddens    = pft.params[pft.index,"wooddens"],
                    crownarea_max = pft.params[pft.index,"crownarea_max"],
                    HEIGHT_MAX  = HEIGHT_MAX
                  )
                  geom_ok <- is.finite(cand.allo$height)    && cand.allo$height    > GEOM_HEIGHT_MIN &&
                    is.finite(cand.allo$crownarea) && cand.allo$crownarea > GEOM_CROWN_MIN  &&
                    is.finite(cand.allo$lai_indiv)  && cand.allo$lai_indiv < GEOM_LAI_IND_MAX
                  
                  # “木质朝目标方向前进”？
                  agb_before <- AbvGrndWood(updated.individual)
                  agb_after  <- AbvGrndWood(cand.indiv)
                  wood_forward <- (gap > 0 && agb_after > agb_before) || (gap < 0 && agb_after < agb_before)
                  
                  # ---- NEW: 按 allometry$error.string 处理特定结果码 ----
                  err <- if (!is.null(cand.allo$error.string)) as.character(cand.allo$error.string) else "OK"
                  
                  if (err != "OK") {
                    if (err == "NegligibleLeafMass") {
                      if (gap < 0 && attempt < MAX_ATTEMPT_STEP) {
                        this.biomass.inc <- this.biomass.inc * 0.5   # 或 0.3 更激进
                        attempt <- attempt + 1
                        next
                      } else {
                        restart_round <- TRUE            # 回滚到本轮起点
                        result.code <- "NEED_DENSITY_FALLBACK"
                        break
                      }
                    } else if (err == "LowWoodDensity") {
                      # 软化目标（来自你注释的公式）
                      # current.target.AbvGrndWood.rel.change <- 1.1 * current.target.AbvGrndWood.rel.change
                      # current.target.densindiv.rel.change   <- target.overall.rel.change / current.target.AbvGrndWood.rel.change
                      # # 回到本轮基线重新来过
                      # updated.individual <- pre_round_indiv
                      # restart_round <- TRUE
                      ## 直接认定本轮 size nudge 不可行
                      result.code   <- "LowWoodDensity"
                      restart_round <- FALSE   # 别再重新开始这一轮了
                      break         # 跳出当前 line-search / nudge 循环
                    } else if (err == "MaxHeightExceeded") {
                      # 个体太大：密度↑10%，AGB 目标按 1/1.1 缩小
                      current.target.densindiv.rel.change   <- current.target.densindiv.rel.change * 1.1
                      current.target.AbvGrndWood.rel.change <- current.target.AbvGrndWood.rel.change / 1.1
                      updated.individual <- pre_round_indiv
                      restart_round <- TRUE
                    } else {
                      # 其他未知码：把 result.code 带出，交给 Step-3 统一处理
                      result.code <- err
                    }
                  }
                  
                  # 若刚才改了目标并要求重来：跳出line-search repeat，回到外层 repeat 顶部
                  if (restart_round) break
                  # 若设置了其他 result.code（非 OK），也应跳出内层循环
                  if (exists("result.code") && result.code != "NOTCONVERGED") break
                  
                  
                  
                  if (geom_ok && wood_forward) {
                    # —— 提交：更新个体与 litter，写回几何
                    updated.individual <- cand.indiv
                    updated.individual$height    <- cand.allo$height
                    updated.individual$crownarea <- cand.allo$crownarea
                    updated.individual$lai_indiv <- cand.allo$lai_indiv
                    updated.individual$lai       <- cand.allo$lai
                    updated.individual$deltafpc  <- cand.allo$deltafpc
                    updated.individual$fpc       <- cand.allo$fpc
                    updated.individual$boleht    <- cand.allo$boleht
                    
                    # —— 记录本次调整产生的 litter（adjust.biomass.LPJGUESS 返回的是“每株”增量）
                    litter_leaf_accum <- litter_leaf_accum + (if (is.null(try.list$litter_leaf_inc)) 0 else try.list$litter_leaf_inc) * cand.indiv$densindiv
                    litter_root_accum <- litter_root_accum + (if (is.null(try.list$litter_root_inc)) 0 else try.list$litter_root_inc) * cand.indiv$densindiv
                    exceeds_cmass_sum <- exceeds_cmass_sum + (if (is.null(try.list$exceeds_cmass)) 0 else try.list$exceeds_cmass)
                    
                  } else {
                    # —— 回滚这次尝试
                    updated.individual <- indiv_before
                  }
                  break
                }
              } # end repeat line-search
              
              ## —— 立刻跳出中层 for，交给外层 repeat 的 `next` 去“重来” ——
              if (restart_round) break   
              
              # 进展守卫：若几乎没改进（<10%），走一个 ±10% 兜底小步（仍受 max_step 限制）
              new_gap <- target.AbvGrndWood - AbvGrndWood(updated.individual)
              if (abs(new_gap) >= 0.9 * abs(prev_gap)) {
                bump <- if (gap > 0)  0.1 * current_agb else -0.1 * current_agb
                bump <- sign(bump) * min(abs(bump), max_step)
                
                # 再试一次（同样走事务方式）
                indiv_before <- updated.individual
                try.list <- adjust.biomass.scaling.LPJGUESS(
                  individual = updated.individual,
                  biomass.inc = bump,
                  sla       = pft.params[pft.index,"sla"],
                  wooddens  = pft.params[pft.index,"wooddens"],
                  lifeform  = pft.params[pft.index,"lifeform"],
                  k_latosa  = pft.params[pft.index,"k_latosa"],
                  k_allom2  = pft.params[pft.index,"k_allom2"],
                  k_allom3  = pft.params[pft.index,"k_allom3"]
                )
                cand.indiv <- try.list$individual
                
                ## 用 bump 前的状态做 before，避免 updated.individual 在中间被改导致比较混乱
                agb_before <- AbvGrndWood(indiv_before)
                agb_after  <- AbvGrndWood(cand.indiv)
                
                ## 用 bump 前 gap 的方向判断“是否朝目标前进”
                gap_before <- target.AbvGrndWood - agb_before
                wood_forward <- (gap_before > 0 && agb_after > agb_before) ||
                  (gap_before < 0 && agb_after < agb_before)
                if (wood_forward) {
                  
                  cand.allo <- allometry(
                    lifeform = pft.params[pft.index,"lifeform"],
                    cmass_leaf  = cand.indiv$cmass_leaf,
                    cmass_sap   = cand.indiv$cmass_sap,
                    cmass_heart = cand.indiv$cmass_heart,
                    densindiv   = cand.indiv$densindiv,
                    age         = cand.indiv$age,
                    fpc         = cand.indiv$fpc,
                    deltafpc    = cand.indiv$deltafpc,
                    sla         = pft.params[pft.index,"sla"],
                    k_latosa    = pft.params[pft.index,"k_latosa"],
                    k_rp        = pft.params[pft.index,"k_rp"],
                    k_allom1    = pft.params[pft.index,"k_allom1"],
                    k_allom2    = pft.params[pft.index,"k_allom2"],
                    k_allom3    = pft.params[pft.index,"k_allom3"],
                    wooddens    = pft.params[pft.index,"wooddens"],
                    crownarea_max = pft.params[pft.index,"crownarea_max"],
                    HEIGHT_MAX  = HEIGHT_MAX
                  )
                  
                  err <- if (!is.null(cand.allo$error.string)) as.character(cand.allo$error.string) else "OK"
                  geom_ok <- is.finite(cand.allo$height)    && cand.allo$height    > GEOM_HEIGHT_MIN &&
                    is.finite(cand.allo$crownarea) && cand.allo$crownarea > GEOM_CROWN_MIN  &&
                    is.finite(cand.allo$lai_indiv) && cand.allo$lai_indiv < GEOM_LAI_IND_MAX
                  
                  if (err == "OK" && geom_ok){
                    ## —— 提交：个体 + litter + 写回派生量（lai/fpc 等）——
                    updated.individual <- cand.indiv
                    
                    updated.individual$height    <- cand.allo$height
                    updated.individual$crownarea <- cand.allo$crownarea
                    updated.individual$lai_indiv <- cand.allo$lai_indiv
                    updated.individual$lai       <- cand.allo$lai
                    updated.individual$deltafpc  <- cand.allo$deltafpc
                    updated.individual$fpc       <- cand.allo$fpc
                    updated.individual$boleht    <- cand.allo$boleht
                    
                    litter_leaf_accum <- litter_leaf_accum +
                      (if (is.null(try.list$litter_leaf_inc)) 0 else try.list$litter_leaf_inc) * cand.indiv$densindiv
                    litter_root_accum <- litter_root_accum +
                      (if (is.null(try.list$litter_root_inc)) 0 else try.list$litter_root_inc) * cand.indiv$densindiv
                    exceeds_cmass_sum <- exceeds_cmass_sum +
                      (if (is.null(try.list$exceeds_cmass)) 0 else try.list$exceeds_cmass)
                    
                  }else{
                    ## allometry 不通过：回滚
                    updated.individual <- indiv_before
                  }
                  
                } else {
                  updated.individual <- indiv_before
                }
                new_gap <- target.AbvGrndWood - AbvGrndWood(updated.individual)
              }
              prev_gap <- new_gap
              
              if (abs(new_gap) / max(abs(target.AbvGrndWood), .Machine$double.eps) <= AbvGrndWood.epsilon) {
                result.code <- "FIRST"; break
              }
              if (counter == 99) result.code <- "NOTCONVERGED"
            } # end inner for
            # 若本轮因为 error 需要调参后重来，直接进入下一轮 repeat
            if (restart_round && result.code != "NEED_DENSITY_FALLBACK") next
            
            if (result.code != "NOTCONVERGED" && result.code != "NEED_DENSITY_FALLBACK") break
            # if (result.code != "NOTCONVERGED") break
            
            ## ---- 3) 线搜索仍失败：密度补偿一轮（温和 + FPC 软上限） ----
            if (density_retry >= MAX_DENSITY_RETRIES) break
            
            current_agb <- AbvGrndWood(updated.individual)
            residual_gap <- target.AbvGrndWood - current_agb
            if (residual_gap < 0){
              lambda <- max(0.5, 1 - 0.3 * min(1, abs(residual_gap)/max(pre_round_agb, 1e-12)))
              updated.individual <- adjust.density.LPJGUESS(updated.individual, lambda)
              # 重新算几何并继续外层循环
              allr <- allometry(
                lifeform = pft.params[pft.index,"lifeform"],
                cmass_leaf  = updated.individual$cmass_leaf,
                cmass_sap   = updated.individual$cmass_sap,
                cmass_heart = updated.individual$cmass_heart,
                densindiv   = updated.individual$densindiv,
                age         = updated.individual$age,
                fpc         = updated.individual$fpc,
                deltafpc    = updated.individual$deltafpc,
                sla         = pft.params[pft.index,"sla"],
                k_latosa    = pft.params[pft.index,"k_latosa"],
                k_rp        = pft.params[pft.index,"k_rp"],
                k_allom1    = pft.params[pft.index,"k_allom1"],
                k_allom2    = pft.params[pft.index,"k_allom2"],
                k_allom3    = pft.params[pft.index,"k_allom3"],
                wooddens    = pft.params[pft.index,"wooddens"],
                crownarea_max = pft.params[pft.index,"crownarea_max"],
                HEIGHT_MAX  = HEIGHT_MAX
                )
              # updated.individual$height    <- allr$height      # 若保持 state 几何，可不写回
              # updated.individual$crownarea <- allr$crownarea   # 同上
              updated.individual$lai_indiv <- allr$lai_indiv
              updated.individual$lai       <- allr$lai
              updated.individual$deltafpc  <- allr$deltafpc
              updated.individual$fpc       <- allr$fpc
              updated.individual$boleht    <- allr$boleht
              density_retry <- density_retry + 1 
              next
            }
            
            lambda_extra_raw <- 1 + 0.3 * residual_gap / max(pre_round_agb, 1e-12) # 单轮最多加30%份额
            lambda_extra_raw <- max(1.0, min(lambda_extra_raw, 1.5))
            
            # PFT 级 FPC 软上限收缩
            fpc_now_pft <- calculateGridcellVariablePerPFT(model.state, "fpc", pft.params, min.diam = min.diam)
            shrink <- 1.0
            if (is.finite(fpc_now_pft[pft.index]) && fpc_now_pft[pft.index] > 0) {
              # shrink <- min(1.0, FPC_CAP / (fpc_now_pft[pft.index] * current.target.densindiv.rel.change * lambda_extra_raw))
              shrink <- min(1.0, FPC_CAP / (fpc_now_pft[pft.index] * lambda_extra_raw))
            }
            lambda_extra <- lambda_extra_raw * shrink
            
            # 应用密度补偿，并立刻刷新几何
            updated.individual <- adjust.density.LPJGUESS(updated.individual, lambda_extra)
            allr <- allometry(
              lifeform = pft.params[pft.index,"lifeform"],
              cmass_leaf  = updated.individual$cmass_leaf,
              cmass_sap   = updated.individual$cmass_sap,
              cmass_heart = updated.individual$cmass_heart,
              densindiv   = updated.individual$densindiv,
              age         = updated.individual$age,
              fpc         = updated.individual$fpc,
              deltafpc    = updated.individual$deltafpc,
              sla         = pft.params[pft.index,"sla"],
              k_latosa    = pft.params[pft.index,"k_latosa"],
              k_rp        = pft.params[pft.index,"k_rp"],
              k_allom1    = pft.params[pft.index,"k_allom1"],
              k_allom2    = pft.params[pft.index,"k_allom2"],
              k_allom3    = pft.params[pft.index,"k_allom3"],
              wooddens    = pft.params[pft.index,"wooddens"],
              crownarea_max = pft.params[pft.index,"crownarea_max"],
              HEIGHT_MAX  = HEIGHT_MAX
            )
            # updated.individual$height    <- allr$height
            # updated.individual$crownarea <- allr$crownarea
            updated.individual$lai_indiv <- allr$lai_indiv
            updated.individual$lai       <- allr$lai
            updated.individual$deltafpc  <- allr$deltafpc
            updated.individual$fpc       <- allr$fpc
            updated.individual$boleht    <- allr$boleht
            
            density_retry <- density_retry + 1
          } # end repeat (density retries)
          
          ## ---- 4) 最终兜底：仍 NOTCONVERGED → 回滚并“密度 only”命中目标 ----
          if (result.code == "NOTCONVERGED") {
            # 回到 Step-1 结束时的干净快照
            updated.individual <- post_density_baseline
            agb_now <- post_density_agb

            if (agb_now > 0) {
              lambda_need_raw <- target.AbvGrndWood / agb_now     # 直接把目标交给密度
              # FPC 软上限
              fpc_now_pft <- calculateGridcellVariablePerPFT(model.state, "fpc", pft.params, min.diam = min.diam)
              shrink <- 1.0
              if (is.finite(fpc_now_pft[pft.index]) && fpc_now_pft[pft.index] > 0) {
                shrink <- min(1.0, FPC_CAP / (fpc_now_pft[pft.index] * current.target.densindiv.rel.change * lambda_need_raw))
              }
              lambda_final <- lambda_need_raw * shrink
              
              updated.individual <- adjust.density.LPJGUESS(updated.individual, lambda_final)
              # 刷新几何
              allr <- allometry(
                lifeform = pft.params[pft.index,"lifeform"],
                cmass_leaf  = updated.individual$cmass_leaf,
                cmass_sap   = updated.individual$cmass_sap,
                cmass_heart = updated.individual$cmass_heart,
                densindiv   = updated.individual$densindiv,
                age         = updated.individual$age,
                fpc         = updated.individual$fpc,
                deltafpc    = updated.individual$deltafpc,
                sla         = pft.params[pft.index,"sla"],
                k_latosa    = pft.params[pft.index,"k_latosa"],
                k_rp        = pft.params[pft.index,"k_rp"],
                k_allom1    = pft.params[pft.index,"k_allom1"],
                k_allom2    = pft.params[pft.index,"k_allom2"],
                k_allom3    = pft.params[pft.index,"k_allom3"],
                wooddens    = pft.params[pft.index,"wooddens"],
                crownarea_max = pft.params[pft.index,"crownarea_max"],
                HEIGHT_MAX  = HEIGHT_MAX
              )
              # updated.individual$height    <- allr$height
              # updated.individual$crownarea <- allr$crownarea
              updated.individual$lai_indiv <- allr$lai_indiv
              updated.individual$lai       <- allr$lai
              updated.individual$deltafpc  <- allr$deltafpc
              updated.individual$fpc       <- allr$fpc
              updated.individual$boleht    <- allr$boleht
              
              result.code <- "DENSITY_ONLY"
            } else {
              # AGB_now 为 0 的极端情形：直接标记失败并保持原样
              result.code <- "FAILED_NO_WOOD"
            }
          }
          # ---------- end Step 2 ----------
          
          ## ---------- Step 3 : finalize by result.code ----------
          
          # 方便取 patch 层对象
          pp <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft
          
          # 定义分支
          OK_CODES        <- c("FIRST", "DENSITY_ONLY")
          RESEED_CODES    <- c("LowWoodDensity","MaxHeightExceeded", "FAILED_NO_WOOD", "NOTCONVERGED")
          
          if (result.code %in% OK_CODES) {
            # 1) 正常路径：写回个体
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]] <- updated.individual
            
            # 2) 入池 litter（若 Step-2 有产生）
            if (litter_leaf_accum != 0 || litter_root_accum != 0) {
              # 取现有 C:N；若无则给默认（与你旧注释一致）
              leaf_litter_cton <- tryCatch(pp$litter_leaf[[pft.index]] / pp$nmass_litter_leaf[[pft.index]], error = function(e) NA_real_)
              root_litter_cton <- tryCatch(pp$litter_root[[pft.index]] / pp$nmass_litter_root[[pft.index]], error = function(e) NA_real_)
              if (!is.finite(leaf_litter_cton) || leaf_litter_cton <= 0) leaf_litter_cton <- 30.0
              if (!is.finite(root_litter_cton) || root_litter_cton <= 0) root_litter_cton <- 63.0
              
              pp$litter_leaf[[pft.index]] <- pp$litter_leaf[[pft.index]] + litter_leaf_accum
              pp$litter_root[[pft.index]] <- pp$litter_root[[pft.index]] + litter_root_accum
              pp$nmass_litter_leaf[[pft.index]] <- pp$litter_leaf[[pft.index]] / leaf_litter_cton
              pp$nmass_litter_root[[pft.index]] <- pp$litter_root[[pft.index]] / root_litter_cton
            }
            
            # 3) 写回 patch
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft <- pp
            
            if (trace) {
              message(sprintf("OK [%s]  AGB=%.6f  dens=%.6f", 
                              result.code, AbvGrndWood(updated.individual), updated.individual$densindiv))
            }
            
          } else if (result.code %in% RESEED_CODES) {
            # —— 病态/不可信：回滚到 Step-1 的干净基线，并标记 need_seed
            # （注意：post_density_baseline / post_density_agb 需在 Step-1 结束后就保存）
            updated.individual <- post_density_baseline
            
            # 回滚后写回个体
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]] <- updated.individual
            
            # 不把本轮累计的 litter 入池（因为我们回滚了）
            # 只设置 need_seed = TRUE，提示下一轮补播/重建
            if (!is.null(pp$need_seed)) {
              pp$need_seed[[pft.index]] <- TRUE
              model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft <- pp
            }
            
            if (trace) {
              warning(sprintf("RESEED [%s]  rollback to post-density baseline; marked need_seed for PFT %d",
                              result.code, this.pft.id))
            }
            
          } else {
            # —— 未知码：保守处理——写回个体但不入池 litter，仅告警
            model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]] <- updated.individual
            if (trace) warning(sprintf("UNKNOWN result.code = %s; wrote individual, skipped litter.", result.code))
          }
          
          # （可选）记录 exceeds_cmass 总量
          if (!isTRUE(all.equal(exceeds_cmass_sum, 0))) {
            warning(sprintf("Non-zero exceeds_cmass (%.3g) at stand %d patch %d PFT %d", 
                            exceeds_cmass_sum, stand.counter, patch.counter, this.pft.id))
          }
          ## ---------- end Step 3 ----------
          
        } # if individual is alive
        
      } # for each individual
      
    } # for each patch
  } # for each stand
  return(model.state)
  
}
