calculate_pls <- function(df, flux.name, var.options, seed = 1, 
                          n.val = 6, flux.mean, flux.sd){
  # 1. SETUP -------------------------------------------------------------------
  # Rename flux of interest
  colnames(df)[colnames(df)==flux.name] <- "flux"
  
  # Initialize variables
  pruning.df <- data.frame(index = NULL, var = NULL, VIP = NULL, RMSEP = NULL)
  vars.nix   <- NULL
  y.RMSEP    <- NULL
  vars.keep  <- var.options
  model.ids  <- which(df$year == 0)
  
  # 2. VARIABLE PRUNING -------------------------------------------------------
  # Prune by VIP - eliminate variables with lowest VIP one by one
  for (v in 1:length(var.options)) {
    # Generate PLSR model for all remaining variables
    fmla.vars.keep <- paste0("flux ~ ", paste0(vars.keep, collapse="+")) %>%
                      as.formula()
    PLS.model.all  <- plsr(formula = fmla.vars.keep,
                           data = df[model.ids,],
                           method = "oscorespls",
                           validation = "CV",
                           segment.type = "random",
                           segments = 10)
    
    # Pick number of components and generate model limited to ncomp
    ncomp          <- selectNcomp(PLS.model.all, method = "onesigma")
    PLS.model      <- plsr(formula = fmla.vars.keep,
                           data = df[model.ids,],
                           method = "oscorespls",
                           validation = "CV",
                           segment.type = "random",
                           segments = 10,
                           ncomp = ncomp)
    
    # Extract RMSE for the cross-validated model
    this.RMSEP <- RMSEP(PLS.model, 
                        estimate = "CV", 
                        intercept = FALSE)$val %>%
                  drop()
    this.RMSEP <- c(this.RMSEP, rep(NA, (10 - PLS.model$ncomp)))
    y.RMSEP    <- rbind(y.RMSEP, this.RMSEP)
    
    # Extract last row of VIP values
    if (length(vars.keep) > 1 & ncomp > 1) {
      PLS.VIP <- VIP(PLS.model)[nrow(VIP(PLS.model)),]
    } else {
      PLS.VIP <- VIP(PLS.model)[length(vars.keep)]
    }
    worst.VIP <- PLS.VIP[order(PLS.VIP)][1]
    worst.var <- names(worst.VIP)

    # Track VIP of variable removed and RMSE of last model with that variable
    pruning.df[v,"index"] <- v
    pruning.df[v,"var"]   <- worst.var
    pruning.df[v,"VIP"]   <- worst.VIP
    pruning.df[v,"RMSEP"] <- min(this.RMSEP, na.rm = TRUE)

    # Remove vars with lowest VIP from variable lists
    vars.nix   <- c(vars.nix, worst.var)
    vars.keep  <- vars.keep[-which(vars.keep == worst.var)]
  } # end pruning loop
  rownames(y.RMSEP) <- NULL
  
  # Pick winning formula
  pruning.df <- pruning.df %>% filter(.data$VIP >= 0.9)
  best_combo <- pruning.df$index[which.min(pruning.df$RMSEP)]
  vars.keep      <- vars.nix[best_combo:length(vars.nix)]
  fmla.vars.keep <- paste0("flux ~ ", paste0(vars.keep, collapse="+")) %>%
                    as.formula()
  
  # Refigure correct ncomp for this winning formula 
  PLS.model.all  <- plsr(formula = fmla.vars.keep, 
                         data = df[model.ids,],
                         method = "oscorespls",
                         validation = "CV",
                         segment.type = "random",
                         segments = 10)
  ncomp          <- selectNcomp(PLS.model.all, method = "onesigma")
  
  # 3. CALIBRATION, VALIDATION, PREDICTION -------------------------------------
  df          <- df %>%
                 select(c("city.index", "year", vars.keep, "flux"))
  
  # Assign groups
  set.seed(seed)
  df$group            <- "predict"
  df$group[model.ids] <- "cal"
  val.ids             <- sample(model.ids, n.val)
  df$group[val.ids]   <- "val"
  cal.ids             <- which(df$group == "cal")
  
  # Generate PLSR model
  PLS.model <- plsr(formula = fmla.vars.keep,
                    data = df[cal.ids,],
                    ncomp = ncomp,
                    method = "oscorespls",
                    validation = "CV",
                    segment.type = "random",
                    segments = 10)
  # VIP
  if (ncomp > 1) {
    PLS.VIP <- t(as.data.frame(VIP(PLS.model)[ncomp,]))
  } else {
    PLS.VIP <- t(as.data.frame(VIP(PLS.model)))
  }
  rownames(PLS.VIP) <- NULL
  
  # Make predictions
  all_comps   <- predict(PLS.model, ncomps = ncomp, newdata = df) %>% drop()
  if (ncomp > 1) {
    df$estimate <- all_comps[,ncomp]
  } else {
    df$estimate <- all_comps
  }
  
  # Convert to real units
  df <- df %>%
        mutate(parflow = flux*flux.sd + flux.mean,
               plsr = estimate*flux.sd + flux.mean) %>%
        select(city.index, year, group, parflow, plsr)
  
  # Calculate goodness-of-fit
  fit.metrics <- NULL
  for (type in c("cal", "val")) {
    for (metric in c("RMSE", "PBIAS", "R2", "NSE")) {
      obs <- df$parflow[df$group == type]
      sim <- df$plsr[df$group == type]
      fit <- calculate_fit(sim, obs, metric)
      
      this.fit    <- data.frame(group = type, 
                                fit.metric = metric, 
                                fit.value = fit)
      fit.metrics <- rbind(fit.metrics, this.fit)
    }
  }
  
  return(list(PLS.model = PLS.model,
              PLS.VIP = PLS.VIP,
              fit.metrics = fit.metrics,
              results = df))
}