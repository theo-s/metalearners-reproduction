# Reproduce Simulation SI 5 (piecewise linear).
library(causalToolbox)

#### Get parameters
if (!interactive()){
  suppressPackageStartupMessages(library("argparse"))

  parser <- ArgumentParser()

  parser$add_argument("--n", type = "integer", default = 100000, help = "Maximum N")
  parser$add_argument("--r", type = "integer", default = 20, help = "Number of reps to run")

  args <- parser$parse_args()

  N <- args$n
  r <- args$r
} else {
  N <- 300000
  r <- 20
}
print(paste0("N: ", N, " r: ", r))

for (rep in 1:r) {

  n_range <- c(5000, 10000, 20000, 100000, 300000)
  n_range <- n_range[which(n_range <= N)]
  results <- data.frame(N = n_range)
  results$XRF <- NA
  results$XBART <- NA
  results$SRF <- NA
  results$SBART <- NA
  results$TRF <- NA
  results$TBART <- NA
  filename <- paste0("code/Appendix/results/piecewise_linear",rep,"EMSE.csv")

  for (n in n_range) {
    exp <- simulate_causal_experiment(ntrain = n,
                                      ntest = 1000,
                                      dim = 20,
                                      pscore = "rct5",
                                      mu0 = "fullLocallyLinear",
                                      tau = "fullLocallyLinear",
                                      noEffect = TRUE)


    feature_train <- exp$feat_tr
    w_train <- exp$W_tr
    yobs_train <- exp$Yobs_tr

    #locally_linear_experiment$tau_te

    # Train the X Learner with BART and RF
    print(paste0("Training XRF, N = ", n))
    xl_rf <- X_RF(feat = feature_train, tr = w_train, yobs = yobs_train)
    print(paste0("Training XBART, N = ", n))
    xl_bart <- X_BART(feat = feature_train, tr = w_train, yobs = yobs_train)
    print(paste0("Training SRF, N = ", n))
    sl_rf <- S_RF(feat = feature_train, tr = w_train, yobs = yobs_train)

    print(paste0("Training SBART, N = ", n))
    sl_bart <- S_BART(feat = feature_train, tr = w_train, yobs = yobs_train)
    print(paste0("Training TRF, N = ", n))
    tl_rf <- T_RF(feat = feature_train, tr = w_train, yobs = yobs_train)
    print(paste0("Training TBART, N = ", n))
    tl_bart <- T_BART(feat = feature_train, tr = w_train, yobs = yobs_train)

    # estimate the CATE
    feature_test <- exp$feat_te

    cate_esti_xrf <- EstimateCate(xl_rf, feature_test)
    cate_esti_xbart <- EstimateCate(xl_bart, feature_test)
    cate_esti_srf <- EstimateCate(sl_rf, feature_test)
    cate_esti_sbart <- EstimateCate(sl_bart, feature_test)
    cate_esti_trf <- EstimateCate(tl_rf, feature_test)
    cate_esti_tbart <- EstimateCate(tl_bart, feature_test)

    # evaluate the performance
    cate_true <- exp$tau_te
    results$XRF[which(results$N == n)] <- mean((cate_esti_xrf - cate_true)^2)
    results$XBART[which(results$N == n)] <- mean((cate_esti_xbart - cate_true)^2)
    results$SRF[which(results$N == n)] <- mean((cate_esti_srf - cate_true)^2)
    results$SBART[which(results$N == n)] <- mean((cate_esti_sbart - cate_true)^2)
    results$TRF[which(results$N == n)] <- mean((cate_esti_trf - cate_true)^2)
    results$TBART[which(results$N == n)] <- mean((cate_esti_tbart - cate_true)^2)


    # Save the intermediate results
    write.csv(results,
              file = filename,
              row.names = FALSE)

    # Clean up the environment
    rm(xl_rf, xl_bart, sl_bart, tl_bart, tl_rf)

  }
}
