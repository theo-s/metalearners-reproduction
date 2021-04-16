# Recreate Simulation SI 3 (complex non-linear).
library(causalToolbox)
library(ggplot2)
library(dplyr)
library(reshape)
library(ggrepel)
library(viridis)

#?simulate_causal_experiment
n_range <- c(5000, 10000, 20000)
results <- data.frame(N = n_range)
results$XRF <- NA
results$XBART <- NA
results$SRF <- NA
results$SBART <- NA
results$TRF <- NA
results$TBART <- NA

for (n in n_range) {
  exp <- simulate_causal_experiment(ntrain = n,
                                    ntest = 1000,
                                    dim = 20,
                                    pscore = "rct5",
                                    mu0 = "complexNonLinear",
                                    tau = "complexNonLinear")


  feature_train <- exp$feat_tr
  w_train <- exp$W_tr
  yobs_train <- exp$Yobs_tr

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
            file = "results/complex_nonlinearEMSE.csv",
            row.names = FALSE)

  # Clean up the environment
  rm(xl_rf, xl_bart, sl_rf, sl_bart, tl_bart, tl_rf)
}

min <- min(results %>%
             melt(id = "N") %>%
             select(value))

max <- max(results %>%
             melt(id = "N") %>%
             select(value))

results %>%
  melt(id = "N") %>%
  filter(substr(variable, 2,5) == "BART") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("XBART" = "X Learner (BART)",
                                                       "SBART" = "S Learner (BART)",
                                                       "TBART" = "T Learner (BART)"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line()+
  geom_point()+
  scale_y_log10(limits = c(min, max))+
  theme_bw()+
  scale_color_viridis_d()+
  labs(x = "Training Size", y = "MSE")

ggsave(filename = "figures/complex_nonlinear_BART.pdf", height = 6, width = 6)

results %>%
  melt(id = "N") %>%
  filter(substr(variable, 2,5) != "BART") %>%
  dplyr::rename(Estimator = variable) %>%
  dplyr::mutate(Estimator = plyr::revalue(Estimator, c("XRF" = "X Learner (RF)",
                                                       "SRF" = "S Learner (RF)",
                                                       "TRF" = "T Learner (RF)"))) %>%
  ggplot(aes(x = N, y = value, color = Estimator))+
  geom_line()+
  geom_point()+
  scale_y_log10(limits = c(min, max))+
  theme_bw()+
  scale_color_viridis_d()+
  labs(x = "Training Size", y = "MSE")

ggsave(filename = "figures/complex_nonlinear_RF.pdf", height = 6, width = 6)



