# Recreate Simulation SI 4 (global linear).
library(causalToolbox)
library(npcausal)
library(ggplot2)
library(dplyr)
library(reshape)
library(ggrepel)
library(Rforestry)

#?simulate_causal_experiment
results <- data.frame(N = c(1000, 2000, 5000, 10000, 20000))
results$XRF <- NA
results$XBART <- NA
results$SRF <- NA
results$SBART <- NA
results$TRF <- NA
results$TBART <- NA

forestry_params <- list(
  relevant.Variable = 1:20,
  ntree = 500,
  replace = TRUE,
  sample.fraction = 0.9,
  mtry = round(10),
  nodesizeSpl = 5,
  nodesizeAvg = 5,
  splitratio = 1,
  middleSplit = TRUE
)

for (n in c(1000, 2000, 5000, 10000, 20000)) {
  locally_linear_experiment <- simulate_causal_experiment(ntrain = n,
                                                          ntest = 1000,
                                                          dim = 5,
                                                          alpha = 0,
                                                          pscore = "rct5",
                                                          mu0 = "fullLinearStrong",
                                                          tau = "fullLinearStrong",
                                                          noEffect = TRUE)


  feature_train <- locally_linear_experiment$feat_tr
  w_train <- locally_linear_experiment$W_tr
  yobs_train <- locally_linear_experiment$Yobs_tr

  #locally_linear_experiment$tau_te

  # Train the X Learner with BART and RF
  print(paste0("Training XRF, N = ", n))
  xl_rf <- X_RF(feat = feature_train, tr = w_train, yobs = yobs_train,
                mu.forestry = forestry_params,
                tau.forestry = forestry_params,
                e.forestry = forestry_params)
  print(paste0("Training XBART, N = ", n))
  xl_bart <- X_BART(feat = feature_train, tr = w_train, yobs = yobs_train)
  print(paste0("Training SRF, N = ", n))
  sl_rf <- S_RF(feat = feature_train, tr = w_train, yobs = yobs_train,
                mu.forestry = forestry_params, nthread = 0)

  print(paste0("Training SBART, N = ", n))
  sl_bart <- S_BART(feat = feature_train, tr = w_train, yobs = yobs_train)
  print(paste0("Training TRF, N = ", n))
  tl_rf <- T_RF(feat = feature_train, tr = w_train, yobs = yobs_train,
                mu0.forestry = forestry_params,
                mu1.forestry = forestry_params)
  print(paste0("Training TBART, N = ", n))
  tl_bart <- T_BART(feat = feature_train, tr = w_train, yobs = yobs_train)

  # estimate the CATE
  feature_test <- locally_linear_experiment$feat_te

  cate_esti_xrf <- EstimateCate(xl_rf, feature_test)
  cate_esti_xbart <- EstimateCate(xl_bart, feature_test)
  cate_esti_srf <- EstimateCate(sl_rf, feature_test)
  cate_esti_sbart <- EstimateCate(sl_bart, feature_test)
  cate_esti_trf <- EstimateCate(tl_rf, feature_test)
  cate_esti_tbart <- EstimateCate(tl_bart, feature_test)

  # evaluate the performance
  cate_true <- locally_linear_experiment$tau_te
  results$XRF[which(results$N == n)] <- mean((cate_esti_xrf - cate_true)^2)
  results$XBART[which(results$N == n)] <- mean((cate_esti_xbart - cate_true)^2)
  results$SRF[which(results$N == n)] <- mean((cate_esti_srf - cate_true)^2)
  results$SBART[which(results$N == n)] <- mean((cate_esti_sbart - cate_true)^2)
  results$TRF[which(results$N == n)] <- mean((cate_esti_trf - cate_true)^2)
  results$TBART[which(results$N == n)] <- mean((cate_esti_tbart - cate_true)^2)


  # Save the intermediate results
  write.csv(results,
            file = "results/piecewise_linearEMSE.csv",
            row.names = FALSE)

  # Clean up the environment
  rm(xl_rf, xl_bart, sl_bart, tl_bart, tl_rf)

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
  scale_y_log10(limits = c(1, max))+
  theme_bw()+
  scale_color_viridis_d()+
  labs(x = "Training Size", y = "MSE")

ggsave(filename = "figures/piecewise_linear_BART.pdf", height = 6, width = 6)

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
  scale_y_log10(limits = c(1, max))+
  theme_bw()+
  scale_color_viridis_d()+
  labs(x = "Training Size", y = "MSE")

ggsave(filename = "figures/piecewise_linear_RF.pdf", height = 6, width = 6)


