
data {
  int<lower=0> N;                     // number of observations
  int<lower=1> J; // number of predictors
  int<lower=1> K; // number of predictors

  int<lower=0> N_Natal;
  int<lower=0> N_Rearing;
  int<lower=0> N_Pair;
  int<lower=0> N_Year;
  int<lower=0> N_Ped;
  int<lower=0> N_NoParents;
  int<lower=0> N_ParentsOffspring;
  int<lower=0> N_ParentsNoOffspring;

  vector[N] y;                        // response  
  matrix[N, J] X; // matrix of predictors
  matrix[N_Rearing, K] XV; // matrix of predictors
  vector[N_Ped] MSV;                      // Mendelian sampling variance

  int<lower=0, upper=N_Natal> Natal_id[N];
  int<lower=0, upper=N_Rearing> Rearing_id[N];
  int<lower=0, upper=N_Pair> Pair_id[N];
  int<lower=0, upper=N_Year> Year_id[N];
  int<lower=0, upper=N_Ped> animal_id[N];     // animal id relating y to animal in pedigree (refers to row number of dam and sire)
  int<lower=0, upper=N_Ped> dam[N_Ped];       // dam id in order of animal id
  int<lower=0, upper=N_Ped> sire[N_Ped];      // sire id  
  int<lower=0, upper=N_Ped> NoParents[N_NoParents];
  int<lower=0, upper=N_Ped> ParentsOffspring[N_ParentsOffspring];
  int<lower=0, upper=N_Ped> ParentsNoOffspring[N_ParentsNoOffspring];
}

transformed data{
  // Compute, thin, and then scale QR decomposition
  matrix[N, J] Q = qr_Q(X)[, 1:J] * sqrt(N - 1.0);
  matrix[J, J] R = qr_R(X)[1:J, ] / sqrt(N - 1.0);
  matrix[J, J] R_inv = inverse(R);

  // Compute, thin, and then scale QR decomposition
  matrix[N_Rearing, K] QV = qr_Q(XV)[, 1:K] * sqrt(N_Rearing - 1.0);
  matrix[K, K] RV = qr_R(XV)[1:K, ] / sqrt(N_Rearing - 1.0);
  matrix[K, K] RV_inv = inverse(RV);

  vector[N_Ped] MSsd = sqrt(MSV);
}


parameters {
  vector[J] beta_tilde; // fixed effects

  vector[K] betaV_tilde; // fixed effects
  
  vector [N_Natal] Natal_scaled;
  vector [N_Rearing] Rearing_scaled;
  vector [N_Pair] Pair_scaled;
  vector [N_Year] Year_scaled;
  
  vector [N_Rearing] log_sigma_E_scaled;

  vector [N_NoParents] A_NoParents;  // scaled breeding value
  vector [N_ParentsOffspring] A_ParentsOffspring;
  vector [N_ParentsNoOffspring] A_ParentsNoOffspring;

  real <lower=0> sigma_Natal;
  real <lower=0> sigma_Rearing;
  real <lower=0> sigma_Year;
  real <lower=0> sigma_Pair;
  real <lower=0> sigma_A;

  real <lower=0> sigma_Rearing_V;

}


model {
  vector [N] mu;
  vector [N_Rearing] muV;
  vector [N_Natal] Natal_effects;
  vector [N_Rearing] Rearing_effects;
  vector [N_Pair] Pair_effects;
  vector [N_Year] Year_effects;
  vector [N_Rearing] sigma_E_effects;
  vector [N_Ped] A_scaled;          // scaled breeding value
  vector [N_Ped] A;                    // breeding value

  int PO_i;
  real A_mu;
  vector [N_ParentsNoOffspring] A_ParentsNoOffspring_mean;

  A_scaled[1] = 0;

  A_NoParents ~ normal( 0 , 1 );
  A_scaled[NoParents] = A_NoParents;
  
  for (i in 1:N_ParentsOffspring)
  {
    PO_i = ParentsOffspring[i];
    A_mu = (A_scaled[dam[PO_i]] + A_scaled[sire[PO_i]])*0.5;
    A_ParentsOffspring[i] ~ normal( A_mu, MSsd[PO_i]);
    A_scaled[PO_i] = A_ParentsOffspring[i];
  }

  A_ParentsNoOffspring_mean = (A_scaled[dam[ParentsNoOffspring]] + A_scaled[sire[ParentsNoOffspring]])*0.5;
  A_ParentsNoOffspring ~ normal( A_ParentsNoOffspring_mean , MSsd[ParentsNoOffspring]);
  A_scaled[ParentsNoOffspring] = A_ParentsNoOffspring ;
  
  Natal_scaled ~ normal( 0 , 1 );
  Rearing_scaled ~ normal( 0 , 1 );
  Pair_scaled ~ normal( 0 , 1 );
  Year_scaled ~ normal( 0 , 1 );

  sigma_Rearing ~ cauchy(0,5);
  sigma_Natal ~ cauchy(0,5);
  sigma_Pair ~ cauchy(0,5);
  sigma_Year ~ cauchy(0,5);
  sigma_A ~ cauchy(0,5);
  sigma_Rearing_V ~ cauchy(0,5);

  Natal_effects = Natal_scaled * sigma_Natal;
  Rearing_effects = Rearing_scaled * sigma_Rearing;
  Pair_effects = Pair_scaled * sigma_Pair;
  Year_effects = Year_scaled * sigma_Year;
  A = A_scaled * sigma_A;

  mu = Q * beta_tilde + A[animal_id] + Natal_effects[Natal_id] + Rearing_effects[Rearing_id] + Pair_effects[Pair_id] + Year_effects[Year_id];
  muV = QV * betaV_tilde;
  
  log_sigma_E_scaled ~ normal(0,1);
  sigma_E_effects = exp(muV + sigma_Rearing_V*log_sigma_E_scaled);
  y ~ normal(mu, sigma_E_effects[Rearing_id]);
  
}

generated quantities {
      vector[J] beta = R_inv * beta_tilde; // coefficients on x
      vector[K] betaV = RV_inv * betaV_tilde; // coefficients on x

}
