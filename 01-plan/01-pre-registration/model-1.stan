
functions {
  // Michaelis-Menten growth rate
  real growth_rate(real Nitrogen, real r_max, real K_m) {
    return r_max * Nitrogen / (K_m + Nitrogen);
  }
  
  // Michaelis-Menten pathogen effect
  real pathogen_effect(real Nitrogen, real b_max, real K_mb) {
    return b_max * Nitrogen / (K_mb + Nitrogen);
  }
  
  // Lotka-Volterra competition model with nitrogen and pathogen effects
  vector competition_model(real t,
                           vector state,
                           real Nitrogen,
                           real Pathogen,
                           real r_SN_max,
                           real r_SI_max,
                           real K_m_SN,
                           real K_m_SI,
                           real b_SN_max,
                           real b_SI_max,
                           real K_mb_SN,
                           real K_mb_SI,
                           real alpha_NI,
                           real alpha_IN,
                           real K_SN,
                           real K_SI) {
    // Unpack state variables
    real SN = state[1];  // Native species biomass
    real SI = state[2];  // Invasive species biomass
    
    // Calculate pathogen effect
    real beta_SN = pathogen_effect(Nitrogen, b_SN_max, K_mb_SN);
    real beta_SI = pathogen_effect(Nitrogen, b_SI_max, K_mb_SI);
    
    // Calculate nitrogen-dependent growth rates
    real r_SN_effective = growth_rate(Nitrogen, r_SN_max, K_m_SN) * (1 - beta_SN * Pathogen);
    real r_SI_effective = growth_rate(Nitrogen, r_SI_max, K_m_SI) * (1 - beta_SI * Pathogen);
    
    // Define the ODEs
    real dSN = r_SN_effective * SN * (1 - (SN + alpha_NI * SI) / K_SN);
    real dSI = r_SI_effective * SI * (1 - (SI + alpha_IN * SN) / K_SI);
    
    // Return the derivatives as a vector
    return [dSN, dSI]';
  }
}

data {
  int<lower=1> N;              // Number of data points
  real t0;                     // Initial time
  real t[N];                   // Observation times
  real Nitrogen;               // Nitrogen level (assumed constant for now)
  real Pathogen;               // Pathogen presence (0 or 1)
  real SN_0;                   // Initial biomass of native species
  real SI_0;                   // Initial biomass of invasive species
  real SN_obs[N];              // Observed native species biomass
  real SI_obs[N];              // Observed invasive species biomass
}

parameters {
  // Growth and competition parameters
  real<lower=0> r_SN_max;
  real<lower=0> r_SI_max;
  real<lower=0> K_m_SN;
  real<lower=0> K_m_SI;
  real<lower=0> b_SN_max;
  real<lower=0> b_SI_max;
  real<lower=0> K_mb_SN;
  real<lower=0> K_mb_SI;
  real<lower=0> alpha_NI;
  real<lower=0> alpha_IN;
  real<lower=0> K_SN;
  real<lower=0> K_SI;

  // Measurement error
  real<lower=0> sigma_SN;
  real<lower=0> sigma_SI;
}

model {
  // Initial state
  vector[2] y0 = [SN_0, SI_0]';
  
  // Solve the ODE system
  vector[2] y[N] = ode_rk45(
    competition_model,
    y0,
    t0,
    t,
    Nitrogen,
    Pathogen,
    r_SN_max,
    r_SI_max,
    K_m_SN,
    K_m_SI,
    b_SN_max,
    b_SI_max,
    K_mb_SN,
    K_mb_SI,
    alpha_NI,
    alpha_IN,
    K_SN,
    K_SI
  );
  
  // Priors
  r_SN_max ~ normal(1, 0.5);
  r_SI_max ~ normal(1, 0.5);
  K_m_SN ~ normal(1, 0.5);
  K_m_SI ~ normal(1, 0.5);
  b_SN_max ~ normal(0.5, 0.2);
  b_SI_max ~ normal(0.5, 0.2);
  K_mb_SN ~ normal(1, 0.5);
  K_mb_SI ~ normal(1, 0.5);
  alpha_NI ~ normal(0.1, 0.05);
  alpha_IN ~ normal(0.1, 0.05);
  K_SN ~ normal(10, 2);
  K_SI ~ normal(10, 2);
  sigma_SN ~ normal(0, 1);
  sigma_SI ~ normal(0, 1);
  
  // Likelihood
  for (i in 1:N) {
    SN_obs[i] ~ normal(y[i, 1], sigma_SN);
    SI_obs[i] ~ normal(y[i, 2], sigma_SI);
  }
}
