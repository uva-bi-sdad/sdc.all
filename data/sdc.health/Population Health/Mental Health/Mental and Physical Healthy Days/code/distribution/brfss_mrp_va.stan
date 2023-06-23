data {
  
  int<lower = 0> N; // number of samples
  int<lower = 0, upper = 1> y[N]; // outcome
  int<lower = 0> J; // Number of ps cells
  int<lower = 0> num_obs_cnty; // number of observed counties in brfss
  int<lower = 0> num_cnty; // true number of counties
  int<lower = 0> num_tract; // number of tracts to estimate
  int<lower = 0> num_age; // number of age cats
  int<lower = 0> num_race; // number of race cats
  real state_prev;  // estimated state level prevalence
  vector<lower = 1, upper = 2>[N] gender;  // gender covariate
  int<lower = 1, upper = num_obs_cnty> cnty[N];  //county random effect
  int<lower = 1, upper = num_age> age[N]; // age random effect
  int<lower = 1, upper = num_race> race[N]; // race random effect
  vector[num_obs_cnty] poverty; // poverty covariate
  vector[num_obs_cnty] labor_force; // labor force covariate
  vector[num_obs_cnty] less_than_HS; // less than HS covariate
  vector[num_tract] poverty_tract; // poverty prediction covariate
  vector[num_tract] labor_force_tract; // labor force prediction covariate
  vector[num_tract] less_than_HS_tract; // less than HS prediction covariate
  
  // population sizes for poststratification
  vector[num_tract * J] N_pop;
  
  // missing county indicators
  vector<lower = 0, upper = 1>[num_cnty] cnty_miss_ind; 
  
  // map from tract to county
  int<lower = 1, upper = num_cnty> tract_cnty_map[num_tract];
  
}

parameters {
  
  vector[5] ybeta;
  real<lower = 0> sigma_ybeta;
  
  // varying intercepts for county
  real<lower = 0> sigma_cnty;
  real<lower = 0> sigma_age;
  real<lower = 0> sigma_race;
  vector<multiplier = sigma_cnty>[num_obs_cnty] a_cnty;
  vector<multiplier = sigma_age>[num_age] a_age;
  vector<multiplier = sigma_race>[num_race] a_race;
  
}

model {
    
  vector[N] y_p = inv_logit(ybeta[1] +
                            ybeta[2] * gender +
                            ybeta[3] * poverty[cnty] +
                            ybeta[4] * labor_force[cnty] +
                            ybeta[5] * less_than_HS[cnty] +
                            a_cnty[cnty] +
                            a_age[age] +
                            a_race[race]);
                            
   y ~ bernoulli(y_p);
   
  // random intercept priors
  a_cnty ~ normal(0, sigma_cnty);
  a_age ~ normal(0, sigma_age);
  a_race ~ normal(0, sigma_race);
  // beta priors
  ybeta[1] ~ normal(state_prev, sigma_ybeta);
  ybeta[2] ~ normal(0, sigma_ybeta);
  ybeta[3] ~ normal(0, sigma_ybeta); 
  ybeta[4] ~ normal(0, sigma_ybeta);
  ybeta[5] ~ normal(0, sigma_ybeta);
  // hyperpriors for random intercepts
  sigma_cnty ~ normal(0, 0.5);
  sigma_age ~ normal(0, 0.5);
  sigma_race ~ normal(0, 0.5);
  sigma_ybeta ~ normal(0, 0.5);

}

generated quantities {
  
  // impute missing county random effects
  real mean_a_cnty = mean(a_cnty);
  vector[num_cnty] a_cnty_full;
  int cnty_ind = 1;
  vector[num_tract] y_tract;
  int ps_ind = 1;
  vector[num_tract * J] y_tract_ps;
  int tract_cnty_ind;
  int tract_ind = 1;
  
  for (i in 1:num_cnty) {
    if (cnty_miss_ind[i] == 1) {
      a_cnty_full[i] = mean_a_cnty;
    } else {
      a_cnty_full[i] = a_cnty[cnty_ind];
      cnty_ind += 1;
    }
  }
  
  // poststratification
  for (tract_id in 1:num_tract) {
    tract_cnty_ind = tract_cnty_map[tract_id];
      for (race_ind in 1:num_race) {
        for (gender_ind in 1:2) {
          for (age_ind in 1:num_age) {
            y_tract_ps[ps_ind] = inv_logit(ybeta[1] +
                                           ybeta[2] * gender_ind + 
                                           ybeta[3] * poverty_tract[tract_id] +
                                           ybeta[4] * labor_force_tract[tract_id] +
                                           ybeta[5] * less_than_HS_tract[tract_id] +
                                           a_cnty_full[tract_cnty_ind] + 
                                           a_age[age_ind] +
                                           a_race[race_ind]);
            ps_ind += 1;
          }
        }
      }
    }
  
  for (j in 1:num_tract) {
    y_tract[j] = sum(N_pop[tract_ind:(tract_ind + J - 1)] .* y_tract_ps[tract_ind:(tract_ind + J - 1)]) / sum(N_pop[tract_ind:(tract_ind + J - 1)]);
    tract_ind += J;
  }
  
}
