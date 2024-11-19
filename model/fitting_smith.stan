

data {

  int<lower=1> T;                           // number of observations PTB(trial length)
  int<lower=1> t_channel;                   // number of channel trial
  array[t_channel] int trial;               // trial idx of channel trial
  vector[T] y;                     // observations PTB 
  vector[t_channel] f;                      // channel force

}

parameters {
  real<lower = 0, upper = 1> as;        // slow factor of a
  real<lower = 0, upper = 1> theta_a;   // for calculate af
  real<lower = 0, upper = 1> bf;        // fast factor of b
  real<lower = 0, upper = 1> theta_b;   // for calculate bs

  real<lower=0> sigma_s;                // system noise
}

transformed parameters {
  real af;  // fast factor of a
  real bs;  // slow factor of b

  vector[T] x;   // motor memory
  vector[T] xf;  // fast memory
  vector[T] xs;  // slow memory

  
  af = as * theta_a;  // af ≤ as
  bs = bf * theta_b;  // bs ≤ bf

  
  x[1] = 0.2;
  xf[1] = 0.2;
  xs[1] = 0;

  for (t in 2:T) {
    xf[t] = af * xf[t - 1] + bf * (y[t - 1] - x[t - 1]);
    xs[t] = as * xs[t - 1] + bs * (y[t - 1] - x[t - 1]);
    x[t] = xf[t] + xs[t];
  }
}

model {
  
  as ~ beta(1, 1);
  bf ~ beta(1, 1);
  theta_a ~ beta(1, 1);
  theta_b ~ beta(1, 1);
  sigma_s ~ normal(1, 1);


  f ~ normal(x[trial], sigma_s);
}
