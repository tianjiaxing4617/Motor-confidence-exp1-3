

data {

  int<lower=1> T;                           // number of observations PTB(trial length)
  int<lower=1> t_channel;                   // number of channel trial
  array[t_channel] int trial;               // trial idx of channel trial
  vector[T] y;                     // observations PTB 
  vector[t_channel] f;                      // channel force

}

parameters {
  

  real<lower = 0, upper = 1> a;   // a*x_meta+b*error
  real<lower = 0, upper = 1> b;

  real<lower=0> sigma_s;    // system noise
  real<lower=0> sigma_ob;   // ob noise

}



transformed parameters {
 

  vector[T] x;     //memory

  x[1] = 0;

  for (t in 2:T) {
    x[t] =a*x[t-1]+b*(y[t-1]-x[t-1]);  // update of motor memory
    
  }

}

model {
// initial
  a ~ normal(0.4, sigma_ob);
  b ~ normal(0.8,sigma_ob);
  sigma_s~ normal(5, sigma_ob);
  
  for (c in 1:t_channel){
    target += normal_lpdf(f[c]|x[trial[c]],sigma_s);  // only use the selection at channel trial , fitting the force
  };  


}

