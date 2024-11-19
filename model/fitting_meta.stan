data {

  int<lower=1> t_choice_max;      // total trial number of selection trial
  array[t_choice_max] int trial;        // trial idx of selection trial
  int<lower=1> T;                 // number of datas (length)
  int<lower=1> t_channel;                   // number of channel trial
  array[t_channel] int trial_channel;                     // trial idx of channel trial
  int<lower=1> N;                   // Number of iterations
  int<lower=1> K;    
  vector[T] y;                      // PTB of the data                    

  array[t_choice_max] int select;        // selection data
  array[t_channel] real f;         // force in channel trial (f_pvel*1000/pvel)
 
}

parameters {
 
  real<lower=0,upper=1> pi1;      // subject initial probability of tool A
  vector<lower=0,upper=1>[K] A;     // transition probabilities of subject
  real<lower=0,upper=1> sigma_r;               // varience of ob
  real<lower=0,upper=1> a;              // parameter A of the VKF
  real<lower=0,upper=1> rho;          // parameter rho of the VKF
  real <lower=0> q;                //initial of the system variance  in the VKF
  real <lower=0,upper=0.2> K_motor;

  real <lower=0> sigma_err;
  real <lower=0> d;
  real error_bias;
  real bias;
}


transformed parameters {
  
real u1_ini= 10;   //initial value of the estimate state.
real u2_ini= 0;


  ////////////////////////////////////////////// ///////// 
//             Meta memory parameters part            //
///////////////////////////////////////////////////////   
   

vector[T] En;                       // entropy of the tools

 
////////////////////////////////////////////// ///////// 
//             Meta memory parameters part            //
///////////////////////////////////////////////////////   
    // vector[K] A;
    
    vector[T] u_motor1_prediction;         // estimate of simulation state of the tools
    vector[T] u_motor2_prediction;         
    vector[T] u_meta1_prediction;         // estimate of simulation state of the tools
    vector[T] u_meta2_prediction;         
    vector[T] P_meta1_prediction;         // estimate of simulation selection uncertainty
    vector[T] P_meta2_prediction;         
    vector[T] u_meta1_in;                 // iterations of simulation state of the tools
    vector[T] u_meta2_in;
    vector[T] P_meta1_in;                 // iterations of simulation selection uncertainty
    vector[T] P_meta2_in;         
    vector[T] u_motor1;                    // simulation state of the tools
    vector[T] u_motor2;
    vector[T] u_meta1;                    // simulation state of the tools
    vector[T] u_meta2;
    vector[T] P_meta1;                    // simulation selection uncertainty
    vector[T] P_meta2;
    vector[T] alpha_prediction1;          // estimate of alpha
    vector[T] alpha_prediction2;
    vector[T] beta_prediction1;           // estimate of beta
    vector[T] beta_prediction2;
    vector[T] alpha1_in;                  // iterations estimate of alpha
    vector[T] alpha2_in;
    vector[T] beta1_in;                   // iterations estimate of beta
    vector[T] beta2_in;
    vector[T] alpha1;
    vector[T] alpha2;
    vector[T] beta1; 
    vector[T] beta2;
    vector[T] Epsilon1;                   // epsilon in the VKF
    vector[T] Epsilon2;    
    vector[T] k1;                          // Kalman gain
    vector[T] k2;  

///////////////////////////////////////////////////////  
//////////////////////////////////////////////  
//             initial part                 //
////////////////////////////////////////////// 
//////////////////////////////////////////////  
//                For toolA                 //
////////////////////////////////////////////// 
  u_motor1_prediction[1] = u1_ini;
  u_meta1_prediction[1] = u1_ini;
  P_meta1_prediction[1] = 10;
  u_meta1_in[1] = u1_ini;
  P_meta1_in[1] = 10;
  alpha_prediction1[1] = 1;
  beta_prediction1[1] = 1;
  alpha1_in[1] = 1;
  beta1_in[1] = 1;
  u_motor1[1]=u1_ini;
  u_meta1[1]=u1_ini;
  P_meta1[1]=10;
  alpha1[1]=1;
  beta1[1]=1;
  Epsilon1[1] =1;
  k1[1] =1;
  // A[1]=0.9;
  // A[2]=0.1;
///////////////////////////////////////////////////////  
//////////////////////////////////////////////  
//                For tool B              //
////////////////////////////////////////////// 

  u_motor2_prediction[1] = u2_ini;
  u_meta2_prediction[1] = u2_ini;
  P_meta2_prediction[1] = 10;
  u_meta2_in[1] = u2_ini;
  P_meta2_in[1] = 10;
  alpha_prediction2[1] = 1;
  beta_prediction2[1] = 1;
  alpha2_in[1] = 1;
  beta2_in[1] = 1;
  u_motor2[1]=u2_ini;
  u_meta2[1]=u2_ini;
  P_meta2[1]=10;
  alpha2[1]=1;
  beta2[1]=1;
  Epsilon2[1] =1;
  k2[1] =1;
  vector[T] meta; 
//////////////////////////////////////////////  
/////////////// HMM part /////////////////////
//////////////////////////////////////////////


  array[T] vector[K] logpro;                 // log likelihood of two tools 
  array[T] vector<lower=0,upper=1>[K] pro;  // probability of two context
  vector[t_channel] logf;                        // likelihood for estimate force

  vector[T] motor;      // motor memoroy

  vector[K] mu;              // initial value of two tools 
  
  vector[T] motor_err;
  vector[T] meta_err;
  
// Forward algorithm log p(z_t = j | x_{1:t})

  array[K] real accumulator;         //accumulator value of the loglikelihood
  accumulator[1]=0;
  accumulator[2]=0;

  mu[1]=u1_ini;   
  mu[2]=u2_ini;
  motor[1]=0;
  meta[1]=0;
  pro[1,1]=1;
  pro[1,2]=0;  
    
  logpro[1,1] = log(pi1)+ normal_lpdf(y[1] | mu[1], sigma_r);   // initial value of the log likelihood 
  logpro[1,2] = log(1-pi1)+ normal_lpdf(y[1] | mu[2], sigma_r);  // initial value of the log likelihood 
  logf[1]=0;                                          // initial value of the log likelihood                                 
    

 
// //////////////////////////////////////////////////////////////////
    for (t in 2:T) {
      
      u_motor1[t]=a*(u_motor1[t-1]+pro[t-1,1]*K_motor*(y[t-1]-u_motor1[t-1]))+bias;
      u_motor2[t]=a*(u_motor2[t-1]+pro[t-1,2]*K_motor*(y[t-1]-u_motor2[t-1]))+bias;
      motor[t]= pro[t-1,1]*u_motor1[t]+ pro[t-1,2]*u_motor2[t];//executed motor command
      for (j in 1:K) { // j = current (t)
        for (i in 1:K) { // i = previous (t-1)
                         // Murphy (2012) Eq. 17.48
                         // belief state      + transition prob + local evidence at t
         
          // motor planning based on the motor memory
          
          if(i==1&&j==1){
            accumulator[i] =  0.5*logpro[t-1, i] + 0.5*log(A[1])+normal_lpdf(y[t] |u_motor1[t], sigma_r); // Forward algorithm
          }
          else if(i==1&&j==2){
            accumulator[i] =  0.5*logpro[t-1, i] + 0.5*log(1-A[1])+normal_lpdf(y[t]|u_motor2[t], sigma_r);
          }
          else if(i==2&&j==1){
            accumulator[i] =  0.5*logpro[t-1, i] + 0.5*log(A[2])+normal_lpdf(y[t] |u_motor1[t], sigma_r);
          }
          else if(i==2&&j==2){
            accumulator[i] =  0.5*logpro[t-1, i] + 0.5*log(1-A[2])+normal_lpdf(y[t] |u_motor2[t], sigma_r); 
          }
        
        }
        logpro[t,j] = log_sum_exp(accumulator);
        
      
      }
      
      pro[t] = softmax(logpro[t]);   
      
// toolA for meta learning         
         u_meta1_prediction[t] = a*u_meta1[t-1];
         P_meta1_prediction[t] = a*a*P_meta1[t-1]+q;
         alpha_prediction1[t] = rho*alpha1[t-1];
         beta_prediction1[t] = rho*beta1[t-1];
         alpha1_in[t] = alpha_prediction1[t]+0.5;
         beta1_in[t] = beta_prediction1[t];
         
         for (n in 1:N) {
           Epsilon1[t]=beta1_in[t]/alpha1_in[t];
           k1[t]=P_meta1_prediction[t]/(P_meta1_prediction[t]+Epsilon1[t]);
           u_meta1_in[t]=u_meta1_prediction[t]+pro[t,1]*k1[t]*(y[t]-u_meta1_prediction[t]);
           // u_meta1_in[t]=u_meta1_prediction[t]+pro[t-1,1]*k1[t]*(motor[t]-u_meta1_prediction[t]);
           P_meta1_in[t]=P_meta1_prediction[t]-k1[t]*P_meta1_prediction[t];
           beta1_in[t]=beta_prediction1[t]+0.5*(y[t]-u_meta1_in[t])^2+0.5*P_meta1_in[t];
           // beta1_in[t]=beta_prediction1[t]+0.5*(motor[t]-u_meta1_in[t])^2+0.5*P_meta1_in[t];
         }
         u_meta1[t]=u_meta1_in[t];
         P_meta1[t]=P_meta1_in[t];
         alpha1[t]=alpha1_in[t];
         beta1[t]=beta1_in[t];
// toolB for meta learning           
         u_meta2_prediction[t] = a*u_meta2[t-1];
         P_meta2_prediction[t] = a*a*P_meta2[t-1]+q;
         alpha_prediction2[t] = rho*alpha2[t-1];
         beta_prediction2[t] = rho*beta2[t-1];
         alpha2_in[t] = alpha_prediction2[t]+0.5;
         beta2_in[t] = beta_prediction2[t];
         
         for (n in 1:N) {
           Epsilon2[t]=beta2_in[t]/alpha2_in[t];
           k2[t]=P_meta2_prediction[t]/(P_meta2_prediction[t]+Epsilon2[t]);
           u_meta2_in[t]=u_meta2_prediction[t]+pro[t,2]*k2[t]*(y[t]-u_meta2_prediction[t]);
           // u_meta2_in[t]=u_meta2_prediction[t]+pro[t-1,2]*k2[t]*(motor[t]-u_meta2_prediction[t]);
           P_meta2_in[t]=P_meta2_prediction[t]-k2[t]*P_meta2_prediction[t];
           beta2_in[t]=beta_prediction2[t]+0.5*(y[t]-u_meta2_in[t])^2+0.5*P_meta2_in[t];
           // beta2_in[t]=beta_prediction2[t]+0.5*(motor[t]-u_meta2_in[t])^2+0.5*P_meta2_in[t];
         }
         u_meta2[t]=u_meta2_in[t];
         P_meta2[t]=P_meta2_in[t];
         alpha2[t]=alpha2_in[t];
         beta2[t]=beta2_in[t];    

  
  } 

  
  // pro[T] = softmax(logpro[T-1]);  // calculte the last trial value
  motor[T]= pro[T-1,1]*u_motor1[T]+ pro[T-1,2]*u_motor2[T];
  
  logf[1] = normal_lpdf(f[1]|motor[trial_channel[1]],sigma_r);  //(channel force | motor memory)
  
  for(t in 2:T){
    meta[t]= pro[t-1,1]*u_meta1[t]+ pro[t-1,2]*u_meta2[t];
  }
  
  for(t in 1:T){
    En[t] = log(sqrt(2*pi()*P_meta1_prediction[t]))-log(sqrt(2*pi()*P_meta2_prediction[t]));
  }
  
  for (c in 2:t_channel){
   // logf[c] = logf[c-1]+normal_lpdf(f[c]|motor[trial_channel[c]],sigma_r)+normal_lpdf(f[c]|meta[trial_channel[c]],sigma_r);  // only use the selection at channel trial
   logf[c] = logf[c-1]+normal_lpdf(f[c]|motor[trial_channel[c]],sigma_r);  // only use the selection at channel trial
  }



}

model {
   // Prior distributions
  // lamada1~normal(0,2);
  // lamada2~normal(0,2);
  a~normal(0.99,0.3);
  rho~normal(0.3,1);  
  q~normal(2,3);
  pi1~ normal(0.99,1);
  d~ normal(4,1);
  error_bias~ normal(0,1);
  bias~ normal(0,1);
  A[1]~ lognormal(0.9,0.3);
  A[2]~ lognormal(0.1,0.3);

  // Observation model

  K_motor ~ lognormal(0.1,0.5);
  sigma_r ~ lognormal(0, 1);
  sigma_err ~ lognormal(0, 1);


  //Likelihood calculation
    
  target += log_sum_exp(logpro[T])+ logf[t_channel];

  
}

