data {
  int N; // number of counties (e.g. number of rows in data)
  
  int cases[N];
  int deaths[N];
  
  int nStates;
  int <lower=1, upper=nStates> state;
}

parameters {
  real intercept;
  real 
}

model {
  row_vector[8] p;
  p = inv_logit(alpha + beta * x);
  alpha ~ normal( 0.0, 100);
  beta ~ normal( 0.0, 100);
  r ~ binomial(n, p);
}