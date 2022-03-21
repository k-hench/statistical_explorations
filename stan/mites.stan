// Mites model
functions {
real[] dpop_dt( real t,   // time
real[] pop_init,          // initial state {lynx, hares}
real[] theta,             // parameters
real[] x_r, int[] x_i) {  // unused
real L = pop_init[1];
real H = pop_init[2];
real bh = theta[1];
real mh = theta[2];
real ml = theta[3];
real bl = theta[4];
// differential equations
real dH_dt = (bh - mh * L) * H;
real dL_dt = (bl * H - ml) * L;
return { dL_dt , dH_dt };
}
}
data {
int<lower=0> N;
// number of measurement times
int<lower=0> mites[N,2];
// measured populations
real<lower=0> days[N];
// days from start of experiment
}
parameters {
real<lower=0> theta[4];
// { bh, mh, ml, bl }
real<lower=0> pop_init[2];
// initial population state
real<lower=0> sigma[2];
// measurement errors
}
transformed parameters {
real pop[N, 2];
pop[1,1] = pop_init[1];
pop[1,2] = pop_init[2];
pop[2:N,1:2] = integrate_ode_rk45(
dpop_dt, pop_init, 0, days[2:N], theta,
rep_array(0.0, 0), rep_array(0, 0),
1e-5, 1e-3, 5e2);
}
model {
// priors
theta[1] ~ normal( 3*0.5 , 1 );
//bh
theta[2] ~ normal( 0.01*0.5 , 0.1 ); //mh
theta[3] ~ normal( 0.001*0.5 , 0.1 ); //bl
theta[4] ~ normal( 1*0.5 , 1 );
//ml
sigma ~ exponential( 1 );
pop_init[1] ~ normal( mites[1,1] , 50 );
pop_init[2] ~ normal( mites[1,2] , 50 );
// observation model
// connect latent population state to observed pelts
for ( t in 1:N )
for ( k in 1:2 )
mites[t,k] ~ lognormal( log(pop[t,k]) , sigma[k] );
}
generated quantities {
real mites_pred[N,2];
for ( t in 1:N )
for ( k in 1:2 )
mites_pred[t,k] = lognormal_rng( log(pop[t,k]) , sigma[k] );
}