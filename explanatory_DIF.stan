data {
int<lower=0> n_people;
int<lower=0> n_items;
int<lower=0> n_observations;
int<lower=0, upper=n_people> respondentid[n_observations];
int<lower=0, upper=n_items> itemid[n_observations];
int<lower=0, upper=1> response[n_observations];
int<lower=0, upper=1> group_long[n_observations];
vector[n_people] group;
vector[n_items] DIFpredict;
}

parameters {
vector<lower=0>[n_items] a;
vector[n_items] b;
vector[n_people] theta;
vector[n_items] D_raw;
real beta0;
real beta1;
real<lower=0> sigma2;
real foc_mean;
//  real D[n_items];
}

transformed parameters {
vector[n_items] mu;
vector[n_items] ss_err;
vector[n_items] ss_reg;
vector[n_people] mu_theta;
real R2;
vector[n_items] D;

mu_theta = foc_mean*group;

for (j in 1:n_items) {
mu[j] = beta0 + beta1*DIFpredict[j];
}

D = mu + sigma2*D_raw;

for (j in 1:n_items) {
ss_err[j] = pow((D[j]-mu[j]),2);
ss_reg[j] = pow((mu[j]-mean(D[])),2);
}

R2 = sum(ss_reg[])/(sum(ss_reg[])+sum(ss_err[]));
}

model {
vector[n_observations] eta;

a ~ lognormal(0, 1);
b ~ normal(0, 1);
theta ~ normal(mu_theta, 1);
D_raw ~ normal(0, 1);
//  D ~ normal(mu, sigma2);
foc_mean ~ normal(0, 4);
beta0 ~ normal(0, 1);
beta1 ~ normal(0, 1);
sigma2 ~ normal(0, 10);

for(i in 1:n_observations){
eta[i] = a[itemid[i]]*(theta[respondentid[i]] - (b[itemid[i]] + D[itemid[i]] * group_long[i]));
}

response ~ bernoulli_logit(eta);
}
