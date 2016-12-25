data {
	int T;
	int J;
	
	matrix[J, T] k;
	matrix[J, T] y;

	vector[J] y_s;
}
parameters {
	vector<lower=0>[T] sig;
	vector<lower=0, upper=1>[T] del;

	//vector<lower=0>[T] nu[J];
	vector<lower=0>[T] gma[J];

	real<lower=0> tau;
}
transformed parameters {
	vector[T] rho;

	for(i in 1:T)
		rho[i] = (1-sig[i])/sig[i];
}
model {
	log(tau) ~ normal(0,1);

	sig ~ normal(1, 1);
	del ~ normal(0.5, 1);

	for(i in 1:J)
	{
		//nu[i] ~ normal(1, 0.5);
		log(gma[i]) ~ normal(0, 2);
	}

	for(j in 1:J)
		for(t in 1:T)
			y[j,t] ~ normal( log(gma[j][t]) - (1/rho[t])*log(del[t]*pow(k[j,t], -rho[t]) + (1 - del[t])), tau*y_s[j] );
}
