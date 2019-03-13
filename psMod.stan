
data{
 int<lower=1> nc; // number of control subjects
 int<lower=1> nt; // number of treated subjects
 int<lower=0> ncov; // number of covariates

 real Yctl[nc]; // control outcomes
 real Ytrt[nt]; // treatment outcomes

 matrix[nc,ncov] Xctl; // covariate matrix for controls
 matrix[nt,ncov] Xtrt; //covariate matrix for treated

 int<lower=0,upper=1> first[nt]; // 1=firster 0=skipper
}


parameters{
 real alphaTF; // Y-intercept for treatment firsters
 real alphaTS; // Y-intercept for treatment skippers
 real alphaCF; // Y-intercept for control firsters
 real alphaCS; // Y-intercept for control skippers

 real alphaU; // intercept for usage model
 vector[ncov] betaU; // coefficients for covariates in usage model
 vector[ncov] betaY; // coefficients for covariates in outcome model

 // residual standard deviation:
 real<lower=0> sigTF;
 real<lower=0> sigTS;
 real<lower=0> sigCF;
 real<lower=0> sigCS;
}


transformed parameters{
 //declare new parameters:
 real skipperATE; //Avg. Effect for skippers
 real firsterATE; //Avg. Effect for firsters
 real ATEdiff; //Difference btw Avg Effects
 vector[nc] piC; //Pr(Firster) for controls
 vector[nt] piT; //Pr(Firster) for treateds

 //define new parameters:
 skipperATE=alphaTS-alphaCS;
 firsterATE=alphaTF-alphaCF;
 ATEdiff=firsterATE-skipperATE;
 piC=inv_logit(alphaU+Xctl*betaU);
 piT=inv_logit(alphaU+Xtrt*betaU);
}


model{
 // vectors of intercepts and residual SDs for treated
 // students. useful for vectorizing:
 vector[nt] alphaT;
 vector[nt] sigT;

 // The ? functions as if-else A?B:C returns B if A=1 and C otherwise
 for(i in 1:nt){
  alphaT[i]= first[i]?alphaTF:alphaTS;
  sigT[i]= first[i]?sigTF:sigTS;
 }


 skipperATE~normal(0,.5);
 firsterATE~normal(0,.5);


 first~bernoulli(piT);
 Ytrt~normal(alphaT+Xtrt*betaY,sigT);


 for(i in 1:nc)
  target += log_sum_exp(
   log(piC[i]) + normal_lpdf(Yctl[i] | alphaCF+Xctl[i,]*betaY,sigCF),
   log((1-piC[i])) + normal_lpdf(Yctl[i] |alphaCS+Xctl[i,]*betaY,sigCS));
} //closes "model{"


generated quantities{
 int<lower=0,upper=1> first_repC[nc]; //M_T for controls
 real Ytrt_rep[nt]; // outcomes for treateds
 real Yctl_rep[nc]; // outcomes for controls

 for(i in 1:nt){
  if(first[i]==1)
   Ytrt_rep[i]=normal_rng(alphaTF+Xtrt[i,]*betaY,sigTF);
  else
   Ytrt_rep[i]=normal_rng(alphaTS+Xtrt[i,]*betaY,sigTS);
 }
 for(i in 1:nc){
  first_repC[i]=bernoulli_rng(piC[i]);
  if(first_repC[i]==1)
   Yctl_rep[i]=normal_rng(alphaCF+Xctl[i,]*betaY,sigCF);
  else
   Yctl_rep[i]=normal_rng(alphaCS+Xctl[i,]*betaY,sigCS);
 }
}

