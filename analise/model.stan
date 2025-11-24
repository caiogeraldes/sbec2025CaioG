data{
  array[296] int Prec;
  array[296] int A;
  array[296] int Dist;
  array[296] int Dia;
  array[296] int Auth;
  array[296] int Th;
  array[296] int PoS;
  array[296] int Cop;
  array[296] int ID;
  vector[2] alpha_pos;
  vector[2] alpha_th;
}
parameters{
  real b_c;
  real<lower=0> b_pos;
  simplex[2] delta_pos;
  real<lower=0> b_th;
  simplex[2] delta_th;
  vector[15] z_auth;
  real<lower=0> s_auth;
  real a_bar;
  vector[296] z_sent;
  real<lower=0> s_sent;
  real b_dia_bar;
  vector[2] z_dia;
  real<lower=0> s_dia;
  real b_dist;
}
transformed parameters{
  real d_part;
  real d_adj;
  real d_noun;
  real d_recip;
  real d_exp;
  real d_agent;
  d_agent = b_th * (delta_th[1] + delta_th[2]);
  d_exp = b_th * delta_th[1];
  d_recip = b_th * 0;
  d_noun = b_pos * (delta_pos[1] + delta_pos[2]);
  d_adj = b_pos * delta_pos[1];
  d_part = b_pos * 0;
}
model{
  vector[296] p;
  vector[3] delta_j_pos;
  vector[3] delta_j_th;
  b_dist ~ normal( 0 , 1 );
  s_dia ~ exponential( 1 );
  z_dia ~ normal( 0 , 1 );
  b_dia_bar ~ normal( 0 , 1 );
  s_sent ~ exponential( 1 );
  z_sent ~ normal( 0 , 1 );
  a_bar ~ normal( 0 , 1 );
  s_auth ~ exponential( 1 );
  z_auth ~ normal( 0 , 1 );
  delta_th ~ dirichlet( alpha_th );
  delta_j_th = append_row(0, delta_th);
  b_th ~ lognormal( 0 , 1 );
  delta_pos ~ dirichlet( alpha_pos );
  delta_j_pos = append_row(0, delta_pos);
  b_pos ~ lognormal( 0 , 1 );
  b_c ~ normal( 0 , 1 );
  for ( i in 1:296 ) {
    p[i] = a_bar + z_sent[ID[i]] * s_sent + b_c * Cop[i] + b_pos * sum(delta_j_pos[1:PoS[i]]) + b_th * sum(delta_j_th[1:Th[i]]) + z_auth[Auth[i]] * s_auth + b_dia_bar + z_dia[Dia[i]] * s_dia + b_dist * Dist[i];
    p[i] = inv_logit(p[i]);
  }
  A ~ bernoulli( p );
}
generated quantities{
  vector[296] log_lik;
  vector[296] p;
  vector[3] delta_j_pos;
  vector[3] delta_j_th;
  vector[15] b_auth;
  vector[296] a_sent;
  vector[2] b_dia;
  b_dia = b_dia_bar + z_dia * s_dia;
  a_sent = a_bar + z_sent * s_sent;
  b_auth = z_auth * s_auth;
  delta_j_th = append_row(0, delta_th);
  delta_j_pos = append_row(0, delta_pos);
  for ( i in 1:296 ) {
    p[i] = a_bar + z_sent[ID[i]] * s_sent + b_c * Cop[i] + b_pos * sum(delta_j_pos[1:PoS[i]]) + b_th * sum(delta_j_th[1:Th[i]]) + z_auth[Auth[i]] * s_auth + b_dia_bar + z_dia[Dia[i]] * s_dia + b_dist * Dist[i];
    p[i] = inv_logit(p[i]);
  }
  for ( i in 1:296 ) log_lik[i] = bernoulli_lpmf( A[i] | p[i] );
}

