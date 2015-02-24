calculate_d1 = function(S, K, q, r, sig, T_mature, t)
{
  return((log(S/K)+(r-q+sig^2/2)*(T_mature-t))/sig/sqrt(T_mature-t))
}
calculate_d2 = function(S, K, q, r, sig, T_mature, t)
{
  return((log(S/K)+(r-q-sig^2/2)*(T_mature-t))/sig/sqrt(T_mature-t))
}

BS_Option_Price = function(S, K, q, r, sig, T_mature, t, type)
{
  d1 = calculate_d1(S, K, q, r, sig, T_mature, t)
  d2 = calculate_d2(S, K, q, r, sig, T_mature, t)
  if (type == "Call"){
    return(S*exp(-q*(T_mature-t))*pnorm(d1)-K*exp(-r*(T_mature-t))*pnorm(d2))
  } else if (type == "Put"){
    return(-S*exp(-q*(T_mature-t))*pnorm(-d1)+K*exp(-r*(T_mature-t))*pnorm(-d2))
  } else {return(NA)}
}

S_all = c(25, 30, 26, 22, 27)
t_all = c(0,1,2,3,4)/13/4
diff = 0
K = 30
r = 0.02
q = 0
T_mature = 1/4
sig = 0.3
option = 1000
share = 400
cash = 10000

for (i in c(1:5))
{
  S = S_all[i]
  t = t_all[i]
  P = BS_Option_Price(S, K, q, r, sig, T_mature, t, type="Put")
  cat("Week ",i-1, " Before: ", option, "  ,", option*P,"  ,", share, "  ,", share*S, "  ,", cash, "Total: ", option*P+share*S+cash, "\n")
  d1 = calculate_d1(S, K, q, r, sig, T_mature, t)
  d2 = calculate_d2(S, K, q, r, sig, T_mature, t)
  Delta_P = -exp(-q*(T_mature-t))*pnorm(-d1)
  x = round(-Delta_P*option)
  cash = cash - (x-share)*S
  share = x
  cat("Week ",i-1, " After: ", option, "  ,", option*P,"  ,", share, "  ,", share*S, "  ,", cash, "Total: ", option*P+share*S+cash, "\n")
  cash = cash*exp(r*diff)
}
