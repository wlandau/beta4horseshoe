get_truth = function(chain_list){
  Scenario(chain_list[[1]])@supplement$truth
}

get_beta4 = function(est, truth){
  f = flatten_starts(truth)
  f = f[grep("beta_4", names(f))]
  cbind(est[grep("beta_4", rownames(est)),c("mean", "sd")], truth = f)
}

ci = function(distribution = "normal", x, level = 0.95){
  p = 1 - (1 - level)/2
  df = 6
  if(distribution == "normal"){
    lower = qnorm(1 - p, x$mean, x$sd)
    upper = qnorm(p, x$mean, x$sd)
  } else if (distribution == "Laplace"){
    lower = qlaplace(1 - p, x$mean, x$sd/sqrt(2))
    upper = qlaplace(p, x$mean, x$sd/sqrt(2))
  } else if (distribution == "t"){
    lower = qt(1 - p, df = df)*x$sd*sqrt((df-2)/df) + x$mean
    upper = qt(p, df = df)*x$sd*sqrt((df-2)/df) + x$mean
  } else {
    stop("Distribution not found.")
  }
  d = data.frame(
    lower = lower,
    upper = upper
  )
  colnames(d) = paste0(distribution, "_", colnames(d))
  d
}

cis = function(d, side = "lower"){
  out = do.call(cbind, lapply(c("normal", "Laplace", "t"), ci, x = d))
  out = cbind(out, truth = d$truth)
  out = out[order(out$truth),]
  out
}

my_line = function(x,y,...){
  points(x,y,...)
  abline(a = 0,b = 1,...)
}

my_pairs = function(d){
  pairs(d, lower.panel = my_line, upper.panel = my_line)
}

plot_lower = function(x){
  d =  x[,grep("lower", colnames(x))]
  my_pairs(d)
}

plot_upper = function(x){
  d =  x[,grep("upper", colnames(x))]
  my_pairs(d)
}

rates = function(x){
  df = NULL
  ns = c("Laplace", "t", "normal")
  for(i in ns){
    up = x[,paste0(i, "_upper")] > x$truth
    low = x[,paste0(i, "_lower")] < x$truth
    out = data.frame(
      upper_bound_high_enough = mean(up),
      lower_bound_low_enough = mean(low),
      ci_covers_truth = mean(up & low))
    df = rbind(df, out)
  }
  rownames(df) = ns
  print(xtable(df, digits = 5), file = "rates.tex")
}
