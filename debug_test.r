dd_o <- density(proir_data, adjust = 0.4, from = 0, t = max_range, n = 1000, weights = proir_data_w)
yy <- round(dd_o$y,6)
min(yy[yy>0])
plot(dd_o$x,dd_o$y)

plot(dd_o$x[1:50],  dd_o$y[1:50], col = "black", # rescaled
     type = "l", xlab = "P(Black)", ylab = "Density")

dd_samples <- density(network_samples, adjust = adjust_samples, from = 0, t = max_range, n = 1000, weights = network_samples_w)
plot(dd_samples$x,dd_samples$y)
dd_samples <- density(network_samples, adjust = 0.5, from = 0, t = max_range, n = 1000)
plot(dd_samples$x,  dd_samples$y, col = "black", # rescaled
     type = "l", xlab = "P(Black)", ylab = "Density")

posterior <- dd_o$y * dd_samples$y
plot(dd_samples$x,posterior / sum(posterior))

dd_o <- density(
    samples.result$network.samples.raw.values,adjust =0.5, 
    from=0 , t= max(samples.result$network.samples.raw.values) +5, n = 1000)

plot(dd_o$x,dd_o$y)
hdr <- hdrcde::hdr.den((samples.result$network.samples.raw.values),prob=90)
expm1(hdr$hdr)
expm1(hdr$mode)


expm1(HPDI(samples.result$network.samples.raw.values, prob = 0.90))
expm1(0.734)
expm1(3.636)
exp(mean(samples.result$network.samples.raw.values))



dev.new()
posterior_dist <- get_posterior_dist(samples.result$network.samples.raw.values, data_as_proir )
wtd.mean(expm1(posterior_dist$data_value),posterior_dist$posterior_w)
sqrt(wtd.var(expm1(posterior_dist$data_value),posterior_dist$posterior_w))
plot(posterior_dist$data_value,posterior_dist$posterior_w+0.01)


dev.new()
posterior_dist <- get_posterior_dist(samples.result$network.samples.raw.values, data_as_proir ,adjust_samples=0.5, adjust_proir= 0.7 )

plot(posterior_dist$data_value,posterior_dist$posterior_w)
hdr_post <- hdrcde::hdr.den(den = list(x=posterior_dist$data_value,y=posterior_dist$posterior_w),prob=90)
expm1(hdr_post$hdr)
(hdr_post$mode)
expm1(wtd.quantile((posterior_dist$data_value),weights = round(posterior_dist$posterior_w,5)))
(wtd.quantile(expm1(posterior_dist$data_value),weights = round(posterior_dist$posterior_w,5)))

expm1(wtd.mean((posterior_dist$data_value),weights = round(posterior_dist$posterior_w,5)))
(wtd.mean(expm1(posterior_dist$data_value),weights = round(posterior_dist$posterior_w,5)))



s.back <- sample(x = expm1(posterior_dist$data_value), size = 1000000,prob = round(posterior_dist$posterior_w,5) , replace = TRUE)
mean(s.back)
sd(s.back)
quantile(s.back)
dens(log1p(s.back))

low_range <- 3.0668
high_range <- 3.7158
data_mask <- posterior_dist$data_value >= low_range & posterior_dist$data_value <= high_range
sum(posterior_dist$posterior_p[data_mask])
IQR(posterior_dist$data_value)
posterior_dist$posterior_quantile[4] -  posterior_dist$posterior_quantile[2]

low_range <- posterior_dist$posterior_quantile[2]
high_range <- posterior_dist$posterior_quantile[4]

posterior_dist_samples_only <- get_posterior_dist(samples.result$network.samples.raw.values,adjust_samples=0.2)
wtd.mean(expm1(posterior_dist_samples_only$data_value),posterior_dist_samples_only$posterior_w)
sqrt((wtd.var(expm1(posterior_dist_samples_only$data_value),posterior_dist_samples_only$posterior_w)))
wtd.quantile(expm1(posterior_dist_samples_only$data_value),weights = posterior_dist_samples_only$posterior_w)
plot(posterior_dist_samples_only$data_value,posterior_dist_samples_only$posterior_w)



quantile(expm1(data_as_proir))

dev.new()
dd_data <- density(
    data_as_proir,adjust =0.5, 
    from=0 , t= max(data_as_proir) + 1, n = 1000, )
hdr <- hdrcde::hdr.den(den=dd_data,prob=50 )
log1p((mean(expm1(data_as_proir))))
plot(dd_data$x,dd_data$y)
abline( v=max(data_as_proir) , lty=1 )
abline( v=-1, lty=1 )
plot(data_as_proir[order(data_as_proir)])


dev.new()

hdr.den(den = list(x=expm1(posterior_dist$data_value),y=posterior_dist$posterior_w),prob=50)


#############


expm1(mean(samples.result$network.samples.raw.values))
mean(expm1(samples.result$network.samples.raw.values))
expm1(sd(samples.result$network.samples.raw.values))


expm1(quantile(samples.result$network.samples.raw.values))
quantile(expm1(samples.result$network.samples.raw.values))

quantile(samples.result$network.samples.raw.values)

quantile(samples.result$network.samples.raw.values)


m <- mean(samples.result$network.samples.values)
s <- sd(samples.result$network.samples.raw.values)
expm1(m)

hdr <- hdrcde::hdr.den((samples.result$network.samples.raw.values))
hdr1 <- hdrcde::hdr((samples.result$network.samples.raw.values), prob = 90)
expm1(hdr$hdr)
hdr2 <- hdrcde::hdr.den(den = list(x = (posterior_dist$data_value), y = posterior_dist$posterior_w), prob = 85)
expm1(hdr2$hdr)

quantile(posterior_dist$post_samples)

hdr3 <- hdrcde::hdr.den(log1p(posterior_dist$post_samples), prob = 90)
expm1(hdr3$hdr)

hdr_50 <- hdrcde::hdr(samples.result$network.samples.raw.values, prob = 50)



          samples.result <- get.mixed.samples(fittedbn, bn_df_norm, taxas[i],
            evidence = input_evidence,
            n.samples = 100000,
            org_data_min_weight = org_data_min_weight,
            HPDI_correct = 0.98
          )
 samples.result$network.samples.quantile
expm1(quantile(samples.result$network.samples.raw.values))

posterior_dist <- get_posterior_dist(samples.result$network.samples.raw.values, data_as_proir,
                adjust_samples = 1, adjust_proir = 1
              )
posterior_dist <- posterior_stats(posterior_dist)
posterior_dist$posterior_quantile

expm1(hdr_50$hdr)
hdr_50 <- hdrcde::hdr(den = list(x = posterior_dist$data_value, y = posterior_dist$posterior_w), prob = 50)
HPDI(log(posterior_dist$post_samples),prob=0.5)

h_1(den = list(x = posterior_dist$data_value, y = posterior_dist$posterior_w), prob = 50)

all_roots <- function (f, interval,
  lower = min(interval), upper = max(interval), n = 100L, ...) {
  x <- seq(lower, upper, len = n + 1L)
  fx <- f(x, ...)
  roots <- x[which(fx == 0)]
  fx2 <- fx[seq(n)] * fx[seq(2L,n+1L,by=1L)]
  index <- which(fx2 < 0)
  for (i in index)
    roots <- c(roots, uniroot(f, lower = x[i], upper = x[i+1L], ...)$root)
  return(sort(roots))
}

calc.falpha <- function(x=NULL, den, alpha, nn=5000) {
    # Calculates falpha needed to compute HDR of density den.
    # Also finds approximate mode.
    # Input: den = density on grid.
    #          x = independent observations on den
    #      alpha = level of HDR
    # Called by hdr.box and hdr.conf

    if(is.null(x))
        calc.falpha(x=sample(den$x, nn, replace=TRUE, prob=den$y), den, alpha)
    else
    {
        fx <- approx(den$x,den$y,xout=x,rule=2)$y
        falpha <- quantile(sort(fx), alpha)
        mode <- den$x[den$y==max(den$y)]
        return(list(falpha=falpha,mode=mode,fx=fx))
    }
}

hdr.ends <- function(den,falpha) {
    miss <- is.na(den$x) # | is.na(den$y)
    den$x <- den$x[!miss]
    den$y <- den$y[!miss]
    n <- length(den$x)
    # falpha is above the density, so the HDR does not exist
    if(falpha > max(den$y))
        return(list(falpha=falpha,hdr=NA) )
    f <- function(x, den, falpha) {
      approx(den$x, den$y-falpha, xout=x)$y
    }
    intercept <- all_roots(f, interval=range(den$x), den=den, falpha=falpha)
    ni <- length(intercept)
    # No roots -- use the whole line
    if(ni == 0L)
      intercept <- c(den$x[1],den$x[n])
    else {
      # Check behaviour outside the smallest and largest intercepts
      if(f(0.5*(head(intercept,1) + den$x[1]), den, falpha) > 0)
        intercept <- c(den$x[1],intercept)
      if(f(0.5*(tail(intercept,1) + den$x[n]), den, falpha) > 0)
        intercept <- c(intercept,den$x[n])
    }
    # Check behaviour -- not sure if we need this now
    if(length(intercept) %% 2)
      warning("Some HDRs are incomplete")
      #  intercept <- sort(unique(intercept))
    return(list(falpha=falpha,hdr=intercept))
}


h_1 <- function (x = NULL, prob = c(50, 95, 99), den = NULL, h = hdrbw(BoxCox(x, lambda), mean(prob)), lambda = 1, nn = 5000, all.modes = FALSE)  {
    if (!is.null(x)) {
        r <- diff(range(x))
        if (r == 0) 
            stop("Insufficient data")
    }
    if (is.null(den)) 
        den <- tdensity(x, bw = h, lambda = lambda)
    alpha <- sort(1 - prob/100)
    falpha <- hdrcde::calc.falpha(x, den, alpha, nn = nn)
    hdr.store <- matrix(NA, length(alpha), 100)
    for (i in 1:length(alpha)) {
        junk <- hdr.ends(den, falpha$falpha[i])$hdr
        if (length(junk) > 100) {
            junk <- junk[1:100]
            warning("Too many sub-intervals. Only the first 50 returned.")
        }
        hdr.store[i, ] <- c(junk, rep(NA, 100 - length(junk)))
    }
    cj <- colSums(is.na(hdr.store))
    hdr.store <- matrix(hdr.store[, cj < nrow(hdr.store)], nrow = length(prob))
    rownames(hdr.store) <- paste(100 * (1 - alpha), "%", sep = "")
    if (all.modes) {
        y <- c(0, den$y, 0)
        n <- length(y)
        idx <- ((y[2:(n - 1)] > y[1:(n - 2)]) & (y[2:(n - 1)] > 
            y[3:n])) | (den$y == max(den$y))
        mode <- den$x[idx]
    }
    else mode <- falpha$mode
    return(list(hdr = hdr.store, mode = mode, falpha = falpha$falpha))
}

taxas <- colnames(bn_df_taxas)
bn_df_norm_selected_taxas <- bn_df_norm_filtered_REV %>% select(all_of(taxas))
bn_df_norm_selected_taxas <- expm1(bn_df_norm_selected_taxas)
df.data <- bn_df_norm_selected_taxas

summary_stats(df.data )
x <- apply(bn_df_norm_selected_taxas,2,quantile)
xdf <- as.data.frame(t(x))

y <- precis(bn_df_norm_selected_taxas,prob=.5)
quantile()

data_values = bn_df_norm_selected_taxas[,1]
custom.confid(bn_df_norm_selected_taxas[,2], prob=0.5)

custom.confid <- function(data_values, prob=0.95 ) {
  # Calculate the mean of the sample data
  mean_value <- mean(data_values)

  # Compute the size
  n <- length(data_values)

  # Find the standard deviation
  standard_deviation <- sd(data_values)

  # Find the standard error
  standard_error <- standard_deviation / sqrt(n)
  alpha <- 1 - prob
  degrees_of_freedom <- n - 1
  t_score <- qt(p = alpha / 2, df = degrees_of_freedom, lower.tail = F)
  margin_error <- t_score * standard_error

  # Calculating lower bound and upper bound
  lower_bound <- mean_value - margin_error
  upper_bound <- mean_value + margin_error
  if (lower_bound < 0) lower_bound = 0
  list(
    lower_bound = lower_bound,
    upper_bound = upper_bound
  )
}

df <- data.frame(
                    Evidence = factor(c(
                        rep("Reference", length(count_data_REV)),
                        rep("Target", length(count_data_TEV))
                    )),
                    Count = c(log1p(count_data_REV), log1p(count_data_TEV))
                )
p <- ggplot(df, aes(x=Count, fill=Evidence)) + geom_density(alpha=0.4)
plot(p)


dev.new()
hist(count_data_REV,  density = 80,col = 'red' , main = paste("Histogram of" , selected_taxa ) , xlab ="Normalized Count")
hist(count_data_TEV,  density = 80,col = 'blue' , )
