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
