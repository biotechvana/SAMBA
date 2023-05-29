set_shared_session_info <- function(res) {
  shared_session_info$fittedbn <- res$fittedbn
  shared_session_info$bn_df_variables <- res$bn_df_variables
  shared_session_info$bn_df_norm <- res$bn_df_norm
  shared_session_info$bn_df_taxas <- res$bn_df_taxas
  all_vars_bn_df_variables <- colnames(res$bn_df_variables)
  only_factor_variables <- all_vars_bn_df_variables[sapply(res$bn_df_variables, class) == "factor"]
  shared_session_info$factor_variables <- as.data.frame( res$bn_df_variables[,only_factor_variables])
  colnames(shared_session_info$factor_variables) <- only_factor_variables
  shared_session_info$taxa_names <- colnames(res$bn_df_taxas)
  shared_session_info$exposure_variables <- colnames(res$bn_df_variables)
  shared_session_info$outcome_variables <- colnames(res$bn_df_taxas)
  ## for building dagitty we need to see if we already have it or not
  if (is.null(res$dagitty)) {
    ## build it and add all impliedConditionalIndependencies
    res$dagitty <- bn_to_dagitty(res$fittedbn)
    ## for the moment make this init here
    exposure_variables <- colnames(res$bn_df_variables)
    outcome_variables <- colnames(res$bn_df_taxas)
    
    dagitty::exposures(res$dagitty) <- exposure_variables
    dagitty::outcomes(res$dagitty) <- outcome_variables
  }
  shared_session_info$dagitty <- res$dagitty
  if (!is.null(res$testable_implications_taxa_vars)) {
    shared_session_info$testable_implications_taxa_vars <- res$testable_implications_taxa_vars
  }
  if (!is.null(res$fittedbn_custom)) {
    shared_session_info$fittedbn_custom <- res$fittedbn_custom
  }
  
  var_list <- list()
  for (i in 1:ncol(shared_session_info$factor_variables)) {
    var_list[[colnames(shared_session_info$factor_variables)[i]]] <- levels(shared_session_info$factor_variables[, i])
  }
  shared_session_info$var_list <- var_list
  shared_session_info
}
incProgress <- function(x, detail = "Sampling ..."){
  
}
withProgress <- function(...){
  
}
filter_by_evidence <- function (bn_df_input,l_var){
  filt_data <- bn_df_input
  for (v in 1:length(l_var)) {
    filt_data <- filt_data[filt_data[[names(l_var[v])]] == l_var[[v]][1], ]
  }
  filt_data
}
##########################################
file <- "/home/ahafez/Downloads/wetransfer_zinb-filt-diet_2023-03-10_1523/ZINB FILT DIET/2023-02-22_14.44.34_complete_network.RData"
result_env <- readRDS(file)
shared_session_info <- list()
shared_session_info <- set_shared_session_info(result_env)
res<-result_env
shared_session_info$build_env <- result_env
session_data <- shared_session_info
local_data<-list()
if (!is.null(session_data$bn_df_variables)) {
  variables <- session_data$bn_df_variables[, sapply(session_data$bn_df_variables, class) == "factor"]
  var_list <- list()
  for (i in 1:ncol(variables)) {
    var_list[[colnames(variables)[i]]] <- levels(variables[, i])
  }
  local_data$var_list <- var_list
} else {
  local_data$var_list <- c()
}
input <- list()
input$evidence1 <- c("EGG.HYDRO")
input$evidence1 <- c("GAIN_PreChallenge")
var_list <- local_data$var_list

data_as_strong_proir <- TRUE
target_nodes <- colnames(session_data$bn_df_taxas)
simple_table <- FALSE
df <- custom_fit_prediction(input_evidence, c("T439"), FALSE, TRUE)



result_env.bck <- result_env
################################################################################
############################## test 2 ##########################################
file <- "/home/ahafez/Desktop/samba paper/Real_dataset/EX50NB_filtered/EX50NB/2023-05-10_10.35.59_complete_network.RData"
result_env <- readRDS(file)
shared_session_info <- list()
shared_session_info <- set_shared_session_info(result_env)

res<-result_env
shared_session_info$build_env <- result_env
session_data <- shared_session_info
local_data<-list()
if (!is.null(session_data$bn_df_variables)) {
  variables <- session_data$bn_df_variables[, sapply(session_data$bn_df_variables, class) == "factor"]
  var_list <- list()
  for (i in 1:ncol(variables)) {
    var_list[[colnames(variables)[i]]] <- levels(variables[, i])
  }
  local_data$var_list <- var_list
} else {
  local_data$var_list <- c()
}
input <- list()
input$evidence1 <- c(">20")
input$evidence1 <- c(">20","≤4")


var_list <- local_data$var_list

input_evidence <- list()

for (i in input$evidence1) {
  res <- lapply(var_list, function(x) match(i, x))
  for (j in 1:length(res)) {
    if (!is.na(res[j])) {
      input_evidence[[names(res[j])]] <- i
    }
  }
}


data_as_strong_proir <- TRUE
target_nodes <- colnames(session_data$bn_df_taxas)
simple_table <- FALSE
df <- custom_fit_prediction(input_evidence, c("Bacteria_Actinobacteria_Actinobacteria_Actinomycetales_Micrococcaceae_Arthrobacter"), FALSE, TRUE)

incProgress <- function(x, detail = "Sampling ..."){
  
}
observed_variables_data <- session_data$bn_df_variables

network_samples <- get_samples_all_v2(
  session_data$fittedbn_custom,
  session_data$fittedbn,
  sampling.path,
  observed_variables_data,
  observed_variables,
  average.offset,
  nSamples = 1000000,
  incProgress = incProgress
)
View(network_samples$all_samples)
View(network_samples$exp_counts)


hist(network_samples$all_samples$Bacteria_Proteobacteria_Gammaproteobacteria_Vibrionales_Vibrionaceae_Vibrio)

View(network_samples$all_samples)

result_env.bck <- result_env

init_samples_ <- init_samples[rep(seq_len(nrow(init_samples)), each = 10),]

dt <- data.table::as.data.table( observed_variables_data)
init_samples <- dt[, .N, by = eval(colnames(dt))]
init_samples$N <- init_samples$N/sum(init_samples$N)
nSamples=nrow(init_samples)
hist(ZIM::rzinb(n = 1, k = 1000000, lambda = 10000, omega = 0.0))


grid_info<- list()
for(ob_var in colnames(observed_variables_data)) {
  grid_info[[ob_var]] <-  levels(observed_variables_data[,ob_var])
}

combinations_var_all <- expand.grid(grid_info)
grid_info <- list(
  tissue = levels(bn_data$tissue),
  FM = levels(bn_data$FM),
  FO = levels(bn_data$FO),
  additive = levels(bn_data$additive))

View(session_data$build_env$input_bn_df)
View(bn_data)
train_data <- session_data$build_env$input_bn_df
test_model <-  MASS::glm.nb(Bacteria_Firmicutes_Bacilli_Bacillales_Staphylococcaceae_Staphylococcus ~ 1,
                            train_data)
t1 <- train_data[,c("tissue", "FM", "FO" ,"Bacteria_Firmicutes_Bacilli_Bacillales_Staphylococcaceae_Staphylococcus")]
t1 <- init_samples[,c("tissue", "FM", "FO" ,"Bacteria_Firmicutes_Bacilli_Bacillales_Staphylococcaceae_Staphylococcus")]

t1<- data.table::as.data.table(t1)
       
t1[, list(mrange=round(mean(Bacteria_Firmicutes_Bacilli_Bacillales_Staphylococcaceae_Staphylococcus))), by = c('tissue','FM',"FO")]
                   
summary(test_model)
mean(train_data$Bacteria_Proteobacteria_Gammaproteobacteria_Alteromonadales_Alteromonadaceae_Alishewanella)
################################################################################


zinb.predict <- function (object, newdata, type = c("response", "prob", "count", 
                                    "zero"), na.action = na.pass, at = NULL, ...) 
{
  type <- match.arg(type)
  if (missing(newdata)) {
    rval <- object$fitted.values
    if (type != "response") {
      if (!is.null(object$x)) {
        X <- object$x$count
        Z <- object$x$zero
      }
      else if (!is.null(object$model)) {
        X <- model.matrix(object$terms$count, object$model, 
                          contrasts = object$contrasts$count)
        Z <- model.matrix(object$terms$zero, object$model, 
                          contrasts = object$contrasts$zero)
      }
      else {
        stop("predicted probabilities cannot be computed with missing newdata")
      }
      offsetx <- if (is.null(object$offset$count)) 
        rep.int(0, NROW(X))
      else object$offset$count
      offsetz <- if (is.null(object$offset$zero)) 
        rep.int(0, NROW(Z))
      else object$offset$zero
      mu <- exp(X %*% object$coefficients$count + offsetx)[, 
                                                           1]
      phi <- object$linkinv(Z %*% object$coefficients$zero + 
                              offsetz)[, 1]
    }
  }
  else {
    mf <- model.frame(delete.response(object$terms$full), 
                      newdata, na.action = na.action, xlev = object$levels)
    X <- model.matrix(delete.response(object$terms$count), 
                      mf, contrasts = object$contrasts$count)
    Z <- model.matrix(delete.response(object$terms$zero), 
                      mf, contrasts = object$contrasts$zero)
    offsetx <- pscl:::model_offset_2(mf, terms = object$terms$count, 
                              offset = FALSE)
    offsetz <- pscl:::model_offset_2(mf, terms = object$terms$zero, 
                              offset = FALSE)
    if (is.null(offsetx)) 
      offsetx <- rep.int(0, NROW(X))
    if (is.null(offsetz)) 
      offsetz <- rep.int(0, NROW(Z))
    if (!is.null(object$call$offset)) 
      offsetx <- offsetx + eval(object$call$offset, newdata)
    mu <- exp(X %*% object$coefficients$count + offsetx)[, 
                                                         1]
    phi <- object$linkinv(Z %*% object$coefficients$zero + 
                            offsetz)[, 1]
    rval <- (1 - phi) * mu
  }
  if (type == "count") 
    rval <- mu
  if (type == "zero") 
    rval <- phi
  if (type == "prob") {
    if (!is.null(object$y)) 
      y <- object$y
    else if (!is.null(object$model)) 
      y <- model.response(object$model)
    else stop("predicted probabilities cannot be computed for fits with y = FALSE and model = FALSE")
    yUnique <- if (is.null(at)) 
      0:max(y)
    else at
    nUnique <- length(yUnique)
    rval <- matrix(NA, nrow = length(rval), ncol = nUnique)
    dimnames(rval) <- list(rownames(X), yUnique)
    switch(object$dist, poisson = {
      rval[, 1] <- phi + (1 - phi) * exp(-mu)
      for (i in 2:nUnique) rval[, i] <- (1 - phi) * dpois(yUnique[i], 
                                                          lambda = mu)
    }, negbin = {
      theta <- object$theta
      rval[, 1] <- phi + (1 - phi) * dnbinom(0, mu = mu, 
                                             size = theta)
      for (i in 2:nUnique) rval[, i] <- (1 - phi) * dnbinom(yUnique[i], 
                                                            mu = mu, size = theta)
    }, geometric = {
      rval[, 1] <- phi + (1 - phi) * dnbinom(0, mu = mu, 
                                             size = 1)
      for (i in 2:nUnique) rval[, i] <- (1 - phi) * dnbinom(yUnique[i], 
                                                            mu = mu, size = 1)
    })
  }
  rval
}


###############

node_model
node_model$coefficients$count
node_model_summary <- summary(node_model)
printCoefmat(as.data.frame(node_model$coefficients$count), digits = 3, signif.legend = FALSE)
sqrt(diag(vcov(node_model)))
ypp = vcov(node_model, model = "count")
yp <- predict(node_model, type = "response")
yy <- cbind(y=y,yp=round(yp),zero=predict(node_model, type = "zero"))
exp(mean(log1p(yy$count)))
mean(yy$count)
yy <- cbind(init_samples[,cp_observed_variables]  ,data.frame( zero = round(predict(node_model,init_samples,type="zero"),6) , count = round(predict(node_model,init_samples,type="count")) , response= round(predict(node_model,init_samples_EX,type="response"))))
ZIM::qzinb(p = seq(0,0.99,by=0.01), k = 1, lambda = 71, omega = 0.1)
sum(ZIM::dzinb(x = 5:15, k = 1000, lambda = 100, omega = 0.0))
ZIM::pzinb(q = c(90,101), k = 1, lambda = 1000, omega = 0)
plot(ZIM::pzinb(q = 0:1000, k = 100, lambda = 10, omega = 0))
plot(ZIM::dzinb(x = 0:10000, k = 100, lambda = 10000, omega = 0))
ZIM::dzinb(x = 70:70, k = 0.1, lambda = 70, omega = 0.0)

qs = ZIM::qzinb(p = seq(0,1,by=0.1), k = 1, lambda = 100, omega = 0.1)
ZIM::pzinb(q = qs, k = 1, lambda = 100, omega = 0)
plot(qs,ZIM::pzinb(q = qs, k = 1, lambda = 100, omega = 0))
plot(ZIM::qzinb(p = seq(0,1,by=0.01), k = .1, lambda = 10, omega = 0.10))
hist(ZIM::rzinb(100000, k = 1, lambda = 71, omega = 0.0))



xp <- 61302
sum(yp[71,10:10000])
plot(1:xp,yp[72,1:xp])
full_data <- session_data$build_env$bn_df_norm
bn_fit <- session_data$fittedbn
fitted(node_model,method="zero")
y <- simulate(node_model)
x <- data.frame(zero = round(fitted(node_model,method="zero")) , count = round(fitted(node_model,method="count")) , response= round(fitted(node_model,method="response")))



ym = pscl::zeroinfl(y ~ 1,data= data.frame(y=as.integer(final_counts)),dist = "negbin" )
ymf <- predict(ym,type= "zero")
plot(ZIM::dzinb(x = 0:100, k = ym$theta, lambda = exp(ym$coefficients$count), omega = ymf[1]))
hist(ZIM::rzinb(1000, k = ym$theta, lambda = exp(ym$coefficients$count), omega = ymf[1]))
plot(ZIM::qzinb(p = seq(0,0.991,by=0.001), k = ym$theta, lambda = exp(ym$coefficients$count), omega = ymf[1]))
(ZIM::qzinb(p = seq(0,0.991,by=0.1), k = ym$theta, lambda = exp(ym$coefficients$count), omega = ymf[1]))

summary(ym)
View(QuickStartExample$x)
x <- QuickStartExample$x
y <- QuickStartExample$y

x <- full_data[,parents(bn_fit,node)]
x <- makeX(x)
# for(xx in colnames(x)) {
#   if(is.factor(x[,xx])) {
#     x[,xx] <- as.numeric(x[,xx])
#   }
# }
# x <- as.matrix(x)
y <- full_data[,node]
y <- round(expm1(y))
fit <- glmnet(x, y, MASS::negative.binomial(theta = 40))
coef(fit)
plot(fit)
set.seed(29)
nx <- matrix(rnorm(5 * 20), 5, 20)
yy <- cbind(round((predict(fit, newx=x, type = "response"))),y=y,yp)
##### Test Filters
taxa_count_filters <- list (
  filterBA = "After",
  filter_option = "Total",
  filterThrT = "25,50", # Select global filter threshold
  filterCountsT = 10 # Specify a minimum number of counts to apply this filter
)
net_dir="."
taxa_count_filters <- list (
  filterBA = "After",
  filter_option = "Group",
  filterThrG =  "EGG.HYDRO-25,GAIN_PreChallenge-25,LS.AQUA-25",# Select filter threshold for each variable condition
  filterVariable = "Experiments",
  filterCountsG = 10 # "Specify a minimum number of counts to apply this filter"
)

taxa_count_filters <- list (
  filterBA = "After",
  filter_option = "Group",
  filterThrG =  "EGG.HYDRO-25,GAIN_PreChallenge-25,LS.AQUA-25",# Select filter threshold for each variable condition
  filterVariable = "Experiments",
  filterCountsG = 10 # "Specify a minimum number of counts to apply this filter"
)




apply_after_filter(shared_session_info$build_env,"/home/data/git/samba/files/EX1",taxa_count_filters)

### 
for(xx in x) {
  print(summary(xx))
}
summary(x[[12]])
