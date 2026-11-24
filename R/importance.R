# Model importance and effect displays for the Framingham teaching dashboards.
# Importance means validation-set loss of predictive performance after shuffling a
# predictor. It is NOT an estimate of causality or a coefficient significance test.

fhs_importance_plot <- function(z, heading, subtitle = NULL) {
  stopifnot(is.data.frame(z), all(c('feature','importance') %in% names(z)))
  z <- z[is.finite(z$importance), , drop=FALSE]
  if (!nrow(z)) stop('No finite validation importance scores were available.')
  z <- z[order(z$importance), , drop=FALSE]
  plotly::plot_ly(z, x=~importance, y=~factor(feature, levels=feature),
                  type='bar', orientation='h', marker=list(color='#087e92'),
                  hovertemplate='%{y}: %{x:.4f}<extra></extra>') |>
    plotly::layout(title=list(text=heading), xaxis=list(title='Loss of validation performance after permutation'),
                   yaxis=list(title='Predictor'), margin=list(l=145),
                   annotations=if (is.null(subtitle)) list() else list(list(
                     text=subtitle,x=0,y=-0.22,xref='paper',yref='paper',showarrow=FALSE,
                     xanchor='left',font=list(size=11))))
}

fhs_effect_plot <- function(z, heading, x_title='Effect estimate (95% confidence interval)', reference=1) {
  # Plot an effect-estimate chart, NOT permutation feature importance.
  stopifnot(all(c('term','estimate','lower','upper') %in% names(z)))
  z <- z[is.finite(z$estimate) & is.finite(z$lower) & is.finite(z$upper),,drop=FALSE]
  if (!nrow(z)) stop('No finite coefficient confidence intervals are available.')
  z <- z[order(z$estimate),,drop=FALSE]
  plotly::plot_ly(z,x=~estimate,y=~factor(term,levels=term),type='scatter',mode='markers',
    marker=list(size=9,color='#087e92'),
    error_x=list(type='data',symmetric=FALSE,array=z$upper-z$estimate,
                 arrayminus=z$estimate-z$lower,visible=TRUE),
    hovertemplate='%{y}: %{x:.3f}<extra></extra>') |>
    plotly::layout(title=heading,xaxis=list(title=x_title),yaxis=list(title='Term'),
       shapes=list(list(type='line',x0=reference,x1=reference,y0=0,y1=1,
                        xref='x',yref='paper',line=list(dash='dash',color='#b15b4a'))),
       margin=list(l=155))
}

fhs_count_predict <- function(fit, newdata) {
  p <- suppressWarnings(as.numeric(stats::predict(fit,newdata=newdata,type='response')))
  if(length(p)!=nrow(newdata) || any(!is.finite(p))) stop('Invalid count predictions.')
  p
}

fhs_count_importance <- function(x, seed=2026, repetitions=2L) {
  # x is the count-regression comparison returned by the count tab.
  fit <- x$models[[x$best]]
  valid <- x$validation[[x$best]]
  features <- x$predictors
  outcome <- x$outcome
  if(is.null(fit)||is.null(valid)||nrow(valid)<12) stop('Fit count models before requesting importance.')
  rmse <- function(dd) sqrt(mean((as.numeric(dd[[outcome]])-fhs_count_predict(fit,dd))^2))
  baseline <- rmse(valid)
  set.seed(seed)
  scores <- vapply(features,function(v) {
    mean(replicate(repetitions,{
      shuffled <- valid
      shuffled[[v]] <- sample(valid[[v]])
      tryCatch(rmse(shuffled)-baseline,error=function(e)NA_real_)
    }),na.rm=TRUE)
  },numeric(1))
  z <- data.frame(feature=names(scores),importance=unname(scores),row.names=NULL)
  z[order(z$importance,decreasing=TRUE),,drop=FALSE]
}

fhs_survival_risk <- function(method, model, newdata) {
  risk <- switch(method,
    'Cox PH'=as.numeric(stats::predict(model,newdata=newdata,type='lp')),
    'Stratified Cox'=as.numeric(stats::predict(model,newdata=newdata,type='lp')),
    'Weibull AFT'=-as.numeric(stats::predict(model,newdata=newdata,type='lp')),
    'Lognormal AFT'=-as.numeric(stats::predict(model,newdata=newdata,type='lp')),
    'Loglogistic AFT'=-as.numeric(stats::predict(model,newdata=newdata,type='lp')),
    'Survival tree'=-as.numeric(stats::predict(model,newdata=newdata)),
    'Random survival forest (ranger)'=rowSums(predict(model,data=newdata)$chf),
    'Random survival forest (SRC)'=as.numeric(predict(model,newdata=newdata)$predicted),
    'Oblique survival forest'=as.numeric(predict(model,new_data=newdata,pred_type='risk')),
    'Bayesian survival (rstanarm)'=-as.numeric(stats::predict(model,newdata=newdata,type='response')),
    stop('No supported survival risk predictor for selected method.')
  )
  if(length(risk)!=nrow(newdata)||any(!is.finite(risk))) stop('Invalid survival risk predictions.')
  risk
}

fhs_survival_importance <- function(z, predictors, seed=2026, repetitions=2L) {
  method <- as.character(z$table$model[1L])
  model <- z$models[[method]]
  if(is.null(model)) stop('Fit survival models first.')
  va <- z$data[z$split$validation,,drop=FALSE]
  tr <- z$data[z$split$train,,drop=FALSE]
  # Reproduce the training-median and training-mode missing-data substitutions.
  va[predictors] <- fhs_impute(tr[predictors],va[predictors])$other
  cindex <- function(dd) {
    risk <- fhs_survival_risk(method,model,dd)
    survival::concordance(survival::Surv(dd$time_years,dd$event_status)~risk,
                           reverse=TRUE)$concordance
  }
  baseline <- cindex(va)
  if(!is.finite(baseline)) stop('Validation concordance is undefined.')
  set.seed(seed)
  scores <- vapply(predictors,function(v) {
    mean(replicate(repetitions,{
      dd <- va
      dd[[v]] <- sample(va[[v]])
      tryCatch(baseline-cindex(dd),error=function(e)NA_real_)
    }),na.rm=TRUE)
  },numeric(1))
  z <- data.frame(feature=names(scores),importance=unname(scores),row.names=NULL)
  z[order(z$importance,decreasing=TRUE),,drop=FALSE]
}

fhs_coefficient_intervals <- function(model, scale='hazard') {
  s <- summary(model)
  if(is.null(s$coefficients)) stop('Coefficient summary unavailable.')
  coef <- stats::coef(model)
  se <- sqrt(diag(stats::vcov(model)))
  nms <- intersect(names(coef),names(se))
  nms <- setdiff(nms,c('(Intercept)','Log(scale)'))
  if (!length(nms)) stop('No eligible model effects were estimated.')
  estimate <- coef[nms]
  lower <- estimate - stats::qnorm(.975)*se[nms]
  upper <- estimate + stats::qnorm(.975)*se[nms]
  if(scale %in% c('hazard','rate','odds')) {
     estimate <- exp(estimate);lower <- exp(lower);upper <- exp(upper)
  }
  data.frame(term=nms,estimate=unname(estimate),lower=unname(lower),upper=unname(upper))
}

fhs_count_effects <- function(z) {
  method <- z$best
  fit <- z$models[[method]]
  if(method %in% c('Poisson','Negative binomial'))
    return(fhs_coefficient_intervals(fit,'rate'))
  if(grepl('Zero-inflated',method,fixed=TRUE)) {
     cf <- stats::coef(fit)
     vc <- stats::vcov(fit)
     se <- sqrt(diag(vc))
     nm <- intersect(names(cf),names(se))
     nm <- setdiff(nm,c('count_(Intercept)','zero_(Intercept)'))
     if(!length(nm)) stop('No coefficient terms to display.')
     return(data.frame(term=nm,estimate=exp(cf[nm]),lower=exp(cf[nm]-1.96*se[nm]),
                       upper=exp(cf[nm]+1.96*se[nm])))
  }
  if(grepl('Zero-truncated',method,fixed=TRUE)) {
     cf<-stats::coef(fit);se<-sqrt(diag(stats::vcov(fit)))
     nm<-intersect(names(cf),names(se));nm<-nm[!grepl('Intercept',nm)]
     if(!length(nm)) stop('No coefficient terms to display.')
     return(data.frame(term=nm,estimate=exp(cf[nm]),lower=exp(cf[nm]-1.96*se[nm]),
                       upper=exp(cf[nm]+1.96*se[nm])))
  }
  stop('Effects are unavailable for this family.')
}

fhs_cox_adjusted_effects <- function(adjusted) {
  a <- fhs_coefficient_intervals(adjusted$crude,'hazard')
  b <- fhs_coefficient_intervals(adjusted$adjusted,'hazard')
  a$term <- paste0('Crude: ',a$term)
  b$term <- paste0('Adjusted: ',b$term)
  rbind(a,b)
}
