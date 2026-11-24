# Run from the application folder after installing requirements.
source('R/core.R')
source('R/importance.R')
stopifnot(requireNamespace('plotly',quietly=TRUE))
x <- fhs_prepare(fhs_load('fhs.csv'))
d <- stats::na.omit(x[,c('illustrative_event_burden','age1','sysbp1','bmi1'),drop=FALSE])
sp <- fhs_split(nrow(d),.7,.15)
tr<-d[sp$train,,drop=FALSE]; va<-d[sp$validation,,drop=FALSE]
f <- stats::glm(illustrative_event_burden~age1+sysbp1+bmi1,data=tr,family=poisson())
r <- list(best='Poisson',models=list(Poisson=f),validation=list(Poisson=va),
          outcome='illustrative_event_burden',predictors=c('age1','sysbp1','bmi1'))
ci<-fhs_count_importance(r)
stopifnot(nrow(ci)==3,all(is.finite(ci$importance)))
stopifnot(inherits(fhs_importance_plot(ci,'Count importance'),'plotly'))
ce<-fhs_count_effects(r)
stopifnot(nrow(ce)>0,inherits(fhs_effect_plot(ce,'Count effects'),'plotly'))
cat('PASS: count-model importance, rate ratios and interactive graphics.\n')
