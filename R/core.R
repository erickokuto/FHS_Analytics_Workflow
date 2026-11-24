# FHS shared analytical functions. All user input is treated as untrusted.
# Models serve teaching and retrospective research, not individual medical decisions.
# Clustering may use a reproducible 1,200-row sample; results are exploratory.
FHS_EVENTS <- c('anychd','death','cvd','stroke','angina','hospmi','mi_fchd','hyperten')
FHS_TIME <- c(anychd='timechd',death='timedth',cvd='timecvd',stroke='timestrk',angina='timeap',hospmi='timemi',mi_fchd='timemifc',hyperten='timehyp')
FHS_BASE <- c('age1','sex1','sysbp1','diabp1','totchol1','bmi1','cursmoke1','cigpday1','diabetes1','bpmeds1','heartrte1','glucose1','prevchd1','prevhyp1','prevstrk1')
FHS_NO <- c('No','Female','0','FALSE')
`%||%` <- function(a,b) if (is.null(a) || length(a)==0) b else a
fhs_load <- function(path) {
  x <- utils::read.csv(path, stringsAsFactors=FALSE, check.names=TRUE, na.strings=c('','NA','N/A'))
  names(x) <- tolower(names(x))
  if (!nrow(x) || !ncol(x)) stop('The uploaded file has no usable rows or columns.')
  if (anyDuplicated(names(x))) stop('The file contains repeated column names.')
  for (nm in names(x)) if (is.character(x[[nm]])) x[[nm]] <- trimws(x[[nm]])
  x
}
fhs_prepare <- function(x, exclude_prevalent=FALSE) {
  if (!is.data.frame(x)) stop('Upload a CSV table.')
  if ('randid' %in% names(x)) x <- x[!duplicated(x$randid) & !is.na(x$randid),,drop=FALSE]
  if (exclude_prevalent && 'prevchd1' %in% names(x)) x <- x[!is.na(x$prevchd1) & x$prevchd1=='No',,drop=FALSE]
  for (v in intersect(FHS_EVENTS,names(x))) x[[v]] <- factor(x[[v]],levels=c('No','Yes'))
  for (v in names(x)) if (is.character(x[[v]])) x[[v]] <- as.factor(x[[v]])
  if (all(c('sysbp1','diabp1') %in% names(x))) x$bp_pulse_pressure <- x$sysbp1-x$diabp1
  if ('bmi1' %in% names(x)) x$bmi_class <- cut(x$bmi1,c(-Inf,18.5,25,30,Inf),labels=c('Underweight','Healthy range','Overweight','Obesity'),right=FALSE)
  if ('age1' %in% names(x)) x$age_band <- cut(x$age1,c(-Inf,40,50,60,Inf),labels=c('Under 40','40-49','50-59','60+'),right=FALSE)
  if (all(c('bmi1','bmi2') %in% names(x))) x$bmi_change_1_to_2 <- x$bmi2-x$bmi1
  # Educational composite only. It combines overlapping outcomes; not independent event incidence.
  es <- intersect(c('angina','hospmi','stroke','death'),names(x))
  if (length(es)) x$illustrative_event_burden <- rowSums(sapply(x[es],function(z) as.integer(as.character(z)=='Yes')),na.rm=FALSE)
  x
}
fhs_catalogue <- function(x) data.frame(variable=names(x),class=vapply(x,function(z) paste(class(z),collapse='/'),''),n_missing=vapply(x,function(z) sum(is.na(z)),0L),pct_missing=round(vapply(x,function(z) mean(is.na(z))*100,0),1),distinct=vapply(x,function(z) length(unique(z[!is.na(z)])),0L),row.names=NULL)
fhs_numeric <- function(x) names(x)[vapply(x,is.numeric,logical(1)) & names(x)!='randid']
fhs_cat <- function(x) names(x)[vapply(x,function(z) is.factor(z)||is.character(z)||is.logical(z),logical(1))]
fhs_describe <- function(x) {
 nums <- fhs_numeric(x)
 if (!length(nums)) return(data.frame())
 do.call(rbind,lapply(nums,function(v){z <- x[[v]]; nz<-z[is.finite(z)];data.frame(variable=v,n=length(nz),missing=sum(is.na(z)),mean=if(length(nz))mean(nz) else NA_real_,sd=if(length(nz)>1)sd(nz) else NA_real_,median=if(length(nz))median(nz) else NA_real_,q1=if(length(nz))unname(quantile(nz,.25)) else NA_real_,q3=if(length(nz))unname(quantile(nz,.75)) else NA_real_,min=if(length(nz))min(nz) else NA_real_,max=if(length(nz))max(nz) else NA_real_)}))
}
fhs_counts <- function(x,vars=fhs_cat(x)) {
 if (!length(vars)) return(data.frame())
 do.call(rbind,lapply(vars,function(v){a<-as.character(x[[v]]);tab<-table(ifelse(is.na(a),'Missing',a));data.frame(variable=v,category=names(tab),n=as.integer(tab),pct=round(100*as.vector(tab)/length(a),1),row.names=NULL)}))
}
fhs_split <- function(n,train=.7,val=.15,seed=2026) {
 if(n<35)stop('At least 35 complete observations are needed for model splitting.')
 if (any(!is.finite(c(train,val))) || train<=0||val<=0||train+val>=1) stop('Training and validation fractions must be positive and leave a test set.')
 set.seed(seed); z<-sample.int(n); nt<-floor(n*train); nv<-floor(n*val)
 list(train=z[seq_len(nt)],validation=z[seq.int(nt+1,nt+nv)],test=z[seq.int(nt+nv+1,n)])
}
fhs_predictors <- function(x, outcome, optional=NULL) {
 allowed <- intersect(FHS_BASE,names(x))
 if(!is.null(optional)) allowed<-intersect(allowed,optional)
 # Only baseline variables, never other outcome/time variables, follow-up or derived post-event fields.
 allowed <- setdiff(allowed,outcome)
 allowed[vapply(x[allowed],function(z)length(unique(z[!is.na(z)]))>=2,logical(1))]
}
fhs_model_frame <- function(x, outcome, predictors, max_n=40000) {
 if (!length(predictors)||!outcome %in% names(x)) stop('Select an available outcome and one or more valid baseline predictors.')
 if (nrow(x)>max_n) stop('Dataset exceeds the interactive modelling limit.')
 d<-x[,unique(c(outcome,predictors)),drop=FALSE]
 d <- d[!is.na(d[[outcome]]),,drop=FALSE]
 if(nrow(d)<35) stop('Too few rows with a recorded outcome.')
 # Impute missing predictors using training set ONLY after splitting (see fit function).
 d
}
fhs_impute <- function(train, other) {
 for (v in names(train)) {
   if(is.numeric(train[[v]])) {
     med<-stats::median(train[[v]],na.rm=TRUE)
     if(!is.finite(med))med<-0
     train[[v]][is.na(train[[v]])]<-med
     other[[v]][is.na(other[[v]])]<-med
   } else {
     vals<-as.character(train[[v]]); tbl<-sort(table(vals),decreasing=TRUE)
     mode<-if(length(tbl))names(tbl)[1] else 'Unknown'
     lev<-unique(c(as.character(stats::na.omit(train[[v]])),as.character(stats::na.omit(other[[v]])),mode))
     train[[v]]<-factor(ifelse(is.na(vals),mode,vals),levels=lev)
     oz<-as.character(other[[v]]);other[[v]]<-factor(ifelse(is.na(oz),mode,oz),levels=lev)
   }
 }
 list(train=train,other=other)
}
fhs_metrics <- function(y,p,type='binary') {
 good<-is.finite(p)&!is.na(y);y<-y[good];p<-p[good]
 if(length(y)<5) return(c(n=length(y),rmse=NA,mae=NA,r2=NA,accuracy=NA,precision=NA,recall=NA,specificity=NA,f1=NA,brier=NA,auc=NA))
 if(type=='regression') return(c(n=length(y),rmse=sqrt(mean((y-p)^2)),mae=mean(abs(y-p)),r2=if(stats::var(y)>0) 1-sum((y-p)^2)/sum((y-mean(y))^2) else NA_real_))
 yy<-if(is.factor(y))as.integer(as.character(y)=='Yes') else as.integer(y)
 pp<-as.integer(p>=.5);tp<-sum(pp==1 & yy==1);fp<-sum(pp==1 & yy==0);fn<-sum(pp==0 & yy==1);tn<-sum(pp==0 & yy==0)
 safe<-function(a,b) if(b) a/b else NA_real_
 prec<-safe(tp,tp+fp);rec<-safe(tp,tp+fn)
 auc<-NA_real_
 if(length(unique(yy))==2) auc<-(sum(rank(p)[yy==1])-sum(yy==1)*(sum(yy==1)+1)/2)/(sum(yy==1)*sum(yy==0))
 c(n=length(yy),accuracy=mean(pp==yy),precision=prec,recall=rec,specificity=safe(tn,tn+fp),f1=if(is.finite(prec+rec)&&prec+rec>0)2*prec*rec/(prec+rec) else NA_real_,brier=mean((yy-p)^2),auc=auc)
}
fhs_fit_model <- function(method,train,validation,test,outcome,predictors,type,seed=2026) {
 requireNamespace('stats')
 dat<-train[,c(outcome,predictors),drop=FALSE]; valid<-validation[,c(outcome,predictors),drop=FALSE]; tst<-test[,c(outcome,predictors),drop=FALSE]
 imp<-fhs_impute(dat[predictors],valid[predictors]);dat[predictors]<-imp$train;valid[predictors]<-imp$other
 imp2<-fhs_impute(dat[predictors],tst[predictors]);tst[predictors]<-imp2$other
 fm<-stats::reformulate(predictors,response=outcome)
 if(type=='binary') {
   dat[[outcome]]<-factor(dat[[outcome]],levels=c('No','Yes'));valid[[outcome]]<-factor(valid[[outcome]],levels=c('No','Yes'));tst[[outcome]]<-factor(tst[[outcome]],levels=c('No','Yes'))
   if(length(unique(stats::na.omit(dat[[outcome]])))<2)stop('Both outcome classes must appear in training.')
 }
 set.seed(seed)
 family <- if (type=='binary')stats::binomial() else stats::gaussian()
 model <- switch(method,
   'GLM / linear' = stats::glm(fm,data=dat,family=family),
   'Decision tree'= {if(!requireNamespace('rpart',quietly=TRUE))stop('Install rpart');rpart::rpart(fm,data=dat,method=if(type=='binary')'class' else 'anova',control=rpart::rpart.control(cp=.01,minsplit=20))},
   'Random forest'={if(!requireNamespace('randomForest',quietly=TRUE))stop('Install randomForest');randomForest::randomForest(fm,data=dat,ntree=180)},
   'Gradient boosting'={if(!requireNamespace('gbm',quietly=TRUE))stop('Install gbm');gbm::gbm(fm,data=if(type=='binary') {tmp<-dat;tmp[[outcome]]<-as.integer(tmp[[outcome]]=='Yes');tmp} else dat,distribution=if(type=='binary')'bernoulli' else 'gaussian',n.trees=120,interaction.depth=2,shrinkage=.05,bag.fraction=.75,verbose=FALSE)},
   'Support vector machine'={if(!requireNamespace('e1071',quietly=TRUE))stop('Install e1071');e1071::svm(fm,data=dat,probability=(type=='binary'),scale=TRUE)},
   'XGBoost'={if(!requireNamespace('xgboost',quietly=TRUE))stop('Install xgboost'); m<-stats::model.matrix(stats::delete.response(stats::terms(fm)),dat)[,-1,drop=FALSE];yy<-if(type=='binary')as.integer(dat[[outcome]]=='Yes') else as.numeric(dat[[outcome]]);list(booster=xgboost::xgboost(data=m,label=yy,nrounds=100,max_depth=3,eta=.07,objective=if(type=='binary')'binary:logistic' else 'reg:squarederror',verbose=0,nthread=2),columns=colnames(m),terms=stats::delete.response(stats::terms(fm)))},
   stop('Unsupported algorithm')
 )
 predict_one<-function(nd){
   p <- switch(method,
   'GLM / linear'=stats::predict(model,newdata=nd,type='response'),
   'Decision tree'=if(type=='binary')predict(model,newdata=nd,type='prob')[,'Yes'] else as.numeric(predict(model,newdata=nd)),
   'Random forest'=if(type=='binary')predict(model,newdata=nd,type='prob')[,'Yes'] else as.numeric(predict(model,newdata=nd)),
   'Gradient boosting'= {if(type=='binary') { # gbm uses 0/1 response, see fit stage
       as.numeric(predict(model,newdata=nd,n.trees=120,type='response'))
      } else as.numeric(predict(model,newdata=nd,n.trees=120,type='response'))},
   'Support vector machine'=if(type=='binary') {z<-predict(model,newdata=nd,probability=TRUE);pr<-attr(z,'probabilities');if('Yes' %in% colnames(pr))pr[,'Yes'] else as.numeric(z=='Yes')} else as.numeric(predict(model,newdata=nd)),
   'XGBoost'={m<-stats::model.matrix(model$terms,nd)[,-1,drop=FALSE];new<-matrix(0,nrow(m),length(model$columns),dimnames=list(NULL,model$columns));common<-intersect(colnames(m),model$columns);new[,common]<-m[,common,drop=FALSE];as.numeric(predict(model$booster,new))}
   ); as.numeric(p)
 }
 list(method=method,model=model,validation=fhs_metrics(valid[[outcome]],predict_one(valid),if(type=='binary')'binary' else 'regression'),test=fhs_metrics(tst[[outcome]],predict_one(tst),if(type=='binary')'binary' else 'regression'),test_predictions=data.frame(observed=as.character(tst[[outcome]]),prediction=predict_one(tst)),validation_data=valid,predict_fun=predict_one)
}
fhs_supervised <- function(x,outcome,predictors,methods,train=.7,val=.15,seed=2026) {
 d<-fhs_model_frame(x,outcome,predictors)
 type<-if(is.factor(d[[outcome]]) && all(stats::na.omit(as.character(d[[outcome]])) %in% c('No','Yes'))) 'binary' else if(is.numeric(d[[outcome]])) 'regression' else stop('Outcome must be numeric or Yes/No.')
 s<-fhs_split(nrow(d),train,val,seed); results<-list(); errors<-character()
 for(m in methods) tryCatch({results[[m]]<-fhs_fit_model(m,d[s$train,,drop=FALSE],d[s$validation,,drop=FALSE],d[s$test,,drop=FALSE],outcome,predictors,type,seed)},error=function(e)errors[[m]]<<-conditionMessage(e))
 if(!length(results)) stop(paste('No model fitted:',paste(names(errors),errors,collapse='; ')))
 metric<-if(type=='binary')'auc' else 'rmse'
 tbl<-do.call(rbind,lapply(results,function(r){q<-r$validation;z<-r$test
  extra<-if(type=='binary') c('accuracy','precision','recall','specificity','f1','brier','auc') else c('rmse','mae','r2')
  row<-data.frame(model=r$method,validation_metric=unname(q[metric]),test_metric=unname(z[metric]),test_n=unname(z['n']))
  for(k in extra) row[[paste0('test_',k)]]<-unname(z[k])
  row
 }))
 o<-order(if(type=='binary') -ifelse(is.finite(tbl$validation_metric),tbl$validation_metric,-Inf) else ifelse(is.finite(tbl$validation_metric),tbl$validation_metric,Inf))
 tbl<-tbl[o,,drop=FALSE];list(table=tbl,results=results,best=tbl$model[1],type=type,metric=metric,errors=errors,predictors=predictors,outcome=outcome,split=s,source=d,ids=if('randid'%in%names(x))x[as.integer(rownames(d)),'randid'] else seq_len(nrow(d)))
}
fhs_importance <- function(fit,max_vars=12,seed=2026) {
 r<-fit$results[[fit$best]];s<-fit$split;d<-r$validation_data;out<-fit$outcome
 good<-!is.na(d[[out]]);d<-d[good,,drop=FALSE]
 # Permutation feature importance, validation-only; avoids misleading native methods across algorithms.
 score<-function(p) {q<-fhs_metrics(d[[out]],p,if(fit$type=='binary')'binary' else 'regression');unname(q[fit$metric])}
 base<-score(r$predict_fun(d));set.seed(seed)
 imp<-sapply(fit$predictors,function(v){b<-d; b[[v]]<-sample(b[[v]]); sc<-tryCatch(score(r$predict_fun(b)),error=function(e)NA_real_);if(fit$type=='binary')base-sc else sc-base})
 z<-data.frame(feature=names(imp),importance=as.numeric(imp));z<-z[order(z$importance,decreasing=TRUE),,drop=FALSE];utils::head(z,max_vars)
}
fhs_survival_frame <- function(x,event='anychd',covars=NULL,exclude_prevalent=TRUE) {
 tm<-unname(FHS_TIME[event]);if(length(tm)!=1 || !all(c(event,tm)%in%names(x)))stop('Event or follow-up time absent.')
 d<-x
 if(exclude_prevalent && event=='anychd' && 'prevchd1'%in%names(d))d<-d[!is.na(d$prevchd1)&as.character(d$prevchd1)=='No',,drop=FALSE]
 keep<-unique(c(event,tm,covars));d<-d[,keep,drop=FALSE]
 d$time_years<-as.numeric(d[[tm]]);d$event_status<-as.integer(as.character(d[[event]])=='Yes')
 d<-d[is.finite(d$time_years)&d$time_years>0&!is.na(d$event_status),,drop=FALSE]
 d
}
fhs_surv_models <- function(x,event,predictors,methods,train=.7,val=.15,seed=2026,exclude_prevalent=TRUE) {
 d<-fhs_survival_frame(x,event,predictors,exclude_prevalent);d<-d[,unique(c('time_years','event_status',predictors)),drop=FALSE]
 if(sum(d$event_status)<15)stop('Too few events for survival model comparison.')
 s<-fhs_split(nrow(d),train,val,seed)
 tr<-d[s$train,,drop=FALSE];va<-d[s$validation,,drop=FALSE];te<-d[s$test,,drop=FALSE]
 ip<-fhs_impute(tr[predictors],va[predictors]);tr[predictors]<-ip$train;va[predictors]<-ip$other
 ip<-fhs_impute(tr[predictors],te[predictors]);te[predictors]<-ip$other
 fm<-stats::as.formula(paste('survival::Surv(time_years,event_status) ~ ',paste(sprintf('`%s`',predictors),collapse='+')))
 rows<-list();mods<-list();err<-character()
 for(m in methods)tryCatch({
   if(m=='Cox PH') {
     mod<-survival::coxph(fm,data=tr,x=TRUE);risk<-as.numeric(stats::predict(mod,newdata=te,type='lp'))
   } else if(m=='Stratified Cox') {
     f<-stats::as.formula(paste('survival::Surv(time_years,event_status) ~',paste(sprintf('`%s`',setdiff(predictors,'sex1')),collapse='+'),' + strata(sex1)'))
     if(!'sex1'%in%predictors || length(predictors)<2)stop('Select sex1 and at least one additional predictor.')
     mod<-survival::coxph(f,data=tr,x=TRUE);risk<-as.numeric(stats::predict(mod,newdata=te,type='lp'))
   } else if(m %in% c('Weibull AFT','Lognormal AFT','Loglogistic AFT')){
     dist<-switch(m,'Weibull AFT'='weibull','Lognormal AFT'='lognormal','Loglogistic AFT'='loglogistic')
     mod<-survival::survreg(fm,data=tr,dist=dist);risk<- -as.numeric(stats::predict(mod,newdata=te,type='lp'))
   } else if(m=='Survival tree'){
     if(!requireNamespace('rpart',quietly=TRUE))stop('Install rpart');mod<-rpart::rpart(fm,data=tr,method='exp');risk<- -as.numeric(stats::predict(mod,newdata=te))
   } else if(m=='Random survival forest (ranger)'){
     if(!requireNamespace('ranger',quietly=TRUE))stop('Install ranger');mod<-ranger::ranger(fm,data=tr,num.trees=200,seed=seed);risk<-rowSums(predict(mod,data=te)$chf)
   } else if(m=='Random survival forest (SRC)'){
     if(!requireNamespace('randomForestSRC',quietly=TRUE))stop('Install randomForestSRC');mod<-randomForestSRC::rfsrc(fm,data=tr,ntree=200);risk<-as.numeric(predict(mod,newdata=te)$predicted)
   } else if(m=='Oblique survival forest'){
     if(!requireNamespace('aorsf',quietly=TRUE))stop('Install aorsf');mod<-aorsf::orsf(fm,data=tr, n_tree=150);risk<-as.numeric(predict(mod,new_data=te,pred_type='risk'))
   } else if(m=='Bayesian survival (rstanarm)'){
     if(!requireNamespace('rstanarm',quietly=TRUE))stop('Install rstanarm (Stan toolchain required).')
     mod<-rstanarm::stan_surv(fm,data=tr,basehaz='weibull',chains=2,iter=600,refresh=0,seed=seed)
     risk<- -as.numeric(stats::predict(mod,newdata=te,type='response'))
   } else stop('Unsupported survival model')
   risk_val <- if(m %in% c('Cox PH','Stratified Cox')) as.numeric(stats::predict(mod,newdata=va,type='lp')) else if(m %in% c('Weibull AFT','Lognormal AFT','Loglogistic AFT')) -as.numeric(stats::predict(mod,newdata=va,type='lp')) else if(m=='Survival tree') -as.numeric(stats::predict(mod,newdata=va)) else if(m=='Random survival forest (ranger)') rowSums(predict(mod,data=va)$chf) else if(m=='Random survival forest (SRC)') as.numeric(predict(mod,newdata=va)$predicted) else if(m=='Oblique survival forest') as.numeric(predict(mod,new_data=va,pred_type='risk')) else -as.numeric(stats::predict(mod,newdata=va,type='response'))
   ci<-survival::concordance(survival::Surv(te$time_years,te$event_status)~risk,reverse=TRUE)$concordance
   vi<-survival::concordance(survival::Surv(va$time_years,va$event_status)~risk_val,reverse=TRUE)$concordance
   rows[[m]]<-data.frame(model=m,validation_c_index=vi,test_c_index=ci,test_n=nrow(te),test_events=sum(te$event_status))
   mods[[m]]<-mod
 },error=function(e)err[[m]]<<-conditionMessage(e))
 if(!length(rows))stop(paste('No survival model could fit:',paste(names(err),err,collapse='; ')))
 table<-do.call(rbind,rows);table<-table[order(table$validation_c_index,decreasing=TRUE),,drop=FALSE];list(table=table,models=mods,errors=err,data=d,split=s)
}
fhs_km <- function(d,group=NULL) {
 if(!requireNamespace('survival',quietly=TRUE))stop('Install survival.')
 if(is.null(group) || !nzchar(group)) return(survival::survfit(survival::Surv(time_years,event_status)~1,data=d))
 d$group<-as.factor(d[[group]]);survival::survfit(survival::Surv(time_years,event_status)~group,data=d)
}
fhs_cluster <- function(x,features,methods=c('K-means','Hierarchical Ward','PAM','DBSCAN'),k=3,seed=2026) {
 d<-x[,features,drop=FALSE];d<-d[,vapply(d,is.numeric,logical(1)),drop=FALSE]
 d<-d[,vapply(d,function(v)sum(is.finite(v))>2 && stats::sd(v,na.rm=TRUE)>0,logical(1)),drop=FALSE]
 if(ncol(d)<2 || nrow(d)<15)stop('Select two or more varying numeric measurements.')
 for(v in names(d)){median_value<-stats::median(d[[v]],na.rm=TRUE);d[[v]][!is.finite(d[[v]])]<-median_value}
 # Distance-based clustering on >1,200 records is expensive in a hosted Shiny session.
 if(nrow(d)>1200L){set.seed(seed);d<-d[sample.int(nrow(d),1200L),,drop=FALSE]}
 z<-scale(d);pc<-stats::prcomp(z,center=FALSE,scale.=FALSE)$x[,1:2,drop=FALSE];set.seed(seed)
 results<-list();stats<-list();errors<-character()
 for(m in methods)tryCatch({
   labels<-switch(m,'K-means'=stats::kmeans(z,centers=k,nstart=12)$cluster,
     'Hierarchical Ward'=stats::cutree(stats::hclust(stats::dist(z),method='ward.D2'),k=k),
     'PAM'={if(!requireNamespace('cluster',quietly=TRUE))stop('Install cluster');cluster::pam(z,k=k)$clustering},
     'DBSCAN'={if(!requireNamespace('dbscan',quietly=TRUE))stop('Install dbscan');dbscan::dbscan(z,eps=as.numeric(stats::quantile(dbscan::kNNdist(z,k=5),.85)),minPts=6)$cluster})
   results[[m]]<-data.frame(PC1=pc[,1],PC2=pc[,2],cluster=factor(ifelse(labels==0,'Noise',paste0('Cluster ',labels))))
   nonnoise<-labels!=0;sil<-NA_real_
   if(sum(nonnoise)<=1000 && length(unique(labels[nonnoise]))>1 && sum(nonnoise)>length(unique(labels[nonnoise]))) {
      sil<-mean(cluster::silhouette(as.integer(factor(labels[nonnoise])),stats::dist(z[nonnoise,,drop=FALSE]))[,'sil_width'])
   }
   stats[[m]]<-data.frame(method=m,clusters=length(unique(labels[nonnoise])),noise=sum(!nonnoise),silhouette=sil)
 },error=function(e)errors[[m]]<<-conditionMessage(e))
 if(!length(stats))stop(paste('Clustering failed:',paste(errors,collapse='; ')))
 list(plots=results,table=do.call(rbind,stats),errors=errors)
}
