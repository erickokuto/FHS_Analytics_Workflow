# Framingham Heart Study: Analytics Workflow (teaching / methodological exploration).
source('R/core.R',local=TRUE)
source('R/importance.R',local=TRUE)
source('R/plots.R',local=TRUE)
library(shiny)
library(ggplot2)
library(DT)
library(plotly)
library(survival)
options(shiny.maxRequestSize=75*1024^2)
base_data <- fhs_prepare(fhs_load('fhs.csv'))
text_help <- function(s) tags$p(class='fhs-note',s)
dt <- function(x) DT::datatable(x,options=list(pageLength=8,scrollX=TRUE),rownames=FALSE,filter='top')
style <- tags$head(tags$style(HTML('
 body{background:#f5f8fb;color:#1a3140} .navbar-default{background:#083b52;border:0} .navbar-default .navbar-nav>li>a,.navbar-default .navbar-brand{color:white!important} .well{background:white;border:1px solid #dfebf0;border-radius:9px} .fhs-note{color:#506879;font-size:13px} .btn-primary{background:#087e92;border-color:#087e92} .shiny-output-error{color:#a12626} h3{color:#09394f;font-weight:650} .container-fluid{padding:16px 25px}')))
ui <- navbarPage(title='FHS | Analytics Workflow',header=style,id='tabs',
 tabPanel('Data and cleaning',sidebarLayout(sidebarPanel(fileInput('file','Upload one FHS CSV',accept='.csv'),checkboxInput('exclude_prev','Exclude prevalent CHD (for incident CHD analysis)',FALSE),actionButton('run_clean','Update data management',class='btn-primary'),hr(),numericInput('train','Training (%)',70,min=40,max=90),numericInput('val','Validation (%)',15,min=5,max=35),numericInput('test','Testing (%)',15,min=5,max=35),textOutput('split_info'),text_help('Only one data file is uploaded. Split assignments are created internally at model fitting.')),mainPanel(h3('Data audit and feature engineering'),verbatimTextOutput('shape'),DTOutput('quality'),downloadButton('download_clean','Download cleaned data'),h3('Data preview'),DTOutput('preview')))),
 tabPanel('Exploratory analysis',sidebarLayout(sidebarPanel(uiOutput('eda_num_ui'),uiOutput('eda_group_ui'),uiOutput('eda_x_ui'),actionButton('run_eda','Run / refresh all exploratory graphics',class='btn-primary')),mainPanel(h3('Distribution and group exploration'),plotlyOutput('hist',height='300px'),plotlyOutput('box',height='300px'),plotlyOutput('density',height='300px'),plotlyOutput('scatter',height='300px'),plotlyOutput('missing',height='300px')))),
 tabPanel('Descriptive statistics',sidebarLayout(sidebarPanel(actionButton('run_desc','Update all descriptive tables',class='btn-primary'),text_help('Every numeric variable: valid n, missing, mean, SD, median, quartiles, minimum and maximum. Every categorical variable: category n and percentage, including missing.')),mainPanel(h3('Continuous variables'),DTOutput('numeric_table'),h3('Categorical variables'),DTOutput('categorical_table')))),
 tabPanel('Statistical inference',sidebarLayout(sidebarPanel(selectInput('test_type','Statistical test',c('Variance equality (Levene)','Independent Welch t-test','Pooled independent t-test','Paired t-test','One-way ANOVA','Two-way ANOVA','ANCOVA','Chi-square / Fisher exact','McNemar paired test','Mantel-Haenszel stratified test')),uiOutput('inf_vars'),actionButton('run_infer','Run statistical test and update correlation matrix',class='btn-primary'),text_help('Choose variables appropriate to the test; missing observations are excluded for the selected test only.')),mainPanel(h3('Test output'),verbatimTextOutput('infer_result'),h3('Correlation matrix: numeric variables'),plotlyOutput('corr',height='520px'),text_help('Pearson correlation uses pairwise complete data; inspect missingness and potential non-linearity.')))),
 tabPanel('Supervised learning',sidebarLayout(sidebarPanel(uiOutput('super_y'),uiOutput('super_features'),checkboxGroupInput('methods','Compare algorithms',choices=c('GLM / linear','Decision tree','Random forest','Gradient boosting','Support vector machine','XGBoost'),selected=c('GLM / linear','Decision tree','Random forest')),numericInput('seed','Random seed',2026,min=1),actionButton('run_super','Train and compare models',class='btn-primary'),actionButton('run_imp','Refresh supervised importance'),downloadButton('export_evidence','Export results for Decision Support'),text_help('Validation selects the best model; held-out test is shown separately. Baseline covariates only, to avoid post-outcome leakage.')),mainPanel(h3('Model comparison'),DTOutput('models'),plotlyOutput('model_plot'),h3('Best-model feature importance'),plotlyOutput('importance'),h3('Held-out outcomes and predictions'),plotlyOutput('test_results'),plotlyOutput('roc_plot'),h3('Model diagnostics'),verbatimTextOutput('model_errors')))),
 tabPanel('Count regression',sidebarLayout(sidebarPanel(selectInput('count_y','Count outcome',choices=c('illustrative_event_burden')),checkboxGroupInput('count_methods','Models',c('Poisson','Negative binomial','Zero-inflated Poisson','Zero-inflated NB','Zero-truncated Poisson','Zero-truncated NB'),selected=c('Poisson','Negative binomial')),actionButton('run_count','Fit and compare count models',class='btn-primary'),actionButton('refresh_count_importance','Refresh count-model importance'),text_help('If a selected model fails, the diagnostic panel reports its cause; fit Poisson first. The sample FHS dataset has no independent recurrent-event count. Illustrative event burden sums overlapping outcomes and is for teaching only. Truncated models condition on counts >0.')),mainPanel(h3('Count model comparison'),DTOutput('count_table'),plotlyOutput('count_plot'),h3('Best count model: validation permutation importance'),plotlyOutput('count_importance',height='400px'),h3('Count-model exponentiated coefficient estimates'),plotlyOutput('count_effects',height='380px'),verbatimTextOutput('count_errors')))),
 tabPanel('Clustering',sidebarLayout(sidebarPanel(uiOutput('clust_features'),checkboxGroupInput('clust_methods','Compare clustering algorithms',choices=c('K-means','Hierarchical Ward','PAM','DBSCAN'),selected=c('K-means','Hierarchical Ward','PAM')),sliderInput('k','Number of groups (non-DBSCAN)',min=2,max=8,value=3),actionButton('run_cluster','Run / refresh clustering and PCA',class='btn-primary'),text_help('Distance-based clustering uses a reproducible maximum 1,200-record sample to control server memory.')),mainPanel(h3('Method comparison'),DTOutput('cluster_table'),selectInput('cluster_view','Colour-coded PCA projection',choices=c('K-means','Hierarchical Ward','PAM','DBSCAN')),plotlyOutput('cluster_plot',height='440px'),verbatimTextOutput('cluster_errors')))),
 tabPanel('Survival exploration',sidebarLayout(sidebarPanel(uiOutput('surv_event'),uiOutput('surv_group'),selectInput('surv_curve','Curve',choices=c('Kaplan-Meier','Nelson-Aalen cumulative hazard','Cumulative incidence (1-S)')),actionButton('run_surv','Fit / refresh survival curves and tests',class='btn-primary'),text_help('Use matched event/time fields. Baseline prevalent CHD can be excluded for first incident CHD. Standard KM estimates do not adjust for competing causes.')),mainPanel(h3('Time-to-event curves'),plotlyOutput('surv_curveplot',height='440px'),h3('Log-rank, weighted tests and median survival'),verbatimTextOutput('surv_test'),DTOutput('surv_summary')))),
 tabPanel('Survival models',sidebarLayout(sidebarPanel(uiOutput('surv_event_model'),uiOutput('surv_features'),checkboxGroupInput('surv_methods','Compare survival models',choices=c('Cox PH','Stratified Cox','Weibull AFT','Lognormal AFT','Loglogistic AFT','Survival tree','Random survival forest (ranger)','Random survival forest (SRC)','Oblique survival forest','Bayesian survival (rstanarm)'),selected=c('Cox PH','Weibull AFT','Random survival forest (ranger)')),actionButton('run_surv_mod','Fit and evaluate survival models',class='btn-primary'),actionButton('refresh_survival_importance','Refresh survival-model importance'),text_help('Test-set Harrell concordance is displayed. Optional forests and Bayesian Stan models need additional packages; Bayesian fitting is computationally intensive.')),mainPanel(h3('Survival model comparison'),DTOutput('surv_mod_tbl'),plotlyOutput('surv_mod_plot'),h3('Best survival model: validation permutation importance'),plotlyOutput('surv_importance',height='400px'),h3('Cox regression hazard ratios (when fitted)'),plotlyOutput('surv_effects',height='400px'),h3('Cox proportional-hazards diagnostics'),verbatimTextOutput('cox_assumptions'),h3('Unfitted algorithms and reasons'),verbatimTextOutput('surv_mod_errors')))),
 tabPanel('Confounding and effect modification',sidebarLayout(sidebarPanel(uiOutput('adj_event'),uiOutput('adj_exposure'),uiOutput('adj_conf'),uiOutput('adj_modifier'),actionButton('run_adjust','Update adjustment and interaction analysis',class='btn-primary'),actionButton('refresh_adjust_effects','Refresh adjusted hazard ratio plot')),mainPanel(h3('Unadjusted versus adjusted Cox models'),DTOutput('adjust_table'),h3('Crude and adjusted hazard ratios with 95% confidence intervals'),plotlyOutput('adjust_effects',height='420px'),h3('Effect modification and proportional hazards'),verbatimTextOutput('adjust_summary'),text_help('An interaction is not proof of causal effect modification. Mediation requires separate identification assumptions and is not estimated from these retrospective observations.')))),
 tabPanel('Method notes',fluidPage(h3('Study context and safe interpretation'),p('Framingham Heart Study longitudinal teaching extract: 4,434 records in the uploaded file, 75 original variables, repeated examinations and follow-up outcomes. The dataset includes 194 prevalent CHD cases at baseline; the incident-CHD demonstration excludes them by default in survival tabs.'),p('Most models use only first-examination variables as predictors to avoid using later follow-up information to predict earlier outcomes. Missingness in examination 2/3 is not missing at random by assumption.'),p('Statistical comparisons are exploratory and do not establish causality or clinical suitability. These teaching models should not be used for medical diagnosis or individual treatment.'),tags$a(href='https://biolincc.nhlbi.nih.gov/media/teachingstudies/FHS_Teaching_Longitudinal_Data_Documentation_2021a.pdf',target='_blank','FHS teaching data dictionary (NHLBI)'),br(),tags$a(href='https://cran.r-project.org/web/views/Survival.html',target='_blank','CRAN Survival Analysis task view')))
)
server <- function(input,output,session) {
 raw <- reactive({if(is.null(input$file))base_data else fhs_prepare(fhs_load(input$file$datapath))})
 valid_split <- reactive({req(input$train,input$val,input$test);validate(need(input$train+input$val+input$test==100,'Training, validation and testing percentages must total 100.'));list(train=input$train/100,val=input$val/100)})
 output$split_info <- renderText({paste('Training',input$train,'%, validation',input$val,'%, testing',input$test,'%. Total must be 100%.')})
 clean <- eventReactive(input$run_clean,{x<-raw();if(isTRUE(input$exclude_prev))x<-fhs_prepare(x,TRUE);x},ignoreNULL=FALSE)
 output$shape<-renderPrint({x<-clean();cat('Participants:',nrow(x),'\nVariables:',ncol(x),'\nUnique IDs:',if('randid'%in%names(x))length(unique(x$randid)) else NA,'\n')})
 output$quality<-renderDT(dt(fhs_catalogue(clean())))
 output$preview<-renderDT(dt(utils::head(clean(),100)))
 output$download_clean<-downloadHandler(filename=function()'fhs_cleaned.csv',content=function(file)write.csv(clean(),file,row.names=FALSE))
 output$eda_num_ui<-renderUI(selectInput('eda_num','Numeric measurement',choices=fhs_numeric(clean()),selected='sysbp1'))
 output$eda_group_ui<-renderUI(selectInput('eda_group','Group',choices=fhs_cat(clean()),selected='sex1'))
 output$eda_x_ui<-renderUI(selectInput('eda_x','Second numeric measurement',choices=fhs_numeric(clean()),selected='age1'))
 eda <- eventReactive(input$run_eda,{req(input$eda_num,input$eda_group,input$eda_x);x<-clean();validate(need(all(c(input$eda_num,input$eda_group,input$eda_x)%in%names(x)),'Variables no longer exist.'));list(data=x,num=input$eda_num,grp=input$eda_group,x=input$eda_x)},ignoreNULL=FALSE)
 output$hist<-renderPlotly({e<-eda();fhs_plot_hist(e$data,e$num,paste('Distribution of',e$num))})
 output$box<-renderPlotly({e<-eda();fhs_plot_box(e$data,e$num,e$grp,paste(e$num,'by',e$grp))})
 output$density<-renderPlotly({e<-eda();fhs_plot_density(e$data,e$num,e$grp,'Density by category')})
 output$scatter<-renderPlotly({e<-eda();fhs_plot_scatter(e$data,e$x,e$num,e$grp,'Exploration of two continuous measures')})
 output$missing<-renderPlotly({e<-eda();a<-head(fhs_catalogue(e$data)[order(fhs_catalogue(e$data)$pct_missing,decreasing=TRUE),],25);fhs_plot_bars(a$variable,a$pct_missing,'Missing data: 25 most affected variables','Missing (%)')})
 desc <- eventReactive(input$run_desc,clean(),ignoreNULL=FALSE)
 output$numeric_table<-renderDT(dt(round_numeric(fhs_describe(desc()))))
 output$categorical_table<-renderDT(dt(fhs_counts(desc())))
 output$inf_vars<-renderUI({x<-clean();tagList(selectInput('inf_y','Measurement or outcome',choices=names(x),selected='sysbp1'),selectInput('inf_g','Group or comparison variable',choices=names(x),selected='sex1'),selectInput('inf_g2','Second group / paired variable',choices=names(x),selected='cursmoke1'),selectInput('inf_strata','Stratification variable',choices=names(x),selected='sex1'))})
 infer<-eventReactive(input$run_infer,{x<-clean();req(input$inf_y,input$inf_g,input$inf_g2,input$inf_strata);tryCatch({a<-input$inf_y;b<-input$inf_g;c<-input$inf_g2;s<-input$inf_strata;f<-reformulate(b,response=a);d<-x[,unique(c(a,b,c,s)),drop=FALSE]
 switch(input$test_type,
 'Variance equality (Levene)'={ok<-!is.na(d[[a]]) & !is.na(d[[b]]);q<-d[ok,c(a,b),drop=FALSE];q$group<-factor(q[[b]]);if(nlevels(q$group)<2)stop('At least two groups required.');q$deviation<-abs(q[[a]]-ave(q[[a]],q$group,FUN=median));summary(stats::aov(deviation~group,data=q))},
 'Independent Welch t-test'=stats::t.test(f,data=d,var.equal=FALSE),
 'Pooled independent t-test'=stats::t.test(f,data=d,var.equal=TRUE),
 'Paired t-test'=stats::t.test(d[[a]],d[[b]],paired=TRUE),
 'One-way ANOVA'=summary(stats::aov(f,data=d)),
 'Two-way ANOVA'=summary(stats::aov(reformulate(c(b,c),response=a),data=d)),
 'ANCOVA'=summary(stats::aov(reformulate(c(b,c),response=a),data=d)),
 'Chi-square / Fisher exact'={tab<-table(d[[a]],d[[b]]);if(any(dim(tab)<2))stop('At least two categories per variable are required.');if(any(suppressWarnings(stats::chisq.test(tab)$expected)<5))stats::fisher.test(tab) else stats::chisq.test(tab)},
 'McNemar paired test'=stats::mcnemar.test(table(d[[a]],d[[b]])),
 'Mantel-Haenszel stratified test'={tab<-table(d[[a]],d[[b]],d[[s]]);if(any(dim(tab)[1:2]!=2))stop('Mantel-Haenszel requires two binary variables.');stats::mantelhaen.test(tab)})},error=function(e)paste('Test could not run:',conditionMessage(e)))},ignoreNULL=FALSE)
 output$infer_result<-renderPrint(print(infer()))
 output$corr<-renderPlotly({req(input$run_infer>=0);x<-clean();nums<-fhs_numeric(x);nums<-nums[vapply(x[nums],function(z)sum(is.finite(z))>3&&stats::sd(z,na.rm=TRUE)>0,logical(1))];validate(need(length(nums)>1,'Need at least two varying numeric fields.'));nums<-head(nums,20);z<-stats::cor(x[,nums,drop=FALSE],use='pairwise.complete.obs');plotly::layout(plotly::plot_ly(x=colnames(z),y=rownames(z),z=z,type='heatmap',zmin=-1,zmax=1,colors=c('#b2182b','#f7f7f7','#2166ac')),title='Pearson correlation matrix')})
 output$super_y<-renderUI(selectInput('super_out','Model outcome',choices=unique(c(intersect(FHS_EVENTS,names(clean())),fhs_numeric(clean()))),selected='anychd'))
 output$super_features<-renderUI(checkboxGroupInput('super_pred','Baseline predictors',choices=fhs_predictors(clean(),input$super_out%||%'anychd'),selected=intersect(c('age1','sex1','sysbp1','bmi1','cursmoke1','diabetes1'),fhs_predictors(clean(),input$super_out%||%'anychd'))))
 supervised<-eventReactive(input$run_super,{sp<-valid_split();req(input$methods,input$super_pred,input$super_out);fhs_supervised(clean(),input$super_out,input$super_pred,input$methods,sp$train,sp$val,input$seed)},ignoreNULL=TRUE)
 output$models<-renderDT({req(supervised());dt(supervised()$table)})
 output$model_plot<-renderPlotly({z<-supervised();fhs_plot_bars(z$table$model,z$table$validation_metric,paste('Validation model comparison:',z$metric),z$metric)})
 importance<-eventReactive(list(input$run_super,input$run_imp),{req(input$run_super > 0);fhs_importance(supervised())},ignoreNULL=FALSE)
 output$importance<-renderPlotly({z<-importance();fhs_importance_plot(z,paste('Best validation model:',supervised()$best))})
 output$test_results<-renderPlotly({z<-supervised();fhs_plot_predictions(z$results[[z$best]]$test_predictions,z$type)})
 output$export_evidence<-downloadHandler(filename=function()'fhs_analytics_to_decision_support.csv',content=function(file){z<-supervised();d<-z$results[[z$best]]$test_predictions;d$randid<-z$ids[z$split$test];d$model<-z$best;d$outcome<-z$outcome;write.csv(d[,c('randid','model','outcome','observed','prediction')],file,row.names=FALSE)})
 output$roc_plot<-renderPlotly({
    z<-supervised();d<-z$results[[z$best]]$test_predictions
    p<-as.numeric(d$prediction)
    if(z$type=='binary'){
      yy<-as.integer(as.character(d$observed)=='Yes');ok<-is.finite(p)&!is.na(yy)
      yy<-yy[ok];p<-p[ok]
      validate(need(length(unique(yy))==2,'ROC requires both observed event classes in the held-out test set.'))
      thresholds<-sort(unique(c(0,1,p)),decreasing=TRUE)
      roc<-do.call(rbind,lapply(thresholds,function(t){q<-p>=t;data.frame(tpr=sum(q & yy==1)/sum(yy==1),fpr=sum(q & yy==0)/sum(yy==0))}))
      plotly::plot_ly(x=roc$fpr,y=roc$tpr,type='scatter',mode='lines',line=list(color='#087e92',width=2)) |>
        plotly::layout(title='Held-out test ROC curve',xaxis=list(title='False positive rate',range=c(0,1)),yaxis=list(title='True positive rate',range=c(0,1)),shapes=list(list(type='line',x0=0,y0=0,x1=1,y1=1,line=list(dash='dash'))))
    }else{
      y<-suppressWarnings(as.numeric(d$observed));ok<-is.finite(y)&is.finite(p)
      validate(need(sum(ok)>2,'Not enough numeric test records for residual diagnostics.'))
      plotly::plot_ly(x=p[ok],y=y[ok]-p[ok],type='scatter',mode='markers',marker=list(color='#087e92')) |>
        plotly::layout(title='Held-out test residuals',xaxis=list(title='Predicted'),yaxis=list(title='Observed minus predicted'))
    }
  })
 output$model_errors<-renderPrint({z<-supervised();if(length(z$errors))z$errors else cat('All selected models completed.')})
 output$clust_features<-renderUI(checkboxGroupInput('clust_features','Numeric clustering features',choices=fhs_numeric(clean()),selected=intersect(c('age1','sysbp1','diabp1','totchol1','bmi1','glucose1'),fhs_numeric(clean()))))
 clustering<-eventReactive(input$run_cluster,{req(input$clust_features,input$clust_methods);fhs_cluster(clean(),input$clust_features,input$clust_methods,input$k)},ignoreNULL=TRUE)
 output$cluster_table<-renderDT({z<-clustering();dt(z$table)})
 output$cluster_plot<-renderPlotly({z<-clustering();available<-names(z$plots);validate(need(length(available)>0,'No clustering model completed.'));method<-if(input$cluster_view %in% available) input$cluster_view else available[[1]];fhs_plot_pca(z$plots[[method]],paste('PCA clusters:',method))})
 output$cluster_errors<-renderPrint({z<-clustering();if(length(z$errors))z$errors else 'All selected clustering algorithms completed.'})
 output$surv_event<-renderUI(selectInput('surv_ev','Event',choices=intersect(names(FHS_TIME),names(clean())),selected='anychd'))
 output$surv_group<-renderUI(selectInput('surv_grp','Stratify curves',choices=c('Overall'='',fhs_cat(clean())),selected='sex1'))
 survdat<-eventReactive(input$run_surv,{req(input$surv_ev);fhs_survival_frame(clean(),input$surv_ev,if(isTRUE(nzchar(input$surv_grp)))input$surv_grp else NULL,TRUE)},ignoreNULL=FALSE)
 survfit_obj<-reactive({d<-survdat();fhs_km(d,input$surv_grp%||%'')})
 output$surv_curveplot<-renderPlotly({fhs_plot_survival(survfit_obj(),input$surv_curve,paste(input$surv_curve,'by',if(isTRUE(nzchar(input$surv_grp)))input$surv_grp else 'overall cohort'))})
 output$surv_test<-renderPrint({d<-survdat();if(!isTRUE(nzchar(input$surv_grp)))return(cat('Select a grouping variable for log-rank tests.'));d$group<-factor(d[[input$surv_grp]]);print(survival::survdiff(survival::Surv(time_years,event_status)~group,data=d));cat('\nFleming-Harrington weighted test (rho=0.5):\n');print(survival::survdiff(survival::Surv(time_years,event_status)~group,data=d,rho=.5));cat('\nWeighted test (rho=1):\n');print(survival::survdiff(survival::Surv(time_years,event_status)~group,data=d,rho=1))})
 output$surv_summary<-renderDT({s<-survfit_obj();tab<-summary(s)$table;tab<-if(is.null(dim(tab)))data.frame(group='Overall',t(as.matrix(tab)),check.names=FALSE) else data.frame(group=rownames(tab),tab,check.names=FALSE,row.names=NULL);dt(tab)})
 output$surv_event_model<-renderUI(selectInput('surv_mod_event','Outcome for survival models',choices=intersect(names(FHS_TIME),names(clean())),selected='anychd'))
 output$surv_features<-renderUI(checkboxGroupInput('surv_predictors','Baseline survival predictors',choices=fhs_predictors(clean(),input$surv_mod_event%||%'anychd'),selected=intersect(c('age1','sex1','sysbp1','bmi1','cursmoke1'),fhs_predictors(clean(),input$surv_mod_event%||%'anychd'))))
 survival_models<-eventReactive(input$run_surv_mod,{sp<-valid_split();req(input$surv_methods,input$surv_predictors);fhs_surv_models(clean(),input$surv_mod_event,input$surv_predictors,input$surv_methods,sp$train,sp$val)},ignoreNULL=TRUE)
 output$surv_mod_tbl<-renderDT({z<-survival_models();dt(z$table)})
 output$surv_mod_plot<-renderPlotly({z<-survival_models();fhs_plot_bars(z$table$model,z$table$test_c_index,'Held-out test concordance (selected by validation)','Harrell concordance')})
 surv_imp <- eventReactive(list(input$run_surv_mod,input$refresh_survival_importance),{req(input$run_surv_mod > 0);fhs_survival_importance(survival_models(),input$surv_predictors)},ignoreNULL=FALSE)
 output$surv_importance<-renderPlotly({tryCatch({z<-surv_imp();fhs_importance_plot(z,paste('Best validation survival model:',survival_models()$table$model[1]),'Higher values mean larger validation concordance reduction')},error=function(e)validate(need(FALSE,paste('Survival importance unavailable:',conditionMessage(e)))))})
 output$surv_effects<-renderPlotly({z<-survival_models();nm<-intersect(c('Cox PH','Stratified Cox'),names(z$models));validate(need(length(nm)>0,'Fit a Cox model to show hazard ratios.'));tryCatch(fhs_effect_plot(fhs_coefficient_intervals(z$models[[nm[1]]],'hazard'),paste('Cox coefficients:',nm[1]),'Hazard ratio (95% CI)',1),error=function(e)validate(need(FALSE,paste('Cox effect plot unavailable:',conditionMessage(e)))))})
 output$cox_assumptions<-renderPrint({z<-survival_models();if('Cox PH'%in%names(z$models))print(survival::cox.zph(z$models[['Cox PH']])) else cat('Select Cox PH to inspect proportional-hazards diagnostics.')})
 output$surv_mod_errors<-renderPrint({z<-survival_models();if(length(z$errors))z$errors else 'All selected survival algorithms completed.'})
 output$adj_event<-renderUI(selectInput('adj_ev','Time-to-event outcome',choices=intersect(names(FHS_TIME),names(clean())),selected='anychd'))
 output$adj_exposure<-renderUI(selectInput('adj_x','Exposure',choices=intersect(FHS_BASE,names(clean())),selected='cursmoke1'))
 output$adj_conf<-renderUI(checkboxGroupInput('adj_c','Potential confounders',choices=intersect(FHS_BASE,names(clean())),selected=intersect(c('age1','sex1','sysbp1'),names(clean()))))
 output$adj_modifier<-renderUI(selectInput('adj_m','Potential modifier',choices=c('None'='',intersect(FHS_BASE,names(clean()))),selected='sex1'))
 adjusted<-eventReactive(input$run_adjust,{req(input$adj_ev,input$adj_x);cov<-setdiff(input$adj_c,input$adj_x);d<-fhs_survival_frame(clean(),input$adj_ev,unique(c(input$adj_x,cov,input$adj_m)),TRUE);d<-stats::na.omit(d);f1<-stats::as.formula(paste('survival::Surv(time_years,event_status)~',input$adj_x));f2<-stats::reformulate(c(input$adj_x,cov),response='survival::Surv(time_years,event_status)');a<-survival::coxph(f1,data=d);b<-survival::coxph(f2,data=d);data.frame(model=c('Crude','Adjusted'),N=c(a$n,b$n),AIC=c(AIC(a),AIC(b)),exposure_HR=c(exp(stats::coef(a)[1]),exp(stats::coef(b)[1]))) -> tab;inter<-if(nzchar(input$adj_m)&&input$adj_m!=input$adj_x) tryCatch({fm<-stats::as.formula(paste('survival::Surv(time_years,event_status)~',paste(c(input$adj_x,cov),collapse='+'),'+',input$adj_x,':',input$adj_m));summary(survival::coxph(fm,data=d))},error=function(e)conditionMessage(e)) else 'Select a modifier distinct from exposure.';list(table=tab,interaction=inter,ph=survival::cox.zph(b),crude=a,adjusted=b)},ignoreNULL=TRUE)
 output$adjust_table<-renderDT({dt(adjusted()$table)})
 adjust_effects <- eventReactive(list(input$run_adjust,input$refresh_adjust_effects),{req(input$run_adjust > 0);fhs_cox_adjusted_effects(adjusted())},ignoreNULL=FALSE)
 output$adjust_effects <- renderPlotly({tryCatch({z<-adjust_effects();fhs_effect_plot(z,'Crude and adjusted Cox model effects','Hazard ratio (95% CI)',1)},error=function(e)validate(need(FALSE,paste('Adjusted effects unavailable:',conditionMessage(e)))))})
 output$adjust_summary<-renderPrint({z<-adjusted();print(z$interaction);cat('\nProportional hazards check:\n');print(z$ph)})
 # Count modelling; methods which lack optional packages are reported, not allowed to crash tabs.
 counts<-eventReactive(input$run_count,{
   req(input$count_methods)
   x<-clean();y<-input$count_y
   pre<-intersect(c('age1','sex1','sysbp1','bmi1','cursmoke1'),names(x))
   d<-stats::na.omit(x[,c(y,pre),drop=FALSE]);form<-stats::reformulate(pre,response=y)
   sp<-valid_split();s<-fhs_split(nrow(d),sp$train,sp$val)
   tr<-d[s$train,,drop=FALSE];va<-d[s$validation,,drop=FALSE];te<-d[s$test,,drop=FALSE]
   out<-list();errors<-list();models<-list();validations<-list()
   for(method in input$count_methods)tryCatch({
      positive<-grepl('Zero-truncated',method,fixed=TRUE)
      a<-if(positive)tr[tr[[y]]>0,,drop=FALSE] else tr
      v<-if(positive)va[va[[y]]>0,,drop=FALSE] else va
      b<-if(positive)te[te[[y]]>0,,drop=FALSE] else te
      if(nrow(a)<25||nrow(v)<10||nrow(b)<10)stop('Too few observations for this count model.')
      fit<-switch(method,
       'Poisson'=stats::glm(form,data=a,family=poisson()),
       'Negative binomial'={if(!requireNamespace('MASS',quietly=TRUE))stop('Install MASS');MASS::glm.nb(form,data=a)},
       'Zero-inflated Poisson'={if(!requireNamespace('pscl',quietly=TRUE))stop('Install pscl');pscl::zeroinfl(stats::as.formula(paste(deparse(form),'| 1')),data=a,dist='poisson')},
       'Zero-inflated NB'={if(!requireNamespace('pscl',quietly=TRUE))stop('Install pscl');pscl::zeroinfl(stats::as.formula(paste(deparse(form),'| 1')),data=a,dist='negbin')},
       'Zero-truncated Poisson'={if(!requireNamespace('VGAM',quietly=TRUE))stop('Install VGAM');VGAM::vglm(form,VGAM::pospoisson(),data=a)},
       'Zero-truncated NB'={if(!requireNamespace('VGAM',quietly=TRUE))stop('Install VGAM');VGAM::vglm(form,VGAM::posnegbinomial(),data=a)})
      pv<-fhs_count_predict(fit,v);pt<-fhs_count_predict(fit,b)
      vr<-sqrt(mean((v[[y]]-pv)^2));tt<-sqrt(mean((b[[y]]-pt)^2))
      out[[method]]<-data.frame(method=method,validation_RMSE=vr,test_n=nrow(b),
        test_RMSE=tt,test_MAE=mean(abs(b[[y]]-pt)),AIC=stats::AIC(fit),
        population=if(positive)'Positive counts only' else 'All counts')
      models[[method]]<-fit;validations[[method]]<-v
   },error=function(e) errors[[method]]<<-conditionMessage(e))
   if(!length(out))return(list(table=data.frame(method=character(),validation_RMSE=numeric(),test_n=integer(),test_RMSE=numeric(),test_MAE=numeric(),AIC=numeric(),population=character()),errors=errors,models=models,validation=validations,best=NULL,predictors=pre,outcome=y))
   tbl<-do.call(rbind,out)
   # Do not rank conditional positive-count and full-cohort models against each other.
   preferred<-tbl[tbl$population=='All counts' & is.finite(tbl$validation_RMSE),,drop=FALSE]
   if(!nrow(preferred))preferred<-tbl[is.finite(tbl$validation_RMSE),,drop=FALSE]
   if(!nrow(preferred))return(list(table=tbl,errors=c(errors,selection='No finite validation RMSE'),models=models,validation=validations,best=NULL,predictors=pre,outcome=y))
   winner<-preferred$method[which.min(preferred$validation_RMSE)]
   list(table=tbl,errors=errors,models=models,validation=validations,
        best=winner,predictors=pre,outcome=y)
 },ignoreNULL=TRUE)
 output$count_table<-renderDT({z<-counts();dt(z$table)})
 output$count_plot<-renderPlotly({z<-counts()$table;fhs_plot_bars(z$method,z$test_RMSE,'Held-out test RMSE (different populations for truncated models)','RMSE')})
 count_imp <- eventReactive(list(input$run_count,input$refresh_count_importance),{req(input$run_count > 0);fhs_count_importance(counts())},ignoreNULL=FALSE)
 output$count_importance<-renderPlotly({validate(need(!is.null(counts()$best),'No count model completed; see the diagnostic panel.'));tryCatch({z<-count_imp();fhs_importance_plot(z,paste('Best validation count model:',counts()$best),'Higher values mean greater validation RMSE after shuffling')},error=function(e)validate(need(FALSE,paste('Count importance unavailable:',conditionMessage(e)))))})
 output$count_effects<-renderPlotly({req(input$run_count > 0);validate(need(!is.null(counts()$best),'No count model fitted; see diagnostic panel.'));tryCatch({z<-fhs_count_effects(counts());fhs_effect_plot(z,paste('Count model:',counts()$best),'Exponentiated coefficient (95% CI)',1)},error=function(e){validate(need(FALSE,paste('Effect plot unavailable:',conditionMessage(e))))})})
 output$count_errors<-renderPrint({if(length(counts()$errors))counts()$errors else 'All selected count models fitted.'})
}
round_numeric<-function(x){for(v in names(x))if(is.numeric(x[[v]]))x[[v]]<-round(x[[v]],3);x}
shinyApp(ui,server)
