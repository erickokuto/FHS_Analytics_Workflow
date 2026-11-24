# Reliable direct Plotly charts: avoid ggplotly evaluating data-mask expressions.
# Plot helpers validate incoming data and return an explanatory Shiny message when empty.
fhs_plot_validation <- function(condition, message) {
  if (!isTRUE(condition)) shiny::validate(shiny::need(FALSE,message))
  invisible(TRUE)
}
fhs_plot_empty <- function(msg) {
  plotly::layout(plotly::plot_ly(),annotations=list(list(x=.5,y=.5,xref='paper',yref='paper',text=msg,showarrow=FALSE)))
}
fhs_plot_hist <- function(d,var,heading,group=NULL) {
  fhs_plot_validation(var %in% names(d),paste('Measurement unavailable:',var))
  if(is.null(group) || !nzchar(group) || !group %in% names(d)) {
    v<-as.numeric(d[[var]]);v<-v[is.finite(v)]
    fhs_plot_validation(length(v)>3,'Not enough non-missing observations for a histogram.')
    p<-plotly::plot_ly(x=v,type='histogram',nbinsx=35,marker=list(color='#087e92'))
  } else {
    groups<-as.character(d[[group]]);groups[is.na(groups)]<-'Missing';groups<-factor(groups)
    p<-plotly::plot_ly()
    for(g in levels(groups)) {
      v<-as.numeric(d[[var]][groups==g]);v<-v[is.finite(v)]
      if(length(v)>2)p<-plotly::add_histogram(p,x=v,name=g,opacity=.55,nbinsx=30)
    }
    p<-plotly::layout(p,barmode='overlay')
  }
  plotly::layout(p,title=heading,xaxis=list(title=var),yaxis=list(title='Participants'))
}
fhs_plot_box <- function(d,num,group,heading) {
  fhs_plot_validation(all(c(num,group)%in%names(d)),'Choose a valid measurement and grouping variable.')
  p<-plotly::plot_ly();g<-as.character(d[[group]]);g[is.na(g)]<-'Missing'
  for(level in head(unique(g),15)) {
    v<-as.numeric(d[[num]][g==level]);v<-v[is.finite(v)]
    if(length(v)>2)p<-plotly::add_boxplot(p,y=v,name=level,boxpoints=FALSE)
  }
  fhs_plot_validation(length(p$x$data)>0,'No groups have enough observed numeric measurements.')
  plotly::layout(p,title=heading,yaxis=list(title=num),xaxis=list(title=group))
}
fhs_plot_density <- function(d,num,group,heading) {
  fhs_plot_validation(all(c(num,group)%in%names(d)),'Choose a valid numeric and grouping variable.')
  p<-plotly::plot_ly();g<-as.character(d[[group]]);g[is.na(g)]<-'Missing'
  for(level in head(unique(g),12)) {
    v<-as.numeric(d[[num]][g==level]);v<-v[is.finite(v)]
    if(length(v)>3 && stats::sd(v)>0) {
      est<-stats::density(v,n=256)
      p<-plotly::add_lines(p,x=est$x,y=est$y,name=level,hoverinfo='x+y+name')
    }
  }
  fhs_plot_validation(length(p$x$data)>0,'No groups have sufficient variation for a density curve.')
  plotly::layout(p,title=heading,xaxis=list(title=num),yaxis=list(title='Density'))
}
fhs_plot_scatter <- function(d,x,y,group=NULL,heading='Scatterplot') {
  fhs_plot_validation(all(c(x,y)%in%names(d)),'Select valid x and y variables.')
  v<-is.finite(as.numeric(d[[x]])) & is.finite(as.numeric(d[[y]]));d<-d[v,,drop=FALSE]
  fhs_plot_validation(nrow(d)>2,'Not enough complete observations for a scatterplot.')
  p<-plotly::plot_ly()
  if(!is.null(group) && nzchar(group) && group %in% names(d)) {
    g<-as.character(d[[group]]);g[is.na(g)]<-'Missing'
    for(level in head(unique(g),12)) {
      inds<-which(g==level)
      if(length(inds))p<-plotly::add_markers(p,x=as.numeric(d[[x]][inds]),y=as.numeric(d[[y]][inds]),name=level,marker=list(size=5,opacity=.5))
    }
  } else p<-plotly::add_markers(p,x=as.numeric(d[[x]]),y=as.numeric(d[[y]]),marker=list(color='#087e92',size=5,opacity=.55),showlegend=FALSE)
  plotly::layout(p,title=heading,xaxis=list(title=x),yaxis=list(title=y))
}
fhs_plot_bars <- function(labels,values,heading,xlabel='Value',horizontal=TRUE) {
  labels<-as.character(labels);values<-as.numeric(values);ok<-is.finite(values)&!is.na(labels)
  labels<-labels[ok];values<-values[ok]
  fhs_plot_validation(length(values)>0,'No finite results to display. Run the analysis and examine its diagnostics.')
  if(horizontal){o<-order(values);labels<-labels[o];values<-values[o]
    p<-plotly::plot_ly(x=values,y=factor(labels,levels=labels),type='bar',orientation='h',marker=list(color='#087e92'))
    plotly::layout(p,title=heading,xaxis=list(title=xlabel),yaxis=list(title=''),margin=list(l=170))
  }else{
    p<-plotly::plot_ly(x=labels,y=values,type='bar',marker=list(color='#087e92'))
    plotly::layout(p,title=heading,yaxis=list(title=xlabel))
  }
}
fhs_plot_pca <- function(d,heading) {
  fhs_plot_validation(all(c('PC1','PC2','cluster')%in%names(d)),'PCA coordinates were not generated. Run clustering again.')
  fhs_plot_validation(nrow(d)>1,'No clustering observations to display.')
  p<-plotly::plot_ly();g<-as.character(d$cluster)
  for(level in unique(g)){
    z<-d[g==level,,drop=FALSE]
    p<-plotly::add_markers(p,x=z$PC1,y=z$PC2,name=level,marker=list(size=5,opacity=.65))
  }
  plotly::layout(p,title=heading,xaxis=list(title='Principal component 1'),yaxis=list(title='Principal component 2'))
}
fhs_plot_survival <- function(fit,curve='Kaplan-Meier',heading='Time-to-event curves') {
  a<-summary(fit)
  fhs_plot_validation(length(a$time)>0,'No recorded survival event times for this selection.')
  haz<-a$cumhaz
  if(is.null(haz) || length(haz)!=length(a$time)) {
    # Nelson-Aalen cumulative hazard, summed separately in each fitted group.
    group_id<-if(is.null(a$strata)) rep('Overall',length(a$time)) else as.character(a$strata)
    increments<-ifelse(a$n.risk>0,a$n.event/a$n.risk,0)
    haz<-unsplit(lapply(split(increments,group_id),cumsum),group_id)
  }
  val<-switch(curve,'Kaplan-Meier'=a$surv,'Nelson-Aalen cumulative hazard'=haz,'Cumulative hazard'=haz,'Cumulative incidence (1-S)'=1-a$surv,'Cumulative incidence'=1-a$surv,a$surv)
  g<-if(is.null(a$strata))rep('Overall',length(a$time)) else as.character(a$strata)
  fhs_plot_validation(length(g)==length(val) && length(val)==length(a$time),'The survival summary has mismatched curve vectors.')
  p<-plotly::plot_ly()
  for(level in unique(g)){
    ids<-which(g==level);ord<-order(a$time[ids]);ids<-ids[ord]
    if(length(ids)>0){
      initial<-if(curve=='Kaplan-Meier')1 else 0
      p<-plotly::add_trace(p,x=c(0,a$time[ids]),y=c(initial,val[ids]),type='scatter',mode='lines',line=list(shape='hv',width=2),name=level)
    }
  }
  plotly::layout(p,title=heading,xaxis=list(title='Follow-up years'),yaxis=list(title=curve))
}
fhs_plot_predictions <- function(d,type) {
  fhs_plot_validation(all(c('observed','prediction')%in%names(d)),'No held-out predictions available.')
  p<-as.numeric(d$prediction);good<-is.finite(p)&!is.na(d$observed)
  fhs_plot_validation(sum(good)>2,'Held-out prediction values are missing or invalid.')
  if(type=='binary') {
    grp<-as.character(d$observed);out<-plotly::plot_ly()
    for(level in unique(grp[good])){
      v<-p[good & grp==level];if(length(v))out<-plotly::add_boxplot(out,y=v,name=level)
    }
    plotly::layout(out,title='Held-out outcome groups and predicted probabilities',yaxis=list(title='Predicted event probability'))
  }else{
    y<-suppressWarnings(as.numeric(d$observed))
    ok<-good&is.finite(y)
    fhs_plot_validation(sum(ok)>2,'Observed test values could not be interpreted as numeric.')
    out<-plotly::plot_ly(x=y[ok],y=p[ok],type='scatter',mode='markers',marker=list(color='#087e92',opacity=.5))
    plotly::layout(out,title='Held-out observed versus predicted',xaxis=list(title='Observed'),yaxis=list(title='Predicted'))
  }
}
