# Verify packaging and source visibility, without invoking the Shiny runtime.
required<-c('app.R','R/core.R','R/importance.R','R/plots.R','fhs.csv','install_packages.R','tests/smoke_test.R')
missing<-required[!file.exists(required)]
if(length(missing))stop('Missing: ',paste(missing,collapse=', '))
cat('PASS: expected application files are present.\n')
