# Run in RStudio from this application folder: source('install_packages.R')
# Core dependencies cover Shiny, descriptive / inference and common models.
core <- c('shiny','ggplot2','plotly','DT','survival','MASS','cluster','rpart','randomForest','gbm','e1071','ranger','rsconnect')
missing <- core[!vapply(core,requireNamespace,logical(1),quietly=TRUE)]
if(length(missing)) install.packages(missing,repos='https://cloud.r-project.org')
cat('Core dependencies checked.\n')
# Optional models: uncomment only the packages needed.
optional <- c('xgboost','dbscan','randomForestSRC','aorsf','pscl','VGAM','car','rstanarm')
cat('Optional method packages (not installed automatically):\n',paste(optional,collapse=', '),'\n')
# To install a chosen optional package: install.packages('dbscan')
# rstanarm and randomForestSRC may require a suitable R version/toolchain.
