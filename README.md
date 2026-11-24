# FHS interactive teaching dashboards

## Supplied files
- `FHS_Analytics_Workflow/`: data management, exploratory/descriptive/inferential statistics, supervised/count/survival models, cluster analysis and methodological diagnostics.
- `FHS_Decision_Support/`: retrospective cohort review, risk factor patterns, time-to-event evidence, model evidence, illustrative record-review filters, scenario exploration, and Analytics evidence CSV import.
- `Deploy_FHS_Dashboards_to_Shinyapps_IO.R`: safe deployment helpers for both apps.
- `fhs.csv` is bundled inside **each** folder so each app can be deployed independently. The single CSV uploader inside each app replaces that session's sample data. Nothing is persisted on the hosting server.

## Getting started
1. Extract `FHS_Both_Dashboards.zip` into `D:/WAB 2209 StatisticalComputing 2026/FHS_Analytics_and_Decision_Support` or any other directory. If elsewhere, update `FHS_PARENT_DIR` in the deployment script.
2. Open RStudio, set working directory to one application folder, then run `source('install_packages.R')`.
3. Run `source('tests/structure_check.R')` and `source('tests/smoke_test.R')`. Fix any failures before publication.
4. Run `shiny::runApp('.')`. Explore the tabs by clicking their **Run/Update** buttons.
5. To deploy, return to parent folder, run `source('Deploy_FHS_Dashboards_to_Shinyapps_IO.R')`; then `fhs_check_both()`, `fhs_authorised()`, `fhs_deploy('analytics')` and `fhs_deploy('decision')`. Only your own RStudio's local secure rsconnect configuration should contain the token.
6. Expected app names: `fhs-analytics-workflow` and `fhs-decision-support` in account `odassolutions`. These are *not* confirmed live URLs.

## Evidence hand-off
1. In Analytics Workflow, choose a binary/numeric outcome and baseline predictors, click **Train and compare models**, then **Export results for Decision Support**.
2. In Decision Support, upload the **same source dataset**, then upload the exported predictions under **Analytics hand-off**, click **Validate and connect evidence**.
3. The evidence CSV is validated by columns and participant ID membership. This does *not* import executable model objects. It represents only held-out test predictions.

## Data and scientific caveats
- Original upload contains 4,434 records and 75 source columns. 194 participants have previous CHD at baseline. Incident CHD survival functions exclude those records by default.
- Source data contain three exam rounds; all-missing HDL/LDL baseline columns are intentionally not imputed. Cohort loss, measurement availability and censoring deserve explicit discussion.
- `timechd`, `timedth`, `timecvd`, etc. are expressed in **years** in this supplied CSV; authoritative teaching datasets in other formats may express time in **days**. Check units when uploading a different file.
- Models default to baseline (exam 1) predictors; outcomes or follow-up measurements are not used for prediction. Preprocessing is estimated on training data only for supervised modelling. This sample-based development does not replace external validation.
- The teaching `illustrative_event_burden` is the sum of angina, hospitalised myocardial infarction, stroke and death indicators. Events overlap; this is **not** a measured count of independent recurrent events. Count-model results are methodological exercises and should not be presented as event incidence findings. Zero-truncated models use only positive counts; they are evaluated on a different subset and their metrics must not be ranked directly against full-population models.
- Classification models use default threshold 0.5; calibration and sensitivity analysis are necessary before research conclusions.
- Cox and AFT models, survival trees and survival forests can be compared on held-out concordance, but survival test concordance is distinct from repeated cross-validation. The Bayesian model requires `rstanarm` and Stan; optional methods may fail if packages/toolchains are missing and the interface reports those failures.
- `Cox PH` must be checked using Schoenfeld residuals; stratified Cox and AFT are included as alternatives when PH assumptions are not supported. No causal mediation effects are claimed; that requires further identification and study design.
- No medical diagnosis, patient-level risk recommendation or treatment advice is provided. Review priorities use transparent illustrative filters only.

## Dependencies
Core: shiny, ggplot2, plotly, DT, survival, MASS, cluster, rpart, randomForest, gbm, e1071, ranger, rsconnect.
Optional: xgboost, dbscan, randomForestSRC, aorsf, pscl, VGAM, car, rstanarm. Optional algorithms fail with a clearly reported package requirement rather than silently running a different model.

## Sources
Official NHLBI Framingham Teaching Longitudinal Data Documentation: https://biolincc.nhlbi.nih.gov/media/teachingstudies/FHS_Teaching_Longitudinal_Data_Documentation_2021a.pdf
CRAN Survival Analysis task view: https://cran.r-project.org/web/views/Survival.html

## Validation status
Sources were checked statically and archives validated as ZIP. **An R runtime is unavailable in the authoring environment, so package installation, Shiny startup, R parsing and model execution cannot be certified here.** Please run the included tests and click each tab in RStudio before deploying. In particular, optional Bayesian/forest/count packages are version and platform sensitive.


### November 2026 importance update

The shared `R/importance.R` module provides horizontal validation-permutation importance plots and separate coefficient/effect interval plots. See the parent `README_START_HERE.md` for details. Run `source('tests/importance_test.R')` before deployment. This dashboard is for retrospective education, not clinical advice.


## Plotting repairs (November 2026)
Direct Plotly charts replace problematic ggplotly dynamic selectors. New file `R/plots.R` is required for deployment. Clustering uses a reproducible maximum 1,200-row sample to control server memory, so results reflect the sample and not every participant. Run `source('tests/plot_tests.R')` as well as the existing smoke tests, and review model-specific diagnostics before publishing.


## Publish the source on GitHub
See `GITHUB_SETUP_GUIDE.md`. Source `Publish_FHS_Dashboards_to_GitHub.R` and run `fhs_github_check("analytics")`. GitHub repositories are distinct from deployed shinyapps.io applications.
