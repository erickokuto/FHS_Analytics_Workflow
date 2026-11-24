# FHS dashboard GitHub publishing guide

**Account:** `erickokuto`  
**Repository:** `FHS_Analytics_Workflow`  
**Default visibility:** Private

1. Make a backup of any existing project directory and extract this folder into `D:/WAB 2209 StatisticalComputing 2026/FHS_Analytics_and_Decision_Support/`.
2. Open `FHS_Analytics_Workflow.Rproj` in RStudio. Source `Publish_FHS_Dashboards_to_GitHub.R`.
3. Run `fhs_github_install()` if GitHub packages are missing. For authentication, run `fhs_github_setup_token()` and then `gitcreds::gitcreds_set()` in the **Console**, not in a script.
4. Run `fhs_github_check("analytics")` then `fhs_github_prepare("analytics")`.
5. In the RStudio Git tab, review files, especially `fhs.csv`, stage approved files and **Commit**. The included FHS dataset is teaching data but should still be reviewed for distribution permission and record-level confidentiality.
6. Run `fhs_github_publish("analytics")` and type `PUBLISH` only when ready. It creates a **private** repository and uploads committed content.
7. For later updates, commit first then run `fhs_github_push("analytics")`, typing `PUSH` to confirm.

Expected repository after successful publication: `https://github.com/erickokuto/FHS_Analytics_Workflow`.

**Important:** GitHub does not host the live Shiny dashboard. Use shinyapps.io separately. The GitHub helper does not contain account tokens, does not upload on source, and never force pushes. Never commit rsconnect metadata, secrets or unauthorised participant data. `.gitignore` cannot remove files already tracked or secrets from previous Git history.

If an online repository already exists, this helper stops rather than overwriting it. Clone/merge that repository carefully before pushing.
