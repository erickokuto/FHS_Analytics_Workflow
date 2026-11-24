###############################################################################
# FHS GITHUB PUBLISHING - ANALYTICS WORKFLOW AND DECISION SUPPORT
# Owner: erickokuto
# Separate PRIVATE GitHub repositories; one RStudio project per Shiny app.
# No files are uploaded simply by sourcing this script.
# No file chooser, recursion, embedded passwords or GitHub tokens.
# GitHub stores the source code; shinyapps.io hosts the running dashboards.
###############################################################################

# SECTION 1. CONFIGURATION
FHS_GITHUB_USER <- "erickokuto"
FHS_PARENT_DIR <- "D:/WAB 2209 StatisticalComputing 2026/FHS_Analytics_and_Decision_Support"
FHS_REPOSITORIES <- c(analytics = "FHS_Analytics_Workflow",
                      decision = "FHS_Decision_Support")

# If you extracted the combined ZIP elsewhere, change FHS_PARENT_DIR above.
# If currently inside either app's RStudio project, the path can be inferred.
fhs_project_dir <- function(which = c("analytics", "decision")) {
  which <- match.arg(which)
  repo <- unname(FHS_REPOSITORIES[[which]])
  candidates <- c(file.path(FHS_PARENT_DIR, repo),
                  file.path(getwd(), repo),
                  if (basename(getwd()) == repo) getwd() else character(),
                  file.path(dirname(getwd()), repo))
  matches <- candidates[vapply(candidates, function(p) {
    dir.exists(p) && file.exists(file.path(p, "app.R")) &&
      file.exists(file.path(p, "R", "core.R"))
  }, logical(1))]
  if (!length(matches)) {
    stop("Cannot locate ", repo, ". Check FHS_PARENT_DIR, extract the ZIP, ",
         "and confirm app.R and R/core.R are present.", call. = FALSE)
  }
  normalizePath(matches[[1L]], winslash = "/", mustWork = TRUE)
}

# SECTION 2. LOW-LEVEL GIT HELPER, NO WORKING-DIRECTORY SIDE EFFECTS
fhs_git <- function(which, args, quiet = FALSE) {
  exe <- Sys.which("git")
  if (!nzchar(exe)) stop("Git not found. Install Git for Windows and restart RStudio.")
  p <- fhs_project_dir(which)
  result <- suppressWarnings(system2(exe, args = c("-C", shQuote(p), args),
                                     stdout = TRUE, stderr = TRUE))
  status <- attr(result, "status")
  status <- if (is.null(status)) 0L else as.integer(status)
  if (!quiet && length(result)) cat(paste(result, collapse = "\n"), "\n")
  list(status = status, output = result)
}

fhs_github_install <- function() {
  pkgs <- c("usethis", "gh", "gitcreds")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) install.packages(missing,
                                       repos = "https://cloud.r-project.org")
  invisible(TRUE)
}

fhs_github_setup_token <- function() {
  cat("Open https://github.com/settings/tokens and create a token for ",
      FHS_GITHUB_USER, ".\n", sep = "")
  cat("Use the minimum access required for private repositories.\n")
  cat("Then run gitcreds::gitcreds_set() in the RStudio Console.\n")
  cat("Never put credentials in scripts, screenshots or the repository.\n")
  utils::browseURL("https://github.com/settings/tokens")
  invisible(TRUE)
}

# SECTION 3. SECURITY AND STRUCTURE CHECKS
fhs_github_check <- function(which = c("analytics", "decision")) {
  which <- match.arg(which)
  p <- fhs_project_dir(which)
  cat("\nGitHub preflight:", unname(FHS_REPOSITORIES[[which]]), "\nPath:", p, "\n")
  expected <- c("app.R", "R/core.R", "R/importance.R", "R/plots.R",
                "fhs.csv", ".gitignore", "README.md", "install_packages.R")
  missing <- expected[!file.exists(file.path(p, expected))]
  if (length(missing)) stop("Missing files: ", paste(missing, collapse = ", "))
  if (!nzchar(Sys.which("git"))) stop("Git executable is not available")
  if (!requireNamespace("gh", quietly = TRUE)) {
    cat("Install 'gh' to check GitHub authentication.\n")
  } else {
    login <- tryCatch(gh::gh_whoami()$login, error = function(e) NA_character_)
    if (!is.na(login) && tolower(login) != tolower(FHS_GITHUB_USER))
      stop("Authenticated GitHub account is ", login,
           ", expected ", FHS_GITHUB_USER)
    if (is.na(login)) cat("GitHub authentication has not yet been verified.\n")
    else cat("Authenticated GitHub account:", login, "\n")
  }
  # Basic scan for accidentally embedded credentials. It is not exhaustive.
  scan <- list.files(p, recursive = TRUE, full.names = TRUE, all.files = TRUE)
  scan <- scan[!grepl("/(\\.git|rsconnect|\\.Rproj.user)/", gsub("\\\\", "/", scan))]
  scan <- scan[file.exists(scan) & !dir.exists(scan)]
  names_bad <- scan[grepl("(?i)(token|secret|password|credential|Connect_To_Shiny_io)",
                          basename(scan), perl = TRUE)]
  if (length(names_bad)) stop("Review potentially sensitive filenames: ",
                             paste(basename(names_bad), collapse = ", "))
  plain <- scan[grepl("\\.(R|r|md|txt|json|yml|yaml|cfg|ini)$", scan,
                       ignore.case = TRUE)]
  plain <- plain[!is.na(file.info(plain)$size) & file.info(plain)$size < 1e6]
  found <- vapply(plain, function(f) {
    x <- tryCatch(readLines(f, warn = FALSE), error = function(e) character())
    any(grepl("gh[pousr]_[A-Za-z0-9]{20,}|github_pat_[A-Za-z0-9_]{20,}",
              x, perl = TRUE)) ||
      any(grepl("(?i)(token|secret)\\s*=\\s*['\"][A-Za-z0-9]{20,}",
                x, perl = TRUE))
  }, logical(1))
  if (any(found)) stop("Possible credentials found in: ",
                       paste(basename(plain[found]), collapse = ", "))
  cat("PASS: required application files and basic credential checks.\n")
  cat("REMINDER: Review fhs.csv sharing rights and Git history before publishing.\n")
  if (dir.exists(file.path(p, ".git"))) fhs_git(which, c("status", "--short"))
  invisible(TRUE)
}

fhs_github_prepare <- function(which = c("analytics", "decision")) {
  which <- match.arg(which)
  fhs_github_check(which)
  p <- fhs_project_dir(which)
  parent <- dirname(p)
  while (parent != dirname(parent)) {
    if (file.exists(file.path(parent, ".git")))
      stop("Existing parent Git repository found at ", parent,
           ". Do not initialise a nested repository.")
    parent <- dirname(parent)
  }
  if (!file.exists(file.path(p, ".git"))) {
    result <- fhs_git(which, c("init", "-b", "main"))
    if (result$status != 0L) stop("Git initialisation failed.")
  } else cat("Existing Git repository preserved.\n")
  cat("NEXT: Review the Git tab in RStudio, stage approved files and Commit.\n")
  cat("Then run: fhs_github_publish('", which, "')\n", sep = "")
  invisible(TRUE)
}

fhs_review_tracked <- function(which) {
  tracked <- fhs_git(which, c("ls-files", "--cached"), quiet = TRUE)
  if (tracked$status != 0L) stop("Could not read Git index")
  bad <- tracked$output[grepl("(^|/)(rsconnect/|\\.env|\\.Renviron|\\.Rhistory|\\.RData|\\.Rproj.user/)|(?i:token|secret|password|credential)",
                             tracked$output, perl = TRUE)]
  if (length(bad)) stop("Do not publish sensitive files: ",
                        paste(bad, collapse = ", "))
  if (!length(tracked$output)) stop("No tracked files; stage and commit first.")
  invisible(TRUE)
}

fhs_require_clean_commit <- function(which) {
  status <- fhs_git(which, c("status", "--porcelain"), quiet = TRUE)
  if (status$status != 0L || length(status$output))
    stop("Uncommitted or untracked files found. Review and commit first.")
  head <- fhs_git(which, c("rev-parse", "--verify", "HEAD"), quiet = TRUE)
  if (head$status != 0L) stop("No commit exists yet; make the first commit.")
  fhs_review_tracked(which)
}

fhs_expected_remote <- function(which) {
  repo <- unname(FHS_REPOSITORIES[[which]])
  x <- paste0(FHS_GITHUB_USER, "/", repo)
  c(paste0("https://github.com/", x),
    paste0("git@github.com:", x),
    paste0("ssh://git@github.com/", x))
}

# SECTION 4. FIRST PUBLICATION - PRIVATE BY DEFAULT, EXPLICIT CONFIRMATION
fhs_github_publish <- function(which = c("analytics", "decision")) {
  which <- match.arg(which)
  if (!interactive()) stop("Run interactively in the RStudio Console.")
  fhs_github_install()
  fhs_github_check(which)
  fhs_require_clean_commit(which)
  login <- tryCatch(gh::gh_whoami()$login, error = function(e) NA_character_)
  if (is.na(login) || tolower(login) != tolower(FHS_GITHUB_USER))
    stop("GitHub authentication is not verified for ", FHS_GITHUB_USER)
  remote <- fhs_git(which, c("remote", "get-url", "origin"), quiet = TRUE)
  if (remote$status == 0L && length(remote$output)) {
    cat("Existing origin detected. No new repository will be created.\n")
    cat("Verify that it is correct, then use fhs_github_push().\n")
    return(invisible(FALSE))
  }
  repo <- unname(FHS_REPOSITORIES[[which]])
  existing <- tryCatch(gh::gh("GET /repos/{owner}/{repo}",
                              owner = FHS_GITHUB_USER, repo = repo),
                       error = function(e) NULL)
  if (!is.null(existing)) stop("Repository already exists online. Connect/clone it safely; no overwrite performed.")
  cat("\nCREATE PRIVATE repository: https://github.com/", FHS_GITHUB_USER,
      "/", repo, "\n", sep = "")
  cat("All committed files and history will be uploaded.\n")
  if (!identical(trimws(readline("Type PUBLISH to continue: ")), "PUBLISH")) {
    cat("Cancelled; nothing uploaded.\n")
    return(invisible(FALSE))
  }
  usethis::proj_set(fhs_project_dir(which), force = TRUE)
  usethis::use_github(private = TRUE)
  invisible(TRUE)
}

# SECTION 5. SUBSEQUENT UPDATES - NO FORCE PUSHES
fhs_github_push <- function(which = c("analytics", "decision")) {
  which <- match.arg(which)
  if (!interactive()) stop("Run interactively in the RStudio Console.")
  fhs_github_check(which)
  fhs_require_clean_commit(which)
  remote <- fhs_git(which, c("remote", "get-url", "origin"), quiet = TRUE)
  if (remote$status != 0L || !length(remote$output))
    stop("No origin remote. Publish or connect the repository first.")
  actual <- sub("\\.git$", "", remote$output[[1L]])
  if (!actual %in% fhs_expected_remote(which))
    stop("Unexpected Git remote. Check 'git remote -v'; no push performed.")
  branch <- fhs_git(which, c("branch", "--show-current"), quiet = TRUE)
  if (branch$status != 0L || !length(branch$output) || !nzchar(branch$output[[1L]]))
    stop("Cannot determine local branch.")
  repo <- unname(FHS_REPOSITORIES[[which]])
  cat("Push branch '", branch$output[[1L]], "' to ", FHS_GITHUB_USER,
      "/", repo, "?\n", sep = "")
  if (!identical(trimws(readline("Type PUSH to continue: ")), "PUSH")) {
    cat("Cancelled; nothing uploaded.\n")
    return(invisible(FALSE))
  }
  result <- fhs_git(which, c("push", "-u", "origin", branch$output[[1L]]))
  if (result$status != 0L) stop("Push failed; read Git output. Never force push.")
  invisible(TRUE)
}

fhs_github_check_both <- function() {
  fhs_github_check("analytics")
  fhs_github_check("decision")
  invisible(TRUE)
}

cat("\nFHS GitHub publishing helper loaded (no upload performed).\n")
cat("Run: fhs_github_check_both()\n")
cat("Then: fhs_github_prepare('analytics') and fhs_github_prepare('decision')\n")
cat("Commit each project; publish each explicitly with fhs_github_publish(...).\n")
