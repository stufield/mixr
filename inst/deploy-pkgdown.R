# ----------------------------------------------------------------
# 1) install dev version of {pkgdown} into a tmp library ('dev_lib')
# 2) install current version of pkg and update
# 3) update .libPaths() to use dev_lib
# 4) deploy the pkgdown website to a remote branch (bb-pkgdown)
# 5) update the manifest file with writeManifest()
# 6) clean up and delete ephemeral (worktree) directories/branches/libs
# ----------------------------------------------------------------
# Author: Stu Field
# Usage: Rscript --vanilla inst/deploy-pkgdown.R
# -----------------------------
dev_lib <- tempfile("lib-")
dir.create(dev_lib)                 # create tmp library location
.libPaths(c(dev_lib, .libPaths()))  # push dev_lib to top of stack
repo <- "https://cloud.r-project.org"

# get `remotes`
utils::install.packages(
  "remotes", lib = dev_lib, dependencies = FALSE, repos = repo
)

# get newest from cran `pkgdown`
remotes::install_cran(
  c("rlang", "rmarkdown", "rsconnect", "downlit", "pkgdown"),
  repos = repo, dependencies = TRUE, lib = dev_lib, force = TRUE
)

# install current package into `dev_lib`
invisible(
  base::system2(
    "R", c("CMD", "INSTALL", "--use-vanilla", paste0("--library=", dev_lib),
           "--resave-data", "--with-keep.source", ".")
  )
)

library(pkgdown, lib.loc = dev_lib)
library(withr)

# git-wrapper
git <- function(...) {
  cat("Running git", c(...), "\n")
  base::system2("git", c(...), stdout = TRUE)
}

deploy2branch <- function(dir, branch, remote = "origin") {
  # enable if on remote machine with CI
  #git("remote", "set-branches", remote, branch)
  git("fetch", remote, branch)
  git("worktree", "add", "--track", "-B", branch, dir, paste0(remote, "/", branch))
  defer(git("branch", "-D", branch))      # rm local branch (last)
  defer(git("worktree", "remove", "--force", dir))   # rm worktree (first)
  pkg <- as_pkgdown(".", override = list(destination = dir))
  commit_sha <- git("rev-parse", "HEAD")
  commit_message <- sprintf("Built site for %s: %s@%s", pkg$package, pkg$version,
                            substr(commit_sha, 1, 7))
  clean_site(pkg)
  build_site(pkg, devel = FALSE, preview = FALSE, install = FALSE, new_process = FALSE)
  with_dir(dir, {
    rsconnect::writeManifest(appDir          = pkg$dst_path,
                             appPrimaryDoc   = "index.html",
                             contentCategory = "site")
    git("add", "-A", ".")
    git("commit", "--allow-empty", "-m", encodeString(commit_message, quote = "'"))
    writeLines(paste0("* Deploying to Bitbucket '", remote, ":", branch, "'"))
    git("remote", "-v")
    git("push", "--force", remote, paste0("HEAD:", branch))
  })
  invisible(NULL)
}

deploy2branch(dir = tempfile("pkgdown-"), branch = "bb-pkgdown")
unlink(dev_lib, recursive = TRUE, force = TRUE)
