## Test environments

* local macOS R installation, R 4.5.0
* continuous integration via GH actions:
  * macOS latest release
  * windows latest release
  * ubuntu 24.04.2 LTS and devel and oldrel-1
* [win-builder](https://win-builder.r-project.org/) (release and devel)
* [macOS-builder](https://mac.r-project.org/macbuilder/submit.html)
* [R-hub](https://r-hub.github.io/rhub/): All platforms expect
  * `gcc15` for which package {fs} fails to install;
  * `nosuggests` which fails because it cannot build the vignettes that require
  dependencies listed in `Suggests` field of `DESCRIPTION` file.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.
