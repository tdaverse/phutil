## Resubmission

This is a resubmission. In this version I have:

- Extended the package description to include citations for methods implemented
in the package;
- Modified the validation benchchmark vignette to properly reset graphical
settings to user's defaults after changing them.

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
