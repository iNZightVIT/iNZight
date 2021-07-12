## Test environments

* local ubuntu 21.04, R 4.0.5
* ubuntu 16.04 (github actions) R release and devel
* win-builder (devel and release)

## Notes

* macOS not supported due to lack of working RGtk2 binaries - Simon Urbanek is aware of the issue but will not be fixing
* users only use the `iNZight()` function, which launches the GUI

## R CMD check results

0 errors | 0 warnings | 2 notes

* This is a new submission
* Suggested packages iNZightModules and FutureLearnData: these are available at https://r.docker.stat.auckland.ac.nz (added under additional repositories)
* Windows R-devel warning: this is coming from the 'gWidgets2' package - I have alerted the maintainer.

## Downstream dependencies

There are currently no downstream dependencies for this package.
