# Version 3.1
__Release date:__ 13 December, 2016

Main changes in this update are within the Inference functions;
t-tests performed on request.

## New Features

- __Hypothesis testing__: new option under Get Inference
  - one-sample t-tests, two-sample t-tests, ANOVA, and one- and two-way Chi-square testing
  - ability to adjust null value and alternative hypothesis in t-tests
  - ability to specify pooled variance alternative for two-sample t-test
- Store predicted values and residuals under Get Summary (for scatter plots and ANOVA)
- Added `SURFIncomeSurvey` data set (200 row subset) to examples

## Minor Changes

- "factor" reworded to "categorical" in summary/inference output
- some of the inference output has been adjusted slightly for consistency (see above)

## Bug Fixes

- Fix a bug in inference output where reorded factors' confidence intervals and p-values were _not_ reordered!
- Fix issue where confidence intervals for two-way table row proportions were ordered incorrectly


## Patches

### Patch 3.1.1 - 23/03/2017

- Additions:
  - Time Series module update to new UI
  - Added Import Data (Beta) module (alongside existing one)
  - Save plots as SVG or Interactive HTML (Beta)
- Update Gapminder data
- Survey design object is now passed to Model Fitting module (will soon be depreciated)
- Various other bug fixes in supporting packages


***

# Version 3.0
__Release date:__ 5 Septemer, 2016

This is a major new release of iNZight, so many things have changed.
We hope you like them!

## New Features

Here's just a few of the major new features to iNZight.

- Example data sets available from the File menu
- Play time can now be stopped, and the interval adjusted
- Switch variables with the one below by clicking the "down" arrow to the right of the variable box
- __Maps Module__ - visualise geographical data:
  - Coordinates: if your data set includes GPS coordinates for each observation,
    iNZight will let you plot them on a map.
  - Countries: if observations have country names associated with them,
    you can colour countries on a map by another variable.
    Try this out with the Gapminder data set included in Example Data!
- Dual Window mode - if you've used VIT, then this will be familiar to you. From File > Preferences,
  you can choose to have a separate window for graphs (as opposed to the built-in one).


## Major Changes

We can't list everything here, but some of the important ones are listed below.

- Plot toolbar has new icons
- Add to Plot window completely redesigned to make plot modification easier
- Resize points has been adjusted to resize the _area_ proportional to the value.
  Also added an "emphasize" option, which scales sizes linearly.
- __COLOUR__: new, better palettes have been introduced, including [viridis](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html),
  which are colourblind friendly.
- Multiple Response has been overhauled to be more intuitive and fit in with the rest of iNZight.
- (temporary) Just a quick workaround for analysing tables in iNZight: Dataset > Expand Table


## Minor Changes

Given the magnitude of changes, there's little point mentioning the minor ones and bug fixes!



## Patches

### 3.0.1 - 23/09/2016

- Fix up labels and icons so plot toolbar and plot menu match
- Fix a bug where two-level bar plots failed to draw
- Fix a bug where empty levels in two-way bar plots failed to draw
- Fix locator for dot plots
- Fix the updates available checker

### 3.0.2 - 04/10/2016

- Add "Updates Available" text back to the title (when they're available, of course!)

### 3.0.3 - 12/10/2016

- Bug fix: can now change colours of histograms

***
# Version 2.5
__Release date__: 08 November, 2015

## New Features

- specify colours for dotplots and scatterplots from the Add to Plot > Code more variables screen.


### Patch 2.5.1 - 16/11/2015

- correct the names and URLs in the 'Help > User Guides' menu
- fix bug that occurs when 'Colour by' variable has only one unique value


***
# Version 2.4
__Release date__: 12 October, 2015

## New Features

- Include FPC (finite population corrections) in survey designs

## Major Changes

- Replace the drag-drop boxes for variabes with drop-down
  boxes for an alternative method of variable
  selection. Drag-and-drop functionality has been retained.

## Bug Fixes

- Fix the enabling/disabling of buttons when survey designs
  are specified.
- When closing the survey design window, they specified design
  is now checked for validity before saving the design.
- Fix bug in the Multiple Response module that gave an
  "inverse" error message.
- Various other small fixes throughout iNZight and other
  dependency pacakges. See their change logs for details.


### Patch 2.4.1 - 21/10/2015

- Remove dependency on `RODBC` package; this causes
  issues on Mac attempting to manually install.

### PATCH 2.4.2 - 28/10/2015

- Add option to display labels internally on numeric x factor
- Fix from iNZightPlots for Get Summary (`n.missing` now correct)

### Patch 2.4.3 - 02/11/2015

- Improve smoothness of dotplot transitions by only redrawing
  when the scaling has changed.


***
# Version 2.3
__Release date__: 4 August, 2015

## New Features

- SURVEY DESIGNS: iNZight can now handle survey designs
  (currently Strata, 2-stage (nesting) clustering, and a weighting
  variable. Plots take account of this structure and inference that
  displays (currently only trend lines on bivariate plots) use
  functions from the `survey` packages and thus incorporate weights.
- Adjust the axis limits from the Add to Plot menu.
- Adjust the number of bars displayed on a bar plot from the
  Add to Plot menu.

## Minor Changes

- the plot tool bar has been added to the Menubar at the top
  of the page. This allows users to work in a dual-window mode
  (with a narrow iNZight interface and a separate plotting window)
  and still be able to access plot functions.

## Bug Fixes

- fixes to a few issues occuring in dotplots
- display an error if Add to Plot is called without any
  variables having been selected


***
# Version 2.2
__Release date__: 16 June, 2015

## New Features

- users can specify a window size in File > Preferences
- Links to the User Guides found on our website have been
  added, including help buttons in the Add To Plot and Inference
  Information windows.

## Major Changes

- LOCATE functionality now included for dot plots. Also fixes
  several bugs in the previous update for locating points in
  scatter plot.

## Minor Changes

- the "Row Operations" menu has been renamed "Data"

### Patch 2.2.0-1 - 1/07/2015

- iNZightPlots package: fix colour of segmented bar plots to
  match the legend

### Patch 2.2.0-2 - 27/7/2015

- Fix mapping of confidence intervals (instead of comparison
  intervals) to Year 12 intervals in dot plots.

***
# Version 2.1
__Release date__: 25 May, 2015

## New Features

- New hexagonal binning plot available to large samples
- Live updating sliders for the "Add to Plot" panel, which is
  now embedded into the main window rather than creating an
  additional pop-up window

## Major Changes

- The "Add to Plot", "Remove Additions" and
  "Inference Information" windows have been integrated into the main
  window with the aim of making the interface cleaner and easier to
  navigate.
- When colouring points by a numeric variable, a continuous colour
  range is used rather than a discrete one.
- The plotting package has been rewritten to accomodate survey
  designs (this will be integrated with iNZight itself at a later
  date). However, the plots have been modified to be cleaner and far
  more efficient. Additionally, the algorithms used for the
  inference information have been updated. For more information on
  this, see the iNZightPlots change log.
- The "Get Summary" and "Get Inference" outputs have been
  redesigned to hopefully give a more intuitive, cleaner
  presentation of the information.
- From dot plots and histograms, the comparison (and confidence)
  interval values can be read from the graph by clicking 'Get Values'
  in the 'Inference Information' panel. This includes the Year 12 intervals
  applied to medians.
- LOCATOR: functionality for scatterplots vastly improved,
  allowing selection of related points, retaining points over
  multiple graphcs, and selecting points by a variable.
  (similar upgrades for dot plots coming in v2.2)

## Minor Changes

- the number of missing values is now displayed on the plot,
  broken down by variable
- the list of options in the 'Add to Plot' window are now in a drop
  down menu, rather than radio buttons, to save space
- many other small UI improvements

## Bug Fixes

- if `colour by` is set, but the graph is changed to a histogram,
  the legend is removed from the plot
- fixes a bug in the plotting that was causing large values to fall off the axes
  of the plots
- includes other patches from the previous version of iNZight

### Patch 2.1.1 - 09/06/2015

- fix a dependency issue required for aggregating data sets


***
# Version 2.0.4
__Release date__: 18 November 2014

## New Features

- The data set name is displayed above the data view
- The variable type is now displayed as a prefix to the variable
  names in the variable view
- A new "Updates Available" shows up when new versions are
  released
- A new Help tab that includes Licence information, and a link
  to the FAQ and Support pages

## Minor Changes

- In the "Remove Additions" window, "Remove segmentation" has
  been renamed to "Remove colour by" for bar plots, and
  "Restore default labels" is added to remove customised plot
  labels

## Bug Fixes

- Removes development text from Load Data window
- Fixes a bug in the Locate Points feature when the scales are
  vastly different
- Scroll bars added to Reorder Levels window
- The menu has been moved above the plot tabs, so it no longer
  gets cut off

### Patch 2.0.4-1 - 12/12/2014

- fixed up the plot device in Time Series on Linux machines

### Patch 2.0.4-3 - 10/02/2015

- fix a bug causing write-protected directories to crash

### Patch 2.0.4-4 - 10/03/2015

- ping for an internet connection before trying to check for updates


***
# Version 2.0.3
__Release date__: 17 September, 2014

## Major Changes

- A new Multiple Response analysis module has been added to the
  Advanced menu. This utilises the iNZightMR package developed by
  Junjie Zeng

## Minor Changes

- "Add to Plot" window modified for barcharts - can show only
  CIs, and also selects comparison and confidence intervals by default

## Bug Fixes

- Includes a suite of bug fixes in the iNZightPlots package - see its
  NEWS file for these


***
# Version 2.0.2
__Release date__: 17 June, 2014

## New Features

- can now cycle through all two-variable plots for a selected
  Variable 1
- "play" button for the first subset by variable
  (automatically cycle levels)
- data manipulation features added and modified

## Minor Changes

- can now specify to identify min/max points in a dot plot
- in the Add to Plot window for barcharts, reworded "segment by" to "colour by"
  for consistency.
- the transparency slider now runs from 0% to 100%

## Bug Fixes

- identify functionality works for subsets (except when multi plots at once)


***
# Version 2.0.1
__Release date__: 27 March, 2014

## New Features

-  Identify feature added to the "Add To Plot" menu. This
  allows users to interactively label points with `id` or any other
  variable. This feature works for both dot plots and scatter plots.

## Minor Changes

- Users can force scatter plots when the sample size is large,
  rather than using the alternative grid-density plot.

- Missing observations can be converted to categorical
  levels. For categorical variables, the result is a new factor with
  the same levels as the original variable, but the additional
  `missing` factor level. For continuous variables, a new factor
  variable with two levels, `missing` and `not missing` is created.

## Bug Fixes

- Vertical sliders added to the `Rename Levels` and `Rename
      factor levels` windows.
- Reorder levels now works as expected, including sort by
  frequency.
- Bug where the iNZight window closes even if a user clicks
  `cancel` has been fixed.


***
# Version 2.0
__Release date__: 18 January, 2014

## Major Release

The entire iNZight module of iNZightVIT has been rewritten,
with a multitude of changes along the way.
