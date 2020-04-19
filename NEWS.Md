- move makeNames() method (part of iNZDataModWin) to 'iNZightTools' package
  as iNZightTools::make_names()
- specify `stringsAsFactors = TRUE` for upcoming R 4.0.0

## iNZight 3.5.0

- load from clipboard option
- add option to import data from a URL as well as local files
- remove equal variance checkbox for survey design (two way t-test)
- survey design code pasted together with a new line when `deparse` returns multiple lines (#281, @tmelliott)


## iNZight 3.5
__Release date__: 11 November 2019

- allow users to change column types for all import types (cat/num)
- reset axis limits when either V1 or V2 change (temp fix until something less aggressive is implemented)
- make title/xlab/ylab boxes reactive to keystroke
- add RData/rda and RDS import methods to main Import window, removing Load and Save options from File menu
- disable plotly for ridgeline density plots
- use temporary directory for ggplot thumbnails
- some naming changes to plot modification UI
- deleting plots in "Plot History" window will now remove them from the plot list in the window immediately
- gridplots now have palette selection
- cut-point option for diverging stacked bar charts
- the plot history window now reruns the code used for previous plots instead of storing the plot itself
- the plot history window now has a textbox for users to modify code and run in an temporary environment
- sortable plots now have ascending/descending option
- enable inference for survey designs
- addon modules now available (as a developmental feature)
- add option to export as rda
- allow user to select which Excel sheet to import
- enable transparency for ridgeline plots
- enable palettes, colouring, etc. for barcode plots

## iNZight 3.4.6
__Release date__: 23 September 2019

- add gallery of previously stored plots for ggplots
- rearrange controls for ggplots
- add ability to rotate x/y axis for ggplots
- change the default number of observations a square represents in gridplots


## iNZight 3.4.5
__Release date__: 2 September 2019

- adds option to perform hypothesis test on a single proportion
- allow users to request simulated p-value from chi-square tests
- add option to change the number of observations a square represents in gridplots
- add option for ridgeline ggplot
- add controls for the width and method of swarming for beeswarm plots
- label changes to ggplot controls


## iNZight 3.4.4
__Release date__: 26 August 2019

- fix warnings that occur from `family = "normal"` fonts
- fix preferences storage location on Windows Install version of iNZight
- fix bug preventing Get Summary/Inference from working with FT plots


## iNZight 3.4.3
__Release date__: 13 August 2019

- allow user to remove calibration design from calibration window
- change how frequency variable is stored so it is retained after filter/modifying
  the dataset
- improve how replicate weights are displayed, and addition of 'remove' button
- add option to dataset menu to view the (entire) dataset using `utils::View()`
- add search box to filter long variable lists (iNZight home panel)
- add option to change ggplot theme of Financial Times plots
- add beeswarm option for plotting (using ggbeeswarm)
- add ability to include captions for sources in Financial Times plots


## iNZight 3.4.2
__Release date__: 22 July 2019

- modules directory no longer used (or created) on Windows, to help the updater
- fix bug in point sizing when removing/adding box plot from dotplots


## iNZight 3.4.1
__Release date__: 15 July 2019

- allow users to specify replicate weights in the survey design
- specify post stratification information to existing survey design
- specify frequency column to use in plots, summary, and inference
- add suite of Financial Times plots (using ggplot2) (#211, @daniel-barnett)


# iNZight 3.4
__Release date__: 30 April 2019

### Dates and times
- convert variables into datetimes (Variables > Dates and times)
- extract time information from a datetime variable (such as the hour, or year)

### Merging and reshaping data sets
- reshape data from long to wide, and vice versa
- merge data sets using `*_join()`
- join data sets by appending rows

### Other changes and fixes
- add axis transformation (log-10) for numeric variables
- switch between counts/percentages in bar plots
- import SAS files (.sas7bdat and .xpt)
- add dataset validation module
- allow switching between categorical and numeric data types in import window
- fixes, and export additional variables, for interactive maps
- when importing RDS files, character variables are converted to factors


## iNZight 3.3.7
__Release date__: 05 April 2019

- small change to patch bug in iNZightPlots package


## iNZight 3.3.6
__Release date__: 12 February 2019

- fix a bug in Get Inference which showed x and y the wrong way around


## iNZight 3.3.5
__Release date__: 01 February 2019

- update API for module windows to create header (title), body (with scrollbar), and footer (for buttons)


## iNZight 3.3.4
__Release date:__ 16 January 2019

- fix a bug in which the preferences menu wouldn't open up


## iNZight 3.3.3
__Release date:__ 07 January 2019

- menu now reacts to the environment (plus major internal code refactoring)
- now showing a dataset summary under Get Summary when no variables selected
- updated the maps landing page
- specifying survey design now writes code history

- remove NULL arguments from survey design call
- modules path is prepended to libPaths

- added unit testing


## iNZight 3.3.2
__Release date:__ 15 November 2018

- add dataset summary information to Get Summary (when no variables selected)
- add interact button
- fix call for survey design objects (remove NULL arguments)
- fix bug that prevented iNZight to launch on some macs


## iNZight 3.3.1
__Release date:__ 10 August 2018

- fix bug in reading non-csv files


# iNZight 3.3
__Release date:__ 15 August 2018

- import data now uses updated functions which return the file-specified code,
  available from Advanced > R Code history
- maps module completely renovated
- interactive plots have recieved more updates and fixes


## iNZight 3.2.2
__Release date:__ 25 May 2018

- fix bug in import window where some character encodings were being distorted
- fix typo in import window info box
- catch import errors, so the "please wait" dialog hopefully doesn't stay open forever


## iNZight 3.2.1
__Release date:__ 21 Febuary 2018

- fix small issue in import module that prevented reading of non-UTF-8 csv files


# iNZight 3.2
__Release date:__ 23 January 2018

### New Features

- New developmental __code writing__ feature has been added, which writes tidyverse code
  for common data/variable manipulations
- Manage your datasets with the updated data set widget, just above the data set view!
- Added the ability to control font size in Summary/Inference windows via File > Preferences

### Minor Changes

- The __Reorder Levels__ window has been reconfigured to, hopefully, make it easier to use
- Filtering no longer clears the slots
- Import data [beta] is ... no longer beta!
- Updated the splash screen to be up-to-date, and attempt to be friendly ...

### Bug Fixes
- saving residuals/predicted values works if you select only quadratic or cubic


## iNZight 3.1.6
__Release date:__ 12 January 2018

- Add font size option to preferences for the Get Summary/Inference windows


## iNZight 3.1.5
__Release date:__ 02 October 2017

- More bug fixes/minor improvements


## iNZight 3.1.4
__Release date:__ 25 August 2017

- Add basic code history to model fitting module
- few bug fixes


## iNZight 3.1.3
__Release date:__ 18 August 2017

- Fixes for Interactive plots
- BETA release of new Model Fitting module


## iNZight 3.1.2
__Release date:__ 02 June 2017

- Integrate changes in `iNZightPlots`


## iNZight 3.1.1
__Release date:__ 23 March 2017

- Additions:
  - Time Series module update to new UI
  - Added Import Data (Beta) module (alongside existing one)
  - Save plots as SVG or Interactive HTML (Beta)
- Update Gapminder data
- Survey design object is now passed to Model Fitting module (will soon be depreciated)
- Various other bug fixes in supporting packages


# iNZight 3.1
__Release date:__ 13 December 2016

Main changes in this update are within the Inference functions;
t-tests performed on request.

### New Features

- __Hypothesis testing__: new option under Get Inference
  - one-sample t-tests, two-sample t-tests, ANOVA, and one- and two-way Chi-square testing
  - ability to adjust null value and alternative hypothesis in t-tests
  - ability to specify pooled variance alternative for two-sample t-test
- Store predicted values and residuals under Get Summary (for scatter plots and ANOVA)
- Added `SURFIncomeSurvey` data set (200 row subset) to examples

### Minor Changes

- "factor" reworded to "categorical" in summary/inference output
- some of the inference output has been adjusted slightly for consistency (see above)

### Bug Fixes

- Fix a bug in inference output where reorded factors' confidence intervals and p-values were _not_ reordered!
- Fix issue where confidence intervals for two-way table row proportions were ordered incorrectly







## iNZight 3.0.3
__Release date:__ 12 October 2016

- Bug fix: can now change colours of histograms


## iNZight 3.0.2
__Release date:__ 04 October 2016

- Add "Updates Available" text back to the title (when they're available, of course!)


## iNZight 3.0.1
__Release date:__ 23 September 2016

- Fix up labels and icons so plot toolbar and plot menu match
- Fix a bug where two-level bar plots failed to draw
- Fix a bug where empty levels in two-way bar plots failed to draw
- Fix locator for dot plots
- Fix the updates available checker


# iNZight 3.0
__Release date:__ 5 Septemer, 2016

This is a major new release of iNZight, so many things have changed.
We hope you like them!

### New Features

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


### Major Changes

We can't list everything here, but some of the important ones are listed below.

- Plot toolbar has new icons
- Add to Plot window completely redesigned to make plot modification easier
- Resize points has been adjusted to resize the _area_ proportional to the value.
  Also added an "emphasize" option, which scales sizes linearly.
- __COLOUR__: new, better palettes have been introduced, including [viridis](https://cran.r-project.org/web/packages/viridis/vignettes/intro-to-viridis.html),
  which are colourblind friendly.
- Multiple Response has been overhauled to be more intuitive and fit in with the rest of iNZight.
- (temporary) Just a quick workaround for analysing tables in iNZight: Dataset > Expand Table


### Minor Changes

Given the magnitude of changes, there's little point mentioning the minor ones and bug fixes!


***

## iNZight 2.5.1
__Release date:__ 16 November 2015

- correct the names and URLs in the 'Help > User Guides' menu
- fix bug that occurs when 'Colour by' variable has only one unique value


# iNZight 2.5
__Release date__: 08 November, 2015

## New Features

- specify colours for dotplots and scatterplots from the Add to Plot > Code more variables screen.


## iNZight 2.4.3
__Release date:__ 02 November 2015

- Improve smoothness of dotplot transitions by only redrawing
  when the scaling has changed.


## iNZight 2.4.2
__Release date:__ 28 October 2015

- Add option to display labels internally on numeric x factor
- Fix from iNZightPlots for Get Summary (`n.missing` now correct)


## iNZight 2.4.1
__Release date:__ 21 October 2015

- Remove dependency on `RODBC` package; this causes
  issues on Mac attempting to manually install.


# iNZight 2.4
__Release date__: 12 October, 2015

### New Features

- Include FPC (finite population corrections) in survey designs

### Major Changes

- Replace the drag-drop boxes for variabes with drop-down
  boxes for an alternative method of variable
  selection. Drag-and-drop functionality has been retained.

### Bug Fixes

- Fix the enabling/disabling of buttons when survey designs
  are specified.
- When closing the survey design window, they specified design
  is now checked for validity before saving the design.
- Fix bug in the Multiple Response module that gave an
  "inverse" error message.
- Various other small fixes throughout iNZight and other
  dependency pacakges. See their change logs for details.


# iNZight 2.3
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


## iNZight 2.2.0-1
__Release date:__ 01 July 2015

- iNZightPlots package: fix colour of segmented bar plots to
  match the legend

## iNZight 2.2.0-2
__Release date:__ 27 July 2015

- Fix mapping of confidence intervals (instead of comparison
  intervals) to Year 12 intervals in dot plots.


# iNZight 2.2
__Release date__: 16 June, 2015

### New Features

- users can specify a window size in File > Preferences
- Links to the User Guides found on our website have been
  added, including help buttons in the Add To Plot and Inference
  Information windows.

### Major Changes

- LOCATE functionality now included for dot plots. Also fixes
  several bugs in the previous update for locating points in
  scatter plot.

### Minor Changes

- the "Row Operations" menu has been renamed "Data"


## iNZight 2.1.1
__Release date:__ 09 June 2015

- fix a dependency issue required for aggregating data sets


# iNZight 2.1
__Release date__: 25 May, 2015

### New Features

- New hexagonal binning plot available to large samples
- Live updating sliders for the "Add to Plot" panel, which is
  now embedded into the main window rather than creating an
  additional pop-up window

### Major Changes

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

### Minor Changes

- the number of missing values is now displayed on the plot,
  broken down by variable
- the list of options in the 'Add to Plot' window are now in a drop
  down menu, rather than radio buttons, to save space
- many other small UI improvements

### Bug Fixes

- if `colour by` is set, but the graph is changed to a histogram,
  the legend is removed from the plot
- fixes a bug in the plotting that was causing large values to fall off the axes
  of the plots
- includes other patches from the previous version of iNZight



## iNZight 2.0.4-4
__Release date:__ 10 March 2015

- ping for an internet connection before trying to check for updates


## iNZight 2.0.4-3
__Release date:__ 10 February 2015

- fix a bug causing write-protected directories to crash


## iNZight 2.0.4-1
__Release date:__ 12 December 2014

- fixed up the plot device in Time Series on Linux machines


## iNZight 2.0.4
__Release date__: 18 November 2014

### New Features

- The data set name is displayed above the data view
- The variable type is now displayed as a prefix to the variable
  names in the variable view
- A new "Updates Available" shows up when new versions are
  released
- A new Help tab that includes Licence information, and a link
  to the FAQ and Support pages

### Minor Changes

- In the "Remove Additions" window, "Remove segmentation" has
  been renamed to "Remove colour by" for bar plots, and
  "Restore default labels" is added to remove customised plot
  labels

### Bug Fixes

- Removes development text from Load Data window
- Fixes a bug in the Locate Points feature when the scales are
  vastly different
- Scroll bars added to Reorder Levels window
- The menu has been moved above the plot tabs, so it no longer
  gets cut off


## iNZight 2.0.3
__Release date__: 17 September, 2014

### Major Changes

- A new Multiple Response analysis module has been added to the
  Advanced menu. This utilises the iNZightMR package developed by
  Junjie Zeng

### Minor Changes

- "Add to Plot" window modified for barcharts - can show only
  CIs, and also selects comparison and confidence intervals by default

### Bug Fixes

- Includes a suite of bug fixes in the iNZightPlots package - see its
  NEWS file for these


## iNZight 2.0.2
__Release date__: 17 June, 2014

### New Features

- can now cycle through all two-variable plots for a selected
  Variable 1
- "play" button for the first subset by variable
  (automatically cycle levels)
- data manipulation features added and modified

### Minor Changes

- can now specify to identify min/max points in a dot plot
- in the Add to Plot window for barcharts, reworded "segment by" to "colour by"
  for consistency.
- the transparency slider now runs from 0% to 100%

### Bug Fixes

- identify functionality works for subsets (except when multi plots at once)


## iNZight 2.0.1
__Release date__: 27 March, 2014

### New Features

-  Identify feature added to the "Add To Plot" menu. This
  allows users to interactively label points with `id` or any other
  variable. This feature works for both dot plots and scatter plots.

### Minor Changes

- Users can force scatter plots when the sample size is large,
  rather than using the alternative grid-density plot.

- Missing observations can be converted to categorical
  levels. For categorical variables, the result is a new factor with
  the same levels as the original variable, but the additional
  `missing` factor level. For continuous variables, a new factor
  variable with two levels, `missing` and `not missing` is created.

### Bug Fixes

- Vertical sliders added to the `Rename Levels` and `Rename
      factor levels` windows.
- Reorder levels now works as expected, including sort by
  frequency.
- Bug where the iNZight window closes even if a user clicks
  `cancel` has been fixed.


# iNZight 2.0
__Release date__: 18 January, 2014

### Major Release

The entire iNZight module of iNZightVIT has been rewritten,
with a multitude of changes along the way.
