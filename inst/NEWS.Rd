\name{Change Log}
\title{Change Log for iNZight}
\encoding{UTF-8}

\section{Changes in version 2.2.0-1}{
  Release date: 01 July 2015

  \subsection{PATCHES}{
    \itemize{
      \item iNZightPlots package: fix colour of segmented bar plots to
      match the legend
    }
  }
}

\section{Changes in version 2.2}{
  Release date: 16 June 2015

  \subsection{NEW FEATURES}{
    \itemize{
      \item users can specify a window size in File > Preferences
      
      \item Links to the User Guides found on our website have been
      added, including help buttons in the Add To Plot and Inference
      Information windows.
    }
  }

  \subsection{MAJOR CHANGES}{
    \itemize{
      \item LOCATE functionality now included for dot plots. Also fixes
      several bugs in the previous update for locating points in a
      scatter plot. 
    }
  }

  \subsection{MINOR CHANGES}{
    \itemize{
      \item the "Row Operations" menu has been renamed "Data"
    }
  }
}

\section{Changes in version 2.1.1}{
  Release date: 09 June 2015

  \subsection{BUG FIXES}{
    \itemize{
      \item fix a dependency issue required for aggregating data sets
    }
  }
}

\section{Changes in version 2.1}{
  Release date: 25 May 2015

  \subsection{NEW FEATURES}{
    \itemize{
      \item New hexagonal binning plot available to large samples

      \item Live updating sliders for the "Add to Plot" panel, which is
      now embedded into the main window rather than creating an
      additional pop-up window
    }
  }

  \subsection{MAJOR CHANGES}{
    \itemize{
      \item The "Add to Plot", "Remove Additions" and
      "Inference Information" windows have been integrated into the main
      window with the aim of making the interface cleaner and easier to
      navigate.
      
      \item When colouring points by a numeric variable, a continuous colour
      range is used rather than a discrete one.
      
      \item The plotting package has been rewritten to accomodate survey
      designs (this will be integrated with iNZight itself at a later
      date). However, the plots have been modified to be cleaner and far
      more efficient. Additionally, the algorithms used for the
      inference information have been updated. For more information on
      this, see the iNZightPlots change log.

      \item The "Get Summary" and "Get Inference" outputs have been
      redesigned to hopefully give a more intuitive, cleaner
      presentation of the information. 
      
      \item From dot plots and histograms, the comparison (and confidence) 
      interval values can be read from the graph by clicking 'Get Values'
      in the 'Inference Information' panel. This includes the Year 12 intervals
      applied to medians.

      \item LOCATOR: functionality for scatterplots vastly improved,
      allowing selection of related points, retaining points over
      multiple graphcs, and selecting points by a variable.
      (similar upgrades for dot plots coming in v2.2)
    }
  }

  \subsection{MINOR CHANGES}{
    \itemize{
      \item the number of missing values is now displayed on the plot,
      broken down by variable
      
      \item the list of options in the 'Add to Plot' window are now in a drop
      down menu, rather than radio buttons, to save space

      \item many other small UI improvements
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item if 'colour by' is set, but the graph is changed to a histogram,
      the legend is removed from the plot
      
      \item fixes a bug in the plotting that was causing large values to fall off the axes
      of the plots
      
      \item includes other patches from the previous version of iNZight
    }
  }
}

\section{Changes in version 2.0.4}{
  Release date: 18 November 2014
  
  \subsection{NEW FEATURES}{
    \itemize{
      \item The data set name is displayed above the data view

      \item The variable type is now displayed as a prefix to the variable
      names in the variable view
      
      \item A new "Updates Available" shows up when new versions are
      released
      
      \item A new Help tab that includes Licence information, and a link
      to the FAQ and Support pages
    }
  }
  
  \subsection{MINOR CHANGES}{
    \itemize{
      \item In the "Remove Additions" window, "Remove segmentation" has
      been renamed to "Remove colour by" for bar plots, and
      "Restore default labels" is added to remove customised plot
      labels
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Removes development text from Load Data window
      \item Fixes a bug in the Locate Points feature when the scales are
      vastly different
      \item Scroll bars added to Reorder Levels window
      \item The menu has been moved above the plot tabs, so it no longer
      gets cut off
    }
  }

  \subsection{Patch 2.0.4-1}{
    Date: 12-12-2014

    \itemize{
      \item fixed up the plot device in Time Series on Linux machines
    }
  }

  \subsection{Patch 2.0.4-3}{
    Date: 10-02-2015

    \itemize{
      \item fix a bug causing write-protected directories to crash
    }
  }

  \subsection{Patch 2.0.4-4}{
    Date: 10-03-2015

    \itemize{
      \item ping for an internet connection before trying to check for updates
    }
  }
}


\section{Changes in version 2.0.3}{
  Release date: 17 September 2014
  
  \subsection{MAJOR CHANGES}{
    \itemize{
      \item A new Multiple Response analysis module has been added to the
      Advanced menu. This utilises the iNZightMR package developed by
      Junjie Zeng
    }
  }
  
  \subsection{MINOR CHANGES}{
    \itemize{
      \item "Add to Plot" window modified for barcharts - can show onlyake
      CIs, and also selects comparison and confidence intervals by default
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Includes a suite of bug fixes in the iNZightPlots package - see its
      NEWS file for these
    }
  }
}

\section{Changes in version 2.0.2}{
  Release date: 17 June 2014
  
  \subsection{NEW FEATURES}{
    \itemize{
      \item can now cycle through all two-variable plots for a selected
      Variable 1

      \item "play" button for the first subset by variable
      (automatically cycle levels)

      \item data manipulation features added and modified
    }
  }
  
  \subsection{MINOR CHANGES}{
    \itemize{
      \item can now specify to identify min/max points in a dot plot

      \item in the Add to Plot window for barcharts, reworded "segment by" to "colour by" 
      for consistency.

      \item the transparency slider now runs from 0% to 100%
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item  identify functionality works for subsets (except when multi plots at once)
    }
  }
}

\section{Changes in version 2.0.1}{
  Release date: 27 March 2014
  
  \subsection{NEW FEATURES}{
    \itemize{
      \item Identify feature added to the "Add To Plot" menu. This
      allows users to interactively label points with `id` or any other
      variable. This feature works for both dot plots and scatter plots.
    }
  }
  
  \subsection{MINOR CHANGES}{
    \itemize{
      \item Users can force scatter plots when the sample size is large,
      rather than using the alternative grid-density plot.

      \item Missing observations can be converted to categorical
      levels. For categorical variables, the result is a new factor with
      the same levels as the original variable, but the additional
      `missing` factor level. For continuous variables, a new factor
      variable with two levels, `missing` and `not missing` is created.
    }
  }

  \subsection{BUG FIXES}{
    \itemize{
      \item Vertical sliders added to the `Rename Levels` and `Rename
      factor levels` windows.

      \item Reorder levels now works as expected, including sort by
      frequency.

      \item Bug where the iNZight window closes even if a user clicks
      `cancel` has been fixed.
    }
  }
}

\section{Changes in version 2.0.0}{
  Release date: 18 January 2014
  
  \subsection{MAJOR RELEASE}{
    \itemize{

      \item The entire iNZight module of iNZightVIT has been rewritten,
      with a multitude of changes along the way.
      
    }
  }
}
