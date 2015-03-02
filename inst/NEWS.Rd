\name{Change Log}
\title{Change Log for iNZight}
\encoding{UTF-8}

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
