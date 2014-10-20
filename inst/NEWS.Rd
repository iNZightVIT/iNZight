\name{NEWS}
\title{NEWS file for the iNZight package}
\encoding{UTF-8}

\section{Changes in version 2.0.3}{
  \subsection{MAJOR CHANGES}{
    A new Multiple Response analysis module has been added to the
    Advanced menu. This utilises the iNZightMR package developed by
    Junjie Zeng.
  }
  
  \subsection{MINOR CHANGES}{
    \itemize{
      \item "Add to Plot" window modified for barcharts - can show only
      CIs, and also selects comparison and confidence intervals by default
    }
  }

  \subsection{BUG FIXES}{
    Includes a suite of bug fixes in the iNZightPlots package - see its
    NEWS file for these
  }

  \subsection{PATCH 2.0.3-1} {
    Fixed an issue in the Reorder Levels window that now adds scroll
    bars to the window if it's too big.
  }
  \subsection{PATCH 2.0.3-2} {
    Display the data set name
  }
}

\section{Changes in version 2.0.2}{
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
  \subsection{MAJOR RELEASE}{
    \itemize{

      \item The entire iNZight module of iNZightVIT has been rewritten,
      with a multitude of changes along the way.
      
    }
  }
}
