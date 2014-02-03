\name{NEWS}
\title{NEWS file for the iNZight package}
\encoding{UTF-8}

\section{Changes in version 2.0.0}{
  \subsection{MAJOR CHANGES}{
    \itemize{

      \item The entire iNZight module has been re-written using
      gWidgets2 and reference classes. This provides a much more
      extensible environment for the addition of new features.

      \item The plotting functionality has been rewritten using the grid
      package, and has been self contained in its own package called
      iNZightPlots.
      
    }
  }

  \subsection{MINOR CHANGES}{
    \itemize{
      The version number has been incremented to version 2.0, because
      the previous release should technically have been version
      1.x. However, this new release is completely incompatible with the
      old system, so deserves to be version 2.

      This version numbering has been updated over the entire
      `iNZightVIT` bundle.
    }
  }

  \subsection{CHANGES IN iNZightPlots}{
    \itemize{
      \item Transparency added to dot and scatterplots.

      \item Matrix of all levels of each grouping variable when second
      slider is set to `_MULTI`.

      \item New plot types for large sample sizes.

      \item Ability to customize the plot titles and axis labels.
    }
  }
}

\section{Changes in version 1.0.2-20130306}{
  \subsection{MINOR CHANGES}{
    \itemize{

      \item Using 'fivenum' for quartiles instead of 'summary'. This
      will produce correct output instead of a close approximation.

    }
  }
}

\section{Changes in version 1.0.2-20130227}{
  \subsection{MINOR CHANGES}{
    \itemize{

      \item When using 'Get Summary' or 'Get Inference', use 5
      significant figures, instead of rounding to a number of decimal
      places.

    }
  }
}

\section{Changes in version 1.0.2-20130122}{
  \subsection{NEW FEATURES}{
    \itemize{

      \item Model fitting functionality largely broken off into a new
      package: 'iNZightRegression'. 'iNZightRegression' aims to make all
      of the model fitting functionality easily available in an R
      console.

      \item Use new 'iNZightSummary' function for summary
      output. Intended for use as a direct replacement for
      'print.summary'. (From 'iNZightRegression')

      \item When using 'Get Inference', now also show an
      F-statistic. Also shows the degrees of freedom and the associated
      p-value.

      \item Removed 'Anova', 'Goodness of Fit', 'Drop Observations', and
      'Prediction' from model fitting.

      \item Adding partial residual plots. (From 'iNZightRegression')

      \item Adding modified 'plot.lm' plots. Adds bootstrap smoothers to
      the plots. (From 'iNZightRegression')

      \item Adding 'histogramArray' and 'qqplotArray' functions to show
      the differences between residuals in our data and residuals
      *randomly generated* from our model.

      \item Removed added effects plots, removes dependency on 'effects'
      package.

      \item Adding margin of error plots, along with associated multiple
      pairwise comparison output. These are accessed via the 'Factor
      Levels' menu.

    }
  }
  \subsection{BUG FIXES}{
    \itemize{

      \item Ensure 'gWidgets' is using the 'RGtk2' toolkit.

    }
  }
}

\section{Changes in version 1.0.2-20121109}{
  \subsection{BUG FIXES}{
    \itemize{

      \item 'gpairs' wasn't loading the 'barcode' dependency
      correctly. As a result we now explicity load the 'barcode'
      package. This fixes scatter plots with a mixture of continuous and
      categorical variables.

      \item When producing bootstrap inferences on two variables (1
      cont, 1 cat), the estimate of the median was erroneously using the
      mean. Reported by Jim Hogan.

    }
  }
}

\section{Changes in version 1.0.2-20121008}{
  \subsection{NEW FEATURES}{
    \itemize{

      \item Added updating mechanism to keep iNZight up to date without
      grabbing the entire distribution. This can be called by running
      'updateiNZight()'. The iNZightVIT distribution does include a
      script 'UPDATE_iNZightVIT.bat' ('UPDATE_iNZightVIT.command' for
      OSX) to automate this task.

      \item Added a new function 'iNZightBugReport()', which
      provides instructions for creating a bug report.

      \item Version strings are tagged with a date. This is exposed in
      the main window title.

    }
  }

  \subsection{MISC}{
    \itemize{

      \item Reworked the dependency structure of iNZight. This should
      make it slightly faster to load and prints less to the console.

    }
  }

  \subsection{BUG FIXES}{
    \itemize{

      \item Exporting data now uses the correct output formats.

      \item Because of the alpha bug with the 'windows()' device, use
      opaque lines for the 'Compare Series' Time Series feature
      (equivalent in colour to a single line previously).

    }
  }
}

\section{Changes in version 1.0.2-20120822}{
  \subsection{NEW FEATURES}{
    \itemize{

      \item Better handling of large datasets. The data-view is now
      disabled in favour of variable-view only from 200,000 cells.

      \item Added a data reshaping function (from columns per group to
      cases by variable) to "Manipulate variables".

    }
  }
}

\section{Changes in version 1.0.2-20120725}{
  \subsection{NEW FEATURES}{
    \itemize{
      \item A Time Series module added to iNZight (under Advanced).
    }
  }
}

\section{Changes in version 1.0.2-20120417}{
  \subsection{BUG FIXES}{
    \itemize{
      \item Various fixes.
    }
  }
}
