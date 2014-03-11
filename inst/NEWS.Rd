\name{NEWS}
\title{NEWS file for the iNZight package}
\encoding{UTF-8}

\section{Changes in version 2.0.1}{
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
