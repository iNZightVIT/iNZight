if exist D:\a\_temp\Library\RGtk2\gtk\NUL exit 0

echo "Installing gtk ..."
curl -L -o gtk.zip https://inzight.nz/data/gtk+-bundle_2.22.1-20101229_win64.zip
md gtk
7z x gtk.zip -ogtk > nul
del gtk.zip

if exist D:\a\_temp\Library\RGtk2\NUL (
  mv gtk D:\a\_temp\Library\RGtk2\
  exit 0
)

echo "Installing RGtk2 ..."
Rscript -e "if (!requireNamespace('remotes', quietly = TRUE)) install.packages('remotes')"
Rscript -e "Sys.setenv(GTK_PATH = file.path(getwd(), 'gtk')); remotes::install_github(c('tmelliott/RGtk2/RGtk2', 'tmelliott/cairoDevice'), type = 'source')"

mv gtk D:\a\_temp\Library\RGtk2\
