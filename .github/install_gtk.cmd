if exist D:\a\_temp\Library\RGtk2\gtk\NUL exit 0

echo "Installing gtk ..."
curl -L -o gtk.zip https://inzight.nz/data/gtk+-bundle_2.22.1-20101229_win64.zip
md gtk
7z x gtk.zip -ogtk > nul
del gtk.zip

echo "Installing RGtk2 ..."
Rscript .github\install_gtk_win.R

echo "Moving gtk to RGtk2 ..."
mv gtk D:\a\_temp\Library\RGtk2\
