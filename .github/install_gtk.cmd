echo "Installing RGtk2 ..."
Rscript -e "install.packages('RGtk2', repos = 'https://cloud.r-project.org')"

echo "Installing gtk ..."
curl -L -o gtk.zip http://ftp.gnome.org/pub/gnome/binaries/win64/gtk+/2.22/gtk+-bundle_2.22.1-20101229_win64.zip
md gtk
7z x gtk.zip -ogtk > nul
REM del gtk.zip
mv gtk D:\a\_temp\Library\RGtk2\