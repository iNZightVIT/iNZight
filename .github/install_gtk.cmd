echo "Installing RGtk2 ..."
Rscript -e "install.packages('RGtk2', repos = 'https://cloud.r-project.org')"

echo "Installing gtk ..."
wget http://ftp.gnome.org/pub/gnome/binaries/win64/gtk+/2.22/gtk+-bundle_2.22.1-20101229_win64.zip -o gtk.zip
md gtk
7z x gtk.zip > nul
del gtk.zip
mv gtk D:\a\_temp\Library\RGtk2\
