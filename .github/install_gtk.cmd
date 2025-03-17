echo "Downloading gtk ..."
curl -L -o gtk.zip https://inzight.nz/data/gtk+-bundle_2.22.1-20101229_win64.zip
md gtk
7z x gtk.zip -ogtk > nul
del gtk.zip
