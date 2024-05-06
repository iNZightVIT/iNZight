if exist D:\a\_temp\Library\RGtk2\gtk\NUL exit 0

echo "Installing gtk ..."
curl -L -o gtk.zip http://ftp.gnome.org/pub/gnome/binaries/win64/gtk+/2.22/gtk+-bundle_2.22.1-20101229_win64.zip
md gtk
7z x gtk.zip -ogtk > nul
del gtk.zip

mv gtk D:\a\_temp\

@REM if not exist D:\a\_temp\Library\RGtk2\NUL (
@REM   echo "Installing RGtk2 ..."
@REM   Rscript -e "Sys.setenv(GTK_PATH = file.path(getwd(), 'gtk')); install.packages('RGtk2', repos = 'https://r.docker.stat.auckland.ac.nz', type = 'source', INSTALL_opts = c('--no-multiarch'))"
@REM   mv gtk D:\a\_temp\Library\RGtk2\
@REM )

@REM echo "Installing cairoDevice ..."
@REM if not exist D:\a\_temp\Library\cairoDevice\NUL (
@REM   Rscript -e "Sys.setenv(GTK_PATH = 'D:/a/_temp/Library/RGtk2/gtk'); install.packages('cairoDevice', repos = 'https://r.docker.stat.auckland.ac.nz', type = 'source', INSTALL_opts = c('--no-multiarch'))"
@REM )
