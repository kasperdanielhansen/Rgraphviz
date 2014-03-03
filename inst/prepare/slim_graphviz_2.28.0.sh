#!/bin/bash -e

echo "Unpacking"
rm -Rf graphviz-2.28.0
tar xzf graphviz-2.28.0.tar.gz
cd graphviz-2.28.0

echo "Slimming graphviz"
patch -p1 < ../patches_2.28.0/graphviz-2.28.0-slim.patch

echo "Applying bugfixes"
patch -p1 < ../patches_2.28.0/graphviz-2.28.0-bugs.patch

echo "Fixing lt~obsolete"
patch aclocal.m4 -i ../patches_2.28.0/aclocal.m4-patch
patch libltdl/aclocal.m4 -i ../patches_2.28.0/libltdl-aclocal.m4-patch
mv m4/lt~obsolete.m4 m4/lt-obsolete.m4 
mv libltdl/m4/lt~obsolete.m4 libltdl/m4/lt-obsolete.m4 

echo "Removing directories"
rm -Rf debian doc contrib graphs macosx windows\
  cmd tclpkg rtest share plugin.demo
cd rm -Rf devil gd gdiplus gdk_pixbuf glitz gs gtk\
     lasi ming pango quartz rsvg visio xlib
cd ..
cd lib
  rm -Rf ast cgraph dotgen2 expr gd glcomp gvc.dev\
     gvc.vcproj gvpr ingraphs sfio topfish vmalloc
cd ..
rm lib/rbtree/test_rb

echo "Reconfiguring"
autoreconf
rm -Rf autom4te.cache
cd libltdl
autoreconf
rm -Rf autom4te.cache
cd ..

echo "Fixing configure bug on FreeBSD"
echo "This step may have to be done by hand"
cd graphviz-2.28.0
patch -p1 < ../patches_2.28.0/graphviz-2.28.0-configure.patch
autoreconf
cd ..

echo 'Fixing #include < to be #include "'
cp find_includes.sh ../../src/graphviz/lib
cd ../../src/graphviz/lib
./find_includes.sh

