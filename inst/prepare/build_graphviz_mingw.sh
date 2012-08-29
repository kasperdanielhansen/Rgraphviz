#!/bin/bash -e

## This is where R is installed
R_HOME="/c/Program Files/R/R-2.15.0"
## The next two directories comes from Rtools, the
## bin dir for tools and the bin dir for GCC
RTOOLS_BIN_DIR=/c/Rtools215/bin
RTOOLS_GCC_DIR=/c/Rtools215/gcc-4.6.3/bin
## The next two directories come from MinGW
MINGW_BIN_DIR=/c/MinGWALL/bin
BIN_DIR=/bin

## This is the root of the Rgraphviz source tree
RGRAPHVIZ_DIR=../../../Rgraphviz

## Setup check

echo "Checking setup variables"
if ! test -d "${R_HOME}"; then
  echo "R home directory not found"
  exit 1
fi

if ! test -d "${RTOOLS_BIN_DIR}" -a -d "${RTOOLS_GCC_DIR}"; then
  echo "Rtools dirs not found"
  exit 1
fi

if ! test -d "${MINGW_BIN_DIR}" -a -d "${BIN_DIR}"; then
  echo "MinGW dirs not found"
  exit 1
fi

if ! test -d "${RGRAPHVIZ_DIR}"; then
  echo "Rgraphviz dir not found"
  exit 1
fi

echo "Setup environment variables"
echo "  R_HOME=${R_HOME}"
echo "  RTOOLS_BIN_DIR=${RTOOLS_BIN_DIR}"
echo "  RTOOLS_GCC_DIR=${RTOOLS_GCC_DIR}"
echo "  MINGW_BIN_DIR=${MINGW_BIN_DIR}"
echo "  BIN_DIR=${BIN_DIR}"
echo "  RGRAPHVIZ_DIR=${RGRAPHVIZ_DIR}"

build_graphviz () {

    if test -z "${THIS_ARCH}"; then
	echo "THIS_ARCH does not appear to be set"
	return [1]
    fi

    echo "Running with --arch=${THIS_ARCH}"

    R_CMD="${R_HOME}/bin/R"
    Rscript_CMD="${R_HOME}/bin/Rscript"

    echo "Additional (inferred) environment variables for --arch=${THIS_ARCH}"
    export PATH="${RTOOLS_GCC_DIR}:${MINGW_BIN_DIR}:${BIN_DIR}:${RTOOLS_BIN_DIR}"
    echo "  PATH=${PATH}"
    CC=`"${R_CMD}" --arch ${THIS_ARCH} CMD config CC`
    echo "  CC=${CC}"
    CFLAGS=`"${R_CMD}" --arch ${THIS_ARCH} CMD config CFLAGS`
    echo "  CFLAGS=${CFLAGS}"
    CXX=`"${R_CMD}" --arch ${THIS_ARCH} CMD config CXX`
    echo "  CXX=${CXX}"
    CXXFLAGS=`"${R_CMD}" --arch ${THIS_ARCH} CMD config CXXFLAGS`
    echo "  CXXFLAGS=${CXXFLAGS}"
    MAKE=`"${R_CMD}" --arch ${THIS_ARCH} CMD config MAKE`
    echo "  MAKE=${MAKE}"
    ## It may appear that getting R_ARCH, R_ARCH_BIN is unnecessary given that we have
    ##   THIS_ARCH.  Nevertheless, for the future, I keep them around.
    R_ARCH=`"${Rscript_CMD}" --arch ${THIS_ARCH} -e 'cat(sprintf(\"%s\", {Sys.getenv(\"R_ARCH\")}))'`
    echo "  R_ARCH=${R_ARCH}"
    R_ARCH_BIN=`"${Rscript_CMD}" --arch ${THIS_ARCH} -e 'cat(sprintf(\"%s\", Sys.getenv(\"R_ARCH_BIN\")))'`
    echo "  R_ARCH_BIN=${R_ARCH_BIN}"

    echo "Copying graphviz dir for --arch=${THIS_ARCH}"
    ## Preparing a build dir
    rm -Rf graphviz
    cp -R "${RGRAPHVIZ_DIR}/src/graphviz" . # (not sure why .svn dirs are not copied)

    echo "Patching graphviz dir for --arch=${THIS_ARCH}"
    patch graphviz/lib/common/colxlate.c < patches/colxlate.c-patch
    
    echo "Preparing for configure"
    BUILD_DIR=`pwd`
    cd graphviz
    patch -p1 < ../patches/graphviz_windows-patch
    touch configure
    touch libltdl/configure
    ./configure --disable-silent-rules\
        CC="${CC}" CXX="${CXX}" CFLAGS="${CFLAGS} -g -DR_OK=4 -I../../libltdl" CXXFLAGS="${CXXFLAGS} -g"\
        --prefix="${BUILD_DIR}/libwin${R_ARCH}"\
        --libdir="${BUILD_DIR}/libwin${R_ARCH}/lib"\
        --includedir="${BUILD_DIR}/libwin${R_ARCH}/include"\
        --enable-swig=no\
        --enable-static\
        --disable-shared\
        --with-pic\
        --disable-ltdl\
        --without-x\
        --without-expat\
        --without-devil\
        --without-rsvg\
        --without-ghostscript\
        --without-visio\
        --without-pangocairo\
        --without-lasi\
        --without-glitz\
        --without-freetype2\
        --without-fontconfig\
        --without-rpat\
        --without-glut\
        --without-gts\
        --without-png\
        --without-tcl\
        --without-jpeg 2>&1 | tee ${BUILD_DIR}/graphviz.configure.${THIS_ARCH}.log
    echo "Start make graphviz for --arch=${THIS_ARCH}"
    ${MAKE} 2>&1 | tee ${BUILD_DIR}/graphviz.make.${THIS_ARCH}.log
    echo "Start make install graphviz for --arch=${THIS_ARCH}"
    ${MAKE} install
    cd ${BUILD_DIR}
    rm -Rf ${BUILD_DIR}/libwin${R_ARCH}/share ${BUILD_DIR}/libwin${R_ARCH}/lib/pkgconfig
}

rm -rf libwin

THIS_ARCH=i386
build_graphviz

THIS_ARCH=x64
build_graphviz

echo " "
echo "*************************************************************"
echo "Graphviz has now been build for Windows, inside the directory 'libwin'"
echo "You can add the new libraries to Rgraphviz by copying it into Rgraphviz/src."
echo "Depending on where the script is run relative to the source tree, the following might work:"
echo "  cp -R libwin ../../src"
echo "If everything is under subversion, please do a 'svn stat' and confirm that"
echo "eveything has been updated and nothing needs to be removed."

exit 0
