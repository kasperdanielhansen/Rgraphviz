.onLoad <- function(lib, pkg, where) {
	if(.Platform$OS.type == "windows") {
		whichGvcDll <- unname(Sys.which("gvc.dll"))
		if(nchar(whichGvcDll) >= 7) {
			loadedDlls <- getLoadedDLLs()
			graphPackagePosition <- which("graph" == names(loadedDlls))
			graphPackageLoaded <- (length(graphPackagePosition) > 0)
			if(graphPackageLoaded) {
				graphDllPath <- loadedDlls[[graphPackagePosition]][["path"]]
				graphLibraryPath <- dirname(dirname(graphDllPath))
				graphDllName <- basename(graphDllPath)
				library.dynam.unload(graphDllName, graphLibraryPath)
			}
			dyn.load(whichGvcDll)
			if(graphPackageLoaded) {
				library.dynam(graphDllName, "graph")
			}
		}
	}
	library.dynam( "Rgraphviz", pkg, lib )
	.Call("Rgraphviz_init", PACKAGE="Rgraphviz")
	require("methods")
}
