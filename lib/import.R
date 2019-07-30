# options(repos="http://cran.irsn.fr")
lib.loc = normalizePath(file.path("lib"))
dir.create(lib.loc,showWarnings = F)

#' @test import("tidyverse","jsonlite","rlist","github:timelyportfolio/parcoords")
import = function(...) {
    libs <- list(...)
    for (l in unlist(libs)) { # GitHub or ....
        if (isTRUE(grep(":",l)==1)) {
            src=gsub(":.*","",l)
            n=gsub(".*/","",l)
            path=gsub(".*:","",gsub("/[a-zA-Z0-9]*","",l))
        } else { # CRAN
            src=NULL
            n=l
            path=NULL
        }

        in_base = F
        try(in_base <- library(n,logical.return = T,character.only = T),silent = T)
        if (!in_base) {
            in_loc = F
            try(in_loc <- library(n,logical.return = T,character.only = T,lib.loc = lib.loc) ,silent = T)
            if (!in_loc) {
                if (!is.null(src)) {
                    print(paste0("Using devtools to install ",l))
                    import("devtools")
                    devtools::dev_mode(on=T,path = lib.loc)
                    eval(parse(text=paste0("try(devtools::install_",src,"(file.path(path,n),force=T))")))
                    devtools::dev_mode(on=F)
                } else {
                    print(paste0("Using install.packages to install ",l))
                    try(install.packages(l,lib = lib.loc,keep_outputs=T),silent=F)
                }
                print(paste0("Available packages in ",lib.loc,":\n",paste0(collapse=", ",installed.packages(lib.loc=lib.loc)[,'Package'])))
            } else print(paste0("Loaded package ",l," in ",lib.loc,":\n",paste0(collapse=", ",list.files(lib.loc))))

            if (!library(n,logical.return = T,character.only = T,lib.loc = lib.loc))
                stop(paste0("Cannot load package ",l," as not available in ",lib.loc,":\n",paste0(collapse=", ",list.files(lib.loc))))
        } else print(paste0("Loaded package ",l," in ",paste0(collapse=", ",.libPaths())))
    }
}


