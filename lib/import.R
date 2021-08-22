options(data.frame(repos="http://cran.mirror.garr.it/mirrors/CRAN/"))#http://cran.irsn.fr")
lib.loc = normalizePath(file.path(getwd(),"lib"))
dir.create(lib.loc,showWarnings = F)
.libPaths(lib.loc)

import.info = FALSE

#' @test import("tidyverse","jsonlite","rlist","github:timelyportfolio/parcoords")
import = function(...) {
    libs <- list(...)
    if (!is.na(libs))
    if (length(libs)>0)
    for (l in na.exclude(unlist(libs))) {
        if (isTRUE(grep(":",l)==1)) { # GitHub or ...
            src=gsub(":.*","",l)
            n=gsub(".*/","",l)
            path=gsub(".*:","",gsub("/[a-zA-Z0-9]*","",l))
        } else { # CRAN
            src=NULL
            n=l
            path=NULL
        }

        if (import.info) info = function(t) {cat(t);cat("\n")} else info = function(t){}

        in_base = F
        try(in_base <- library(n,logical.return = T,character.only = T, quietly = T),silent = T)
        if (!in_base) {
            in_loc = F
            try(in_loc <- library(n,logical.return = T,character.only = T, quietly = T,lib.loc = lib.loc) ,silent = T)
            if (!in_loc) {
                if (!is.null(src)) {
                    info(paste0("Using devtools to install ",l))
                    import("devtools")
                    devtools::dev_mode(on=T,path = lib.loc)
                    if (src == "github") Sys.setenv(GITHUB_PAT="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") # github clone only token
                    eval(parse(text=paste0("try(devtools::install_",src,"(file.path(path,n),force=T))")))
                    devtools::dev_mode(on=F)
                } else {
                    info(paste0("Using install.packages to install ",l))
                    try(install.packages(l,lib = lib.loc,keep_outputs=T,dependencies=T),silent=F)
                }
                info(paste0("Available packages in ",lib.loc,": ",paste0(collapse=", ",installed.packages(lib.loc=lib.loc)[,'Package'])))
            } else info(paste0("Loaded package ",l," in ",lib.loc,": ",paste0(collapse=", ",list.files(lib.loc))))

            try_load=F
            try(try_load <- library(n,logical.return = T,character.only = T, quietly = T,lib.loc = lib.loc),silent = T)
            if (!try_load) {
                try(try_load <- library(n,logical.return = T,character.only = T, quietly = F,lib.loc = lib.loc),silent = F)
                stop(paste0("Cannot load package ",l," as not available in ",lib.loc,": ",paste0(collapse=", ",list.files(lib.loc))))
            }
        } else
            info(paste0("Loaded package ",l," in ",paste0(collapse=", ",.libPaths())))
    }
}


