path_to_libraries <- "C:/Users/CRunyon/R/R-Library"
if (dir.exists(path_to_libraries)) {
    .libPaths(c(path_to_libraries, .libPaths()))
}

if (interactive() && Sys.getenv("RSTUDIO") == "" && Sys.getenv("TERM_PROGRAM") == "vscode") {
    vscode_r_session_dir <- "C:/Users/CRunyon/.vscode/extensions/reditorsupport.r-2.8.8/R/session"
    vscode_r_init <- file.path(vscode_r_session_dir, "init.R")
    if (file.exists(vscode_r_init)) {
        local({
            old_wd <- getwd()
            on.exit(setwd(old_wd), add = TRUE)
            setwd(vscode_r_session_dir)
            source(vscode_r_init, local = FALSE)
        })
        if (exists(".First.sys", envir = globalenv(), inherits = FALSE)) {
            .First.sys()
        }
    }
}