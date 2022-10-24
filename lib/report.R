create.md = function(title=getwd(),file="report.Rmd") {
    cat(paste(sep="\n","---",
              paste0("title: '",title,"'"),
              paste0("author: '",Sys.getenv("USERNAME"),"'"),
              paste0("date: '",format(Sys.Date()),"'"),
              "output:",
              "  html_document:",
              "    toc: yes",
              "---\n"),fill=F,append=F,file=file)
}

templr::import("xml2","jsonlite","github:timelyportfolio/parcoords","htmltools","crosstalk","DT")
print.md = function(title=NULL,txt,xml=F,file="report.Rmd") {
    if (isTRUE(xml)) {
        print.md(title,"",xml=F,file=file)
	all = NULL
        try(all <- xml_children(read_xml(paste0("<result>",txt,"</result>"))))
	if (is.null(all)) {warning(paste0("Could not parse as xml:",txt))}
        r_data = NULL
        r_model = NULL
        for (a in all) {
            if (xml_name(a)=="data_json") {
                r_data = paste0("data = data.frame(jsonlite::fromJSON('",gsub("'","\'",xml_text(a)),"'))")
            } else if (xml_name(a)=="model_json") {
                r_model = paste0("model = data.frame(jsonlite::fromJSON('",gsub("'","\'",xml_text(a)),"'))")
            } else if (xml_name(a)=="HTML") {
                print.md(title=NULL,paste0("<html>",paste0(xml_contents(a),collapse = " "),"</html>"),xml=F,file=file)
            } else
                print.md(title=NULL, paste0("  * ",xml_name(a),": ",xml_text(a)),xml=F,file=file)
        }

        if (!is.null(r_data) || !is.null(r_model)) {
            if (is.null(r_data)) r_data = "data = data.frame()"
            if (is.null(r_model)) r_model = "model = data.frame()"
            r_parcoords = paste0("```{r echo=F}\n ",r_data," \n ",r_model," \n",
            " missing = names(model)[!is.element(names(model),names(data))]; \n",
            " if (length(missing)>0) data[[missing]] = NA \n all = rbind(model,data) \n",
            " all$src = c(rep('model',nrow(model)),rep('data',nrow(data))) \n",
            " all <- crosstalk::SharedData$new(all, group='grp",floor(10000*runif(1)),"') \n",
            " htmltools::tagList(htmltools::tags$div(",
            "  parcoords::parcoords(all, reorderable = T,brushMode = '1D-axes-multi',rownames = F,queue = T,alpha = 0.8,",
            "    color = list(colorBy='src',colorScale = htmlwidgets::JS('d3.scale.category10()')) )),",
            "  htmltools::tags$div(DT::datatable(all,extensions=c('Scroller','Buttons'),",
            "    options = list(deferRender = TRUE, scrollY = 400, scroller = TRUE, dom='Bfrtip',buttons=c('copy','csv'))))) \n",
            "```\n")
            print.md(title=NULL, paste0("  * View\n",r_parcoords),xml=F,file=file)
        }

        return()
    }

    if (!is.null(title)) {
        if (substr(title,0,1)=="#")
            cat(paste0("\n",title,"\n"),fill=F,append=T,file=file)
        else
            cat(paste0("\n# ",title,"\n"),fill=F,append=T,file=file)
    }

    cat(paste0("\n",txt,"\n"),fill=F,append=T,file=file)
}

plot.md = function(plot.fun,file="report.Rmd",w=400,h=400,...) {
    name = paste0(".",floor(1000*runif(1)),".png")
    png(name,width = w,height = h)
    plot.fun(...)
    dev.off()

    cat(paste0("![](",name,")"),fill=F,append=T,file=file)
}

templr::import("rmarkdown")
report.md = function(file="report.Rmd") {
    render(file)
}

templr::import("readr")
run.algorithm <- function(algorithm_file,
                          fun,
                          options=NULL) {

    Algorithm_name=gsub("\\.(.*)","",basename(algorithm_file))
    Algorithm_options = templr::parse.algorithm(algorithm_file)$options
    if (!is.null(options))
        for (o in names(options)) Algorithm_options[[o]] = options[[o]]

    report_file=paste0("report_",Algorithm_name,"-",paste0(collapse=",",fun$output),".Rmd")
    cleanchar = function(path) gsub("\"","",  gsub("]","", gsub("[","", path ,fixed=T),fixed=T),fixed=T)
    if (!is.null(options))  # will use report_seed=1.md if option seed was modified from default values
        report_file = gsub("report",paste0("report_",paste0(cleanchar(names(options)),"=",cleanchar(options),collapse="_")),report_file)

    create.md(paste0(Algorithm_name," / ", fun$output),file = report_file)
    print.md("Algorithm",paste0("```{r eval=F}\n",readr::read_file(algorithm_file),"```\n"),file = report_file)
    print.md("Parameters",gsub("\\$","*",paste0(collapse="\n",capture.output(str(Algorithm_options))[-1])),file = report_file)
    if (exists("print.f")) {
    if (is.function(print.f)) {
        print.md("Objective",plot.md(print.f),file = report_file)
    }} else
        print.md("Objective",paste0("```{r eval=F}\n",paste0(capture.output(print(f)),collapse="\n"),"\n```\n"),file = report_file)


    result = NULL
    tryCatch({
        result <- templr::run.algorithm(
            algorithm_file,
            objective_function=fun$f,
            input=fun$input,
            output=fun$output,
            options=options,
            work_dir=".",
            trace=function(...) {
                print(...)
                print.md(title=NULL,txt=...,file=report_file)
            },
            silent=TRUE,save_data=FALSE)
    }, error=function(msg) {
            print.md("ERROR",msg,file=report_file)
            return(NULL)
    })
    
    print.md("Result",result,xml = T,file = report_file)

    report.md(file = report_file)
    return(templr::list.results(result))
}