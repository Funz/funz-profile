source(file.path("../funz-profile/lib/import.R"))

#' @test parse.algorithm("../algorithms/Brent.R")
#' @test parse.algorithm("../algorithms/RSUR.R")
#' @test gsub(perl = T,"(,)(?=(?:[^']|'[^']*')*$)",';',"a=1,b=2,c='list(1,2)'")
parse.algorithm = function(file){
    lines=readLines(file)

    name=unlist(strsplit(file,"/"))
    name=sub("\\.R","",name[length(name)])
    if (nchar(name)==0) name="null"

    title="?"
    help="?"
    authors="?"
    version="?"
    references="?"
    type="?"
    output="?"
    requires=NULL
    options = list()
    options.help = list()

    for (i in 1:length(lines)) {
        if (strtrim(lines[i],7)=="#title:") {
            title=sub("#title:\\s*","",lines[i])
        } else if (strtrim(lines[i],6)=="#help:") {
            help=sub("#help:\\s*","",lines[i])
        } else if (strtrim(lines[i],9)=="#version:") {
            version=sub("#version:\\s*","",lines[i])
        } else if (strtrim(lines[i],9)=="#authors:") {
            authors=sub("#authors:\\s*","",lines[i])
        } else if (strtrim(lines[i],12)=="#references:") {
            references=sub("#references:\\s*","",lines[i])
        } else if (strtrim(lines[i],6)=="#type:") {
            type=sub("#type:\\s*","",lines[i])
        } else if (strtrim(lines[i],8)=="#output:") {
            output=sub("#output:\\s*","",lines[i])
        } else if (strtrim(lines[i],9)=="#require:") {
            requires=strsplit(sub("#require:\\s*","",lines[i]),";")[[1]]
        } else if (strtrim(lines[i],9)=="#options:") {
            str_repl = gsub(perl = T,"(,)(?=(?:[^']|'[^']*')*$)",';',sub("#options:\\s*","",lines[i]))
            options_str=strsplit(str_repl,";")
            for (os in options_str[[1]]){
                ko <- gsub(" ","",fixed=T,unlist(strsplit(unlist(os),"=")))
                options[[ko[1]]]=gsub("'","",ko[2])
                options.help[[ko[1]]]="?"
            }
        } else if (strtrim(lines[i],14)=="#options.help:") {
            str_repl = gsub(perl = T,"(,)(?=(?:[^']|'[^']*')*$)",';',sub("#options.help:\\s*","",lines[i]))
            options_str=strsplit(str_repl,";")
            for (os in options_str[[1]]){
                ko <- unlist(strsplit(unlist(os),"="))
                options.help[[ko[1]]]=gsub("'","",ko[2])
            }
        }
    }

    e = new.env()
    s=NULL
    try(s<-source(file,local=e),silent=T)
    if(is.null(s)) {
        stop(paste0("Cannot source file ",file,":\n",geterrmessage())) #,":\n",paste0(collapse="\n",traceback())))
    }

    e$new = e[[name]]

    if (exists("requires")) import(gsub(" ","",fixed=T,requires))

    return(list(name=name,authors=authors,references=references,help=help,type=tolower(type),output=output,options=options,options.help=options.help,requires=requires,envir=e))
}

#' @test get.algorithm("algorithms/EGO.R","help")
read.algorithm = function(file,info="help"){
    lines=readLines(file)

    name=unlist(strsplit(file,"/"))
    name=sub(".R","",name[length(name)])

    title=NA
    help=NA
    version=NA
    authors=NA
    references=NA
    type=NA
    output=NA
    requires=NA
    options = list()
    options.help = list()

    for (i in 1:length(lines)) {
        if (strtrim(lines[i],7)=="#title:") {
            title=sub("#title:\\s*","",lines[i])
        } else if (strtrim(lines[i],6)=="#help:") {
            help=sub("#help:\\s*","",lines[i])
        } else if (strtrim(lines[i],9)=="#version:") {
            version=sub("#version:\\s*","",lines[i])
        } else if (strtrim(lines[i],6)=="#type:") {
            type=sub("#type:\\s*","",lines[i])
        } else if (strtrim(lines[i],12)=="#references:") {
            references=sub("#references:\\s*","",lines[i])
        } else if (strtrim(lines[i],9)=="#authors:") {
            authors=sub("#authors:\\s*","",lines[i])
        } else if (strtrim(lines[i],8)=="#output:") {
            output=sub("#output:\\s*","",lines[i])
        } else if (strtrim(lines[i],9)=="#require:") {
            requires=strsplit(sub("#require:\\s*","",lines[i]),";")[[1]]
        } else if (strtrim(lines[i],9)=="#options:") {
            str_repl = gsub(perl = T,"(,)(?=(?:[^']|'[^']*')*$)",';',sub("#options:\\s*","",lines[i]))
            options_str=strsplit(str_repl,";")
            for (os in options_str[[1]]){
                ko <- unlist(strsplit(unlist(os),"="))
                options[[ko[1]]]=gsub("'","",ko[2])
            }
        } else if (strtrim(lines[i],14)=="#options.help:") {
            str_repl = gsub(perl = T,"(,)(?=(?:[^']|'[^']*')*$)",';',sub("#options.help:\\s*","",lines[i]))
            options_str=strsplit(str_repl,";")
            for (os in options_str[[1]]){
                ko <- unlist(strsplit(unlist(os),"="))
                options.help[[ko[1]]]=gsub("'","",ko[2])
            }
        }
    }

    return(list(name=name,authors=authors,help=help,type=tolower(type),output=output,requires=requires,options=options,options.help=options.help)[[info]])
}

source(file.path("../funz-profile/lib/report.R"))

import("readr")
run.algorithm = function(file, options = NULL, fun) {

    print("#### Initialize algorithm ####")

    Algorithm_name=gsub("\\.(.*)","",basename(file))

    source(file)
    Algorithm = eval(parse(text=paste("Algorithm <-",Algorithm_name)))

    # source("../../lib/algorithm.R")
    Algorithm_impl = parse.algorithm(file)
    Algorithm_options = Algorithm_impl$options
    if (!is.null(options))
        for (o in names(options)) Algorithm_options[[o]] = options[[o]]

    algorithm = Algorithm(Algorithm_options)


    print("#### Initialize function ####")

    if (is.character(fun)) {
        if (file.exists(fun)) {
        source(fun)
        # f = f
        input = input.f
        output = output.f
        } else stop("file ",fun," not available")
    } else if (is.list(fun)) {
        f = fun$f
        input = fun$input
        output = fun$output
    } else if (is.null(fun)) { #use global variables
        # f = f
        input = input.f
        output = output.f
    } else stop("You must provide target fun argumen (file, list or null to use global env)")

    print("#### Initialize report ####")

    # source("../../lib/report.R")
    report_file=paste0("report_",Algorithm_name,"-",output,".Rmd")
    if (!is.null(options))  # will use report_seed=1.md if option seed was modified from default values
        report_file = gsub("report",paste0("report_",paste0(names(options),"=",options,collapse="_")),report_file)

    create.md(paste0(Algorithm_name," / ", output),file = report_file)
    print.md("Algorithm",paste0("```{r eval=F}\n",read_file(file),"```\n"),file = report_file)
    print.md("Parameters",gsub("\\$","*",paste0(collapse="\n",capture.output(str(Algorithm_options))[-1])),file = report_file)
    if (exists("print.f")) {
    if (is.function(print.f)) {
        print.md("Objective",plot.md(print.f),file = report_file)
    }} else
        print.md("Objective",paste0("```{r eval=F}\n",paste0(capture.output(print(f)),collapse="\n"),"\n```\n"),file = report_file)

    # time stamp to evaluate time between iterations
    t0 = Sys.time()


    print("#### Apply algorithm on function ####")


    print("#### Apply algorithm on function #### : Initial design ")
    X0 = getInitialDesign(algorithm, input, output)
    Y0 = f(X0)
    colnames(Y0) <- output
    Xi <- X0
    Yi <- Y0

    finished = FALSE
    i = 0
    while (!finished) {
        save(list=ls(all.names = T),file=paste0(report_file,".Rdata"))

        t1 = Sys.time()-t0
        print.md(paste0("Iteration ",i, " (in ",format(t1,digits=3),")"),displayResultsTmp(algorithm,Xi,Yi),xml = T,file = report_file)
        t0 <- Sys.time()

        print(paste0("#### Apply algorithm on function #### : Iteration ",i))
        i = i+1

        Xj = getNextDesign(algorithm,Xi,Yi)
        if (is.null(Xj) | length(Xj) == 0) {
            finished = TRUE
        } else {
            Yj <- f(Xj)
            colnames(Yj) <- output
            Xi = rbind(Xi,Xj)
            Yi = rbind(Yi,Yj)
        }
    }

    save(list=ls(all.names = T),file=paste0(report_file,".Rdata"))

    print("#### Apply algorithm on function #### : displayResults")

    result = displayResults(algorithm,Xi,Yi)
    print.md("Result",result,xml = T,file = report_file)

    import("xml2")
    all_results = xml_children(read_xml(paste0("<result>",result,"</result>")))
    result_list = list()
    for (a in all_results) {
        result_list[[xml_name(a)]] = xml_text(a)
    }

    print("#### Build report ####")

    report.md(file = report_file)

    return(result_list)
}


