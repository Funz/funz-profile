source(file.path("../funz-profile/lib/import.R"))

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

import("xml2","jsonlite","parcoords","htmltools","crosstalk","DT")
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

import("rmarkdown")
report.md = function(file="report.Rmd") {
    render(file)
}
