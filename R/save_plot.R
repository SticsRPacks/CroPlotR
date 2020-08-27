#' Save CroPlotR plots
#'
#' @description Save the plots to a pdf file
#'
#' @param plot A list of ggplots : output of `plot_situations()`
#' @param path The path to the directory where to save the plots
#' @param filename Name of the pdf file
#' @param main Main title of the pdf document
#' @param file_per_var If `TRUE`, produces one file per variable instead of one with all variables inside
#' @param stats Output of `statistics_situations`with `all_situations = FALSE`. Needed if `file_per_var` is TRUE.
#' It is used to classify situations according to the descending RMSE.
#' @param force Continue if the plot is not possible ? If `TRUE`, return `NULL`, else return an error.
#' @param verbose Boolean. Print information during execution.
#'
#' @return Save the plots in a pdf file in the folder specified by the `path`
#'
#' @importFrom grDevices dev.off pdf
#' @importFrom graphics par plot.new title
#'
#' @export

save_plot=function(plot,path,filename="Graphs",main="Plots",file_per_var=FALSE,stats=NULL,
                   force=TRUE,verbose=TRUE){

  if(file_per_var && is.null(stats)){
    if(verbose){
      cli::cli_alert_warning("Argument `stats` must be specified when `file_per_var` is 'TRUE'")
    }
    if(force){
      return(NULL)
    }else{
      stop("Argument `stats` missing")
    }
  }

  if(file_per_var && unique(stats$situation)=="all_situations"){
    if(verbose){
      cli::cli_alert_warning("Argument `stats` must be the output of `summary` with `all_situations=FALSE`")
    }
    if(force){
      return(NULL)
    }else{
      stop("`stats` must differentiate the statistical criteria between situations")
    }
  }

  vars= NULL
  for(d in plot){
    vars= c(vars,d$data$variable)
  }
  vars= unique(vars)

  if(file_per_var){
    for(v in vars){
      if(v!="Sit_Name"){
        pdf(file.path(path,paste0(filename,"-",v,".pdf")),
            paper = "a4",
            width = 7,
            height = 10)
        plot.new()
        par(oma = c(2, 0, 4, 0))
        par(mar = c(8, 6, 4, 4))
        title(
          main = paste0(main," : ",v),
          outer = T,
          cex.main = 1.5
        )
        ex = extract_plot(plot,var=v)
        rmse = stats[which(stats$variable==v),]$RMSE
        plt = c()
        for(i in unique(rev(sort(rmse)))){
          ind = which(rmse == i)
          for(j in ind){
            sit = names(plot)[j]
            if(v %in% unique(plot[[sit]]$data$variable)){
              gg = ex[sit]
              gg[[1]]$labels$title = paste0(sit," | RMSE : ",round(i,3))
              plt = c(plt,gg)
            }
          }
        }
        k = 1
        while(k <= length(plt)){
          gg1 = plt[[k]]
          gg2 = ggplot2::ggplot() + ggplot2::theme_void()
          gg3 = ggplot2::ggplot() + ggplot2::theme_void()
          gg4 = ggplot2::ggplot() + ggplot2::theme_void()
          if(k+1 <= length(plt)) gg2 = plt[[k+1]]
          if(k+2 <= length(plt)) gg3 = plt[[k+2]]
          if(k+3 <= length(plt)) gg4 = plt[[k+3]]
          gridExtra::grid.arrange(gg1,gg2,gg3,gg4,ncol=2)
          k = k+4
        }
        dev.off()
      }
    }
  }else{
    pdf(file.path(path,paste0(filename,".pdf")),
        paper = "a4",
        width = 10,
        height = 7)
    plot.new()
    par(oma = c(2, 0, 4, 0))
    par(mar = c(8, 6, 4, 4))
    title(
      main = main,
      outer = T,
      cex.main = 1.5
    )
    plt = c()
    for(i in 1:length(plot)){
      for(va in vars){
        if(va %in% plot[[i]]$data$variable){
          plt = c(plt,extract_plot(plot[i],var=va))
        }
      }
    }
    p = 1
    while(p <= length(plt)){
      gg1 = plt[[p]]
      gg2 = ggplot2::ggplot() + ggplot2::theme_void()
      gg3 = ggplot2::ggplot() + ggplot2::theme_void()
      gg4 = ggplot2::ggplot() + ggplot2::theme_void()
      if(p+1<=length(plt)) gg2 = plt[[p+1]]
      if(p+2<=length(plt)) gg3 = plt[[p+2]]
      if(p+3<=length(plt)) gg4 = plt[[p+3]]
      gridExtra::grid.arrange(gg1,gg2,gg3,gg4,ncol=2)
      p= p+4
    }
    dev.off()
  }

}
