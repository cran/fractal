######################################################
## S+Fractal GUI and demo functionality
##
##   launchExample
##
######################################################

###
# launchExample
###

if (!is.R())
{
  "launchExample" <- function(x, open=TRUE, run=FALSE, appendix=TRUE){

    # check input arguments
    if (!is.character(x) || length(x) > 1)
      stop("x must be a single character string")

    # add extension to file name
    ext <- ".ssc"
    if(!length(grep(ext, x)))
      x <- paste(x, ext, sep = "")

    exampleDir <- file.path(.lib.loc()[1],"fractal",
      ifelse1(appendix,"appexams","bookexams"))
    path <- file.path(exampleDir, x)

    if (!file.exists(path))
      stop(path, " does not exist.")

    if (open)
      guiOpen("Script", FileName=path, Hide=F,
  	  Show="Normal", Top="Auto", Left="Auto", Width="Auto", Height="Auto")

    if (run)
    	source(path)

    invisible(NULL)
  }
}
