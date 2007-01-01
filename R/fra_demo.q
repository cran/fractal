######################################################
## S+Fractal help functions
##
##   frHelp
##   frExample
##
######################################################

###
# frHelp
###

if (!is.R())
{
  "frHelp" <- function(keyword="", section="fractal")
  {
		# bugfix: it is only valid for windows
    if (!is.ui.app("s+gui"))
      stop("frHelp() can only be run from the S-PLUS for Windows.")
      
    checkScalarType(keyword,"character")
    checkScalarType(section,"character")
 
    db <- unname(attached.where(section, nomatch=0))

    if (!db)
      stop("No documentation for section ", section)
    else if (keyword!="" && !is.element(keyword, objects(db)))
      help(quoteSyntax(keyword))
    else {
      chmfile <- file.path(searchPaths()[db], paste(section, "chm", sep="."))
      callBrowse(chmfile, keyword=keyword, option=4)
    }

    invisible(NULL)
  }
}

###
# frExample
###

if (!is.R())
{
  "frExample" <- function(index, run=FALSE, graphics=FALSE)
  {
  	if (!is.ui.app("s+gui"))
      stop("can only be run from the S-PLUS for Windows GUI.")
  	
  	# bugfix: index can be missing or NULL, see below
#   checkScalarType(index,"integer")
    checkScalarType(run,"logical")
    checkScalarType(graphics,"logical")

    "readFormattedTable" <- function(path, splits, sep="|", stringsAsFactors=FALSE){
      z <- scan(path,sep=sep)
      z <- z[nchar(z) > 0]
      data.frame(split(z,splits),stringsAsFactors=stringsAsFactors)
    }

		# bugfix: fractal may be in a directory other than SHOME/library
		
    section <- "fractal"
    db <- unname(attached.where(section, nomatch=0))
    if (!db)
      stop("No documentation for section ", section)
    exampleDir      <- file.path(searchPaths()[db], "appexams")
    
#   exampleDir      <- file.path(getenv("SHOME"),"library","fractal","appexams")

    appExamListPath <- file.path(exampleDir,"example_list.txt")
    appExams        <- readFormattedTable(appExamListPath, c("type","name","category","brief"))
    funs            <- cbind(appExams$brief, appExams$name)
    dimnames(funs)  <- list(NULL, c("menu", "file"))

    # obtain a list of unique function-brief combinations, and sort
    # them based on the lowercase version of the function name
    items <- paste(format(funs[,"file"], justify="left"), funs[, "menu"], sep=" :: ")
    uniq  <- match(unique(items),items)
    uniq  <- uniq[rank(uniq)]
    items <- items[uniq]
    funs  <- funs[uniq,]
    ord   <- order(lowerCase(items))
    items <- items[ord]
    funs  <- funs[ord,]

    # interactive
    if (missing(index)){
      frExample(menu(items, graphics=graphics, title="S+Fractal Examples:"),
        graphics=graphics, run=run)
      return(invisible(NULL))
    }

    # create the name of the example script file
    if (!is.integer(index))
      stop("index must be an integer")
    if (!index)
      return(invisible(NULL))

    fullfile <- file.path(exampleDir, paste(funs[, "file"], ".ssc",sep=""))[index]

    if (!file.exists(fullfile))
      stop(fullfile, " doesn't exist.")

    # open it or run it
    if (run){
    	graphsheet(pages=TRUE)
    	source(fullfile)
    }
    else {
      guiOpen("Script",
  	  FileName = fullfile,
  	  Hide = F,
  	  Show = "Normal")
    }

    invisible(NULL)
  }
}
