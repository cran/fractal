if (!is.R()) {

  .First.lib <- function(libname, pkgname)
  {
    wmtsa.pos <- attached.where("wmtsa", nomatch=0)
    if (!wmtsa.pos) {
      cat("Loading required library: wmtsa.\n")
      library("wmtsa", first=TRUE)
      assign("wmtsa.previously.fractal.loaded", TRUE, frame=0)
    }
    sapa.pos <- attached.where("sapa", nomatch=0)
    if (!sapa.pos) {
      cat("Loading required library: sapa.\n")
      library("sapa", first=TRUE)
      assign("sapa.previously.fractal.loaded", TRUE, frame=0)
    }

    # define local functions
    "createGUI" <- function(chapter="Fractal" , lib = TRUE){

      # define local functions
      "menu.path" <- function(...) paste(... , sep="$")

      "createMenu" <- function(menuPath , menuText , Type="MenuItem" , ...)
        invisible(guiCreate("MenuItem" , Name = menuPath , Type = Type , MenuItemText = menuText , ...))

      "readFormattedTable" <- function(path , splits , sep="|" , stringsAsFactors = FALSE){
        z <- scan(path , sep = sep)
        z <- z[nchar(z) > 0]
        data.frame(split(z , splits) , stringsAsFactors = stringsAsFactors)
      }

      "checkPath" <- function(x , type="dir"){
        type <- match.arg(type , c("dir" , "file"))
        if ((type == "dir" && !is.dir(x)) || (type == "file" && !file.exists(x)))
          stop(x , " does not exist")
        invisible(NULL)
      }

      "createScriptMenus" <- function(x , baseMenuPath , open = TRUE , run = FALSE , appendix = FALSE){

        # check data.frame structure
        if (!is.data.frame(x))
          stop("Input must be a data.frame")

        required <- c("brief" , "filename")
        if (!all(is.element(required , names(x))))
          stop("Input data frame must contain the following columns: " , required)

        for (i in seq(numRows(x))){
          example <- x[i ,]
          fname <- strip.blanks(example$filename)
          path <- paste(baseMenuPath , paste("eg" , i , sep="") , sep="$")
          cmd <- paste("launchExample(\"" , fname , "\", run=" , run , ", open=" , open , ", appendix=" , appendix , ")" , sep="")
          guiCreate("MenuItem" , Name = path , Type="MenuItem" , Action="Expression" , Command = cmd ,
            MenuItemText = example$brief , StatusBarText = strip.blanks(example$brief))
        }

        invisible(NULL)
      }

      "smartOrder" <- function(x){
        w       <- regexpr("[0-9]+", x)
        nums    <- as.numeric(substring(x, w, w+attr(w, "match.length")-1))
        good    <- (!is.na(nums))
        z       <- rep("", length(x))
        z[good] <- sprintf("%03d", nums[good])
        order(sub("[0-9]+", z, x))
      }

      # create menu paths
      baseMenuPath <- menu.path("$$SPlusMenuBar" , chapter)
      exampleMenuPath <- menu.path(baseMenuPath , "Examples")
      helpMenuPath <- menu.path(baseMenuPath , "Help")

      # create file and directory paths
      chapterPath      <- file.path(.lib.loc()[1], lowerCase(chapter))
      bookExampleDir   <- file.path(chapterPath, "bookexams")

      # check existence of file and directory paths
      checkPath(chapterPath)

      # remove any existing related GUI menus
      if (is.element(baseMenuPath , guiGetObjectNames("MenuItem")))
        guiRemove("MenuItem" , Name = baseMenuPath)

      ###
      # Menus
      ###

      # primary chapter menu
      createMenu(baseMenuPath , chapter , Type="Menu")

      # help submenu
      createMenu(helpMenuPath , "&Help" , Type="Menu")
      helpcmd <- paste("\"" , file.path(chapterPath , paste(lowerCase(chapter) , ".chm" , sep="") , fsep="/") , "\"" , sep="")
      helptext <- paste(chapter , " Language Reference" , sep="")
      createMenu(menu.path(helpMenuPath , "chm_help") , helptext , Action="Open" , StatusBarText = helptext , Command = helpcmd)

      # create book exams data.frame
      bookExams <- file.listing(dir=bookExampleDir, pattern="group.*\\.ssc")
      briefs    <- gsub("_", " ", gsub(".ssc", "", gsub("group-","", bookExams), ignore.case=T))
      ix        <- smartOrder(briefs)
      bookExams <- data.frame(filename=bookExams[ix], brief=briefs[ix], stringsAsFactors=FALSE)

      # examples submenu
      createMenu(exampleMenuPath , "Examples" , Type="Menu")
      createScriptMenus(bookExams , exampleMenuPath)

      invisible(NULL)
    }

  if (is.ui.app("s+gui"))
  	createGUI()


  cat("\nWelcome to the FRACTAL package!\n")
  # bugfix: frHelp and frExample are only valid for windows
  if (is.ui.app("s+gui"))
  {
  	cat("\nTo get started, please issue any of the" ,
      "following from the S-PLUS command line:\n\n" ,
      "  frHelp()    # open up the FRACTAL help file\n" ,
      "  frExample() # open executable FRACTAL example files\n" ,
      "\nUse the Fractal pulldown menu to open/run examples or obtain help." ,
      "Clicking on the EXAMPLES link in a CHM help file will open up" ,
      "the corresponding script in S-PLUS.\n")
  }
  invisible(NULL)
  }


  .Last.lib <- function(library , section , .data , where)
  {
    # wmtsa library
    wmtsa.id <- "wmtsa.previously.fractal.loaded"
    if (exists(wmtsa.id, frame=0)){
      was.loaded <- get(wmtsa.id, frame=0)
      if (was.loaded && attached.where("wmtsa", nomatch=0)){
        cat("Detaching library: wmtsa.\n")
        detach("wmtsa")
      }
      remove(wmtsa.id, frame=0)
    }
    # rsmutils library
    sapa.id <- "sapa.previously.fractal.loaded"
    if (exists(sapa.id, frame=0)){
      was.loaded <- get(sapa.id, frame=0)
      if (was.loaded && attached.where("sapa", nomatch=0)){
        cat("Detaching library: sapa.\n")
        detach("sapa")
      }
      remove(sapa.id, frame=0)
    }
  	if (is.ui.app("s+gui") && is.element("SPlusMenuBar$Fractal" , guiGetObjectNames("MenuItem")))
    	guiRemove("MenuItem" , Name="SPlusMenuBar$Fractal")
    invisible(NULL)
  }
}
