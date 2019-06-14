#' Save multiple objects to an excel file
#'
#' Save multiple objects to an excel file
#'
#' Save multiple objects to an excel file
#' @export
save.xlsx <- function (file, objects, names=NULL) {
      require(xlsx, quietly = TRUE)
      #objects <- list(...)
      #fargs <- as.list(match.call(expand.dots = TRUE))
      #objnames <- as.character(fargs)[-c(1, 2)]

      nobjects <- length(objects)
      
      if(!is.null(names)) {
        objnames <- names
      } else {
        if(!is.null(names(objects))) {
          objnames <- names(objects)
        } else {
          objnames <- paste0("Object ", 1:nobjects)
        }
      }

      for (i in 1:nobjects) {
          if (i == 1)
              write.xlsx(objects[[i]], file, sheetName = objnames[i])
          else write.xlsx(objects[[i]], file, sheetName = objnames[i],
              append = TRUE)
      }

      print(paste("Workbook", file, "has", nobjects, "worksheets."))
}
