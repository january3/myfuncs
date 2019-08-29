#' Create a static cache at the specific path
#'
#' Create a static cache at the specific path
#' @param path the path to the object
#' @param name name of the object to save
#' @param exprs expression to generate the object if cache file does not exist
#' @export
static_cache <- function(path, name, exprs) {
        if(file.exists(path)) {
                message(sprintf("Loading object %s from %s", name, path))
    .o <- load(path)
    return(get(.o))
        } else {
                message(sprintf("Calculating and saving %s to %s", name, path))
    assign(name, exprs)
    save(list=name, file=path)
    return(get(name))
  }
}

