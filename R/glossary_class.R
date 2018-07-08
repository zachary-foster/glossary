#' Glossary class
#'
#' This is used to add terms to a glossary
#'
#' @param definitions_path Where the the definitions of terms are stored. This
#'   is used to show the definitions when hovering over a glossary term in the
#'   text.
#' @param glossary_path The file the glossary will be added to. This is used to
#'   link glossary terms in the text to their definitions in the rendered
#'   glossary.
#' @param name The name of the glossary. Useful if you have more than one
#'   glossary.
#' @param terms_used The terms that will be used. Adding terms to the
#'   constructor (instead of `my_gloss$add("new term")`) will include them as if
#'   they were added with `my_gloss$add()`.
#'
#' @return An `R6Class` object of class `Glossary`
#' @family classes
#'
#' @examples
#' \dontrun{
#' my_gloss <- glossary()
#' }
#'
#' @export
glossary <- function(definitions_path, glossary_path, name = "Glossary", terms_used = c()) {
  Glossary$new(
    definitions_path = definitions_path,
    glossary_path = glossary_path,
    name = name,
    terms_used = terms_used
  )
}

Glossary <- R6::R6Class(
  "Glossary",
  public = list(
    name = NULL, # The name of the glossary
    definitions_path = NULL, # Where the the definitions of terms are stored
    glossary_path = NULL, # The file(s) the glossary will be added to
    terms_used = c(), # The terms used so far in this glossary

    initialize = function(definitions_path, glossary_path, name = "Glossary", terms_used = c()) {
      self$definitions_path <- definitions_path
      self$glossary_path <- glossary_path
      self$name <- name
      self$terms_used <- terms_used
      private$term_html <- render_definitions(definitions_path)
    },

    print = function(indent = "  ") {
      cat(paste0(indent, "<Glossary>\n"))
      cat(paste0(indent, paste0("name: ", name)))
      cat(paste0(indent, paste0("definitions_path: ", definitions_path)))
      cat(paste0(indent, paste0("glossary_path: ", paste0(glossary_path, collapse = ", "))))
      cat(paste0(indent, paste0("terms_used: ", paste0(terms_used, collapse = ", "))))
      invisible(self)
    },

    add = function(new_term) {
      if (! is.character(new_term)) {
        stop("Glossary terms must be of type `character`.")
      }
      if (length(new_term) != 1) {
        stop("Glossary terms must be of length 1.")
      }
      if (! new_term %in% names(private$term_html)) {
        stop('The term "', new_term, '" cannot be found in the definitions at "', definitions_path, "'")
      }
      if (! new_term %in% self$terms_used) {
        self$terms_used <- c(self$terms_used, new_term)
      }
    },

    render = function() {
      cat(paste0(private$term_html[sort(self$terms_used)], collapse = "\n"))
    }
  ),

  private = list(
    term_html = NULL
  )
)



render_definitions <- function(definition_path) {
  # Render Rmd file into HTML and save as a vector of length 1
  output_path <- tempfile()
  rmarkdown::render(definition_path, output_format = "html_document", output_file = output_path)
  raw_html <- readr::read_file_raw(output_path)

  # Extract the rendered HTML for each definition
  parsed_html <- xml2::read_html(raw_html)
  parsed_divs <- xml2::xml_find_all(parsed_html, "//div/div")
  parsed_term_html <- as.character(parsed_divs[grepl(parsed_divs,  pattern = "section level2", fixed = TRUE)])

  # Name by term annd return
  names(parsed_term_html) <- stringr::str_match(parsed_term_html, "<h[0-9]{1}>(.+)</h[0-9]{1}>")[,2]
  return(parsed_term_html)
}



