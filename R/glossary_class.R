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
#' @param terms_used The terms that will be used. Adding terms to the
#'   constructor (instead of `my_gloss$add("new term")`) will include them as if
#'   they were added with `my_gloss$add()`.
#' @param header_level How big the headers are for each term in the rendered
#'   glossary. Larger numbers mean smaller titles.
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
glossary <- function(definitions_path, glossary_path = "", terms_used = c(), header_level = 3) {
  Glossary$new(
    definitions_path = definitions_path,
    glossary_path = glossary_path,
    terms_used = terms_used,
    header_level = header_level
  )
}

Glossary <- R6::R6Class(
  "Glossary",
  public = list(
    definitions_path = NULL, # Where the the definitions of terms are stored
    glossary_path = NULL, # The file(s) the glossary will be added to
    terms_used = c(), # The terms used so far in this glossary

    initialize = function(definitions_path, glossary_path, terms_used = c(), header_level = 3) {
      self$definitions_path <- definitions_path
      self$glossary_path <- glossary_path
      self$terms_used <- terms_used
      private$term_html <- render_definitions_html(definitions_path, header_level = header_level)
      private$term_rmd <- render_definitions_rmd(definitions_path, header_level = header_level)
    },

    print = function(indent = "  ") {
      cat(paste0(indent, "<Glossary>\n"))
      cat(paste0(indent, paste0("definitions_path: ", self$definitions_path, "\n")))
      cat(paste0(indent, paste0("glossary_path: ", paste0(self$glossary_path, collapse = ", "), "\n")))
      cat(paste0(indent, paste0("terms_used: ", paste0(self$terms_used, collapse = ", "), "\n")))
      invisible(self)
    },

    add = function(new_term, shown = NULL) {
      if (is.null(shown)) {
        shown <- new_term
      }

      if (! is.character(new_term)) {
        stop("Glossary terms must be of type `character`.")
      }
      if (length(new_term) != 1) {
        stop("Glossary terms must be of length 1.")
      }
      if (! standardize(new_term) %in% standardize(names(private$term_html))) {
        warning(paste0('The term "', new_term, '" cannot be found in the definitions at "',
                       self$definitions_path, "' so no link will be added."))
        return(shown)
      }
      if (! standardize(new_term) %in% standardize(self$terms_used)) {
        self$terms_used <- c(self$terms_used, standardize(new_term))
      }

      # Format link to glossary
      if (is.null(self$glossary_path) || self$glossary_path == "" ) {
        glossary_path_html <- ""
      } else {
        glossary_path_html <- paste0(tools::file_path_sans_ext(self$glossary_path), ".html")
      }
      output <- paste0('<a href ="', glossary_path_html, '#', term_anchor_name(new_term), '">', shown, '</a>')

      # Add html div of glossary contents to reveal when cursor hovers
      # term_gloss_html <- private$term_html[tolower(new_term) == tolower(names(private$term_html))]
      # term_gloss_html <- sub(term_gloss_html, pattern = "^<div ", replacement = '<div class="glossary_div" ')
      # output <- paste0(output, "\n", private$term_html)

      return(output)
    },

    render = function(mode = "html") {
      if (mode == "md") {
        output <- paste0(private$term_rmd[sort(self$terms_used)], collapse = "\n")
      } else if (mode == "html") {
        output <- paste0(private$term_html[sort(self$terms_used)], collapse = "\n")
      } else {
        stop("mode must be 'html' or 'md'")
      }

      knitr::asis_output(output)
    },

    render_all = function(mode = "html") {
      if (mode == "md") {
        output <- paste0(private$term_rmd[sort(names(private$term_rmd))], collapse = "\n")
      } else if (mode == "html") {
        output <- paste0(private$term_html[sort(names(private$term_html))], collapse = "\n")
      } else {
        stop("mode must be 'html' or 'md'")
      }

      knitr::asis_output(output)
    }
  ),


  private = list(
    term_html = NULL,
    term_rmd = NULL
  )
)



render_definitions_html <- function(definition_path, header_level = 3) {
  # Render Rmd file into HTML and save as a vector of length 1
  output_path <- tempfile()
  rmarkdown::render(definition_path, output_format = rmarkdown::html_document(), output_file = output_path, quiet = TRUE)
  raw_html <- readr::read_file_raw(output_path)

  # Extract the rendered HTML for each definition
  parsed_html <- xml2::read_html(raw_html)
  parsed_divs <- xml2::xml_find_all(parsed_html, "//div/div")
  parsed_term_html <- as.character(parsed_divs[grepl(parsed_divs,  pattern = "section")])
  term_names <-  stringr::str_match(parsed_term_html, "<h[0-9]{1}>\n*(.+)\n*</h[0-9]{1}>")[,2]

  # Reset header level and add anchor
  parsed_term_html <- sub(parsed_term_html, pattern = 'class="section level[0-9]{1}"',
                          replacement = paste0('class="section level', header_level, '"'))
  anchor_name <- term_anchor_name(term_names)
  parsed_term_html <- vapply(seq_along(parsed_term_html), FUN.VALUE = character(1), function(i) {
    sub(parsed_term_html[i], pattern = '<h[0-9]{1}>',
        replacement = paste0('<h', header_level, '><a class="glossary_anchor" id="', anchor_name[i], '">'))
  })
  parsed_term_html <- sub(parsed_term_html, pattern = '</h[0-9]{1}>',
                          replacement = paste0('</a></h', header_level, '>'))

  # Name by term and return
  names(parsed_term_html) <- term_names
  return(parsed_term_html)
}


render_definitions_rmd <- function(definition_path, header_level = 3) {
  raw_rmd <- readr::read_file(definition_path)

  # Extract the rendered HTML for each definition
  parsed_rmd <- stringr::str_split(raw_rmd, "\n#{1,5}")[[1]][-1]
  parsed_rmd <- trimws(parsed_rmd)
  term_names <- stringr::str_match(parsed_rmd, "^(.+)\n")[,2]
  parsed_rmd <- sub(parsed_rmd, pattern = "^(.+?)\n", replacement = "")
  parsed_rmd <- trimws(parsed_rmd)

  # Add headers and spacing
  parsed_rmd <- paste0(paste0(rep("#", header_level), collapse = ""), " ",
                       term_names, "\n\n", parsed_rmd, "\n\n")

  # Name by term and return
  names(parsed_rmd) <- term_names
  return(parsed_rmd)
}


term_anchor_name <- function(term_name) {
  paste0(gsub(pattern = " ", replacement = "_", standardize(term_name)), "_anchor")
}


standardize <- function(term) {
  term <- tolower(term)
  term <- gsub(term, pattern = "’", replacement = "'", fixed = TRUE)
  return(term)
}
