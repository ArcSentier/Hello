#' @title
#' Hello World
#'
#' @description
#' `hello` says _"hello"_ in the user-specified language. The user
#' is asked to give her/his name so that the hello message gets
#' personalized.
#'
#' @param who a `character` vector of length 1 that specifies the name
#' of the person to whom the message is adressed.
#'
#' @param lang a `character` vector of length 1 that specifies the
#' preferred language. Default to "EN" for English. Other possible
#' values include "FR" for French, "IT" for Italien, "ES" for
#' Spanish, or "DE" for German. Case is ignored.
#'
#' @param LangData an optional data.frame with two columns each of mode
#' `character`. The first column gives the language codes and the
#' second column gives the corresponding "hello" word. Default to
#' `language`.
#'
#' see `?language`
#'
#' @return
#' a `character` vector with a personalized _"hello"_ message.
#'
#' @examples
#' hello("James")
#' hello("Amelia", "Es")
#'
#' @export

hello <- function(who, lang='EN', LangData=Hello::language) {
  llang = lang |> tolower()
  LangData = data.frame(LangData)
  colnames(LangData) <- c("code", "hello")

  if (is.character(who)) {
    texte = ifelse(
      llang %in% LangData$code,
      stringr::str_c(LangData$hello[LangData$code == llang],
                     ", ",
                     who,
                     "!"),
      stringr::str_c("Sorry, ",
                     who,
                     ", your language ('",
                     lang,
                     "') is not available!")
    )
  } else {
    texte = stringr::str_c("Error in hello(",
                           who,
                           "): Please enter a valid name; see ?hello")
  }
  cat(texte)
}
