toast_showNotif <- function(ui, ...){
  .dots <- list(...)
  duration <- ifelse(is.null(.dots$duration), 10, .dots$duration)
  type <- ifelse(is.null(.dots$type), "error", .dots$type)
  title <- h4(.dots$title)
  id <- .dots$id
  if(is.null(id)) id <- shiny:::createUniqueId(8)
  action <- .dots$action
  icone <- if (type == "warning" | type == "error" | !is.null(.dots$icone)) {
    icon(
      if (type == "warning" | type == "error") "triangle-exclamation" else .dots$icone,
      class = "fa-2x"
    )} else NULL
  ui = div(class="conteneur",
           style = "align-items: center;
           justify-content: center;",
           title,
           div(class="left", icone, style = "margin-right:10px"),
           div(class="right", ui))
  showNotification(ui, duration = duration, closeButton = FALSE, type = type,
                   id = id, action = action)
}
