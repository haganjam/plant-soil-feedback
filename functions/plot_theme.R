
# customised plotting theme
theme_meta <- 
  function(base_size = 10, base_family = "") {
    theme(panel.background = element_rect(fill="white", colour="black", linetype="solid"),
          panel.grid.major =  element_blank(),
          panel.grid.minor =  element_blank(),
          axis.ticks.length = unit(-0.16, "cm"),
          axis.title.x = element_text(colour ="black", size = 10, face = "plain", margin=margin(5,0,0,0,"pt")),
          axis.title.y = element_text(colour = "black", size = 10, face = "plain", margin=margin(0,5,0,0,"pt")),
          axis.text.x = element_text(colour = "black", size=9, face = "plain",  margin=margin(10,0,0,0,"pt")),
          axis.text.y = element_text(colour ="black", size=9, face = "plain", margin=margin(0,10,0,0,"pt")),
          axis.ticks.x = element_line(colour = "black", size = 0.4),
          axis.ticks.y = element_line(colour = "black", size = 0.4),
          legend.text = element_text(colour = "black", size=10, face = "plain"),
          legend.title = element_text(colour = "black", size=10, face = "plain"),
          legend.frame = element_blank(),
          legend.box = element_blank(),
          legend.key = element_blank(),
          legend.background = element_blank())
  }

# label function
fix_labels <- function(x) {
  vars <- names(x)[names(x) %in% c("M", "P", "I")]
  for (i in vars) {
    x[[i]] <-
      if (!is.factor(x[[i]])) {
        factor(x[[i]], levels = c(0, 1))
      } else {
        x[[i]]
      }
  }
  # make labels readable
  if ("M" %in% names(x)) {
    levels(x[["M"]]) <- c("No microbes", "Microbes")
    x[["M"]] <- forcats::fct_relevel(x[["M"]], c("No microbes", "Microbes"))
  }
  if ("P" %in% names(x)) {
    levels(x[["P"]]) <- c("Native", "Invasive")
  }
  if ("I" %in% names(x)) {
    levels(x[["I"]]) <- c("Native alone", "Native + invasive")
  }
  # add minimum back to nitrogen
  if ("N" %in% names(x)) {
    x[["N"]] <- x[["N"]] + log(4) 
  }
  x
}