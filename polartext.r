##################################################################
# Polar Text Implmentation (comment and vector versions)
##################################################################
polartext_c <- function(comment) {

  if(is.null(comment)){return(NULL)}
  
  # Prepare corpus and split comment into sentences
  corpus <- suppressWarnings(Corpus(VectorSource(comment)))
  corpus <- suppressWarnings(tm_map(corpus, sent_detect_nlp))

  # Score each sentence in the comment
  polarity_df <- suppressWarnings(polarity(as.character(corpus[[1]])[1:length( as.character(corpus[[1]]))-1]))
  polarity_df <- counts(polarity_df)
  ln <- length(polarity_df$text.var)

  # Fetch a color scale for each sentence
  g <- ggplot_build(ggplot(data = polarity_df, aes(x=1, y=1, color=polarity_df$polarity)) +
                      scale_color_gradient2(low="red4", high = "#446e9b", midpoint = 0, mid="darkgrey"))

  color_scale <- g$data[[1]]["colour"]

  # Map color scale to polarity and color text via html tags
  html_list <- c("")
  for (i in 1:length(color_scale[[1]])) {
    sent <- paste0('<font color="', color_scale[[1]][i], '">', polarity_df$text.var[i], ' ', '</font>')
    html_list <- paste(html_list, sent)
  }
  incProgress(amount=1/length(opshab[,1]))
  return(html_list)
}

# vector input and dataframe output (one column is original, another column is html)
polartext_d <- function(vec) {
  if(length(vec)==0) {return(NULL)}
  vec <- as.character(vec)
  df <- cbind(Comment=vec, HTML = "")
  df[,2] <- sapply(df[,1], polartext_c)
  return(df)
}
