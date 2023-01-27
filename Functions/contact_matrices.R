# Contact matrices function
contactmatrices <- function(df, title) {
  df <- melt(df, varnames = c("age1", "age2"), value.name = "contacts")
  df <- df %>%
    filter(!is.na(df$age1))
  
  gplot <- ggplot(df,
                  aes(x = age1,
                      y = age2,
                      fill = contacts
                  )
  ) +
    geom_tile() +
    labs(
      title= title,
      x="Age of participant",
      y="Age of contact",
      fill="Contacts"
    ) + 
    scale_fill_gradientn(
      colors=c("#0D5257","#00BF6F", "#FFB81C"),
      breaks= c(0,0,5, 1, 2, 3, 4, 5),
      limits = c(0, 5)
    ) +
    theme(
      legend.position = "right")
  
  gplot}