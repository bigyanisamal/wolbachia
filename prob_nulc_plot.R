prob_nucl_plot <- function(df, start, end) {
  print(df[start:end,])
  conserve_find_plot <- ggplot(data = df[start:end,], mapping = aes(x= position, y = max_prob_wolbachia, colour = factor(max_nucl_wolbachia)), shape=16)+
    geom_point()+
    geom_point(data = df[start:end,], mapping = aes(x=position, y=max_prob_no_wolbachia, colour = factor(max_nucl_no_wolbachia)), shape=17)+
    geom_point(data = df[start:end,], mapping = aes(x=position, y=min_prob_no_wolbachia, colour = factor(min_nucl_no_wolbachia)), shape=15)+
    labs(x="position", y= "probability of nucleotide")+
    labs(colour="Nucleotide")
  ggplotly(conserve_find_plot)
}