
# Plots for palmchat project:

library(ggplot2)
library(gridExtra)

my_function <- function(x) {
  x
}

my_function <- function(x) {
  -x
}


p <- ggplot(data.frame(x=c(0,1)), aes(x = x)) +
          stat_function(fun = my_function, geom = "line", color = "blue") +
  labs(
    x = "Resource Dispersion",
    y = "Colonial Nesting",
    title = "Hypothesis 1: Coloniality primarily influenced by resource dispersion"
  ) + theme_classic()



p2 <- ggplot(data.frame(x=c(0,1)), aes(x = x)) +
  stat_function(fun = my_function2, geom = "line", color = "blue") +
  labs(
    x = "Nest Site Availability",
    y = "Colonial Nesting",
    title = "Hypothesis 2: Coloniality primarily influenced by nest requirements"
  ) + theme_classic()

grid.arrange(p,p2)


library(igraph)


g_gnm <- sample_gnm(n = 100, m = 400) # 100 nodes, 400 edges
plot(g_gnm, vertex.size = 5, vertex.label = NA)



g <- erdos.renyi.game(100, 200, type = "gnm", directed = FALSE)
plot(g, vertex.size = 5, vertex.label = NA)


num_nodes <- 100
num_blocks <- 5

# Define the sizes of the blocks
block_sizes <- rep(num_nodes / num_blocks, num_blocks)

block_sizes

# Define the probability matrix for connections
# p_within: probability of connection within a block
# p_between: probability of connection between different blocks
p_within <- 0.5 # Higher for stronger communities
p_between <- 0.01 # Lower for weaker inter-community connections

# Create the probability matrix
pref_matrix <- matrix(p_between, nrow = num_blocks, ncol = num_blocks)
diag(pref_matrix) <- p_within

# Generate the graph using the Stochastic Block Model
g <- sample_sbm(num_nodes, pref.matrix = pref_matrix, block.sizes = block_sizes)

plot(g, vertex.size = 5, vertex.label = NA)


# Detect communities using a modularity-optimizing algorithm (e.g., Louvain)
communities <- cluster_louvain(g)

# Calculate the modularity of the detected community structure
modularity_score <- modularity(g, communities$membership)

# Print the modularity score
print(paste("Modularity of the network:", modularity_score))

# Plot the network with communities colored
plot(g, vertex.color = communities$membership, layout = layout_with_fr)



### Males vs. females (sex ratio of social modules...)

sexratios <- c(1.66,2,1,3,1.5,.5,.33,2,1,1.5)

boxplot(sexratios)





