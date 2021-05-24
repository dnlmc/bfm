# Entire codebase for the blog post: https://medium.com/@dnlmc/barbells-fractals-memelords-scaling-strategies-for-ethical-action-cca9a868b8a1

library(tidyverse)
library(tidygraph)
library(ggraph)
library(igraph)
library(gifski)
library(ggridges)
library(patchwork)
library(ggtext)

##### Building & visualizing initial graphs ---------------------

# initialize graph via {tidygraph} function
set.seed(33) # random seeds used for reproducible results
graph_er <- play_erdos_renyi(150, 0.03, directed = F)

# visualize via {ggraph}
ggraph(graph_er) + 
  geom_edge_link() + 
  geom_node_point() + 
  theme_graph()
  

set.seed(33)
graph_ba <- play_barabasi_albert(150, power = .01, directed = F)

ggraph(graph_ba, layout = "graphopt") + 
  geom_edge_link() + 
  geom_node_point() + 
  theme_graph()
  

set.seed(33333333)
graph_sw <- play_smallworld(1, 150, 1, .2, loops = F, multiple = F)

set.seed(33)
ggraph(graph_sw, layout = 'nicely') + 
  geom_edge_link() + 
  geom_node_point() +
  theme_graph()
  

##### Add graph metadata ---------------------

# create min-max normalization function
min_max_norm <- function(x) (x - min(x)) / (max(x) - min(x))

# create normally-distributed edge weight vector, then normalize between 0-1
set.seed(33)
edge_weight <- rnorm(337, 0, .3) %>% min_max_norm()

# create lognormally distributed 'resource' vector
set.seed(33)
resources <- exp(1)^rnorm(150, 4.65, 1.5)

# create growth probability vector
set.seed(33)
growth_prob <- ( (min_max_norm(resources) + rbeta(150,9,1)) / 2) 

# add vectors to nodes & edges in graph data structure
graph_er <- graph_er %>% activate(nodes) %>% mutate(resources = resources,
                                                    endangered = resources < 15,
                                                    growth = growth_prob,) %>% 
  				activate(edges) %>% mutate(edge_weight = edge_weight)

# ba graph
set.seed(33)
ba_friend_weight <- rnorm(149,0,.3) %>% min_max_norm() #%>% hist() 

graph_ba <- graph_ba %>% activate(nodes) %>% mutate(resources = resources,
                                                  growth = growth_prob,
                                                  endangered = resources < 15) %>% 
  activate(edges) %>% mutate(edge_weight = ba_friend_weight)  
  
# sw graph
sw_friend_weight <- rnorm(150,0,.3) %>% min_max_norm() #%>% hist() 

set.seed(33)
graph_sw <- graph_sw %>% activate(nodes) %>% mutate(resources = resources,
                                                  growth = growth_prob,
                                                  endangered = resources < 15) %>% 
  activate(edges) %>% mutate(edge_weight = sw_friend_weight)  
  

# core-periphery / hub score data for memelord strategy
# calculate hub scores & add to graph nodes

hub_scores_er <- igraph::hub_score(graph_er, weights = edge_weight)

graph_er <- graph_er %>% activate(nodes) %>% 
  mutate(hub_score_value = hub_scores_er$vector)


hub_scores_ba <- hub_score(graph_ba) # weighting by edge weights was erroring here & i didn't feel like debugging so

graph_ba <- graph_ba %>% activate(nodes) %>% 
  mutate(hub_score_value = hub_scores_ba$vector)


hub_scores_sw <- hub_score(graph_sw, edge_weight)

graph_sw <- graph_sw %>% activate(nodes) %>% 
  mutate(hub_score_value = hub_scores_sw$vector)



##### static graph plots

# ER
set.seed(333^3)
ggraph(graph_er, layout = 'nicely') + 
  geom_edge_link(aes(width = edge_weight, alpha = edge_weight)) + 
  geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + 
  scale_color_manual(values = c('steelblue', 'red2')) +
  scale_alpha(range = c(0, 1), limits = c(0.12,.3), guide = 'none') +
  scale_edge_width_continuous(limits = c(0, 1), range = c(0, .77) ) +
  scale_size_continuous(breaks = c(100,1000,5000,10000,20000),
    limits = c(0, 10000000),
    range = c(1, 217) ) +
  theme_graph()


# BA
set.seed(33)
ggraph(graph_ba, layout = 'gem') + 
  geom_edge_link(aes(width = edge_weight, 
                     alpha = edge_weight)) + 
  geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + 
  scale_color_manual(values = c('steelblue', 'red2')) +
  scale_alpha(range = c(0, 1), guide = 'none', limits = c(0.12,.3)) +
  scale_size_continuous(
    breaks = c(100,1000,5000,10000,20000),
    limits = c(0, 10000000),
    range = c(1, 217) )  +
  scale_edge_width_continuous(
    limits = c(0, 1),
    range = c(0, .77)
  ) +
  theme_graph()


# SW
set.seed(33)
ggraph(graph_sw, layout = 'nicely') + 
  geom_edge_link(aes(width = edge_weight, 
                     alpha = edge_weight)) + 
  geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + 
  scale_color_manual(values = c('steelblue', 'red2')) +
  scale_alpha(range = c(0, 1), guide = 'none', limits = c(0.12,.3)) +
  scale_size_continuous(
    breaks = c(100,1000,5000,10000,20000),
    limits = c(0, 10000000),
    range = c(1, 217) 
  ) +
  scale_edge_width_continuous(
    limits = c(0, 1),
    range = c(0, .77)
  ) +
  theme_graph()


  
##### graph stats ------------------

# resource distribution
as.list(graph_er)$nodes$resources %>% summary()

# plot
as.list(graph_er)$nodes %>% as_tibble() %>% 
  ggplot() + geom_histogram(aes(resources), binwidth = 100) + 
  theme_minimal()
  
# plot histogram between 0-1000
as.list(graph_er)$nodes %>% as_tibble() %>% filter(resources < 1000) %>% 
  ggplot() + geom_histogram(aes(resources), binwidth = 33) + 
  theme_minimal()

# plot smoothed density + precarity threshold
as.list(graph_er)$nodes %>% as_tibble() %>% filter(resources < 1000) %>% 
  ggplot() + geom_area(aes(resources), stat = 'density') + 
  theme_minimal() + geom_vline(xintercept =  15, color = "red3",
                               linetype = "dashed", size = .7) +
  annotate("text", x = 120, y = 0.0045, color = "red3",
                           label = "Precarity threshold")  
                           
                           
# show degree distribution quantiles, total edges & cliques per graph
er_dist_summary <- graph_er %>% degree(mode = 'all') %>% summary()
er_dist_summary[7] <- graph_er %>% gsize()
attributes(er_dist_summary)$names[7] <- 'edges'
er_dist_summary[8] <- graph_er %>% count_max_cliques() 
attributes(er_dist_summary)$names[8] <- 'cliques'

 
ba_dist_summary <- graph_ba %>% degree(mode = 'all') %>% summary()
ba_dist_summary[7] <- graph_ba %>% gsize()
attributes(ba_dist_summary)$names[7] <- 'edges'
ba_dist_summary[8] <- graph_ba %>% count_max_cliques() 
attributes(ba_dist_summary)$names[8] <- 'cliques'

 
sw_dist_summary <- graph_sw %>% degree(mode = 'all') %>% summary()
sw_dist_summary[7] <- graph_sw %>% gsize()
attributes(sw_dist_summary)$names[7] <- 'edges'
sw_dist_summary[8] <- graph_sw %>% count_max_cliques() 
attributes(sw_dist_summary)$names[8] <- 'cliques'


# create tibble of degree distributions
degree_er <- graph_er %>% degree(mode = 'all') %>% as_tibble() %>% 
  transmute(degree = value, graph = 'ER') 
degree_ba <- graph_ba %>% degree(mode = 'all') %>% as_tibble() %>% 
  transmute(degree = value, graph = 'BA') 
degree_sw <- graph_sw %>% degree(mode = 'all') %>% as_tibble() %>% 
  transmute(degree = value, graph = 'SW') 

degree_tbl <- bind_rows(degree_er, degree_ba, degree_sw)

# plot densities & boxplots for degree distributions of each graph
degree_tbl %>% ggplot(aes(x = degree, y = -.3)) +
  geom_boxplot(width = .2, outlier.shape = NA) + geom_jitter(height = 0.0, alpha = 0.07) +
  geom_area(aes(x = degree), inherit.aes = FALSE, stat= 'density') + #aes(fill = graph)
  facet_grid(graph ~.) +
  theme_minimal() +
  theme(axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        panel.grid.minor = element_blank()) +
  ggtitle('Degree distributions', '(connections between nodes)' )
  
  
# edge weight distribution
as.list(graph_er)$edges$edge_weight %>% summary()

# plot edge weight histogram & density
as.list(graph_er)$edges %>% as_tibble() %>% 
  ggplot(aes(edge_weight)) + geom_histogram(aes(y=..density..), binwidth = .03) + 
  geom_density(aes(y=..density..)) +
  theme_minimal()
  
  
##### Simulation code -------------------------

# function to increment node resources by probabilistic cost & growth (mathematical) function
graph_sim_increment <- function(graf){

  graf <- graf %>% activate(nodes) %>% 
    # cost / growth function = current resources * average of random normal variable 
  	# plus previous stochastic growth probability, minus .5
    mutate(resources = resources * (1 + ( ((rnorm(150,.5,.07) + growth) / 2) - .5)),
           endangered = resources < 15)   
  return(graf)  
}


# function to implement & visualize baseline simulation
# logs various values at each iteration & produces a series of plots to compile into an animated gif
sim_gif_maker_er <- function(years){
  
  # create empty tibble to store values from each iteration
  sim_stats_er <<- tibble(iteration = integer(),
                          nodes = integer(),
                          endangered = integer(),
                          precarity_proportion = numeric(),
                          total_resources = numeric(),
                          median_resources = numeric(),
                          mean_resources = numeric(),
                          node_list = list()
  )
  
  # loop thru iterations
  counter <- 1
  while( counter <= (12*years)){
    
    # plots
    set.seed(333^3)
    p1 <- ggraph(graph_er, layout = 'nicely') + # mds kk fr sphere grid nicely gem graphopt
      geom_edge_link(aes(width = edge_weight, alpha= edge_weight)) + 
      geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + #, color = suffering) + #aquamarine4 'steelblue'
      scale_color_manual(values = c('steelblue', 'red')) +
      scale_alpha(range = c(0, 1), guide = 'none', limits = c(0.12,.3)) +
      scale_edge_width_continuous(limits = c(0, 1),
                                  range = c(0, .77) ) +
      scale_size_continuous(
        breaks = c(100,1000,5000,10000,20000),
        limits = c(0, 10000000),
        range = c(1, 217) ) +
      labs(title = 'Erdős–Rényi graph: baseline simulation',
           subtitle=paste('iteration:', counter, '    ',
                          'endangered: ', sum(as.list(graph_er)$nodes$endangered),
                          '    precarity proportion: ', 
                          round(sum(as.list(graph_er)$nodes$endangered) / length(as.list(graph_er)$nodes$endangered),
                                3) * 100, '%')) +
      theme_graph() + theme(plot.margin = margin(.3, .3, .3, .3, "cm"))
    
  
    p2 <- as.list(graph_er)$nodes %>% as_tibble() %>% filter(resources < 1000) %>% 
      ggplot() + geom_area(aes(resources), stat = 'density') + 
      theme_minimal() + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_markdown(size = rel(0.75))) +
      geom_vline(xintercept =  15, color = "red3",
                 linetype = "dashed", size = .7) +
      geom_vline(xintercept =  median(as_tibble(as.list(graph_er)$nodes)$resources), 
                 color = "seagreen",
                 linetype = "solid", size = 1) +
      geom_vline(xintercept =  mean(as_tibble(as.list(graph_er)$nodes)$resources), 
                 color = "green2",
                 linetype = "solid", size = .7) +
      xlab("resource distribution (w/ <span style='color:#FF0000;'>**precarity**</span> threshold, 
           <span style='color:#008000;'>**median**</span> & <span style='color:#32CD32;'>**mean**</span> lines)") +
      xlim(0, 1000) 
    
    patchplot <- p1 / p2 + plot_layout(widths = c(1, 1), heights = c(5.25,.75))  
    
    print(patchplot)
    
    # add row to tibble
    sim_stats_er_row <- tibble_row(iteration = counter,
                                   nodes = length(as.list(graph_er)$nodes$endangered),
                                   endangered = sum(as.list(graph_er)$nodes$endangered),
                                   precarity_proportion = sum(as.list(graph_er)$nodes$endangered) / length(as.list(graph_er)$nodes$endangered),
                                   total_resources = sum(as_tibble(as.list(graph_er)$nodes)$resources),
                                   median_resources = median(as_tibble(as.list(graph_er)$nodes)$resources),
                                   mean_resources = mean(as_tibble(as.list(graph_er)$nodes)$resources),
                                   node_list = list(as_tibble(as.list(graph_er)$nodes)$resources)
    	)
    
    sim_stats_er <<- sim_stats_er %>% bind_rows(sim_stats_er_row)
    
    # increment graph via increment function
    graph_er <- graph_sim_increment(graph_er)
    
    counter <- counter+1
  }
}


# create gif via gifski::save_gif, for 7 'years' worth of 'monthly' iterations
save_gif(sim_gif_maker_er(7), gif_file = "sim_er_7.gif", width = 800, height = 850,
         delay = .12, loop = TRUE, progress = TRUE, res=133)
         
         
# plot simulation logs
l1 <- ggplot(data=sim_stats_er, aes(x=iteration, y=median_resources)) +
        geom_line() + theme_minimal()
l2 <- ggplot(data=sim_stats_er, aes(x=iteration, y=log(mean_resources))) +
  geom_line() + theme_minimal()
l3 <- ggplot(data=sim_stats_er, aes(x=iteration, y=precarity_proportion)) +
  geom_area() + ylim(c(0,1)) + theme_minimal()

l1/l2/l3


# fractal strategy function
graph_sim_fractal_strategy <- function(graf){
  
  # store graph degree variable (# of edges)
  graph_degree <- as.list(graf)$edges %>% nrow()
  
  # loop thru connected node pairs & apply resource reallocation solidarity strategy:
  # if from_node has 3x more resources than to_node, exchange resources in amount based on 
  # from_node's resources & edge_weight (relationship strength) between nodes
  for(edge_i in 1:graph_degree){
    # assign variables
    from_node <- as.list(graf)$edges[edge_i,]$from
    to_node <- as.list(graf)$edges[edge_i,]$to
    e_weight <- as.list(graf)$edges[edge_i,]$edge_weight
    
    from_resource <- as.list(graf)$nodes[from_node,]$resources
    to_resource <- as.list(graf)$nodes[to_node,]$resources
    
    if(from_resource > 3*to_resource){
      # set resource exchange amount
      exchange_amt <- abs(15 - as.list(graf)$nodes[to_node,]$resources) * e_weight
      
      # decrement from_node's resources
      graf <- set_vertex_attr(graf, 'resources', from_node,
                              vertex_attr(graf, 'resources', from_node) - exchange_amt)
      
      #increment to_node's resources
      graf <- set_vertex_attr(graf, 'resources', to_node,
                              vertex_attr(graf, 'resources', to_node) + exchange_amt)
      
    }
    
    # reverse conditions
    if(to_resource > 3*from_resource){
      exchange_amt <-  abs(15 - as.list(graf)$nodes[from_node,]$resources) * e_weight 
      
      graf <- set_vertex_attr(graf, 'resources', to_node,
                              vertex_attr(graf, 'resources', to_node) - exchange_amt)
      
      graf <- set_vertex_attr(graf, 'resources', from_node,
                              vertex_attr(graf, 'resources', from_node) + exchange_amt)
     
    }
    
  }
  
  # reset endangered boolean based on new resource levels
  graf <- graf %>% activate(nodes) %>% 
    		mutate(endangered = resources<15) 
  
  return(graf)
}


# fractal simulation & gif visualization function
fractal_gif_maker_er <- function(years){
  
  # create tibble to store values from each iteration
  sim_stats_er_fractal <<- tibble(iteration = integer(),
                          nodes = integer(), 
                          endangered = integer(),
                          precarity_proportion = numeric(),
                          total_resources = numeric(),
                          median_resources = numeric(),
                          mean_resources = numeric(),
                          node_list = list()
                          )
  
  # loop thru iterations
  counter <- 1
  while( counter <= (12*years)){
    
    # plots
    set.seed(333^3)
    p1 <- ggraph(graph_er, layout = 'nicely') + 
      geom_edge_link(aes(width = edge_weight, alpha= edge_weight)) + 
      geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + #, color = suffering) + #aquamarine4 'steelblue'
      scale_color_manual(values = c('steelblue', 'red')) +
      scale_alpha(range = c(0, 1), guide = 'none', limits = c(0.12,.3)) +
      scale_edge_width_continuous(limits = c(0, 1), range = c(0, .77) ) +
      scale_size_continuous(breaks = c(100,1000,5000,10000,20000),
        limits = c(0, 10000000), range = c(1, 217) ) + 
      labs(title = 'Erdős–Rényi graph: fractal solidarity strategy', 
           subtitle=paste('iteration:', counter, '     ',
                          'endangered: ', sum(as.list(graph_er)$nodes$endangered),
                          '      precarity proportion: ', 
                          round(sum(as.list(graph_er)$nodes$endangered) / length(as.list(graph_er)$nodes$endangered),
                                3) * 100, '%')) +
      theme_graph() + theme(plot.margin = margin(.3, .3, .3, .3, "cm"))
      
    p2 <- as.list(graph_er)$nodes %>% as_tibble() %>% filter(resources < 1000) %>% 
      ggplot() + geom_area(aes(resources), stat = 'density') + 
      theme_minimal() + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_markdown(size = rel(0.75))) +
      geom_vline(xintercept =  15, color = "red3",
                 linetype = "dashed", size = .7) +
      geom_vline(xintercept =  median(as_tibble(as.list(graph_er)$nodes)$resources), 
                 color = "seagreen",
                 linetype = "solid", size = 1) +
      geom_vline(xintercept =  mean(as_tibble(as.list(graph_er)$nodes)$resources), 
                 color = "green2",
                 linetype = "solid", size = .7) +
      xlab("resource distribution (w/ <span style='color:#FF0000;'>**precarity**</span> threshold, 
           <span style='color:#008000;'>**median**</span> & <span style='color:#32CD32;'>**mean**</span> lines)") +
      xlim(0, 1000) 

    patchplot <- p1 / p2 + plot_layout(widths = c(1, 1), heights = c(5.25,.75))  
      
    print(patchplot)
    
    # add row to tibble
    sim_stats_er_fractal_row <- tibble_row(iteration = counter,
                            nodes = length(as.list(graph_er)$nodes$endangered),
                            endangered = sum(as.list(graph_er)$nodes$endangered),
                            precarity_proportion = sum(as.list(graph_er)$nodes$endangered) / length(as.list(graph_er)$nodes$endangered),
                            total_resources = sum(as_tibble(as.list(graph_er)$nodes)$resources),
                            median_resources = median(as_tibble(as.list(graph_er)$nodes)$resources),
                            mean_resources = mean(as_tibble(as.list(graph_er)$nodes)$resources),
                            node_list = list(as_tibble(as.list(graph_er)$nodes)$resources)
                            )
    
    sim_stats_er_fractal <<- sim_stats_er_fractal %>% bind_rows(sim_stats_er_fractal_row)
    
    # increment graph via increment function
    graph_er <- graph_sim_increment(graph_er) 
    
    # apply fractal solidarity strategy
    graph_er <- graph_sim_fractal_strategy(graph_er)
    
    counter <- counter+1
  }
}


# create gif via gifski::save_gif, for 7 'years' worth of 'monthly' iterations
save_gif(sim_gif_maker_er(7), gif_file = "fractal_er_7.gif", width = 800, height = 850,
         delay = .12, loop = TRUE, progress = TRUE, res=133)
         


##### BA graph simulation & animated gif visualization functions ----------
sim_gif_maker_ba <- function(years){
  
  # create empty tibble to store values from each iteration
  sim_stats_ba <<- tibble(iteration = integer(),
                          nodes = integer(),
                          endangered = integer(),
                          precarity_proportion = numeric(),
                          total_resources = numeric(),
                          median_resources = numeric(),
                          mean_resources = numeric(),
                          node_list = list()
  )
  
  # loop thru iterations
  counter <- 1
  while( counter <= (12*years)){
    
    set.seed(333^3)
    p1 <- ggraph(graph_ba, layout = 'gem') + 
      geom_edge_link(aes(width = edge_weight, 
                         alpha = edge_weight)) + 
      geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + 
      scale_color_manual(values = c('steelblue', 'red')) +
      scale_alpha(range = c(0, 1), guide = 'none', limits = c(0.12,.3)) +
      scale_size_continuous(
        breaks = c(100,1000,5000,10000,20000),
        limits = c(0, 10000000),
        range = c(1, 217) )  +
      scale_edge_width_continuous(limits = c(0, 1),
        range = c(0, .77)
      ) +
      labs(title = 'Barabási–Albert graph: baseline simulation',
           subtitle=paste('iteration:', counter, '    ',
                          'endangered: ', sum(as.list(graph_ba)$nodes$endangered),
                          '    precarity proportion: ', 
                          round(sum(as.list(graph_ba)$nodes$endangered) / length(as.list(graph_ba)$nodes$endangered),
                                3) * 100, '%')) +
      theme_graph() + theme(plot.margin = margin(.3, .3, .3, .3, "cm"))
    
    
    p2 <- as.list(graph_ba)$nodes %>% as_tibble() %>% filter(resources < 1000) %>% 
      ggplot() + geom_area(aes(resources), stat = 'density') + 
      theme_minimal() + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_markdown(size = rel(0.75))) +
      geom_vline(xintercept =  15, color = "red3",
                 linetype = "dashed", size = .7) +
      geom_vline(xintercept =  median(as_tibble(as.list(graph_ba)$nodes)$resources), 
                 color = "seagreen",
                 linetype = "solid", size = 1) +
      geom_vline(xintercept =  mean(as_tibble(as.list(graph_ba)$nodes)$resources), 
                 color = "green2",
                 linetype = "solid", size = .7) +
      xlab("resource distribution (w/ <span style='color:#FF0000;'>**precarity**</span> threshold, 
           <span style='color:#008000;'>**median**</span> & <span style='color:#32CD32;'>**mean**</span> lines)") +
      xlim(0, 1000) 
    
    patchplot <- p1 / p2 + plot_layout(widths = c(1, 1), heights = c(5.25,.75))  
    
    print(patchplot)
    
    # add row to tibble
    sim_stats_ba_row <- tibble_row(iteration = counter,
                                   nodes = length(as.list(graph_ba)$nodes$endangered),
                                   endangered = sum(as.list(graph_ba)$nodes$endangered),
                                   precarity_proportion = sum(as.list(graph_ba)$nodes$endangered) / length(as.list(graph_ba)$nodes$endangered),
                                   total_resources = sum(as_tibble(as.list(graph_ba)$nodes)$resources),
                                   median_resources = median(as_tibble(as.list(graph_ba)$nodes)$resources),
                                   mean_resources = mean(as_tibble(as.list(graph_ba)$nodes)$resources),
                                   node_list = list(as_tibble(as.list(graph_ba)$nodes)$resources)
    )
    
    sim_stats_ba <<- sim_stats_ba %>% bind_rows(sim_stats_ba_row)
    
    # increment graph via increment function
    graph_ba <- graph_sim_increment(graph_ba)
    
    counter <- counter+1
    
  }
}

# BA fractal strategy sim & animation
fractal_gif_maker_ba <- function(years){
  
  # create tibble to store values from each iteration
  sim_stats_ba_fractal <<- tibble(iteration = integer(),
                                  nodes = integer(), 
                                  endangered = integer(),
                                  precarity_proportion = numeric(),
                                  total_resources = numeric(),
                                  median_resources = numeric(),
                                  mean_resources = numeric(),
                                  node_list = list()
  )
  
  # loop thru iterations
  counter <- 1
  while( counter <= (12*years)){
    set.seed(333^3)
    p1 <- ggraph(graph_ba, layout = 'gem') + 
      geom_edge_link(aes(width = edge_weight, 
                         alpha = edge_weight)) + 
      geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + 
      scale_color_manual(values = c('steelblue', 'red2')) +
      scale_alpha(range = c(0, 1), guide = 'none', limits = c(0.12,.3)) +
      scale_size_continuous(
        breaks = c(100,1000,5000,10000,20000),
        limits = c(0, 10000000),
        range = c(1, 217) )  +
      scale_edge_width_continuous(
        limits = c(0, 1),
        range = c(0, .77)
      ) +
      labs(title = 'Barabási–Albert graph: fractal solidarity strategy',
           subtitle=paste('iteration:', counter, '    ',
                          'endangered: ', sum(as.list(graph_ba)$nodes$endangered),
                          '    precarity proportion: ', 
                          round(sum(as.list(graph_ba)$nodes$endangered) / length(as.list(graph_ba)$nodes$endangered),
                                3) * 100, '%')) +
      theme_graph() + theme(plot.margin = margin(.3, .3, .3, .3, "cm"))
    
    p2 <- as.list(graph_ba)$nodes %>% as_tibble() %>% filter(resources < 1000) %>% 
      ggplot() + geom_area(aes(resources), stat = 'density') + 
      theme_minimal() + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_markdown(size = rel(0.75))) +
      geom_vline(xintercept =  15, color = "red3",
                 linetype = "dashed", size = .7) +
      geom_vline(xintercept =  median(as_tibble(as.list(graph_ba)$nodes)$resources), 
                 color = "seagreen",
                 linetype = "solid", size = 1) +
      geom_vline(xintercept =  mean(as_tibble(as.list(graph_ba)$nodes)$resources), 
                 color = "green2",
                 linetype = "solid", size = .7) +
      xlab("resource distribution (w/ <span style='color:#FF0000;'>**precarity**</span> threshold, 
           <span style='color:#008000;'>**median**</span> & <span style='color:#32CD32;'>**mean**</span> lines)") +
      xlim(0, 1000) 
    
    patchplot <- p1 / p2 + plot_layout(widths = c(1, 1), heights = c(5.25,.75))  
    
    print(patchplot)
    
    # add row to tibble
    sim_stats_ba_fractal_row <- tibble_row(iteration = counter,
                                   nodes = length(as.list(graph_ba)$nodes$endangered),
                                   endangered = sum(as.list(graph_ba)$nodes$endangered),
                                   precarity_proportion = sum(as.list(graph_ba)$nodes$endangered) / length(as.list(graph_ba)$nodes$endangered),
                                   total_resources = sum(as_tibble(as.list(graph_ba)$nodes)$resources),
                                   median_resources = median(as_tibble(as.list(graph_ba)$nodes)$resources),
                                   mean_resources = mean(as_tibble(as.list(graph_ba)$nodes)$resources),
                                   node_list = list(as_tibble(as.list(graph_ba)$nodes)$resources)
    )
    
    sim_stats_ba_fractal <<- sim_stats_ba_fractal %>% bind_rows(sim_stats_ba_fractal_row)
    
    # increment graph via increment function
    graph_ba <- graph_sim_increment(graph_ba)
    
    # apply fractal solidarity strategy
    graph_ba <- graph_sim_fractal_strategy(graph_ba)
    
    counter <- counter+1
  }
}

# BA barbell strategy simulation & animation 
barbell_gif_maker_ba <- function(years){
  
  # create tibble to store values from each iteration
  sim_stats_ba_barbell <<- tibble(iteration = integer(),
                                  nodes = integer(), 
                                  endangered = integer(),
                                  precarity_proportion = numeric(),
                                  total_resources = numeric(),
                                  median_resources = numeric(),
                                  mean_resources = numeric(),
                                  global_funds = numeric(),
                                  node_list = list()
  )
  
  # loop thru iterations
  counter <- 1
  while( counter <= (12*years)){
    set.seed(333^3)
    p1 <- ggraph(graph_ba, layout = 'gem') + 
      geom_edge_link(aes(width = edge_weight, 
                         alpha = edge_weight)) + 
      geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + 
      scale_color_manual(values = c('steelblue', 'red2')) +
      scale_alpha(range = c(0, 1), guide = 'none', limits = c(0.12,.3)) +
      scale_size_continuous(
        breaks = c(100,1000,5000,10000,20000),
        limits = c(0, 10000000),
        range = c(1, 217) )  +
      scale_edge_width_continuous(
        limits = c(0, 1),
        range = c(0, .77)
      ) +
      labs(title = 'Barabási–Albert graph: barbell solidarity strategy',
           subtitle=paste('iteration:', counter, '    ',
                          'endangered: ', sum(as.list(graph_ba)$nodes$endangered),
                          '    precarity proportion: ', 
                          round(sum(as.list(graph_ba)$nodes$endangered) / length(as.list(graph_ba)$nodes$endangered),
                                3) * 100, '%')) +
      theme_graph() + theme(plot.margin = margin(.3, .3, .3, .3, "cm"))
    
    p2 <- as.list(graph_ba)$nodes %>% as_tibble() %>% filter(resources < 1000) %>% 
      ggplot() + geom_area(aes(resources), stat = 'density') + 
      theme_minimal() + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_markdown(size = rel(0.75))) +
      geom_vline(xintercept =  15, color = "red3",
                 linetype = "dashed", size = .7) +
      geom_vline(xintercept =  median(as_tibble(as.list(graph_ba)$nodes)$resources), 
                 color = "seagreen",
                 linetype = "solid", size = 1) +
      geom_vline(xintercept =  mean(as_tibble(as.list(graph_ba)$nodes)$resources), 
                 color = "green2",
                 linetype = "solid", size = .7) +
      xlab("resource distribution (w/ <span style='color:#FF0000;'>**precarity**</span> threshold, 
           <span style='color:#008000;'>**median**</span> & <span style='color:#32CD32;'>**mean**</span> lines)") +
      xlim(0, 1000) 
    
    patchplot <- p1 / p2 + plot_layout(widths = c(1, 1), heights = c(5.25,.75))  
    
    print(patchplot)
    
    # add row to tibble
    sim_stats_ba_barbell_row <- tibble_row(iteration = counter,
                                           nodes = length(as.list(graph_ba)$nodes$endangered),
                                           endangered = sum(as.list(graph_ba)$nodes$endangered),
                                           precarity_proportion = sum(as.list(graph_ba)$nodes$endangered) / length(as.list(graph_ba)$nodes$endangered),
                                           total_resources = sum(as_tibble(as.list(graph_ba)$nodes)$resources),
                                           median_resources = median(as_tibble(as.list(graph_ba)$nodes)$resources),
                                           mean_resources = mean(as_tibble(as.list(graph_ba)$nodes)$resources),
                                           global_funds = ifelse(exists('global_fund'), global_fund, 0.0),
                                           node_list = list(as_tibble(as.list(graph_ba)$nodes)$resources)
    )
    
    sim_stats_ba_barbell <<- sim_stats_ba_barbell %>% bind_rows(sim_stats_ba_barbell_row)
    
    # increment graph via increment function
    graph_ba <- graph_sim_increment(graph_ba)
    
    # apply barbell solidarity strategy
    graph_ba <- graph_sim_barbell_strategy(graph_ba)
    
    counter <- counter+1
  }
}


# BA memelord strategy simulation & animation
memelord_gif_maker_ba <- function(years){
  
  # create tibble to store values from each iteration
  sim_stats_ba_memelord <<- tibble(iteration = integer(),
                                  nodes = integer(), 
                                  endangered = integer(),
                                  precarity_proportion = numeric(),
                                  total_resources = numeric(),
                                  median_resources = numeric(),
                                  mean_resources = numeric(),
                                  node_list = list()
  )
  
  # loop thru iterations
  counter <- 1
  while( counter <= (12*years)){
    set.seed(333^3)
    p1 <- ggraph(graph_ba, layout = 'gem') + 
      geom_edge_link(aes(width = edge_weight, 
                         alpha = edge_weight)) + 
      geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + 
      scale_color_manual(values = c('steelblue', 'red2')) +
      scale_alpha(range = c(0, 1), guide = 'none', limits = c(0.12,.3)) +
      scale_size_continuous(
        breaks = c(100,1000,5000,10000,20000),
        limits = c(0, 10000000),
        range = c(1, 217) )  +
      scale_edge_width_continuous(
        limits = c(0, 1),
        range = c(0, .77)
      ) +
      labs(title = 'Barabási–Albert graph: memelord solidarity strategy',
           subtitle=paste('iteration:', counter, '    ',
                          'endangered: ', sum(as.list(graph_ba)$nodes$endangered),
                          '    precarity proportion: ', 
                          round(sum(as.list(graph_ba)$nodes$endangered) / length(as.list(graph_ba)$nodes$endangered),
                                3) * 100, '%')) +
      theme_graph() + theme(plot.margin = margin(.3, .3, .3, .3, "cm"))
    
    p2 <- as.list(graph_ba)$nodes %>% as_tibble() %>% filter(resources < 1000) %>% 
      ggplot() + geom_area(aes(resources), stat = 'density') + 
      theme_minimal() + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_markdown(size = rel(0.75))) +
      geom_vline(xintercept =  15, color = "red3",
                 linetype = "dashed", size = .7) +
      geom_vline(xintercept =  median(as_tibble(as.list(graph_ba)$nodes)$resources), 
                 color = "seagreen",
                 linetype = "solid", size = 1) +
      geom_vline(xintercept =  mean(as_tibble(as.list(graph_ba)$nodes)$resources), 
                 color = "green2",
                 linetype = "solid", size = .7) +
      xlab("resource distribution (w/ <span style='color:#FF0000;'>**precarity**</span> threshold, 
           <span style='color:#008000;'>**median**</span> & <span style='color:#32CD32;'>**mean**</span> lines)") +
      xlim(0, 1000) 
    
    patchplot <- p1 / p2 + plot_layout(widths = c(1, 1), heights = c(5.25,.75))  
    
    print(patchplot)
    
    # add row to tibble
    sim_stats_ba_memelord_row <- tibble_row(iteration = counter,
                                           nodes = length(as.list(graph_ba)$nodes$endangered),
                                           endangered = sum(as.list(graph_ba)$nodes$endangered),
                                           precarity_proportion = sum(as.list(graph_ba)$nodes$endangered) / length(as.list(graph_ba)$nodes$endangered),
                                           total_resources = sum(as_tibble(as.list(graph_ba)$nodes)$resources),
                                           median_resources = median(as_tibble(as.list(graph_ba)$nodes)$resources),
                                           mean_resources = mean(as_tibble(as.list(graph_ba)$nodes)$resources),
                                           node_list = list(as_tibble(as.list(graph_ba)$nodes)$resources)
    )
    
    sim_stats_ba_memelord <<- sim_stats_ba_memelord %>% bind_rows(sim_stats_ba_memelord_row)
    
    # increment graph via increment function
    graph_ba <- graph_sim_increment(graph_ba)
    
    # apply memelord solidarity strategy
    graph_ba <- graph_sim_memelord_strategy(graph_ba)
    
    counter <- counter+1
  }
}


##### SW simulation & animation functions ---------------------

# SW baseline simulation & animation
sim_gif_maker_sw <- function(years){
  
  # create empty tibble to store values from each iteration
  sim_stats_sw <<- tibble(iteration = integer(),
                          nodes = integer(),
                          endangered = integer(),
                          precarity_proportion = numeric(),
                          total_resources = numeric(),
                          median_resources = numeric(),
                          mean_resources = numeric(),
                          node_list = list()
  )
  
  # loop thru iterations
  counter <- 1
  while( counter <= (12*years)){
    set.seed(333^3)
    p1 <- ggraph(graph_sw, layout = 'nicely') + 
      geom_edge_link(aes(width = edge_weight, 
                         alpha = edge_weight)) +
      geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + 
      scale_color_manual(values = c('steelblue', 'red')) +
      scale_alpha(range = c(0, 1), guide = 'none', limits = c(0.12,.3)) +
      scale_size_continuous(
        breaks = c(100,1000,5000,10000,20000),
        limits = c(0, 10000000),
        range = c(1, 217) 
      ) +
      scale_edge_width_continuous(limits = c(0, 1),
        range = c(0, .77)
      ) +
      labs(title = 'Watts–Strogatz (small world) graph: baseline simulation',
           subtitle=paste('iteration:', counter, '    ',
                          'endangered: ', sum(as.list(graph_sw)$nodes$endangered),
                          '    precarity proportion: ', 
                          round(sum(as.list(graph_sw)$nodes$endangered) / length(as.list(graph_sw)$nodes$endangered),
                                3) * 100, '%')) +
      theme_graph() + theme(plot.margin = margin(.3, .3, .3, .3, "cm"))
    
    
    p2 <- as.list(graph_sw)$nodes %>% as_tibble() %>% filter(resources < 1000) %>% 
      ggplot() + geom_area(aes(resources), stat = 'density') + 
      theme_minimal() + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_markdown(size = rel(0.75))) +
      geom_vline(xintercept =  15, color = "red3",
                 linetype = "dashed", size = .7) +
      geom_vline(xintercept =  median(as_tibble(as.list(graph_sw)$nodes)$resources), 
                 color = "seagreen",
                 linetype = "solid", size = 1) +
      geom_vline(xintercept =  mean(as_tibble(as.list(graph_sw)$nodes)$resources), 
                 color = "green2",
                 linetype = "solid", size = .7) +
      xlab("resource distribution (w/ <span style='color:#FF0000;'>**precarity**</span> threshold, 
           <span style='color:#008000;'>**median**</span> & <span style='color:#32CD32;'>**mean**</span> lines)") +
      xlim(0, 1000) 
    
    patchplot <- p1 / p2 + plot_layout(widths = c(1, 1), heights = c(5.25,.75))  
    
    print(patchplot)
    
    # add row to tibble
    sim_stats_sw_row <- tibble_row(iteration = counter,
                                   nodes = length(as.list(graph_sw)$nodes$endangered),
                                   endangered = sum(as.list(graph_sw)$nodes$endangered),
                                   precarity_proportion = sum(as.list(graph_sw)$nodes$endangered) / length(as.list(graph_sw)$nodes$endangered),
                                   total_resources = sum(as_tibble(as.list(graph_sw)$nodes)$resources),
                                   median_resources = median(as_tibble(as.list(graph_sw)$nodes)$resources),
                                   mean_resources = mean(as_tibble(as.list(graph_sw)$nodes)$resources),
                                   node_list = list(as_tibble(as.list(graph_sw)$nodes)$resources)
    )
    
    sim_stats_sw <<- sim_stats_sw %>% bind_rows(sim_stats_sw_row)
    
    # increment graph via increment function
    graph_sw <- graph_sim_increment(graph_sw)
    
    counter <- counter+1
  }
}


# SW fractal strategy simulation & animation
fractal_gif_maker_sw <- function(years){
  
  # create empty tibble to store values from each iteration
  sim_stats_sw_fractal <<- tibble(iteration = integer(),
                          nodes = integer(),
                          endangered = integer(),
                          precarity_proportion = numeric(),
                          total_resources = numeric(),
                          median_resources = numeric(),
                          mean_resources = numeric(),
                          node_list = list()
  )
  
  # loop thru iterations
  counter <- 1
  while( counter <= (12*years)){
    set.seed(333^3)
    p1 <- ggraph(graph_sw, layout = 'nicely') + 
      geom_edge_link(aes(width = edge_weight, 
                         alpha = edge_weight)) + 
      geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + 
      scale_color_manual(values = c('steelblue', 'red')) +
      scale_alpha(range = c(0, 1), guide = 'none', limits = c(0.12,.3)) +
      scale_size_continuous(
        breaks = c(100,1000,5000,10000,20000),
        limits = c(0, 10000000),
        range = c(1, 217) 
      ) +
      scale_edge_width_continuous(
        limits = c(0, 1),
        range = c(0, .77)
      ) +
      labs(title = 'Watts–Strogatz (small world) graph: fractal strategy',
           subtitle=paste('iteration:', counter, '    ',
                          'endangered: ', sum(as.list(graph_sw)$nodes$endangered),
                          '    precarity proportion: ', 
                          round(sum(as.list(graph_sw)$nodes$endangered) / length(as.list(graph_sw)$nodes$endangered),
                                3) * 100, '%')) +
      theme_graph() + theme(plot.margin = margin(.3, .3, .3, .3, "cm"))
    
    
    p2 <- as.list(graph_sw)$nodes %>% as_tibble() %>% filter(resources < 1000) %>% 
      ggplot() + geom_area(aes(resources), stat = 'density') + 
      theme_minimal() + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_markdown(size = rel(0.75))) +
      geom_vline(xintercept =  15, color = "red3",
                 linetype = "dashed", size = .7) +
      geom_vline(xintercept =  median(as_tibble(as.list(graph_sw)$nodes)$resources), 
                 color = "seagreen",
                 linetype = "solid", size = 1) +
      geom_vline(xintercept =  mean(as_tibble(as.list(graph_sw)$nodes)$resources), 
                 color = "green2",
                 linetype = "solid", size = .7) +
      xlab("resource distribution (w/ <span style='color:#FF0000;'>**precarity**</span> threshold, 
           <span style='color:#008000;'>**median**</span> & <span style='color:#32CD32;'>**mean**</span> lines)") +
      xlim(0, 1000) 
    
    patchplot <- p1 / p2 + plot_layout(widths = c(1, 1), heights = c(5.25,.75))  
    
    print(patchplot)
    
    # add row to tibble
    sim_stats_sw_fractal_row <- tibble_row(iteration = counter,
                                   nodes = length(as.list(graph_sw)$nodes$endangered),
                                   endangered = sum(as.list(graph_sw)$nodes$endangered),
                                   precarity_proportion = sum(as.list(graph_sw)$nodes$endangered) / length(as.list(graph_sw)$nodes$endangered),
                                   total_resources = sum(as_tibble(as.list(graph_sw)$nodes)$resources),
                                   median_resources = median(as_tibble(as.list(graph_sw)$nodes)$resources),
                                   mean_resources = mean(as_tibble(as.list(graph_sw)$nodes)$resources),
                                   node_list = list(as_tibble(as.list(graph_sw)$nodes)$resources)
    )
    
    sim_stats_sw_fractal <<- sim_stats_sw_fractal %>% bind_rows(sim_stats_sw_fractal_row)
    
    # increment graph via increment function
    graph_sw <- graph_sim_increment(graph_sw)
    
    # apply fractal solidarity strategy
    graph_sw <- graph_sim_fractal_strategy(graph_sw)
    
    counter <- counter+1
  }
}


# SW barbell strategy simulation & animation
barbell_gif_maker_sw <- function(years){
  
  # create empty tibble to store values from each iteration
  sim_stats_sw_barbell <<- tibble(iteration = integer(),
                                  nodes = integer(),
                                  endangered = integer(),
                                  precarity_proportion = numeric(),
                                  total_resources = numeric(),
                                  median_resources = numeric(),
                                  mean_resources = numeric(),
                                  global_funds = numeric(),
                                  node_list = list()
  )
  
  # loop thru iterations
  counter <- 1
  while( counter <= (12*years)){
    set.seed(333^3)
    p1 <- ggraph(graph_sw, layout = 'nicely') + 
      geom_edge_link(aes(width = edge_weight, 
                         alpha = edge_weight)) + 
      geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + 
      scale_color_manual(values = c('steelblue', 'red')) +
      scale_alpha(range = c(0, 1), guide = 'none', limits = c(0.12,.3)) +
      scale_size_continuous(
        breaks = c(100,1000,5000,10000,20000),
        limits = c(0, 10000000),
        range = c(1, 217) 
      ) +
      scale_edge_width_continuous(
        limits = c(0, 1),
        range = c(0, .77)
      ) +
      labs(title = 'Watts–Strogatz (small world) graph: barbell strategy',
           subtitle=paste('iteration:', counter, '    ',
                          'endangered: ', sum(as.list(graph_sw)$nodes$endangered),
                          '    precarity proportion: ', 
                          round(sum(as.list(graph_sw)$nodes$endangered) / length(as.list(graph_sw)$nodes$endangered),
                                3) * 100, '%')) +
      theme_graph() + theme(plot.margin = margin(.3, .3, .3, .3, "cm"))
    
    
    p2 <- as.list(graph_sw)$nodes %>% as_tibble() %>% filter(resources < 1000) %>% 
      ggplot() + geom_area(aes(resources), stat = 'density') + 
      theme_minimal() + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_markdown(size = rel(0.75))) +
      geom_vline(xintercept =  15, color = "red3",
                 linetype = "dashed", size = .7) +
      geom_vline(xintercept =  median(as_tibble(as.list(graph_sw)$nodes)$resources), 
                 color = "seagreen",
                 linetype = "solid", size = 1) +
      geom_vline(xintercept =  mean(as_tibble(as.list(graph_sw)$nodes)$resources), 
                 color = "green2",
                 linetype = "solid", size = .7) +
      xlab("resource distribution (w/ <span style='color:#FF0000;'>**precarity**</span> threshold, 
           <span style='color:#008000;'>**median**</span> & <span style='color:#32CD32;'>**mean**</span> lines)") +
      xlim(0, 1000) 
    
    patchplot <- p1 / p2 + plot_layout(widths = c(1, 1), heights = c(5.25,.75))  
    
    print(patchplot)
    
    # add row to tibble
    sim_stats_sw_barbell_row <- tibble_row(iteration = counter,
                                           nodes = length(as.list(graph_sw)$nodes$endangered),
                                           endangered = sum(as.list(graph_sw)$nodes$endangered),
                                           precarity_proportion = sum(as.list(graph_sw)$nodes$endangered) / length(as.list(graph_sw)$nodes$endangered),
                                           total_resources = sum(as_tibble(as.list(graph_sw)$nodes)$resources),
                                           median_resources = median(as_tibble(as.list(graph_sw)$nodes)$resources),
                                           mean_resources = mean(as_tibble(as.list(graph_sw)$nodes)$resources),
                                           global_funds = ifelse(exists('global_fund'), global_fund, 0.0),
                                           node_list = list(as_tibble(as.list(graph_sw)$nodes)$resources)
    )
    
    sim_stats_sw_barbell <<- sim_stats_sw_barbell %>% bind_rows(sim_stats_sw_barbell_row)
    
    # increment graph via increment function
    graph_sw <- graph_sim_increment(graph_sw)
    
    # apply barbell solidarity strategy
    graph_sw <- graph_sim_barbell_strategy(graph_sw)
    
    counter <- counter+1
  }
}

# SW memelord strategy simulation & animation
memelord_gif_maker_sw <- function(years){
  
  # create empty tibble to store values from each iteration
  sim_stats_sw_memelord <<- tibble(iteration = integer(),
                                  nodes = integer(),
                                  endangered = integer(),
                                  precarity_proportion = numeric(),
                                  total_resources = numeric(),
                                  median_resources = numeric(),
                                  mean_resources = numeric(),
                                  node_list = list()
  )
  
  # loop thru iterations
  counter <- 1
  while( counter <= (12*years)){
    set.seed(333^3)
    p1 <- ggraph(graph_sw, layout = 'nicely') + 
      geom_edge_link(aes(width = edge_weight, 
                         alpha = edge_weight)) + 
      geom_node_point(aes(size = resources, color = endangered, alpha = (1/log10(resources)) )) + 
      scale_color_manual(values = c('steelblue', 'red')) +
      scale_alpha(range = c(0, 1), guide = 'none', limits = c(0.12,.3)) +
      scale_size_continuous(
        breaks = c(100,1000,5000,10000,20000),
        limits = c(0, 10000000),
        range = c(1, 217) 
      ) +
      scale_edge_width_continuous(
        limits = c(0, 1),
        range = c(0, .77)
      ) +
      labs(title = 'Watts–Strogatz (small world) graph: memelord strategy',
           subtitle=paste('iteration:', counter, '    ',
                          'endangered: ', sum(as.list(graph_sw)$nodes$endangered),
                          '    precarity proportion: ', 
                          round(sum(as.list(graph_sw)$nodes$endangered) / length(as.list(graph_sw)$nodes$endangered),
                                3) * 100, '%')) +
      theme_graph() + theme(plot.margin = margin(.3, .3, .3, .3, "cm"))
    
    
    p2 <- as.list(graph_sw)$nodes %>% as_tibble() %>% filter(resources < 1000) %>% 
      ggplot() + geom_area(aes(resources), stat = 'density') + 
      theme_minimal() + 
      theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.title.x = element_markdown(size = rel(0.75))) +
      geom_vline(xintercept =  15, color = "red3",
                 linetype = "dashed", size = .7) +
      geom_vline(xintercept =  median(as_tibble(as.list(graph_sw)$nodes)$resources), 
                 color = "seagreen",
                 linetype = "solid", size = 1) +
      geom_vline(xintercept =  mean(as_tibble(as.list(graph_sw)$nodes)$resources), 
                 color = "green2",
                 linetype = "solid", size = .7) +
      xlab("resource distribution (w/ <span style='color:#FF0000;'>**precarity**</span> threshold, 
           <span style='color:#008000;'>**median**</span> & <span style='color:#32CD32;'>**mean**</span> lines)") +
      xlim(0, 1000) 
    
    patchplot <- p1 / p2 + plot_layout(widths = c(1, 1), heights = c(5.25,.75))  
    
    print(patchplot)
    
    # add row to tibble
    sim_stats_sw_memelord_row <- tibble_row(iteration = counter,
                                           nodes = length(as.list(graph_sw)$nodes$endangered),
                                           endangered = sum(as.list(graph_sw)$nodes$endangered),
                                           precarity_proportion = sum(as.list(graph_sw)$nodes$endangered) / length(as.list(graph_sw)$nodes$endangered),
                                           total_resources = sum(as_tibble(as.list(graph_sw)$nodes)$resources),
                                           median_resources = median(as_tibble(as.list(graph_sw)$nodes)$resources),
                                           mean_resources = mean(as_tibble(as.list(graph_sw)$nodes)$resources),
                                           node_list = list(as_tibble(as.list(graph_sw)$nodes)$resources)
    )
    
    sim_stats_sw_memelord <<- sim_stats_sw_memelord %>% bind_rows(sim_stats_sw_memelord_row)
    
    # increment graph via increment function
    graph_sw <- graph_sim_increment(graph_sw)
    
    # apply memelord solidarity strategy
    graph_sw <- graph_sim_memelord_strategy(graph_sw)
    
    counter <- counter+1
  }
}
