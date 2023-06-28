##           Social Network Analysis              ##
##                 Project II                     ##
##       Dimitris Matsanganis FT f2822212         ##
####################################################

######################### START: Import Libraries ##############################

# Import the necessary libraries.
library(igraph)
library(ggplot2)

########################## END: Import Libraries ###############################

######################### START: Import Datasets ###############################

# The initial data origin from the provided link and is in a compressed format 
# (tweets2009-07.txt.gz).
# You can find this file here:
# https://drive.google.com/file/d/1RjWUg-6KrVOjJPZHHQg-h_9gSSWZUPn-/view
# The compressed file contains the `tweets2009-07.txt` file. 
# The above file was the input and through the created Python file 
# `raw_data_handler.py` output the following 10 CSV files (5 for the mentions 
# and 5 for the hashtags, one for each day).
# You can see more in the relevant Python file.
# The first step to parse the data in R is to import the datasets as dataframes.

# Import the user/mentions files.
df_mentions_07_01 = read.csv("2009.07.01_mentions.csv")
df_mentions_07_02 = read.csv("2009.07.02_mentions.csv")
df_mentions_07_03 = read.csv("2009.07.03_mentions.csv")
df_mentions_07_04 = read.csv("2009.07.04_mentions.csv")
df_mentions_07_05 = read.csv("2009.07.05_mentions.csv")

# Import the hashtags files.
df_hashtags_07_01 = read.csv("2009.07.01_hashtags.csv")
df_hashtags_07_02 = read.csv("2009.07.02_hashtags.csv")
df_hashtags_07_03 = read.csv("2009.07.03_hashtags.csv")
df_hashtags_07_04 = read.csv("2009.07.04_hashtags.csv")
df_hashtags_07_05 = read.csv("2009.07.05_hashtags.csv")

########################## END: Import Datasets ################################

########################## START: Data Handling ################################

# Now to make our analysis easier and to fulfill the instruction regarding 
# having null/na to the user that do not contain any hashtag to their tweets
# we will merge the dataframes to create 5 based on the user and then replace
# the empty fields with the `Null/NA` notations as mentioned.

# Merge the dataframes based on the specified columns the from user and the
# user for the second file.

# 01/07/2009
df_07_01 = merge(df_mentions_07_01, df_hashtags_07_01, by.x = "from",
                 by.y = "user", all.x = TRUE)

# 02/07/2009
df_07_02 = merge(df_mentions_07_02, df_hashtags_07_02, by.x = "from",
                 by.y = "user", all.x = TRUE)

# 03/07/2009
df_07_03 = merge(df_mentions_07_03, df_hashtags_07_03, by.x = "from",
                 by.y = "user", all.x = TRUE)

# 04/07/2009
df_07_04 = merge(df_mentions_07_04, df_hashtags_07_04, by.x = "from",
                 by.y = "user", all.x = TRUE)

# 05/07/2009
df_07_05 = merge(df_mentions_07_05, df_hashtags_07_05, by.x = "from",
                 by.y = "user", all.x = TRUE)

# Now, since the Null/NA is not a hashtag we may remove the '#' from the 
# beginning of this occurrences (no specific instruction is giver therefore, 
# in case we need to maintain the '#' we ignore this commands).
df_07_01$topic_of_interest = gsub("#Null/NA", "Null/NA", 
                                  df_07_01$topic_of_interest)

df_07_02$topic_of_interest = gsub("#Null/NA", "Null/NA", 
                                  df_07_02$topic_of_interest)

df_07_03$topic_of_interest = gsub("#Null/NA", "Null/NA", 
                                  df_07_03$topic_of_interest)

df_07_04$topic_of_interest = gsub("#Null/NA", "Null/NA", 
                                  df_07_04$topic_of_interest)

df_07_05$topic_of_interest = gsub("#Null/NA", "Null/NA", 
                                  df_07_05$topic_of_interest)

# To avoid time consuming procedures and for backup purposes we export the 5 
# final CSV files (Remove the comments to execute them!).
# write.csv(df_07_01, "2009.07.01.csv", row.names = FALSE)
# write.csv(df_07_02, "2009.07.02.csv", row.names = FALSE)
# write.csv(df_07_03, "2009.07.03.csv", row.names = FALSE)
# write.csv(df_07_04, "2009.07.04.csv", row.names = FALSE)
# write.csv(df_07_05, "2009.07.05.csv", row.names = FALSE)

# Re import them to skip the above procedure as a shortcut.
# df_07_01 = read.csv("2009.07.01.csv")
# df_07_02 = read.csv("2009.07.02.csv")
# df_07_03 = read.csv("2009.07.03.csv")
# df_07_04 = read.csv("2009.07.04.csv")
# df_07_05 = read.csv("2009.07.05.csv")

# Remove the initial dataframes since now are not needed, for clarity and 
# optimization of memory purposes.
rm(df_mentions_07_01, df_mentions_07_02, df_mentions_07_03, df_mentions_07_04, 
   df_mentions_07_05, df_hashtags_07_01, df_hashtags_07_02, df_hashtags_07_03,
   df_hashtags_07_04, df_hashtags_07_05)

########################### END: Data Handling #################################

###################### START: Q1: Graph Creation ###############################

# Now, we moved to the creation of the Graphs with the assistance of igraph. 

# We have set the seed for re productivity purposes.
set.seed(2822212)

# Create the graphs with igraph library and the related dataframe.
graph_07_01 = graph_from_data_frame(df_07_01, directed=TRUE)
graph_07_02 = graph_from_data_frame(df_07_02, directed=TRUE)
graph_07_03 = graph_from_data_frame(df_07_03, directed=TRUE)
graph_07_04 = graph_from_data_frame(df_07_04, directed=TRUE)
graph_07_05 = graph_from_data_frame(df_07_05, directed=TRUE)

# Update graph vertices with topic_of_interest attribute - the most 
# frequently used hashtag.
graph_07_01 = set_vertex_attr(graph_07_01, name = "topic_of_interest", 
              value = df_07_01$topic_of_interest[V(graph_07_01)])

graph_07_02 = set_vertex_attr(graph_07_02, name = "topic_of_interest", 
              value = df_07_02$topic_of_interest[V(graph_07_02)])

graph_07_03 = set_vertex_attr(graph_07_03, name = "topic_of_interest", 
              value = df_07_03$topic_of_interest[V(graph_07_03)])

graph_07_04 = set_vertex_attr(graph_07_04, name = "topic_of_interest", 
              value = df_07_04$topic_of_interest[V(graph_07_04)])

graph_07_05 = set_vertex_attr(graph_07_05, name = "topic_of_interest",
              value = df_07_05$topic_of_interest[V(graph_07_05)])

# Validate that the above modification has taken place.
# Check if the "topic_of_interest" attribute is present in the vertices
# has_attribute = "topic_of_interest" %in% names(vertex_attr(graph_07_01))
# 
# if (has_attribute) 
# {
#   cat("The 'topic_of_interest' attribute is present in the subgraph vertices.\n")
# } 
# else
# {
#   cat("The 'topic_of_interest' attribute is NOT present in the subgraph vertices.\n")
# }

####################### END: Q1: Graph Creation ################################

#################### START: Q2: Average degree over time #######################

# Number of vertices for each graph.
number_of_vertices = c(gorder(graph_07_01), gorder(graph_07_02), 
                       gorder(graph_07_03), gorder(graph_07_04), 
                       gorder(graph_07_05))

# Create a table with labels.
table_data = data.frame(row.names = c("July 1st", "July 2nd", "July 3rd",
                                      "July 4th", "July 5th"),
                        number_of_vertices = number_of_vertices)

# Preview the table.
table_data

# Number of edges for each graph.
number_of_edges = c(gsize(graph_07_01), gsize(graph_07_02), gsize(graph_07_03),
                    gsize(graph_07_04), gsize(graph_07_05))
number_of_edges

# Add Number of Edges to the table.
table_data$number_of_edges = number_of_edges

# Rename the column.
colnames(table_data)[1] = "Number of Vertices"
colnames(table_data)[2] = "Number of Edges"

# Preview the updated table.
table_data

# Validation checks prior to find the diameter.
is.weighted(graph_07_01)
E(graph_07_01)$weight 

# Calculate the diameter of each graph (weighted).
diameter_of_the_graph1 = diameter(graph_07_01, directed = TRUE, 
                                  weights= E(graph_07_01)$weight)

diameter_of_the_graph2 = diameter(graph_07_02, directed = TRUE,  
                                  weights= E(graph_07_02)$weight)

diameter_of_the_graph3 = diameter(graph_07_03, directed = TRUE,  
                                  weights= E(graph_07_03)$weight)

diameter_of_the_graph4 = diameter(graph_07_04, directed = TRUE, 
                                  weights= E(graph_07_04)$weight)

diameter_of_the_graph5 = diameter(graph_07_05, directed = TRUE, 
                                  weights= E(graph_07_05)$weight)

# Combine the diameter values into a vector.
diameter_of_the_graphs = c(diameter_of_the_graph1, diameter_of_the_graph2, 
                           diameter_of_the_graph3, diameter_of_the_graph4,
                           diameter_of_the_graph5)

# Preview.
diameter_of_the_graphs

# Add diameter values to the table.
table_data$Diameter = c(diameter_of_the_graph1, diameter_of_the_graph2,
                         diameter_of_the_graph3, diameter_of_the_graph4,
                         diameter_of_the_graph5)

# Rename the column.
colnames(table_data)[3] = "Diameter (Weighted)"

# Preview the updated table.
table_data

# Calculate the diameter of each graph (without weights).
diameter_of_the_graph1_no_weights = diameter(graph_07_01, directed = TRUE,
                                             weights = NA)

diameter_of_the_graph2_no_weights = diameter(graph_07_02, directed = TRUE,
                                             weights = NA)

diameter_of_the_graph3_no_weights = diameter(graph_07_03, directed = TRUE, 
                                             weights = NA)

diameter_of_the_graph4_no_weights = diameter(graph_07_04, directed = TRUE,
                                             weights = NA)

diameter_of_the_graph5_no_weights = diameter(graph_07_05, directed = TRUE,
                                             weights = NA)

# Combine the diameter values into a vector.
diameter_of_the_graphs_no_weights = c(diameter_of_the_graph1_no_weights,
                                      diameter_of_the_graph2_no_weights, 
                                      diameter_of_the_graph3_no_weights,
                                      diameter_of_the_graph4_no_weights,
                                      diameter_of_the_graph5_no_weights)

# Preview.
diameter_of_the_graphs_no_weights

# Add diameter values to the table.
table_data$Diameter = c(diameter_of_the_graph1_no_weights,
                        diameter_of_the_graph2_no_weights,
                        diameter_of_the_graph3_no_weights,
                        diameter_of_the_graph4_no_weights,
                        diameter_of_the_graph5_no_weights)

# Rename the column.
colnames(table_data)[4] = "Diameter (Not Weighted)"

# Preview the updated table.
table_data

# Calculate the average in-degree for each graph.
in_degree = round(sapply(1:5, function(i) {
  mean(degree(get(paste0("graph_07_0", i)),
              v = V(get(paste0("graph_07_0", i))),
              mode = c("in"), loops = FALSE, normalized = FALSE))}), 2)

# Calculate the average out-degree for each graph.
out_degree = round(sapply(1:5, function(i) {
  mean(degree(get(paste0("graph_07_0", i)),
              v = V(get(paste0("graph_07_0", i))), 
              mode = c("out"), loops = FALSE, normalized = FALSE))}), 2)

# Add average in-degree and out-degree to the table.
table_data$Avg_In_Degree = in_degree
table_data$Avg_Out_Degree = out_degree

# Rename the columns.
colnames(table_data)[5] = "Average In-Degree"
colnames(table_data)[6] = "Average Out-Degree"

# Preview the updated and final table.
table_data

# Plots of the above findings.
# The first step we need to do is to define the above table 'table_data', as a 
# dataframe for all the created plot to extract each time the wanted data.

# Create a data frame for table
df_q2_data = as.data.frame(table_data)

# Reset the index and redefine the Date column as a new column.
# (Note: It contains dates in string format as July 1st etc).
df_q2_data$Date = rownames(df_q2_data)
rownames(df_q2_data) = NULL

# Number of vertices plot.
# Barplot Analyzing the 5 Days Trend Regarding the Number of Vertices.
p_vertices_barplot = ggplot(data = df_q2_data, 
                             aes(x = `Date`, 
                                 y = `Number of Vertices`,
                                 fill = `Date`)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  geom_text(aes(label = `Number of Vertices`,  y = `Number of Vertices`), 
            position = position_stack(vjust = 1.03), 
            size = 4, color = "black") +
  labs(x = "Dates of July", y = "Number of Vertices") +
  ggtitle("Barplot Analyzing the 5 Days Trend Regarding the Number of Vertices") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                  family = "Arial", color = "dodgerblue3"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, family = "Arial", 
                                  color = "dodgerblue3"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.5)) +
  coord_cartesian(ylim = c(0, 510000)) +
  scale_y_continuous(labels = scales::comma)

# Preview the plot.
p_vertices_barplot

# Trendline Diagram for Number of Vertices over the 5 Days.
p_vertices_trendline = ggplot(data = df_q2_data, 
                               aes(x = `Date`, 
                                   y = `Number of Vertices`, 
                                   group = 1)) +
  geom_point(size = 4, color = "dodgerblue3", shape = 18) +
  geom_line() +
  geom_text(aes(label = `Number of Vertices`), vjust = -1) + 
  labs(x = "Dates of July", y = "Number of Vertices", 
       title = "Trendline Diagram for Number of Vertices over the 5 Days") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                  family = "Arial", color = "dodgerblue3")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, family = "Arial", 
                                  color = "dodgerblue3"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.5)) +
  coord_cartesian(ylim = c(150000, 530000)) +
  scale_y_continuous(labels = scales::comma)

# Preview the plot.
p_vertices_trendline

# Number of edges plot.
# Barplot Analyzing the 5 Days Trend Regarding the Number of Edges.
p_edges_barplot = ggplot(data = df_q2_data, 
                            aes(x = `Date`, 
                                y = `Number of Edges`,
                                fill = `Date`)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  geom_text(aes(label = `Number of Edges`,  y = `Number of Edges`), 
            position = position_stack(vjust = 1.04), 
            size = 4, color = "black") +
  labs(x = "Dates of July", y = "Number of Edges") +
  ggtitle("Barplot Analyzing the 5 Days Trend Regarding the Number of Edges") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                  family = "Arial", color = "dodgerblue3"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, family = "Arial", 
                                  color = "dodgerblue3"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.5)) +
  coord_cartesian(ylim = c(0, 580000)) +
  scale_y_continuous(labels = scales::comma)

# Preview the plot.
p_edges_barplot

# Trendline Diagram for Number of Edges over the 5 Days.
p_edges_trendline = ggplot(data = df_q2_data, 
                               aes(x = `Date`, 
                                   y = `Number of Edges`, 
                                   group = 1)) +
  geom_point(size = 4, color = "dodgerblue3", shape = 18) +
  geom_line() +
  geom_text(aes(label = `Number of Edges`), vjust = -1) + 
  labs(x = "Dates of July", y = "Number of Edges", 
       title = "Trendline Diagram for Number of Edges over the 5 Days") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                  family = "Arial", color = "dodgerblue3")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, family = "Arial", 
                                  color = "dodgerblue3"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.5)) +
  coord_cartesian(ylim = c(200000, 560000)) +
  scale_y_continuous(labels = scales::comma)

# Preview the plot.
p_edges_trendline

# Diameter of the graph (weighted) plot.
# Barplot Analyzing the 5 Days Trend Regarding the Diameter of the Graph (weighted).
p_diameter_barplot = ggplot(data = df_q2_data, 
                            aes(x = `Date`, 
                                y = `Diameter (Weighted)`,
                                fill = `Date`)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  geom_text(aes(label = `Diameter (Weighted)`,  y = `Diameter (Weighted)`), 
            position = position_stack(vjust = 1.02), 
            size = 4, color = "black") +
  labs(x = "Dates of July", y = "Diameter of the Graph (weighted)") +
  ggtitle("Barplot Analyzing the 5 Days Trend Regarding the Diameter of the Graph (weighted)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                  family = "Arial", color = "dodgerblue3"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, family = "Arial", 
                                  color = "dodgerblue3"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.5)) +
  coord_cartesian(ylim = c(40, 100)) +
  scale_y_continuous(labels = scales::comma)

# Preview the plot.
p_diameter_barplot

# Trendline Diagram for Diameter of the Graph over the 5 Days (weighted).
p_diameter_trendline = ggplot(data = df_q2_data, 
                           aes(x = `Date`, 
                               y = `Diameter (Weighted)`,
                               group = 1)) +
  geom_point(size = 5, color = "dodgerblue3", shape = 18) +
  geom_line() +
  geom_text(aes(label = `Diameter (Weighted)`), vjust = -1) + 
  labs(x = "Dates of July", y = "Diameter of the Graph (weighted)", 
       title = "Trendline Diagram for Diameter of the Graph over the 5 Days (weighted)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                  family = "Arial", color = "dodgerblue3")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, family = "Arial", 
                                  color = "dodgerblue3"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.5)) +
  coord_cartesian(ylim = c(50, 100)) +
  scale_y_continuous(labels = scales::comma)

# Preview the plot.
p_diameter_trendline

# Diameter of the graph (not weighted) plot.
# Barplot Analyzing the 5 Days Trend Regarding the Diameter of the Graph (not weighted).
p_diameterNW_barplot = ggplot(data = df_q2_data, 
                            aes(x = `Date`, 
                                y = `Diameter (Not Weighted)`,
                                fill = `Date`)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  geom_text(aes(label = `Diameter (Not Weighted)`,  y = `Diameter (Not Weighted)`), 
            position = position_stack(vjust = 1.02), 
            size = 4, color = "black") +
  labs(x = "Dates of July", y = "Diameter of the Graph (not weighted)") +
  ggtitle("Barplot Analyzing the 5 Days Trend Regarding the Diameter of the Graph (not weighted)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                  family = "Arial", color = "dodgerblue3"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, family = "Arial", 
                                  color = "dodgerblue3"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.5)) +
  coord_cartesian(ylim = c(28, 70)) +
  scale_y_continuous(labels = scales::comma)

# Preview the plot.
p_diameterNW_barplot

# Trendline Diagram for Diameter of the Graph over the 5 Days (not weighted).
p_diameterNW_trendline = ggplot(data = df_q2_data, 
                              aes(x = `Date`, 
                                  y = `Diameter (Not Weighted)`,
                                  group = 1)) +
  geom_point(size = 5, color = "dodgerblue3", shape = 18) +
  geom_line() +
  geom_text(aes(label = `Diameter (Not Weighted)`), vjust = -1) + 
  labs(x = "Dates of July", y = "Diameter of the Graph (not weighted)", 
       title = "Trendline Diagram for Diameter of the Graph over the 5 Days (not weighted)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                  family = "Arial", color = "dodgerblue3")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, family = "Arial", 
                                  color = "dodgerblue3"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.5)) +
  coord_cartesian(ylim = c(30, 65)) +
  scale_y_continuous(labels = scales::comma)

# Preview the plot.
p_diameterNW_trendline

# Average in-degree plot.
# Barplot Analyzing the 5 Days Trend Regarding the Average In-Degree.
p_indegree_barplot = ggplot(data = df_q2_data, 
                            aes(x = `Date`, 
                                y = `Average In-Degree`,
                                fill = `Date`)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  geom_text(aes(label = `Average In-Degree`,  y = `Average In-Degree`), 
            position = position_stack(vjust = 1.01), 
            size = 4, color = "black") +
  labs(x = "Dates of July", y = "Average In-Degree") +
  ggtitle("Barplot Analyzing the 5 Days Trend Regarding the Average In-Degree") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                  family = "Arial", color = "dodgerblue3"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, family = "Arial", 
                                  color = "dodgerblue3"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.5)) +
  coord_cartesian(ylim = c(1, 1.34)) +
  scale_y_continuous(labels = scales::comma)

# Preview the plot.
p_indegree_barplot

# Trendline Diagram for the Average In-Degree over the 5 Days.
p_indegree_trendline = ggplot(data = df_q2_data, 
                              aes(x = `Date`, 
                                  y = `Average In-Degree`,
                                  group = 1)) +
  geom_point(size = 5, color = "dodgerblue3", shape = 18) +
  geom_line() +
  geom_text(aes(label = `Average In-Degree`), vjust = -1) + 
  labs(x = "Dates of July", y = "Average In-Degree", 
       title = "Trendline Diagram for the Average In-Degree over the 5 Days") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                  family = "Arial", color = "dodgerblue3")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, family = "Arial", 
                                  color = "dodgerblue3"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.5)) +
  coord_cartesian(ylim = c(1.04, 1.34)) +
  scale_y_continuous(labels = scales::comma)

# Preview the plot.
p_indegree_trendline

# Average Out-Degree plot.
# Barplot Analyzing the 5 Days Trend Regarding the Average Out-Degree.
p_outdegree_barplot = ggplot(data = df_q2_data, 
                             aes(x = `Date`, 
                                 y = `Average Out-Degree`,
                                 fill = `Date`)) +
  geom_bar(stat = "identity", color = "black", width = 0.5) +
  geom_text(aes(label = `Average Out-Degree`,  y = `Average Out-Degree`), 
            position = position_stack(vjust = 1.01), 
            size = 4, color = "black") +
  labs(x = "Dates of July", y = "Average Out-Degree") +
  ggtitle("Barplot Analyzing the 5 Days Trend Regarding the Average Out-Degree") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                  family = "Arial", color = "dodgerblue3"),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, family = "Arial", 
                                  color = "dodgerblue3"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.5)) +
  coord_cartesian(ylim = c(1, 1.34)) +
  scale_y_continuous(labels = scales::comma)

# Preview the plot.
p_outdegree_barplot

# Trendline Diagram for the Average Out-Degree over the 5 Days.
p_outdegree_trendline = ggplot(data = df_q2_data, 
                               aes(x = `Date`, 
                                   y = `Average Out-Degree`,
                                   group = 1)) +
  geom_point(size = 5, color = "dodgerblue3", shape = 18) +
  geom_line() +
  geom_text(aes(label = `Average Out-Degree`), vjust = -1) + 
  labs(x = "Dates of July", y = "Average Out-Degree", 
       title = "Trendline Diagram for the Average Out-Degree over the 5 Days") +
  theme_minimal() +
  theme(plot.title = element_text(size = 15, face = "bold", hjust = 0.5, 
                                  family = "Arial", color = "dodgerblue3")) +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, family = "Arial", 
                                  color = "dodgerblue3"),
        legend.position = "none",
        axis.line = element_line(color = "black", size = 0.5)) +
  coord_cartesian(ylim = c(1.04, 1.34)) +
  scale_y_continuous(labels = scales::comma)

# Preview the plot.
p_outdegree_trendline

##################### END: Q2: Average degree over time ########################

######################## START: Q3: Important nodes ############################

# Top-10 Twitter users with regard to In-Degree.

# Create a data frame to store the results.
result_table_in_degree = data.frame()

# July 1st 2009 In-Degree.
df_in_07_01 = data.frame(in_degree_07_01 = sort(strength(graph_07_01, 
                                                 vids = V(graph_07_01),
                                                 mode = "in",
                                                 loops = FALSE, 
                                                 weights = E(graph_07_01)$weight),
                                                 decreasing = TRUE))

df_in_07_01 = df_in_07_01[order(-df_in_07_01$in_degree_07_01), , drop = FALSE]

# Preview the results (only the top 10).
# head(df_in_07_01, 10)

# Append to the result table.
result_table_in_degree = cbind(names_in0701 = rownames(head(df_in_07_01, 10)))
result_table_in_degree = cbind(result_table_in_degree, head(df_in_07_01, 10))

# July 2nd 2009 In-Degree.
df_in_07_02 = data.frame(in_degree_07_02 = sort(strength(graph_07_02, 
                                                         vids = V(graph_07_02),
                                                         mode = "in",
                                                         loops = FALSE, 
                                                         weights = E(graph_07_02)$weight),
                                                decreasing = TRUE))

df_in_07_02 = df_in_07_02[order(-df_in_07_02$in_degree_07_02), , drop = FALSE]

# Preview the results (only the top 10).
# head(df_in_07_02, 10)

# Append to the result table.
result_table_in_degree = cbind(result_table_in_degree, names_in0702 = rownames(head(df_in_07_02, 10)))
result_table_in_degree = cbind(result_table_in_degree, head(df_in_07_02, 10))

# July 3rd 2009 In-Degree.
df_in_07_03 = data.frame(in_degree_07_03 = sort(strength(graph_07_03, 
                                                         vids = V(graph_07_03),
                                                         mode = "in",
                                                         loops = FALSE, 
                                                         weights = E(graph_07_03)$weight),
                                                decreasing = TRUE))

df_in_07_03 = df_in_07_03[order(-df_in_07_03$in_degree_07_03), , drop = FALSE]

# Preview the results (only the top 10).
# head(df_in_07_03, 10)

# Append to the result table.
result_table_in_degree = cbind(result_table_in_degree, names_in0703 = rownames(head(df_in_07_03, 10)))
result_table_in_degree = cbind(result_table_in_degree, head(df_in_07_03, 10))

# July 4th 2009 In-Degree.
df_in_07_04 = data.frame(in_degree_07_04 = sort(strength(graph_07_04, 
                                                         vids = V(graph_07_04),
                                                         mode = "in",
                                                         loops = FALSE, 
                                                         weights = E(graph_07_04)$weight),
                                                decreasing = TRUE))

df_in_07_04 = df_in_07_04[order(-df_in_07_04$in_degree_07_04), , drop = FALSE]

# Preview the results (only the top 10).
# head(df_in_07_04, 10)

# Append to the result table.
result_table_in_degree = cbind(result_table_in_degree, names_in0704 = rownames(head(df_in_07_04, 10)))
result_table_in_degree = cbind(result_table_in_degree, head(df_in_07_04, 10))

# July 5th 2009 In-Degree.
df_in_07_05 = data.frame(in_degree_07_05 = sort(strength(graph_07_05, 
                                                         vids = V(graph_07_05),
                                                         mode = "in",
                                                         loops = FALSE, 
                                                         weights = E(graph_07_05)$weight),
                                                decreasing = TRUE))

df_in_07_05 = df_in_07_05[order(-df_in_07_05$in_degree_07_05), , drop = FALSE]

# Preview the results (only the top 10).
# head(df_in_07_05, 10)

# Append to the result table.
result_table_in_degree = cbind(result_table_in_degree, names_in0705 = rownames(head(df_in_07_05, 10)))
result_table_in_degree = cbind(result_table_in_degree, head(df_in_07_05, 10))

# Reset the index to 1-10.
row.names(result_table_in_degree) = 1:10

# Rename the columns with more related names.
colnames(result_table_in_degree) = c('Users July 1st', 'In-Degree July 1st',
                                     'Users July 2nd', 'In-Degree July 2nd',
                                     'Users July 3rd', 'In-Degree July 3rd',
                                     'Users July 4th', 'In-Degree July 4th',
                                     'Users July 5th', 'In-Degree July 5th')

# Preview the results for In-Degree.
result_table_in_degree

################################################################################

# Top-10 Twitter users with regard to Out-Degree.

# Create a data frame to store the results.
result_table_out_degree = data.frame()

# July 1st 2009 Out-Degree.
df_out_07_01 = data.frame(out_degree_07_01 = sort(strength(graph_07_01, 
                                                         vids = V(graph_07_01),
                                                         mode = "out",
                                                         loops = FALSE, 
                                                         weights = E(graph_07_01)$weight),
                                                decreasing = TRUE))

df_out_07_01 = df_out_07_01[order(-df_out_07_01$out_degree_07_01), , drop = FALSE]

# Preview the results (only the top 10).
# head(df_out_07_01, 10)

# Append to the result table.
result_table_out_degree = cbind(names_out0701 = rownames(head(df_out_07_01, 10)))
result_table_out_degree = cbind(result_table_out_degree, head(df_out_07_01, 10))

# July 2nd 2009 Out-Degree.
df_out_07_02 = data.frame(out_degree_07_02 = sort(strength(graph_07_02, 
                                                         vids = V(graph_07_02),
                                                         mode = "out",
                                                         loops = FALSE, 
                                                         weights = E(graph_07_02)$weight),
                                                decreasing = TRUE))

df_out_07_02 = df_out_07_02[order(-df_out_07_02$out_degree_07_02), , drop = FALSE]

# Preview the results (only the top 10).
# head(df_out_07_02, 10)

# Append to the result table.
result_table_out_degree = cbind(result_table_out_degree, names_out0702 = rownames(head(df_out_07_02, 10)))
result_table_out_degree = cbind(result_table_out_degree, head(df_out_07_02, 10))

# July 3rd 2009 Out-Degree.
df_out_07_03 = data.frame(out_degree_07_03 = sort(strength(graph_07_03, 
                                                         vids = V(graph_07_03),
                                                         mode = "out",
                                                         loops = FALSE, 
                                                         weights = E(graph_07_03)$weight),
                                                decreasing = TRUE))

df_out_07_03 = df_out_07_03[order(-df_out_07_03$out_degree_07_03), , drop = FALSE]

# Preview the results (only the top 10).
# head(df_out_07_03, 10)

# Append to the result table.
result_table_out_degree = cbind(result_table_out_degree, names_out0703 = rownames(head(df_out_07_03, 10)))
result_table_out_degree = cbind(result_table_out_degree, head(df_out_07_03, 10))

# July 4th 2009 Out-Degree.
df_out_07_04 = data.frame(out_degree_07_04 = sort(strength(graph_07_04, 
                                                         vids = V(graph_07_04),
                                                         mode = "out",
                                                         loops = FALSE, 
                                                         weights = E(graph_07_04)$weight),
                                                decreasing = TRUE))

df_out_07_04 = df_out_07_04[order(-df_out_07_04$out_degree_07_04), , drop = FALSE]

# Preview the results (only the top 10).
# head(df_out_07_04, 10)

# Append to the result table.
result_table_out_degree = cbind(result_table_out_degree, names_out0704 = rownames(head(df_out_07_04, 10)))
result_table_out_degree = cbind(result_table_out_degree, head(df_out_07_04, 10))

# July 5th 2009 Out-Degree.
df_out_07_05 = data.frame(out_degree_07_05 = sort(strength(graph_07_05,
                                                           vids = V(graph_07_05),
                                                           mode = "out",
                                                           loops = FALSE, 
                                                           weights = E(graph_07_05)$weight),
                                                  decreasing = TRUE))

df_out_07_05 = df_out_07_05[order(-df_out_07_05$out_degree_07_05), , drop = FALSE]

# Preview the results (only the top 10).
# head(df_out_07_05, 10)

# Append to the result table.
result_table_out_degree = cbind(result_table_out_degree, names_out0705 = rownames(head(df_out_07_05, 10)))
result_table_out_degree = cbind(result_table_out_degree, head(df_out_07_05, 10))

# Reset the index to 1-10.
row.names(result_table_out_degree) = 1:10

# Rename the columns with more related names.
colnames(result_table_out_degree) = c('Users July 1st', 'Out-Degree July 1st',
                                      'Users July 2nd', 'Out-Degree July 2nd',
                                      'Users July 3rd', 'Out-Degree July 3rd',
                                      'Users July 4th', 'Out-Degree July 4th',
                                      'Users July 5th', 'Out-Degree July 5th')

# Preview the results for Out-Degree.
result_table_out_degree

################################################################################

# Top-10 Twitter users with regard to PageRank.

# Create a data frame to store the results.
result_table_rank = data.frame()

# July 1st 2009 PageRank.
rank1 = page_rank(graph_07_01, algo = "prpack",
                   vids = V(graph_07_01), directed = TRUE, damping = 0.85,
                   personalized = NULL, weights = E(graph_07_01)$weight)

# July 2nd 2009 PageRank.
rank2 = page_rank(graph_07_02, algo = "prpack",
                   vids = V(graph_07_02), directed = TRUE, damping = 0.85,
                   personalized = NULL, weights = E(graph_07_02)$weight)

# July 3rd 2009 PageRank.
rank3 = page_rank(graph_07_03, algo = "prpack",
                   vids = V(graph_07_03), directed = TRUE, damping = 0.85,
                   personalized = NULL, weights = E(graph_07_03)$weight)

# July 4th 2009 PageRank.
rank4 = page_rank(graph_07_04, algo = "prpack",
                   vids = V(graph_07_04), directed = TRUE, damping = 0.85,
                   personalized = NULL, weights = E(graph_07_04)$weight)

# July 5th 2009 PageRank.
rank5 = page_rank(graph_07_05, algo = "prpack",
                   vids = V(graph_07_05), directed = TRUE, damping = 0.85,
                   personalized = NULL, weights = E(graph_07_05)$weight)

# Append to the result table using cbind (exact values).
result_table_rank = cbind(Rank_day1 = row.names(data.frame(Rank_day1 = head(sort(rank1$vector, decreasing = TRUE), 10))),
                          Value_day1 = head(sort(rank1$vector, decreasing = TRUE), 10),
                          Rank_day2 = row.names(data.frame(Rank_day2 = head(sort(rank2$vector, decreasing = TRUE), 10))),
                          Value_day2 = head(sort(rank2$vector, decreasing = TRUE), 10),
                          Rank_day3 = row.names(data.frame(Rank_day3 = head(sort(rank3$vector, decreasing = TRUE), 10))),
                          Value_day3 = head(sort(rank3$vector, decreasing = TRUE), 10),
                          Rank_day4 = row.names(data.frame(Rank_day4 = head(sort(rank4$vector, decreasing = TRUE), 10))),
                          Value_day4 = head(sort(rank4$vector, decreasing = TRUE), 10),
                          Rank_day5 = row.names(data.frame(Rank_day5 = head(sort(rank5$vector, decreasing = TRUE), 10))),
                          Value_day5 = head(sort(rank5$vector, decreasing = TRUE), 10))

# Reset the index to 1-10.
row.names(result_table_rank) = 1:10

# Rename the columns with more related names.
colnames(result_table_rank) = c('Users July 1st', 'PageRank July 1st',
                                'Users July 2nd', 'PageRank July 2nd',
                                'Users July 3rd', 'PageRank July 3rd',
                                'Users July 4th', 'PageRank July 4th',
                                'Users July 5th', 'PageRank July 5th')

# Preview the results for PageRank.
result_table_rank

# Lets round the results for better visualization.
# Append to the result table using cbind and round the values to 5 decimal places.
result_table_rank_rounded = cbind(Rank_day1 = row.names(data.frame(Rank_day1 = head(sort(rank1$vector, decreasing = TRUE), 10))),
                                  Value_day1 = round(head(sort(rank1$vector, decreasing = TRUE), 10), 5),
                                  Rank_day2 = row.names(data.frame(Rank_day2 = head(sort(rank2$vector, decreasing = TRUE), 10))),
                                  Value_day2 = round(head(sort(rank2$vector, decreasing = TRUE), 10), 5),
                                  Rank_day3 = row.names(data.frame(Rank_day3 = head(sort(rank3$vector, decreasing = TRUE), 10))),
                                  Value_day3 = round(head(sort(rank3$vector, decreasing = TRUE), 10), 5),
                                  Rank_day4 = row.names(data.frame(Rank_day4 = head(sort(rank4$vector, decreasing = TRUE), 10))),
                                  Value_day4 = round(head(sort(rank4$vector, decreasing = TRUE), 10), 5),
                                  Rank_day5 = row.names(data.frame(Rank_day5 = head(sort(rank5$vector, decreasing = TRUE), 10))),
                                  Value_day5 = round(head(sort(rank5$vector, decreasing = TRUE), 10), 5))

# Reset the index to 1-10.
row.names(result_table_rank_rounded) = 1:10

# Rename the columns with more related names.
colnames(result_table_rank_rounded) = c('Users July 1st', 'PageRank July 1st',
                                        'Users July 2nd', 'PageRank July 2nd',
                                        'Users July 3rd', 'PageRank July 3rd',
                                        'Users July 4th', 'PageRank July 4th',
                                        'Users July 5th', 'PageRank July 5th')

# Preview the results for PageRank. 
result_table_rank_rounded

######################### END: Q3: Important nodes #############################

########################## START: Q4: Communities ##############################

# Convert mention graphs to undirected graphs.
undirected_graph_07_01 = as.undirected(graph_07_01)
undirected_graph_07_02 = as.undirected(graph_07_02)
undirected_graph_07_03 = as.undirected(graph_07_03)
undirected_graph_07_04 = as.undirected(graph_07_04)
undirected_graph_07_05 = as.undirected(graph_07_05)

# Fast Greedy Clustering.
fast_greedy_communities_07_01 = cluster_fast_greedy(undirected_graph_07_01)
fast_greedy_communities_07_02 = cluster_fast_greedy(undirected_graph_07_02)
fast_greedy_communities_07_03 = cluster_fast_greedy(undirected_graph_07_03)
fast_greedy_communities_07_04 = cluster_fast_greedy(undirected_graph_07_04)
fast_greedy_communities_07_05 = cluster_fast_greedy(undirected_graph_07_05)

# Infomap Clustering.
infomap_communities_07_01 = cluster_infomap(undirected_graph_07_01)
infomap_communities_07_02 = cluster_infomap(undirected_graph_07_02)
infomap_communities_07_03 = cluster_infomap(undirected_graph_07_03)
infomap_communities_07_04 = cluster_infomap(undirected_graph_07_04)
infomap_communities_07_05 = cluster_infomap(undirected_graph_07_05)

# Louvain Clustering.
louvain_communities_07_01 = cluster_louvain(undirected_graph_07_01)
louvain_communities_07_02 = cluster_louvain(undirected_graph_07_02)
louvain_communities_07_03 = cluster_louvain(undirected_graph_07_03)
louvain_communities_07_04 = cluster_louvain(undirected_graph_07_04)
louvain_communities_07_05 = cluster_louvain(undirected_graph_07_05)

# Comment on the performance of the algorithms.
# Although, the performance of each algorithm can be evaluated based on factors 
# such as modularity, community structure, and computational efficiency. 

# Fast Greedy Clustering: 
# This algorithm is known for its fast execution and ability to handle large
# graphs. It often produces good community structures but may not always 
# find the globally optimal solution.

# Infomap Clustering: 
# Infomap is based on the idea of optimizing the compression of a random 
# walker's trajectory on the graph. 
# It tends to generate high-quality community structures, capturing both 
# local and global patterns. However, it can be relatively slower compared 
# to other algorithms.

# Louvain Clustering: 
# Louvain is a popular and efficient algorithm that optimizes modularity. 
# It often produces well-defined communities with relatively fast execution.
# However, we need to notice that, it can be sensitive to the order in which 
# nodes are processed and may not always find the globally optimal solution.

# It's important to note that the performance of these algorithms can 
# vary depending on the characteristics of the graph and the specific 
# community structure within the mention graphs. 
# It's recommended to evaluate the results based on your specific dataset 
# and application requirements.

# Select a random user present in all graphs & continue with Louvain.
random_user = "tweetmeme"

# Populate community membership for the selected user in each graph.
membership(louvain_communities_07_01)[random_user]
membership(louvain_communities_07_02)[random_user]
membership(louvain_communities_07_03)[random_user]
membership(louvain_communities_07_04)[random_user]
membership(louvain_communities_07_05)[random_user]

# Analyze the evolution of communities for the user 
# and extract important topics of interest.
community_size_07_01 = sizes(louvain_communities_07_01)
community_size_07_02 = sizes(louvain_communities_07_02)
community_size_07_03 = sizes(louvain_communities_07_03)
community_size_07_04 = sizes(louvain_communities_07_04)
community_size_07_05 = sizes(louvain_communities_07_05)

# July 1st.
day_07_01_communities = membership(louvain_communities_07_01)
mid_community_07_01 = unlist(louvain_communities_07_01[community_size_07_01 > 40 & community_size_07_01 < 80])
graph_07_01_subgraph  = induced_subgraph(graph_07_01, mid_community_07_01)
V(graph_07_01_subgraph )$color = factor(day_07_01_communities[mid_community_07_01])
plot(graph_07_01_subgraph , 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_01_subgraph), 
     margin = 0, 
     vertex.size = 3,
     main = "July 1st, 2009 Twitter Communities")

# July 2nd.
day_07_02_communities = membership(louvain_communities_07_02)
mid_community_07_02 = unlist(louvain_communities_07_02[community_size_07_02 > 40 & community_size_07_02 < 90])
graph_07_02_subgraph = induced_subgraph(graph_07_02, mid_community_07_02)
V(graph_07_02_subgraph)$color = factor(day_07_02_communities[mid_community_07_02])
plot(graph_07_02_subgraph, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_02_subgraph), 
     margin = 0, 
     vertex.size = 3,
     main = "July 2nd, 2009 Twitter Communities")

# July 3rd.
day_07_03_communities = membership(louvain_communities_07_03)
mid_community_07_03 = unlist(louvain_communities_07_03[community_size_07_03 > 30 & community_size_07_03 < 100])
graph_07_03_subgraph = induced_subgraph(graph_07_03, mid_community_07_03)
V(graph_07_03_subgraph)$color = factor(day_07_03_communities[mid_community_07_03])
plot(graph_07_03_subgraph, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_03_subgraph), 
     margin = 0, 
     vertex.size = 3,
     main = "July 3rd, 2009 Twitter Communities")

# July 4th.
day_07_04_communities = membership(louvain_communities_07_04)
mid_community_07_04 = unlist(louvain_communities_07_04[community_size_07_04 > 30 & community_size_07_04 < 100])
graph_07_04_subgraph = induced_subgraph(graph_07_04, mid_community_07_04)
V(graph_07_04_subgraph)$color = factor(day_07_04_communities[mid_community_07_04])
plot(graph_07_04_subgraph, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_04_subgraph), 
     margin = 0, 
     vertex.size = 3,
     main = "July 4th, 2009 Twitter Communities")

# July 5th.
day_07_05_communities = membership(louvain_communities_07_05)
mid_community_07_05 = unlist(louvain_communities_07_05[community_size_07_05 > 40 & community_size_07_05 < 100])
graph_07_05_subgraph = induced_subgraph(graph_07_05, mid_community_07_05)
V(graph_07_05_subgraph)$color = factor(day_07_05_communities[mid_community_07_05])
plot(graph_07_05_subgraph, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_05_subgraph), 
     margin = 0, 
     vertex.size = 3,
     main = "July 5th, 2009 Twitter Communities")

######################### Fast Greedy Approach #################################

# Note: It needs some time to define the clusters!

# Populate community membership for the selected user in each graph.
membership(fast_greedy_communities_07_01)[random_user]
membership(fast_greedy_communities_07_02)[random_user]
membership(fast_greedy_communities_07_03)[random_user]
membership(fast_greedy_communities_07_04)[random_user]
membership(fast_greedy_communities_07_05)[random_user]

# Analyze the evolution of communities for the user and extract important topics of interest.
community_size_07_01_fg = sizes(fast_greedy_communities_07_01)
community_size_07_02_fg = sizes(fast_greedy_communities_07_02)
community_size_07_03_fg = sizes(fast_greedy_communities_07_03)
community_size_07_04_fg = sizes(fast_greedy_communities_07_04)
community_size_07_05_fg = sizes(fast_greedy_communities_07_05)

# July 1st.
day_07_01_communities_fg = membership(fast_greedy_communities_07_01)
mid_community_07_01_fg = unlist(fast_greedy_communities_07_01[community_size_07_01_fg > 55 & community_size_07_01_fg < 70])
graph_07_01_subgraph_fg = induced_subgraph(graph_07_01, mid_community_07_01_fg)
V(graph_07_01_subgraph_fg)$color = factor(day_07_01_communities_fg[mid_community_07_01_fg])
plot(graph_07_01_subgraph_fg, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_01_subgraph_fg), 
     margin = 0, 
     vertex.size = 3,
     main = "July 1st, 2009 Twitter Communities (Fast Greedy)")

# July 2nd.
day_07_02_communities_fg = membership(fast_greedy_communities_07_02)
mid_community_07_02_fg = unlist(fast_greedy_communities_07_02[community_size_07_02_fg > 55 & community_size_07_02_fg < 70])
graph_07_02_subgraph_fg = induced_subgraph(graph_07_02, mid_community_07_02_fg)
V(graph_07_02_subgraph_fg)$color = factor(day_07_02_communities_fg[mid_community_07_02_fg])
plot(graph_07_02_subgraph_fg, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_02_subgraph_fg), 
     margin = 0, 
     vertex.size = 3,
     main = "July 2nd, 2009 Twitter Communities (Fast Greedy)")

# July 3rd.
day_07_03_communities_fg = membership(fast_greedy_communities_07_03)
mid_community_07_03_fg = unlist(fast_greedy_communities_07_03[community_size_07_03_fg > 60 & community_size_07_03_fg < 80])
graph_07_03_subgraph_fg = induced_subgraph(graph_07_03, mid_community_07_03_fg)
V(graph_07_03_subgraph_fg)$color = factor(day_07_03_communities_fg[mid_community_07_03_fg])
plot(graph_07_03_subgraph_fg, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_03_subgraph_fg), 
     margin = 0, 
     vertex.size = 3,
     main = "July 3rd, 2009 Twitter Communities (Fast Greedy)")

# July 4th.
day_07_04_communities_fg = membership(fast_greedy_communities_07_04)
mid_community_07_04_fg = unlist(fast_greedy_communities_07_04[community_size_07_04_fg > 55 & community_size_07_04_fg < 80])
graph_07_04_subgraph_fg = induced_subgraph(graph_07_04, mid_community_07_04_fg)
V(graph_07_04_subgraph_fg)$color = factor(day_07_04_communities_fg[mid_community_07_04_fg])
plot(graph_07_04_subgraph_fg, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_04_subgraph_fg), 
     margin = 0, 
     vertex.size = 3,
     main = "July 4th, 2009 Twitter Communities (Fast Greedy)")

# July 5th.
day_07_05_communities_fg = membership(fast_greedy_communities_07_05)
mid_community_07_05_fg = unlist(fast_greedy_communities_07_05[community_size_07_05_fg > 60 & community_size_07_05_fg < 100])
graph_07_05_subgraph_fg = induced_subgraph(graph_07_05, mid_community_07_05_fg)
V(graph_07_05_subgraph_fg)$color = factor(day_07_05_communities_fg[mid_community_07_05_fg])
plot(graph_07_05_subgraph_fg, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_05_subgraph_fg), 
     margin = 0, 
     vertex.size = 3,
     main = "July 5th, 2009 Twitter Communities (Fast Greedy)")

########################### Infomap Approach ###################################

# Note: It needs EVEN MORE time to define the clusters!

# Populate community membership for the selected user in each graph.
membership(infomap_communities_07_01)[random_user]
membership(infomap_communities_07_02)[random_user]
membership(infomap_communities_07_03)[random_user]
membership(infomap_communities_07_04)[random_user]
membership(infomap_communities_07_05)[random_user]

# Analyze the evolution of communities for the user and extract important topics of interest.
community_size_07_01_im = sizes(infomap_communities_07_01)
community_size_07_02_im = sizes(infomap_communities_07_02)
community_size_07_03_im = sizes(infomap_communities_07_03)
community_size_07_04_im = sizes(infomap_communities_07_04)
community_size_07_05_im = sizes(infomap_communities_07_05)

# July 1st.
day_07_01_communities_im = membership(infomap_communities_07_01)
mid_community_07_01_im = unlist(infomap_communities_07_01[community_size_07_01_im > 55 & community_size_07_01_im < 80])
graph_07_01_subgraph_im = induced_subgraph(graph_07_01, mid_community_07_01_im)
V(graph_07_01_subgraph_im)$color = factor(day_07_01_communities_im[mid_community_07_01_im])
plot(graph_07_01_subgraph_im, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_01_subgraph_im), 
     margin = 0, 
     vertex.size = 3,
     main = "July 1st, 2009 Twitter Communities (Infomap)")

# July 2nd.
day_07_02_communities_im = membership(infomap_communities_07_02)
mid_community_07_02_im = unlist(infomap_communities_07_02[community_size_07_02_im > 40 & community_size_07_02_im < 90])
graph_07_02_subgraph_im = induced_subgraph(graph_07_02, mid_community_07_02_im)
V(graph_07_02_subgraph_im)$color = factor(day_07_02_communities_im[mid_community_07_02_im])
plot(graph_07_02_subgraph_im, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_02_subgraph_im), 
     margin = 0, 
     vertex.size = 3,
     main = "July 2nd, 2009 Twitter Communities (Infomap)")

# July 3rd.
day_07_03_communities_im = membership(infomap_communities_07_03)
mid_community_07_03_im = unlist(infomap_communities_07_03[community_size_07_03_im > 35 & community_size_07_03_im < 100])
graph_07_03_subgraph_im = induced_subgraph(graph_07_03, mid_community_07_03_im)
V(graph_07_03_subgraph_im)$color = factor(day_07_03_communities_im[mid_community_07_03_im])
plot(graph_07_03_subgraph_im, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_03_subgraph_im), 
     margin = 0, 
     vertex.size = 3,
     main = "July 3rd, 2009 Twitter Communities (Infomap)")

# July 4th.
day_07_04_communities_im = membership(infomap_communities_07_04)
mid_community_07_04_im = unlist(infomap_communities_07_04[community_size_07_04_im > 40 & community_size_07_04_im < 100])
graph_07_04_subgraph_im = induced_subgraph(graph_07_04, mid_community_07_04_im)
V(graph_07_04_subgraph_im)$color = factor(day_07_04_communities_im[mid_community_07_04_im])
plot(graph_07_04_subgraph_im, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_04_subgraph_im), 
     margin = 0, 
     vertex.size = 3,
     main = "July 4th, 2009 Twitter Communities (Infomap)")

# July 5th.
day_07_05_communities_im = membership(infomap_communities_07_05)
mid_community_07_05_im = unlist(infomap_communities_07_05[community_size_07_05_im > 35 & community_size_07_05_im < 100])
graph_07_05_subgraph_im = induced_subgraph(graph_07_05, mid_community_07_05_im)
V(graph_07_05_subgraph_im)$color = factor(day_07_05_communities_im[mid_community_07_05_im])
plot(graph_07_05_subgraph_im, 
     vertex.label = NA, 
     edge.arrow.width = 0.8, 
     edge.arrow.size = 0.2, 
     layout = layout_with_fr(graph_07_05_subgraph_im), 
     margin = 0, 
     vertex.size = 3,
     main = "July 5th, 2009 Twitter Communities (Infomap)")

##################### Extract Topic of Interest & Comparing ####################

# Find the index of the community of the random user.
random_user_community_index1 = membership(louvain_communities_07_01)[random_user]
random_user_community_index2 = membership(louvain_communities_07_02)[random_user]
random_user_community_index3 = membership(louvain_communities_07_03)[random_user]
random_user_community_index4 = membership(louvain_communities_07_04)[random_user]
random_user_community_index5 = membership(louvain_communities_07_05)[random_user]

# Check the size of the community the random user is part for each day.
random_user_community_size1 = length(louvain_communities_07_01[random_user_community_index1])
random_user_community_size2 = length(louvain_communities_07_02[random_user_community_index2])
random_user_community_size3 = length(louvain_communities_07_03[random_user_community_index3])
random_user_community_size4 = length(louvain_communities_07_04[random_user_community_index4])
random_user_community_size5 = length(louvain_communities_07_05[random_user_community_index5])

# Define communities.
community_members_07_01 = membership(louvain_communities_07_01)
community_members_07_02 = membership(louvain_communities_07_02)
community_members_07_03 = membership(louvain_communities_07_03)
community_members_07_04 = membership(louvain_communities_07_04)
community_members_07_05 = membership(louvain_communities_07_05)

# Form the members.
random_user_community_members_07_01 = community_members_07_01[community_members_07_01 == random_user_community_index1]
random_user_community_members_07_02 = community_members_07_02[community_members_07_02 == random_user_community_index2]
random_user_community_members_07_03 = community_members_07_03[community_members_07_03 == random_user_community_index3]
random_user_community_members_07_04 = community_members_07_04[community_members_07_04 == random_user_community_index4]
random_user_community_members_07_05 = community_members_07_05[community_members_07_05 == random_user_community_index5]

# Count the number of members in the community.
num_members_07_01 = length(random_user_community_members_07_01)
num_members_07_02 = length(random_user_community_members_07_02)
num_members_07_03 = length(random_user_community_members_07_03)
num_members_07_04 = length(random_user_community_members_07_04)
num_members_07_05 = length(random_user_community_members_07_05)

# Create a table with the result.
members_table = data.frame(num_members_07_01, num_members_07_02, 
                           num_members_07_03, num_members_07_04,
                           num_members_07_05)

colnames(members_table) = c("July 1st", 
                            "July 2nd", 
                            "July 3rd",
                            "July 4th",
                            "July 5th")

rownames(members_table) = "Number of members in random user's community"

# Display the resulting table.
print(members_table)

# Same users between 2 consecutive days.
same_users_01_02 = sum(names(random_user_community_members_07_01) %in% names(random_user_community_members_07_02)) 
same_users_02_03 = sum(names(random_user_community_members_07_02) %in% names(random_user_community_members_07_03)) 
same_users_03_04 = sum(names(random_user_community_members_07_03) %in% names(random_user_community_members_07_04)) 
same_users_04_05 = sum(names(random_user_community_members_07_04) %in% names(random_user_community_members_07_05)) 

# Create a table with the result.
same_users_table = data.frame(same_users_01_02, same_users_02_03, 
                              same_users_03_04, same_users_04_05)

colnames(same_users_table) = c("Same Users between July 1st and July 2nd", 
                               "Same Users between July 2nd and July 3rd",
                               "Same Users between July 3rd and July 4th",
                               "Same Users between July 4th and July 5th")

rownames(same_users_table) = "Number of same users"

# Display the resulting table.
print(same_users_table)

############################### Hashtags #######################################

# July 1st.
community_members_07_01 = membership(louvain_communities_07_01)
community_nodes_07_01 = V(graph_07_01)$name[community_members_07_01 == random_user_community_index1]
community_topics_07_01 = V(graph_07_01)$topic_of_interest[community_members_07_01 == random_user_community_index1]

# Count hashtags.
hashtags_counts_07_01 = table(community_topics_07_01)

# Sort hashtags in descending order.
sorted_topics_07_01 = sort(hashtags_counts_07_01, decreasing = TRUE)

# Exclude the "Null/NA" entry from sorted_topics_07_01 since 
# it will be first all days diferently.
sorted_topics_07_01 = sorted_topics_07_01[names(sorted_topics_07_01) != "Null/NA"]
sorted_topics_07_01 = sorted_topics_07_01[names(sorted_topics_07_01) != "#Null/NA"]

# Top-10 hashtags for the user's community.
most_common_topics_07_01 = names(sorted_topics_07_01)[1:10]

# Create a table with the most common topics and "July 1st" column.
random_user_top10_hashtags_table = data.frame("July 1st" = most_common_topics_07_01)

# Replace dot with a space in the column name
colnames(random_user_top10_hashtags_table) = "July 1st"

# Display the resulting table.
print(random_user_top10_hashtags_table)

# July 2nd.
community_members_07_02 = membership(louvain_communities_07_02)
community_nodes_07_02 = V(graph_07_02)$name[community_members_07_02 == random_user_community_index2]
community_topics_07_02 = V(graph_07_02)$topic_of_interest[community_members_07_02 == random_user_community_index2]

# Count hashtags.
hashtags_counts_07_02 = table(community_topics_07_02)

# Sort hashtags in descending order.
sorted_topics_07_02 = sort(hashtags_counts_07_02, decreasing = TRUE)

# Exclude the "Null/NA" entry from sorted_topics_07_02 since 
# it will be first all days differently.
sorted_topics_07_02 = sorted_topics_07_02[names(sorted_topics_07_02) != "Null/NA"]
sorted_topics_07_02 = sorted_topics_07_02[names(sorted_topics_07_02) != "#Null/NA"]

# Top-10 hashtags for the user's community.
most_common_topics_07_02 = names(sorted_topics_07_02)[1:10]

# Create a table with the most common topics and "July 2nd" column.
random_user_top10_hashtags_table$`July 2nd` = most_common_topics_07_02

# Display the resulting table.
print(random_user_top10_hashtags_table)

# July 3rd.
community_members_07_03 = membership(louvain_communities_07_03)
community_nodes_07_03 = V(graph_07_03)$name[community_members_07_03 == random_user_community_index3]
community_topics_07_03 = V(graph_07_03)$topic_of_interest[community_members_07_03 == random_user_community_index3]

# Count hashtags.
hashtags_counts_07_03 = table(community_topics_07_03)

# Sort hashtags in descending order.
sorted_topics_07_03 = sort(hashtags_counts_07_03, decreasing = TRUE)

# Exclude the "Null/NA" and "#Null/NA" entries from sorted_topics_07_03.
sorted_topics_07_03 = sorted_topics_07_03[names(sorted_topics_07_03) != "Null/NA"]
sorted_topics_07_03 = sorted_topics_07_03[names(sorted_topics_07_03) != "#Null/NA"]

# Top-10 hashtags for the user's community.
most_common_topics_07_03 = names(sorted_topics_07_03)[1:10]

# Add "July 3rd" column to the existing table.
random_user_top10_hashtags_table$`July 3rd` = most_common_topics_07_03

# Display the resulting table.
print(random_user_top10_hashtags_table)

# July 4th.
community_members_07_04 = membership(louvain_communities_07_04)
community_nodes_07_04 = V(graph_07_04)$name[community_members_07_04 == random_user_community_index4]
community_topics_07_04 = V(graph_07_04)$topic_of_interest[community_members_07_04 == random_user_community_index4]

# Count hashtags.
hashtags_counts_07_04 = table(community_topics_07_04)

# Sort hashtags in descending order.
sorted_topics_07_04 = sort(hashtags_counts_07_04, decreasing = TRUE)

# Exclude the "Null/NA" and "#Null/NA" entries from sorted_topics_07_04.
sorted_topics_07_04 = sorted_topics_07_04[names(sorted_topics_07_04) != "Null/NA"]
sorted_topics_07_04 = sorted_topics_07_04[names(sorted_topics_07_04) != "#Null/NA"]

# Top-10 hashtags for the user's community.
most_common_topics_07_04 = names(sorted_topics_07_04)[1:10]

# Add "July 4th" column to the existing table.
random_user_top10_hashtags_table$July_4th = most_common_topics_07_04

# Display the resulting table.
print(random_user_top10_hashtags_table)

# July 5th.
community_members_07_05 = membership(louvain_communities_07_05)
community_nodes_07_05 = V(graph_07_05)$name[community_members_07_05 == random_user_community_index5]
community_topics_07_05 = V(graph_07_05)$topic_of_interest[community_members_07_05 == random_user_community_index5]

# Count hashtags.
hashtags_counts_07_05 = table(community_topics_07_05)

# Sort hashtags in descending order.
sorted_topics_07_05 = sort(hashtags_counts_07_05, decreasing = TRUE)

# Exclude the "Null/NA" entry from sorted_topics_07_05 since it will be first all days differently.
sorted_topics_07_05 = sorted_topics_07_05[names(sorted_topics_07_05) != "Null/NA"]
sorted_topics_07_05 = sorted_topics_07_05[names(sorted_topics_07_05) != "#Null/NA"]

# Top-10 hashtags for the user's community.
most_common_topics_07_05 = names(sorted_topics_07_05)[1:10]

# Create a table with the most common topics and "July 5th" column.
random_user_top10_hashtags_table$`July 5th` = most_common_topics_07_05

# Display the resulting table.
print(random_user_top10_hashtags_table)

########################### END: Q4: Communities ###############################