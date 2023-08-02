## Twitter Mention Graph Analysis: A Five-Day Case Study

This repository contains a comprehensive analysis of a Twitter mention dataset from July 2009. The goal of this case study is to analyze the mention relationships between users, identify the most important topics for each user based on hashtags, and perform various analyses on the graph representation of the data.

### Dataset
The [dataset](https://drive.google.com/file/d/1RjWUg-6KrVOjJPZHHQg-h_9gSSWZUPn-/view) used in this case study consists of tweets from July 2009. Each tweet includes information about the time of posting, user handles, and the text of the tweets. The data has been manipulated and organized into five CSV files, each representing the weighted directed mention graph for a specific day. These CSV files contain information about the users involved in the mentions, the frequency of mentions between users, and the most important topic (hashtag) for each user.

### Analysis Steps
The analysis conducted in this case study follows a systematic approach. For a more detailed approach see the provided documentation:

* Data Manipulation: The raw data is processed using Python to create the five CSV files representing the mention graph for each day through Python.
 
* Graph Creation: R is used to create igraph graphs based on the CSV files. The graph vertices are updated to include the topic attribute for each user, enabling further analysis and visualization.

* Metric Evolution: The evolution of different metrics over the five-day period is examined. Plots are created to visualize changes in the number of vertices, number of edges, graph diameter, average in-degree, and average out-degree. Significant fluctuations in these metrics are identified and discussed.

* Top User Analysis: Data frames are generated for each day, highlighting the top-10 Twitter users based on in-degree, out-degree, and PageRank. Variations in the top-10 lists are observed, indicating changes in user influence and popularity.

* Community Detection: Community detection algorithms, including fast greedy clustering, infomap clustering, and Louvain clustering, are applied to the undirected versions of the mention graphs. The performance of these algorithms is evaluated, and insights are provided on their effectiveness.

* Community Evolution: A specific user present in all five graphs is chosen, and their community evolution is analyzed. Similarities in the communities the user belongs to are identified, along with the most important topics of interest. Shared topics among communities are explored, and a visualization of the graph is created, using different colors to represent each community. Nodes belonging to very small or large communities are filtered out to improve clarity and aesthetics.

### Contributors

- [x] [Dimitris Matsanganis](https://github.com/dmatsanganis)

![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Python](https://img.shields.io/badge/python-3670A0?style=for-the-badge&logo=python&logoColor=ffdd54)
