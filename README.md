## Twitter Mention Graph Analysis: A Five-Day Case_Study

This repository contains a comprehensive analysis of a Twitter mention dataset from July 2009. The goal of this case study is to analyze the mention relationships between users, identify the most important topics for each user based on hashtags, and perform various analyses on the graph representation of the data.

### Dataset
The [dataset](https://drive.google.com/file/d/1RjWUg-6KrVOjJPZHHQg-h_9gSSWZUPn-/view) used in this case study consists of tweets from July 2009. Each tweet includes information about the time of posting, user handles, and the text of the tweets. The data has been manipulated and organized into five CSV files, each representing the weighted directed mention graph for a specific day. These CSV files contain information about the users involved in the mentions, the frequency of mentions between users, and the most important topic (hashtag) for each user.

### Analysis Steps
The analysis conducted in this case study follows a systematic approach. For a more detailed approach see the provided documentation:

* Data Manipulation: The raw data is processed using Python to create the five CSV files representing the mention graph for each day through Python.
 
* Graph Creation: R is used to create igraph graphs based on the CSV files. The graph vertices are updated to include the topic attribute for each user, enabling further analysis and visualization.

* Metric Evolution: The evolution of different metrics over the five-day period is examined. Plots are created to visualize changes in the number of vertices, number of edges, graph diameter, average in-degree, and average out-degree. Significant fluctuations in these metrics are identified and discussed.

![Barplot Analyzing the 5 Days Trend Regarding the Number of Edges](https://github.com/dmatsanganis/Twitter_Mention_Graph_Analysis_A_Five_Day_Case_Study/assets/34712449/ba39b414-f2ea-4672-ab9b-625cb9b806c9)


![Trendline Diagram for Number of Edges over the 5 Days](https://github.com/dmatsanganis/Twitter_Mention_Graph_Analysis_A_Five_Day_Case_Study/assets/34712449/fa2e929f-5ce7-46e5-a87a-ccceb6425df1)


![All Metrics Table](https://github.com/dmatsanganis/Twitter_Mention_Graph_Analysis_A_Five_Day_Case_Study/assets/34712449/39c99eb7-2308-4ecd-90d8-f4fa4339db64)

* Top User Analysis: Data frames are generated for each day, highlighting the top-10 Twitter users based on in-degree, out-degree, and PageRank. Variations in the top-10 lists are observed, indicating changes in user influence and popularity.

<img width="1018" alt="In-Degree Table" src="https://github.com/dmatsanganis/Twitter_Mention_Graph_Analysis_A_Five_Day_Case_Study/assets/34712449/d56f4b26-556c-41f4-8250-d61facf0e453">


<img width="1034" alt="Out-Degree Table" src="https://github.com/dmatsanganis/Twitter_Mention_Graph_Analysis_A_Five_Day_Case_Study/assets/34712449/479a1a08-7a51-4a1d-b5b2-9459e4722492">


<img width="1079" alt="PageRank Rounded Table" src="https://github.com/dmatsanganis/Twitter_Mention_Graph_Analysis_A_Five_Day_Case_Study/assets/34712449/79147cad-ed8a-4407-95a7-eafab0858ba2">

* Community Detection: Community detection algorithms, including fast greedy clustering, infomap clustering, and Louvain clustering, are applied to the undirected versions of the mention graphs. The performance of these algorithms is evaluated, and insights are provided on their effectiveness.

![Louvain Clustering July 5](https://github.com/dmatsanganis/Twitter_Mention_Graph_Analysis_A_Five_Day_Case_Study/assets/34712449/6a1ff209-7c71-4d05-a812-500f6a72880b)


![Fast Greedy Clustering July 2](https://github.com/dmatsanganis/Twitter_Mention_Graph_Analysis_A_Five_Day_Case_Study/assets/34712449/0c066b6e-6bc8-4aa6-9bd8-93fd1a886e9b)

* Community Evolution: A specific user present in all five graphs is chosen, and their community evolution is analyzed. Similarities in the communities the user belongs to are identified, along with the most important topics of interest. Shared topics among communities are explored, and a visualization of the graph is created, using different colors to represent each community. Nodes belonging to very small or large communities are filtered out to improve clarity and aesthetics.

![random_user_tweetmeme_communities_top10_hashtags](https://github.com/dmatsanganis/Twitter_Mention_Graph_Analysis_A_Five_Day_Case_Study/assets/34712449/57863aca-de00-463d-b21d-59db977a1d7e)




![R](https://img.shields.io/badge/r-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Python](https://img.shields.io/badge/python-3670A0?style=for-the-badge&logo=python&logoColor=ffdd54)
