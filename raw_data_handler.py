##            Social Network Analysis             ##
##                 Project II                     ##
##       Dimitris Matsanganis FT f2822212         ##
####################################################

import datetime
import re
from collections import Counter


# Extracts mentions from a tweet using regex.
def parse_tweet(tweet):
    return re.findall(r'@(\w+)', tweet)


# Extracts hashtags from a tweet using regex.
def extract_hashtags(tweet):
    hashtags = re.findall(r'#(\w+)', tweet)
    if len(hashtags) == 0:
        return ['Null/NA']
    else:
        return hashtags


# Reads a tweet from a file and extracts relevant information.
def read_tweet(f):
    f.readline()
    time_line = f.readline()  # Read the line containing timestamp.
    user_line = f.readline()  # Read the line containing username.
    tweet_line = f.readline()  # Read the line containing tweet content.

    # Parse the data.
    timestamp = datetime.datetime.strptime(time_line.split('\t')[1].strip(), '%Y-%m-%d %H:%M:%S')
    username = user_line.split('\t')[1].strip().split('/')[-1]
    tweet = tweet_line.split('\t')[1].strip()

    # Get mentions and hashtags through the above created functions.
    mentions = parse_tweet(tweet)
    hashtags = extract_hashtags(tweet)

    return timestamp, username, mentions, hashtags


# Initialize dictionaries.
aggregate_data_mentions = dict()
aggregate_data_hashtags = dict()

with open('tweets2009-07.txt', 'r', encoding='utf8') as f:
    i = 0
    while True:
        i += 1
        if i % 100000 == 0:
            print('Processed {} records'.format(i))

        try:
            timestamp, username, mentions, hashtags = read_tweet(f)
        except:
            break

        if timestamp < datetime.datetime(2009, 7, 1, 0, 0, 0) or timestamp > datetime.datetime(2009, 7, 5, 23, 59, 59):
            continue

        if timestamp.date() not in aggregate_data_mentions.keys():
            aggregate_data_mentions[timestamp.date()] = dict()
            aggregate_data_hashtags[timestamp.date()] = dict()

        for mention in mentions:
            aggregate_data_mentions[timestamp.date()][(username, mention)] = aggregate_data_mentions[timestamp.date()].get(
                (username, mention), 0) + 1

        for hashtag in hashtags:
            aggregate_data_hashtags[timestamp.date()][(username, hashtag)] = aggregate_data_hashtags[timestamp.date()].get(
                (username, hashtag), 0) + 1

# Output the data separated for each day.
for timestamp, data in aggregate_data_mentions.items():
    with open(timestamp.strftime('%Y.%m.%d') + '_mentions.csv', 'w', encoding='utf8') as f:
        f.write('from, to, weight\n')
        for pair, weight in data.items():
            f.write('{},{},{}\n'.format(pair[0], pair[1], weight))

# Output the data separated for each day.
for timestamp, data in aggregate_data_hashtags.items():

    # Find the most important topic for each user.
    user_topics = {}
    for user, topic in data.keys():
        if user not in user_topics:
            user_topics[user] = []
        user_topics[user].append(topic)

    # Find the most frequent hashtag among all per user and day and also add the '#' to make it more case study
    # friendly.
    most_frequent_topics = {user: '#' + Counter(topics).most_common(1)[0][0] for user, topics in user_topics.items()}

    # Output the user-topic pairs to a CSV file.
    user_topic_filename = timestamp.strftime('%Y.%m.%d') + '_hashtags.csv'
    with open(user_topic_filename, 'w', encoding='utf8') as f:
        f.write('user, topic_of_interest\n')
        for user, topic in most_frequent_topics.items():
            f.write('{},{}\n'.format(user, topic))

# The initial data origin from the provided link and is in a compressed format 
# (tweets2009-07.txt.gz).
# You can find this file here:
# https://drive.google.com/file/d/1RjWUg-6KrVOjJPZHHQg-h_9gSSWZUPn-/view
# The compressed file contains the `tweets2009-07.txt` file. 
# The above file was the input and through the current
# output the following 10 CSV files (5 for the mentions 
# and 5 for the hashtags, one for each day).