from __future__ import division
import csv
import re
import sys

train_data = list()
# \b is anchor for word boundary
# \w is word character
# ? match zero or once, question mark makes the preceding token in the regular expression optional, 
# + match once or more
# * match zero or more
token_pattern = re.compile(r"(?u)\b\w\w+\b")
with open('train.csv', 'rb') as csvfile:
    reader = csv.DictReader(csvfile, delimiter=',', quotechar='"')
    for row in reader:
        query = set(x.lower() for x in token_pattern.findall(row['query']))
        title = set(x.lower() for x in token_pattern.findall(row["product_title"]))
        description = set(x.lower() for x in token_pattern.findall(row["product_description"]))
        query_tokens_in_title = 0.0
        query_tokens_in_description = 0.0
        if len(title) > 0:
            query_tokens_in_title = len(query.intersection(title))/len(title)
        if len(description) > 0:
            query_tokens_in_description = len(query.intersection(description))/len(description)
        train_data.append([query_tokens_in_title, query_tokens_in_description, float(row['median_relevance'])])


print train_data[:5]