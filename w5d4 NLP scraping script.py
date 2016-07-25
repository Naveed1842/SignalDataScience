# Scraping wikipedia for machine learning subjects

import requests
import pandas as pd
from bs4 import BeautifulSoup
import csv



base_url = "https://en.wikipedia.org"
main_page = requests.get('https://en.wikipedia.org/wiki/Category:Machine_learning')
main_page_soup = BeautifulSoup(main_page.text, 'html.parser')

def link_extract(soup_object):
  link_set = []
  skip_phrases = ["https", "Portal", "ikipedia", "wikimedia", "File:", "Help:", "/wiki/Statistics", "/wiki/Computer_science", "mediawiki", "shop.", "Special:", "Main_Page", "?title=", "wikipedia.org"]
  for link in main_page_soup.find_all('a'):
    url = str(link.get('href'))
    if "wiki" in url and not any(phrase in url for phrase in skip_phrases):
      link_set.append(url)
  return link_set

topic_links = link_extract(main_page_soup)

visited_links = []
topic_text_list = []

def link_explore(topic_links, visited_links, topic_text_list):
  count = 0
  for url in topic_links:
    if url not in visited_links:
      print("downloading... ", base_url + url)
      topic_request = requests.get(base_url + url)
      topic_text = topic_request.text
      topic_page_soup = BeautifulSoup(topic_text, 'html.parser')
      sub_topic_links = link_extract(topic_page_soup)
      for link in sub_topic_links:
        if link not in topic_links:
          topic_links.append(link)
      text_bucket = []
      for paragraph in topic_page_soup.find_all('p'):
        text_bucket.append(paragraph.get_text())
      topic_text_list.append(text_bucket)
      # print(topic_text_list[count])
      visited_links.append(url)
      count += 1
  return topic_links, visited_links, topic_text_list

print("Number of original level links: ", len(topic_links))
pre_recurse_topic_links = topic_links
topic_links, visited_links, topic_text_list = link_explore(topic_links, visited_links, topic_text_list)
while pre_recurse_topic_links != topic_links:
  topic_links, visited_links, topic_text_list = link_explore(topic_links, visited_links, topic_text_list)
  
print("Total number of links explored: ", len(topic_links))

# print(topic_text_list[0])
with open("wikipedia_machinelearning_scrape.txt", "w") as txtfile:
  wr = csv.writer(txtfile, delimiter=',')
  wr.writerows(topic_text_list)


