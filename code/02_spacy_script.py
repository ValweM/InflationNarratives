# spacy packages
import spacy
import pandas as pd

# read in data
data = pd.read_csv("D:/2023_MediaReports/data/datatable/dj_news.csv")

# load language model
nlp = spacy.load('en_core_web_sm')

# tokenize the text
def spacy_preprocessing(string): # input a string
  return [token.lemma_ for token in nlp(string) if ((not token.is_stop) and (not token.is_punct))] # output a list of tokens

data['text'] = [spacy_preprocessing(text) for text in data['text']]

data["text"] = data["text"].apply(lambda row: " " .join([w.lemma_ for w in nlp(row)]))

data.to_csv('D:/2023_MediaReports/data/datatable/dj_news_lemma.csv', index = False)

data['text'].head()


