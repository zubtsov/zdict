import requests
import time
from os import listdir
from os.path import isfile, join
import pathlib
import datetime

output_dir = "wictionary/raw"
dict_file = 'C:\\Users\\zubtsov\\Downloads\\zalizniak.txt'

pathlib.Path(output_dir).mkdir(parents=True, exist_ok=True)

dictionary_words = set([line.strip().split(" ")[0] for line in open(dict_file) if line.strip()])
crawled_words = set([f.replace('.html', '') for f in listdir(output_dir) if isfile(join(output_dir, f))])
words_to_be_crawled = dictionary_words - crawled_words

number_of_words_to_crawl = len(words_to_be_crawled)
start_time_secs = time.perf_counter()
current_number_of_crawled_words = 0
for word in words_to_be_crawled:
    output_file_name = output_dir + '\\' + word + '.html'

    if pathlib.Path(output_file_name).exists():
        print("Word '" + word + "' already exists")
        continue

    page = requests.get('https://ru.wiktionary.org/wiki/' + word)
    content = page.text
    w = open(output_file_name, 'w', encoding="utf-8")
    w.write(content)
    w.close()
    print('Proccessed word: ' + word)
    current_number_of_crawled_words += 1
    remaining_time = (time.perf_counter() - start_time_secs) / current_number_of_crawled_words * number_of_words_to_crawl
    print('Estimated remaining time: '+ str(datetime.timedelta(seconds=int(remaining_time))))
    print('Words remaining: {0:6d}'.format(number_of_words_to_crawl - current_number_of_crawled_words))
