import zipfile
import requests
import numpy as np
import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

r = requests.get('http://chrisdone.com/ontime.csv.zip', stream=True)
with open("flights.csv", 'wb') as f:
    for chunk in r.iter_content(chunk_size=1024):
        if chunk:
            f.write(chunk)

zf = zipfile.ZipFile("flights.csv.zip")
filename = zf.filelist[0].filename
fp = zf.extract(filename)
df = pd.read_csv(fp, parse_dates="FL_DATE").rename(columns=str.lower)

df.ix[10:14, ['fl_date', 'tail_num']]
