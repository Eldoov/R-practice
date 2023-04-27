import pandas as pd


pd.option_context('display.height', 500, 'display.max_rows', 500)
df = pd.read_json('/Users/zuowen/Downloads/AllCards.json')
df2 = pd.read_json('/Users/zuowen/Downloads/AllPrices.json')

def start():
    print(df.head())
    print(df2.head())

start()