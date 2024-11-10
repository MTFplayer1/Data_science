from selenium import webdriver
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions
from selenium.webdriver.common.by import By

summonersname = input('Enter your summoners name ')
players = []
player = []
winloss = []
wl = []
allyw = 0
allyl = 0
enemiew = 0
enemiel = 0
driver = webdriver.Chrome()
driver.get(f'https://www.leagueofgraphs.com/summoner/eune/{summonersname}')

posts_loaded = expected_conditions.presence_of_element_located((By.TAG_NAME,'table'))
WebDriverWait(driver,20).until(posts_loaded)

for post in driver.find_elements(By.CLASS_NAME,'summonerColumn'):
    players.append(post.text)
for i in range(len(players)):
    player.append(players[i].split('\n'))
for i in range(20):
    ally = False
    for j in range(5):
        if player[i][j].lower() == summonersname.lower():
            ally = True
        driver.get(f'https://www.leagueofgraphs.com/summoner/eune/{player[i][j]}')
        posts_loaded = expected_conditions.presence_of_element_located((By.TAG_NAME, 'table'))
        WebDriverWait(driver, 15).until(posts_loaded)
        for post in driver.find_elements(By.CLASS_NAME, 'winslosses'):
            winloss.append(post.text)
        if ally == True and j==4:
            for k in range(5):
                wl = winloss[-1-k].split()
                win = int(wl[1])
                allyw += win
                loss = int(wl[3])
                allyl += loss
        if ally == False and j==4:
            for l in range(5):
                wl = winloss[-1-l].split()
                win = int(wl[1])
                enemiew += win
                loss = int(wl[3])
                enemiel += loss
print('my teams:',allyw,allyl,'enemie teams:',enemiew,enemiel)