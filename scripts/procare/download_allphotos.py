import requests
import json
from datetime import date, timedelta
import datetime
import time
import os

os.chdir('CHANGEME')
BEARER = 'Bearer CHANGEME'
kidid='CHANGEME'

START_DATE = date(2022, 12, 18)
END_DATE = date(2022, 12, 24)

URL = "https://api-school.kinderlime.com/api/web/parent/daily_activities/?kid_id=%s&page=1&filters[daily_activity][date_to]={}"%kidid

def getMeta(datetime_from):
    payload={}
    headers = {
    'Authorization': BEARER
    }
    req_url = URL.format(datetime_from, datetime_from)
    response = requests.request("GET", req_url, headers=headers, data=payload)
    return json.loads(response.text)

def saveMedia(dt, metadata):
    strdt=str(dt)
    for i in metadata['daily_activities']:
        if i['activity_date']==strdt:
            activitytime=i.get('activity_time').replace(':','').replace('-','').replace('T','')
            activitytime=activitytime[0:14]
            if i.get('photo_url'):
                filename=strdt+'/'+activitytime+'.jpg'
                if not os.path.exists(strdt):
                    os.makedirs(strdt)
                    print('Created Directory:',strdt)
                if os.path.exists(filename):
                    filename=strdt+'/'+activitytime+'_'+str(datetime.datetime.now().strftime("%s"))+'.jpg'
                print(filename)
                response=requests.request("GET",i['photo_url'])
                with open(filename,'wb') as f:
                    f.write(response.content)
                time.sleep(1)

def main():
    dates=[START_DATE+datetime.timedelta(days=x) for x in range(0,(END_DATE-START_DATE).days+1)]
    for dt in dates:
        print(dt.strftime('%Y-%m-%d'))
        dailyMeta = getMeta(dt)
        saveMedia(dt, dailyMeta)

main()