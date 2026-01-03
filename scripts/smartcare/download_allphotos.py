import requests
import json
from datetime import date, timedelta
import datetime
import time
import os
import mimetypes

os.chdir('')
BEARER = 'Bearer CHANGEME'

START_DATE = date(2024, 7, 9)
END_DATE = date(2025, 7, 31)
START_DATE = date(2025, 7, 31)
END_DATE = date(2025, 11, 26)


def getAttachments(datetime_from,datetime_to):
    url='https://ng-core-api.smart.care/CHANGEME'
    headers = {'Authorization': BEARER}
    payload={"childId":"","startDate":f"{datetime_from}T04:00:00Z","endDate":f"{datetime_to}T03:59:00Z","fileCategoryFilter":0,"pageSize":50,"lastEntityId":""}
    response=requests.post(url,headers=headers,json=payload)
    d=response.json()
    returnh={}
    fnh={}
    for i in d:
        guid=i.get('guid','')
        dt=i.get('dateCreated',datetime_from+'T00:00:00')
        fn=dt[0:10]+'_'+dt[11:19]
        fn=fn.replace('-','').replace(':','')
        if fn not in fnh.keys():
            fnh[fn]=0
        fnh[fn]+=1
        fn+=f'_{fnh[fn]}'
        mimetype=i.get('mimeType','image/png')
        returnh[guid]=(fn,mimetype)
    return returnh

def download_blob(attach):
    for guid,t in attach.items():
        headers = {'Authorization': BEARER}
        output_filename,mime_type=t
        output_filename+= mimetypes.guess_extension(mime_type) or ".png"
        # Stream download to file
        with requests.get(f'https://media.smart.care/api/v2/blobs/{guid}/size/6',stream=True,headers=headers) as r:
            #r.raise_for_status()
            with open(output_filename, "wb") as f:
                for chunk in r.iter_content(chunk_size=8192):
                    if chunk:
                        f.write(chunk)
        time.sleep(1)


def main():
    dates=[START_DATE+datetime.timedelta(days=x) for x in range(0,(END_DATE-START_DATE).days+1)]
    for dt in dates:
        datetime_to=(dt+datetime.timedelta(days=1)).strftime('%Y-%m-%d')
        datetime_from=dt.strftime('%Y-%m-%d')
        print(f'{datetime_from}')
        attachments=getAttachments(datetime_from,datetime_to)
        print(f'number of files: {len(attachments)}')
        download_blob(attachments)

main()