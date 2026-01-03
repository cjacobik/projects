import os,datetime
dir=''
os.chdir(dir)


files = [f for f in os.listdir(dir) if os.path.isfile(os.path.join(dir, f))]

dh={}
for f in files:
    fsuffs=f.strip().split('.')
    fsuff=fsuffs[1]
    ep=os.stat(f).st_mtime
    fp=datetime.datetime.fromtimestamp(ep).strftime('%Y%m%d_%H%M%S')
    i=1
    fp2=fp
    foundit=True
    while foundit:
        if dh.get(fp2) is None:
            fn=fp2
            foundit=False
            dh[fp2]=1
        else: 
            fp2=fp+'_'+str(i)
            i+=1
    os.system('cp '+dir+'/'+f+' '+dir+'_m/'+fn+'.'+fsuff)
