from PyPDF2 import PdfReader, PdfWriter
import os
os.chdir('')

#read pdf fronts and backs
writer = PdfWriter()
for f in ['abc_20230602_1724.pdf','abc_20240429_1245.pdf','abc_20240501_2159.pdf','abc_20240428_1326.pdf','abc_20240501_2051.pdf','abc_20240502_0742.pdf']:
    reader = PdfReader(f, strict=False)
    front=[i for i in reader.pages]
    writer.add_page(front[0])

with open('merged.pdf', "wb") as fp:
    writer.write(fp)