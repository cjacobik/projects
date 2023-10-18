from PyPDF2 import PdfReader, PdfWriter
import os
#os.chdir()

#read pdf fronts and backs
reader = PdfReader('fronts.pdf', strict=False)
front=[i for i in reader.pages]
readerb = PdfReader('backs.pdf', strict=False)
back=[i for i in readerb.pages]

#write merged PDF file
writer = PdfWriter()
for i in range(11):
    writer.add_page(front[i])
    writer.add_page(back[i])

with open('merged.pdf', "wb") as fp:
    writer.write(fp)