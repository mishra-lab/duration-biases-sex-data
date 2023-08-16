import os,re
docs = ['intro','methods','meth.yss','meth.parts','results','discuss']
igs = [
  '(?<!\\\\)%.*',
  '\\\\(par|item|clearpage)',
  '\\\\(input|label)\{.*?\}',
  '\\\\(begin|end)\{(enumerate|itemize)\}.*',
  re.compile('\\\\begin\{figure\}(.*?)\\\\end\{figure\}',re.DOTALL),
  re.compile('\\\\begin\{table\}(.*?)\\\\end\{table\}',re.DOTALL),
  re.compile('\\\\begin\{subequations\}(.*?)\\\\end\{subequations\}',re.DOTALL),
]
reps = [
  ('\\n\\%.*',''),
  ('~',' '),
  ('``','"'),
  ('\'\'','"'),
  ('\\$(.*?)\\$', '{MATH}'),
  ('\\\\(emph|text.*?)\{(.*?)\}', '\\2'),
  ('\\\\(?:sub)*section\{(.*?)\}', '\n\\1\n'),
  ('\\\\paragraph\{(.*?)\}', '\n\\1\n'),
  ('\\\\.*?ref\{.*?\}', 'REF'),
  ('\\\\cite\{.*?\}', '[CITE]'),
]

body = ''

for doc in docs:
  with open(doc+'.tex','r') as f:
    body += '\n\n'+doc.upper()+'_'*100+'\n\n'+f.read()

for k,v in reps:
  body = re.sub(k,v,body)

for i in igs:
  body = re.sub(i,'',body)

for n in range(9):
  body = body.replace('\n\n\n','\n\n')

with open('words/.tmp/body.txt','w') as f:
  f.write(body)

os.system('wc -w words/.tmp/body.txt | cut -d " " -f1')
os.system('wc -w abstract.tex | cut -d " " -f1')

