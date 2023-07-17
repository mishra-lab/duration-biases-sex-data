import os,re
docs = ['intro','methods','meth.yss','meth.partners','results','discuss']
igs = [
  '(?<!\\\\)%.*',
  '\\\\(par|item|floatfoot|clearpage)',
  '\\\\(input|label)\{.*?\}',
  '\\\\(begin|end)\{(enumerate|itemize|figure|table)\}.*',
  '.*\\\\includegraphics.*',
  '\\\\centerline\{',
  re.compile('\\\\begin\{subequations\}(.*?)\\\\end\{subequations\}',re.DOTALL),
]
reps = [
  ('~', ' '),
  ('``','"'),
  ('\'\'','"'),
  ('\\$(.*?)\\$', '\\1'),
  ('\\\\(case|emph|text.*?)\{(.*?)\}', '\\2'),
  ('\\\\(?:sub)*section\{(.*?)\}', '\n\\1\n'),
  ('\\\\paragraph\{(.*?)\}', '\n\\1\n'),
  ('\\\\.*?ref\{.*?\}', 'REF'),
  ('\\\\cite\{.*?\}', '[CITE]'),
]

body = ''

for doc in docs:
  with open(doc+'.tex','r') as f:
    body += '\n\n'+doc.upper()+'\n\n'+f.read()

for k,v in reps:
  body = re.sub(k,v,body)

for i in igs:
  body = re.sub(i,'',body)

for n in range(9):
  body = body.replace('\n\n\n','\n\n')

with open('wc/body.tmp','w') as f:
  f.write(body)

os.system('wc -w wc/body.tmp | cut -d " " -f1 && rm wc/body.tmp')
os.system('wc -w abstract.tex | cut -d " " -f1')

