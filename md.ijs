htmlElement=: {{
name=. x -. LF
eol=. x -. name
;('<',name,'>'),L:0 y,L:0 '</',name,'>',eol
}}

htmlTable=: {{
rows=. 'td' <@:htmlElement"1 ".&.> y
'table' htmlElement ('tr',LF) <@:htmlElement ('th' htmlElement x);rows
}}

htmlList=: {{
items=. ('li',LF) htmlElement y
x htmlElement items
}}

htmlOlist=: 'ol'&htmlList
htmlUlist=: 'ul'&htmlList

htmlPara=: ('p',LF)&htmlElement
htmlBr=: ,&'<br>'

headers=: (,. >: i. 6) <@,&' '@# '#'
htmlHs=: >((,&LF)@('h'&,)@":)&.> >: i.6
whichHeader=: {{ 1 i.~ > ({. @ E.&y)&.> headers }}
isHeader=: (# headers) > whichHeader
processHeader=: {{
wh=. whichHeader y
text=. (wh + 2) }. y
tag=. wh { htmlHs
tag htmlElement text
}}

processSection=: htmlPara`processHeader @. isHeader

strsplit=: #@[ }.&.> [ (E. <;.1 ]) ,
chunks=: [: ('  ',LF)&strsplit&.> (LF,LF) strsplit ]
addBreaks=: >@((>@[,'<br>',LF,>@])/)&.>

markdown=: {{ LF joinstring processSection&.> a:-.~ addBreaks chunks y }}
