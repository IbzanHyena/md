NB. HTML Manipulation

NB. Construct an HTML element with specified name and contents.
NB. x: the name as a string
NB. y: the contents as a string
NB. returns: the element as a string
htmlElement=: {{
NB. Remove any LF from the tag.
name=. x -. LF
NB. Save any LF for later.
eol=. x -. name
;('<',name,'>'),L:0 y,L:0 '</',name,'>',eol
}}

NB. Construct an HTML table from data.
NB. x: the column headers in a boxed array of strings
NB. y: the row data
NB. returns: the table as a string
htmlTable=: {{
rows=. 'td' <@:htmlElement"1 ".&.> y
'table' htmlElement ('tr',LF) <@:htmlElement ('th' htmlElement x);rows
}}

NB. Construct an HTML list from rows.
NB. x: the type of list
NB. y: the rows
NB. returns: the list as a string
htmlList=: {{
items=. ('li',LF) htmlElement y
x htmlElement items
}}

htmlOlist=: 'ol'&htmlList
htmlUlist=: 'ul'&htmlList

htmlPara=: ('p',LF)&htmlElement
htmlBr=: ,&'<br>'

NB. Markdown Parsing

NB. Some utils

NB. For each element, check if y starts with a member of it.
NB. Return the index of the first match.
whichX=: {{ 1 i.~ > ({. @ E.&y)&.> x }}
NB. Is the match less than the length?
NB. No matches => i. returns the length
isX=: ([: # [) > whichX

NB. Construct an array of boxed strings given by
NB. start, x # rep, end, where
NB. 'start rep end'=. y
reps=. {{
'start rep end'=. y
(,. x) <@,~&start@,&end@# rep
}}

NB. These are from the J wiki
NB. https://code.jsoftware.com/wiki/Essays/Non-Overlapping_Substrings
NB. https://code.jsoftware.com/wiki/Phrases/Strings#nossplit
nos=: i.@#@] e. #@[ ({ ~^:a:&0@(,&_1)@(] I. +) { _1 ,~ ]) I.@E.
nossplit=: #@[ }.&.> [ (nos <;.1 ]) ,

NB. The valid start-of-line characters for a Markdown header.
NB. i.e. '# ', '## ', etc.
headers=: (>: i. 6) reps '';'#';' '
NB. The HTML tag names for different headers.
NB. i.e. h1LF, h2,LF, etc.
htmlHs=: >((,&LF)@('h'&,)@":)&.> >: i.6
whichHeader=: headers&whichX
isHeader=: headers&isX

NB. Convert a line of Markdown text into an HTML header.
processHeader=: {{
wh=. whichHeader y
NB. Remove the header characters from the start of the line.
text=. (wh + 2) }. y
tag=. wh { htmlHs
tag htmlElement text
}}

inlineFormatting=: {{
NB. Apply the following formatting delimeters in turn:
NB. - *** strong + emphasised
NB. - **  strong
NB. - ~~  deleted
NB. - __  underlined
NB. - *   emphasised
NB. - _   emphasised
NB. - `   code
NB. - %%% template

strong=: 'strong'&htmlElement
em=: 'em'&htmlElement
both=: strong@em
code=: 'code'&htmlElement
runTemplate=: code @ ": @ ".
apply=. {{ ;(]&.>)`(u&.>)"0 (x nossplit y) }}

y=. '***' both apply y
y=. '**' strong apply y
y=. '~~' 'del'&htmlElement apply y
y=. '__' 'u'&htmlElement apply y
y=. '*' em apply y
y=. '_' em apply y
y=. '`' code apply y
y=. '%%%' runTemplate apply y
;y
}}

processPara=: htmlPara@inlineFormatting

NB. Allow dashes and asterisks
ulists=: ((i. 4) reps '';' ';'- ') , ((i. 4) reps '';' ';'* ')
whichUlist=: ulists&whichX
isUlist=: ulists&isX

NB. Similar to processHeader
processList=: {{
wl=. whichUlist y
separator=. wl {:: ulists
sections=. a: -.~ separator nossplit y
htmlUlist (processSection&.> sections)
}}

NB. So far, only supports paragraphs, headers, and lists.
regime=: isHeader + 2 * isUlist
processSection=: processPara`processHeader`processList @. regime

NB. Start by prepending the delimiter.
NB. Use <;.1 to split into boxed sections everywhere E. is 1
NB. Then use #@[ }.&.> to remove the delimiter from each box.
strsplit=: #@[ }.&.> [ (E. <;.1 ]) ,

NB. Use strsplit to break apart input text into paragraphs (LF LF) and
NB. line breaks (double space LF).
chunks=: [: ('  ',LF)&strsplit&.> (LF,LF) strsplit ]
NB. Fill in linebreaks between each inner box
addBreaks=: >@((>@[,'<br>',LF,>@])/)&.>

markdown=: {{ LF joinstring processSection&.> a:-.~ addBreaks chunks y }}
