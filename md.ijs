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

NB. The valid start-of-line characters for a Markdown header.
NB. i.e. '# ', '## ', etc.
headers=: (,. >: i. 6) <@,&' '@# '#'
NB. The HTML tag names for different headers.
NB. i.e. h1LF, h2,LF, etc.
htmlHs=: >((,&LF)@('h'&,)@":)&.> >: i.6
NB. For each header, see if header E. y starts with a 1.
NB. Unbox the list and get the index of the first 1.
whichHeader=: {{ 1 i.~ > ({. @ E.&y)&.> headers }}
NB. We have a header if i. returns an in-bounds index.
isHeader=: (# headers) > whichHeader
NB. Convert a line of Markdown text into an HTML header.
processHeader=: {{
NB. Index of the header.
wh=. whichHeader y
NB. Remove the header from the start of the line.
text=. (wh + 2) }. y
NB. Retrieve the HTML tag.
tag=. wh { htmlHs
NB. Construct the element.
tag htmlElement text
}}

NB. So far, only supports headers and paragraphs.
processSection=: htmlPara`processHeader @. isHeader

NB. Start by prepending the delimiter.
NB. Use <;.1 to split into boxed sections everywhere E. is 1
NB. Then use #@[ }.&.> to remove the delimiter from each box.
strsplit=: #@[ }.&.> [ (E. <;.1 ]) ,
chunks=: [: ('  ',LF)&strsplit&.> (LF,LF) strsplit ]
addBreaks=: >@((>@[,'<br>',LF,>@])/)&.>

markdown=: {{ LF joinstring processSection&.> a:-.~ addBreaks chunks y }}
