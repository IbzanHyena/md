require 'regex'

NB. Global state - environment variables
INTERNAL_DOMAIN =: ''"_^:(0&-:) getenv 'MD_INTERNAL_DOMAIN'

NB. HTML Manipulation

NB. Construct an HTML element with specified name, attributes, and contents.
NB. x: the name and attributes boxed
NB. the attributes should be an even boxed matrix of strings of alternating
NB. attributes and values.
NB. y: the contents as a string.
NB. returns: the element as a string
htmlElementA =: {{
  'x attributes' =. x
  NB. Remove any LF from the tag and save for later
  name =. x -. LF
  eol  =. x -. name

  NB. Format attributes
  att =. {{ ' ' , y , '=' }}
  val =. {{ '"' , y , '"' }}
  attributes =. ;(att&.>)`(val&.>)"0 attributes
  ;('<',name,attributes,'>'),L:0 y,L:0 '</',name,'>',eol
}}

NB. Construct an HTML element with specified name and contents.
NB. x: the name as a string
NB. y: the contents as a string
NB. returns: the element as a string
htmlElement =: ] htmlElementA~ a: ;~ [

NB. Construct an anchor (a) element with the specified href and contents.
htmlAhref =: {{
  class =. ('external-link';'internal-link') {~ (x -.@:-: '') *. {. INTERNAL_DOMAIN E. x
  y htmlElementA~ 'a' ; < 'href' ; x ; 'class' ; class
}}

NB. htmlAhref =: ] htmlElementA~ 'a' ; [: < 'href' ; [

NB. Construct an HTML table from data.
NB. x: the column headers in a boxed array of strings
NB. y: the row data
NB. returns: the table as a string
htmlTable =: {{
  rows =. 'td' <@:htmlElement"1 ".&.> y
  'table' htmlElement ('tr',LF) <@:htmlElement ('th' htmlElement x);rows
}}

NB. Construct an HTML list from rows.
NB. x: the type of list
NB. y: the rows
NB. returns: the list as a string
htmlList =: {{
  items =. ('li',LF) htmlElement y
  x htmlElement items
}}

htmlOlist =: 'ol'&htmlList
htmlUlist =: 'ul'&htmlList

htmlPara =: ('p',LF)&htmlElement
htmlBr   =: ,&'<br>'

NB. Markdown Parsing

NB. Some utils

NB. For each element, check if y starts with a member of it.
NB. Return the index of the first match.
whichX =: {{ 1 i.~ > ({. @ E.&y)&.> x }}
NB. Is the match less than the length?
NB. No matches => i. returns the length
isX =: ([: # [) > whichX

isWhitespace =: e.&(' ',CRLF,TAB)

NB. Construct an array of boxed strings given by
NB. start, x # rep, end, where
NB. 'start rep end'=. y
reps =. {{
  'start rep end'=. y
  (,. x) <@,~&start@,&end@# rep
}}

NB. These are from the J wiki
NB. https://code.jsoftware.com/wiki/Essays/Non-Overlapping_Substrings
NB. https://code.jsoftware.com/wiki/Phrases/Strings#nossplit
nos =: i.@#@] e. #@[ ({ ~^:a:&0@(,&_1)@(] I. +) { _1 ,~ ]) I.@E.
nossplit =: #@[ }.&.> [ (nos <;.1 ]) ,

NB. The valid start-of-line characters for a Markdown header.
NB. i.e. '# ', '## ', etc.
headers =: (>: i. 6) reps '';'#';' '
NB. The HTML tag names for different headers.
NB. i.e. h1LF, h2,LF, etc.
htmlHs      =: >((,&LF)@('h'&,)@":)&.> >: i.6
whichHeader =: headers&whichX
isHeader    =: headers&isX

NB. Sanitise the text in a header to use as an id
keepchars =: 'abcdefghijklmnopqrstuvwxyz01234567890-'
intersect =: e. # [
sanitise  =: intersect&keepchars @: ((' ';'-')&stringreplace) @: tolower

NB. Convert a line of Markdown text into an HTML header.
processHeader =: {{
  wh =. whichHeader y
  NB. Remove the header characters from the start of the line.
  text =. (wh + 2) }. y
  tag =. wh { htmlHs
  id =. sanitise LF -.~ tolower text
  anchor =. ('a'; <'id' ; id ; 'href' ; '#',id) htmlElementA inlineFormatting text
  (tag ; <'class';'header') htmlElementA anchor
}}

trimTrailingLF =: ] {.~ 1 i.~ [: *./\. LF = ]
applyBetweenDelimiters =: {{ ;(]&.>)`(u&.>)"0 (x nossplit y) }}
flattenChars =: {{ ([: ,/"2 ,&LF"1)^:(<: # $ y) y }}
NB. Replace J's hacked-in box drawing characters with Unicode
boxChars =: (16{a.);'┌';(17{a.);'┬';(18{a.);'┐';(19{a.);'├';(20{a.);'┼';(21{a.);'┤';(22{a.);'└';(23{a.);'┴';(24{a.);'┘';(25{a.);'│';(26{a.);'─'
replaceBoxes =: boxChars&stringreplace
runTemplate =: trimTrailingLF @: replaceBoxes @: flattenChars @: ": @: (". :: (13!:12 @: (''"_)))

backtickrx =: rxcomp '`.+?`'
dbacktickrx =: rxcomp '``.+?``'

inlineFormatting =: {{
  NB. First search for double backtick and single
  NB. backticks, and turn them into code blocks. Apply
  NB. normal inline formatting to the rest.

  strong =: 'strong'&htmlElement
  em     =: 'em'&htmlElement
  both   =: strong@:em
  code   =: 'code'&htmlElement

  NB. This is going to be a bit of a mess...
  
  prest =: {{
    apply =. applyBetweenDelimiters

    y =. '***' both             apply y
    y =. '**'  strong           apply y
    y =. '~~' 'del'&htmlElement apply y
    y =. '__' 'u'&htmlElement   apply y
    y =. '*'   em               apply y
    y =. '_'   em               apply y
    y =. '%%%' runTemplate      apply y
    ;y
  }}

  NB. Process templates inside inline code blocks
  inlineCode =. code @: ('%%%'&(runTemplate applyBetweenDelimiters))
  NB. Remove leading + trailing 2 chars and then wrap in code
  dbt =: inlineCode @: (}.^:2) @: (}:^:2) f.
  NB. Remove leading + trailing char and then wrap in code
  bt =: inlineCode @: }. @: }: f.

  pbt =: {{ ; (prest&.>)`(bt&.>)"0 (backtickrx&rxmatches rxcut ]) y }}
  pdbt =: {{ ; (pbt&.>)`(dbt&.>)"0 (dbacktickrx&rxmatches rxcut ]) y }}
  y =. pdbt y
}}

processPara =: htmlPara @ inlineFormatting

NB. Allow dashes and asterisks
ulists     =: ((i. 4) reps '';' ';'- ') , ((i. 4) reps '';' ';'* ')
whichUlist =: ulists&whichX
isUlist    =: ulists&isX

NB. Similar to processHeader
processList =: {{
  wl        =. whichUlist y
  separator =. wl {:: ulists
  sections  =. a: -.~ separator nossplit y
  htmlUlist (processSection&.> sections)
}}

countBackticks =: [: (i.&0) 2 = [: +/ [: (,: |.) '`' = ]
isCodeblock    =: 3 <: countBackticks

processCodeblock =: {{
  count =. countBackticks y
  NB. Trim the leading and trailing backticks
  y =. count }. y
  y =. }:^:count y
  NB. Search for any text after the leading backticks before whitespace
  count =. {. I. isWhitespace y
  lang  =. count {. y
  NB. trim the language
  y =. count }. y
  y =. '%%%' runTemplate applyBetweenDelimiters y
  y =. ('code';<'class';'language-',lang) htmlElementA y
  'pre' htmlElement y
}}

isAside =: {. @: ('>>> '&E.)
processAside =: ('blockquote';<'class';'aside')&htmlElementA @: processPara @: (4&}.)

NB. So far, only supports paragraphs, headers, lists, codeblocks, and asides.
regime =: isHeader + (2 * isUlist) + (3 * isCodeblock) + 4 * isAside
processSection =: processPara`processHeader`processList`processCodeblock`processAside @. regime

NB. Start by prepending the delimiter.
NB. Use <;.1 to split into boxed sections everywhere E. is 1
NB. Then use #@[ }.&.> to remove the delimiter from each box.
strsplit =: #@[ }.&.> [ (E. <;.1 ]) ,

NB. Use strsplit to break apart input text into paragraphs (LF LF) and
NB. line breaks (double space LF).
chunks =: [: ('  ',LF)&strsplit&.> LF2 strsplit ]
NB. Fill in linebreaks between each inner box
addBreaks =: >@((>@[,'<br>',LF,>@])/)&.>


refrx =: rxcomp '^\s{0,3}\[(.+?)\] {0,3}\: ?(.+)$'
NB. Extract the m groups of regex x from string y
rxextract =: {{ (] rxfrom~ m ({ "2) x rxmatches ]) y }}
isRef =: refrx&rxeq

parseRef =: [: , refrx 1 2 rxextract ]

NB. Split a string into link references and the rest of the text
NB. Preserve the order of both
NB. y: the string to split
NB. returns: the references and remaining text, as a boxed array
splitReferences =: {{
  lines =. LF strsplit y
  refLines =. ; isRef&.> lines
  refs =. lines #~ refLines
  lines =. LF joinstring lines #~ -. refLines
  refs =. _2 ]\ ; parseRef&.> refs

  refs ; lines
}}

inlineimgrx =: rxcomp '!\[(.*?)\]\((.+?)\)'
refimgrx =: rxcomp '!\[(.*?)\]\[(.+?)\]'
inlinelinkrx =: rxcomp '(?<!!)\[(.+?)\]\((.+?)\)'
reflinkrx =: rxcomp '(?<!!)\[(.+?)\]\[(.+?)\]'

NB. Replace links in text with HTML hyperlinks
processLinks =: {{
  NB. Use rxapply to apply a verb to each match within a substring
  makeInline =: {{ (>@[ htmlAhref >@])/ , inlinelinkrx 2 1 rxextract y }}
  makeRef =: {{
    'name ref' =. , reflinkrx 1 2 rxextract y
    try.
      dest =. ({:"1 x) {::~ ({."1 x) i. <ref
    catch.
      echo 'Could not find reference ' , ref
      throw.
    end.
    dest htmlAhref name
  }}
  makeImg =: {{
    'src alt' =. , inlineimgrx 2 1 rxextract y
    ('img';<'src';src;'alt';alt) htmlElementA ''
  }}

  y =. inlineimgrx makeImg rxapply y
  y =. inlinelinkrx makeInline rxapply y
  reflinkrx x&makeRef rxapply y
}}

markdown =: {{
  NB. Pre-process
  y =. toJ y
  NB. Split lines into references and normal lines
  'references lines' =. splitReferences y
  NB. Apply formatting
  lines =. processSection&.> a:-.~ addBreaks chunks lines
  NB. Fill in links
  lines =. references processLinks&.(a:`>) lines
  LF joinstring lines
}}
