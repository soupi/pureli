" Vim syntax file
" modified from a Racket syntax file (http://github.com/wlangstroth/vim-racket.git) and adjusted for pureli
" Language:     Pureli 0.0.1

" Initializing:
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case ignore

" Highlight unmatched parens
syn match pureliError ,[]})],

if version < 600
  set iskeyword=33,35-39,42-58,60-90,94,95,97-122,126,_
else
  setlocal iskeyword=33,35-39,42-58,60-90,94,95,97-122,126,_
endif

" Forms in order of appearance at
" http://docs.pureli-lang.org/reference/index.html
"
syn keyword pureliSyntax module require quote mquote eval
syn keyword pureliSyntax error try trace
syn keyword pureliSyntax #%datum #%expression #%top #%variable-reference #%app
syn keyword pureliSyntax lambda let let* letrec
syn keyword pureliSyntax if cond and or
syn keyword pureliSyntax define defmacro
syn keyword pureliSyntax #%top-interaction

" 12.5 Writing
syn keyword pureliSyntax print! print-file! read! read-file! do! let! pure display!

" lambda sign
syn match pureliSyntax /\<[\u03bb]\>/

" Functions
syn keyword pureliFunc boolean? not
syn keyword pureliFunc number? real? integer?
syn keyword pureliFunc zero? positive? negative?
syn keyword pureliFunc empty? nil? list? string? procedure?

" 3.2.2 General Arithmetic

" 3.2.2.1 Arithmetic
syn keyword pureliFunc + - * / quotient remainder quotient/remainder modulo
syn keyword pureliFunc add1 sub1 abs max min gcd lcm round floor ceiling
syn keyword pureliFunc truncate numerator denominator rationalize

" 3.2.2.2 Number Comparison
syn keyword pureliFunc = < <= > >= <>

" 3.2.2.3 Powers and Roots
syn keyword pureliFunc sqrt integer-sqrt integer-sqrt/remainder
syn keyword pureliFunc expt exp log

" 3.2.2.8 Number-String Conversions
syn keyword pureliFunc show

" 3.9 Pairs and Lists
syn keyword pureliFunc cons car cdr nil
syn keyword pureliFunc list? list length
syn keyword pureliFunc ++ reverse map
syn keyword pureliFunc for-each foldl foldr filter
syn keyword pureliFunc sort member
syn keyword pureliFunc caar cadr cdar cddr caaar caadr cadar caddr cdaar
syn keyword pureliFunc cddar cdddr caaaar caaadr caadar caaddr cadadr caddar
syn keyword pureliFunc cadddr cdaaar cdaadr cdadar cddaar cdddar cddddr

" 3.9.7 Additional List Functions and Synonyms
" (require pureli/list)
syn keyword pureliFunc last take drop split-at slice
syn keyword pureliFunc take-right drop-right split-at-right add-between
syn keyword pureliFunc flatten remove-duplicates filter-map
syn keyword pureliFunc count partition append-map filter-not shuffle

" 3.17 Procedures
syn keyword pureliFunc procedure? apply compose compose1
syn keyword pureliFunc procedure-arity
syn keyword pureliFunc identity const thunk thunk* negate curry curryr


syn match pureliDelimiter !\<\.\>!

syn match pureliSymbol    ,\k+,  contained

syn cluster pureliNormal  contains=pureliSyntax,pureliFunc,pureliDelimiter
syn cluster pureliQuotedStuff  contains=pureliSymbol
syn cluster pureliQuotedOrNormal  contains=pureliDelimiter

syn match pureliConstant  ,\<\*\k\+\*\>,
syn match pureliConstant  ,\<<\k\+>\>,

syn region pureliQuotedStruc start="("rs=s+1 end=")"re=e-1     contains=@pureliQuotedStuff,@pureliQuotedOrNormal contained
syn region pureliQuotedStruc start="#("rs=s+2 end=")"re=e-1    contains=@pureliQuotedStuff,@pureliQuotedOrNormal contained
syn region pureliQuotedStruc start="{"rs=s+1 end="}"re=e-1   contains=@pureliQuotedStuff,@pureliQuotedOrNormal contained
syn region pureliQuotedStruc start="#{"rs=s+2 end="}"re=e-1  contains=@pureliQuotedStuff,@pureliQuotedOrNormal contained
syn region pureliQuotedStruc start="\["rs=s+1 end="\]"re=e-1   contains=@pureliQuotedStuff,@pureliQuotedOrNormal contained
syn region pureliQuotedStruc start="#\["rs=s+2 end="\]"re=e-1  contains=@pureliQuotedStuff,@pureliQuotedOrNormal contained

syn cluster pureliQuotedStuff  add=pureliQuotedStruc

" Non-quoted lists, and strings
syn region pureliStruc matchgroup=Delimiter start="("rs=s+1 matchgroup=Delimiter end=")"re=e-1 contains=@pureliNormal
syn region pureliStruc matchgroup=Delimiter start="#("rs=s+2 matchgroup=Delimiter end=")"re=e-1 contains=@pureliNormal
syn region pureliStruc matchgroup=Delimiter start="{"rs=s+1 matchgroup=Delimiter end="}"re=e-1 contains=@pureliNormal
syn region pureliStruc matchgroup=Delimiter start="#{"rs=s+2 matchgroup=Delimiter end="}"re=e-1 contains=@pureliNormal
syn region pureliStruc matchgroup=Delimiter start="\["rs=s+1 matchgroup=Delimiter end="\]"re=e-1 contains=@pureliNormal
syn region pureliStruc matchgroup=Delimiter start="#\["rs=s+2 matchgroup=Delimiter end="\]"re=e-1 contains=@pureliNormal

" Simple literals
syn region pureliString start=/\%(\\\)\@<!"/ skip=/\\[\\"]/ end=/"/
syn region pureliString start=/#<<\z(.*\)$/ end=/^\z1$/

syn cluster pureliNormal  add=pureliError,pureliConstant,pureliStruc,pureliString
syn cluster pureliQuotedOrNormal  add=pureliString

" Numbers

" anything which doesn't match the below rules, but starts with a #d, #b, #o,
" #x, #i, or #e, is an error
syn match pureliNumberError         "\<#[xdobie]\k*"

syn match pureliContainedNumberError   "\<#o\k*[^-+0-7delfinas#./@]"
syn match pureliContainedNumberError   "\<#b\k*[^-+01delfinas#./@]"
syn match pureliContainedNumberError   "\<#[ei]#[ei]"
syn match pureliContainedNumberError   "\<#[xdob]#[xdob]"

" start with the simpler sorts
syn match pureliNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?\>" contains=pureliContainedNumberError
syn match pureliNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\d\+/\d\+\>" contains=pureliContainedNumberError
syn match pureliNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\d\+/\d\+[-+]\d\+\(/\d\+\)\?i\>" contains=pureliContainedNumberError

" different possible ways of expressing complex values
syn match pureliNumber    "\<\(#[dobie]\)\{0,2}[-+]\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?i\>" contains=pureliContainedNumberError
syn match pureliNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?[-+]\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?i\>" contains=pureliContainedNumberError
syn match pureliNumber    "\<\(#[dobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f][-+]\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?i\>" contains=pureliContainedNumberError
syn match pureliNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?[-+]\(inf\|nan\)\.[0f]i\>" contains=pureliContainedNumberError
syn match pureliNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?@[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?\>" contains=pureliContainedNumberError
syn match pureliNumber    "\<\(#[dobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f]@[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?\>" contains=pureliContainedNumberError
syn match pureliNumber    "\<\(#[dobie]\)\{0,2}[-+]\?\(\d\+\|\d\+#*\.\|\d*\.\d\+\)#*\(/\d\+#*\)\?\([sdlef][-+]\?\d\+#*\)\?@[-+]\(inf\|nan\)\.[0f]\>" contains=pureliContainedNumberError

" hex versions of the above (separate because of the different possible exponent markers)
syn match pureliNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?\>"
syn match pureliNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\x\+/\x\+\>"
syn match pureliNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\x\+/\x\+[-+]\x\+\(/\x\+\)\?i\>"

syn match pureliNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?i\>"
syn match pureliNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?[-+]\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?i\>"
syn match pureliNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\(inf\|nan\)\.[0f][-+]\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?i\>"
syn match pureliNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?[-+]\(inf\|nan\)\.[0f]i\>"
syn match pureliNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?@[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?\>"
syn match pureliNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\(inf\|nan\)\.[0f]@[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?\>"
syn match pureliNumber    "\<\(#x\|#[ei]#x\|#x#[ei]\)[-+]\?\(\x\+\|\x\+#*\.\|\x*\.\x\+\)#*\(/\x\+#*\)\?\([sl][-+]\?\x\+#*\)\?@[-+]\(inf\|nan\)\.[0f]\>"

" these work for any radix
syn match pureliNumber    "\<\(#[xdobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f]i\?\>" contains=pureliContainedNumberError
syn match pureliNumber    "\<\(#[xdobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f][-+]\(inf\|nan\)\.[0f]i\>" contains=pureliContainedNumberError
syn match pureliNumber    "\<\(#[xdobie]\)\{0,2}[-+]\(inf\|nan\)\.[0f]@[-+]\(inf\|nan\)\.[0f]\>" contains=pureliContainedNumberError

syn keyword pureliBoolean  #t #f #true #false #T #F

syn match pureliError   "\<#\\\k*\>"

syn match pureliChar    "\<#\\.\w\@!"
syn match pureliChar    "\<#\\space\>"
syn match pureliChar    "\<#\\newline\>"
syn match pureliChar    "\<#\\return\>"
syn match pureliChar    "\<#\\null\?\>"
syn match pureliChar    "\<#\\backspace\>"
syn match pureliChar    "\<#\\tab\>"
syn match pureliChar    "\<#\\linefeed\>"
syn match pureliChar    "\<#\\vtab\>"
syn match pureliChar    "\<#\\page\>"
syn match pureliChar    "\<#\\rubout\>"
syn match pureliChar    "\<#\\[0-7]\{1,3}\>"
syn match pureliChar    "\<#\\x[0-9a-f]\{1,2}\>"
syn match pureliChar    "\<#\\u[0-9a-f]\{1,6}\>"

syn cluster pureliNormal  add=pureliNumber,pureliBoolean,pureliChar
syn cluster pureliQuotedOrNormal  add=pureliNumber,pureliBoolean

" Command-line parsing
syn keyword pureliExtFunc command-line current-command-line-arguments once-any help-labels multi once-each

syn match pureliSyntax    "#lang "
syn match pureliExtSyntax "#:\k\+"

syn cluster pureliNormal  add=pureliExtFunc,pureliExtSyntax

" syntax quoting, unquoting and quasiquotation
syn region pureliQuoted matchgroup=Delimiter start="['`]" end=![ \t()\[\]";]!me=e-1 contains=@pureliQuotedStuff,@pureliQuotedOrNormal
syn region pureliQuoted matchgroup=Delimiter start="['`](" matchgroup=Delimiter end=")" contains=@pureliQuotedStuff,@pureliQuotedOrNormal
syn region pureliQuoted matchgroup=Delimiter start="['`]\?#(" matchgroup=Delimiter end=")" contains=@pureliQuotedStuff,@pureliQuotedOrNormal

syn region pureliUnquote matchgroup=Delimiter start="\<#,"rs=s+2 end=![ \t\[\]()";]!re=e-1,me=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<#,@"rs=s+3 end=![ \t\[\]()";]!re=e-1,me=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<#,("rs=s+3 end=")"re=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<#,@("rs=s+4 end=")"re=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<#,\["rs=s+3 end="\]"re=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<#,@\["rs=s+4 end="\]"re=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<,"rs=s+1 end=![ \t\[\]()";]!re=e-1,me=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<,@"rs=s+2 end=![ \t\[\]()";]!re=e-1,me=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<,("rs=s+2 end=")"re=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<,@("rs=s+3 end=")"re=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<,#("rs=s+3 end=")"re=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<,@#("rs=s+4 end=")"re=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<,\["rs=s+2 end="\]"re=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<,@\["rs=s+3 end="\]"re=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<,#\["rs=s+3 end="\]"re=e-1 contained contains=@pureliNormal
syn region pureliUnquote matchgroup=Delimiter start="\<,@#\["rs=s+4 end="\]"re=e-1 contained contains=@pureliNormal

syn cluster pureliQuotedStuff add=pureliUnquote

syn region pureliQuoted matchgroup=Delimiter start="#['`]"rs=s+2 end=![ \t()\[\]";]!re=e-1,me=e-1 contains=@pureliQuotedStuff,@pureliQuotedOrNormal
syn region pureliQuoted matchgroup=Delimiter start="#['`]("rs=s+3 matchgroup=Delimiter end=")"re=e-1 contains=@pureliQuotedStuff,@pureliQuotedOrNormal

" Comments
syn match pureliComment /;.*$/ contains=@Spell
syn region pureliMultilineComment start=/#|/ end=/|#/ contains=pureliMultilineComment,@Spell

syn cluster pureliNormal  add=pureliQuoted,pureliComment,pureliMultilineComment
syn cluster pureliQuotedOrNormal  add=pureliComment,pureliMultilineComment


" Synchronization and the wrapping up...
syn sync match matchPlace grouphere NONE "^[^ \t]"
" ... i.e. synchronize on a line that starts at the left margin

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_pureli_syntax_inits")
  if version < 508
    let did_pureli_syntax_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif

  HiLink pureliSyntax             Statement
  HiLink pureliFunc               Function

  HiLink pureliString             String
  HiLink pureliChar               Character
  HiLink pureliBoolean            Boolean

  HiLink pureliNumber             Number
  HiLink pureliNumberError        Error
  HiLink pureliContainedNumberError Error

  HiLink pureliQuoted             Structure
  HiLink pureliQuotedStruc        Structure
  HiLink pureliSymbol             Structure

  HiLink pureliDelimiter          Delimiter
  HiLink pureliConstant           Constant

  HiLink pureliComment            Comment
  HiLink pureliMultilineComment   Comment
  HiLink pureliError              Error

  HiLink pureliExtSyntax          Type
  HiLink pureliExtFunc            PreProc
  delcommand HiLink
endif

let b:current_syntax = "pureli"
