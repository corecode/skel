" vim: tw=0 ts=4 sw=4
" Vim color file

set background=dark
hi clear
if exists("syntax_on")
  syntax reset
endif
let g:colors_name = "2c"

hi Normal														guifg=#dddddd guibg=black
hi Comment		term=bold		ctermfg=DarkCyan				guifg=#80a0ff
hi Constant		term=underline	ctermfg=Magenta					guifg=Magenta
hi Special		term=bold		ctermfg=LightRed				guifg=LightRed
hi Identifier	term=underline	cterm=bold ctermfg=Cyan			guifg=#40ffff
hi Statement	term=bold		ctermfg=white					guifg=white
hi Label		term=underline	ctermfg=red						guifg=red
hi PreProc		term=underline	ctermfg=LightBlue				guifg=lightblue
hi Type			term=underline	ctermfg=LightGreen				guifg=#60ff60 gui=bold
hi Function		term=bold		ctermfg=lightcyan				guifg=lightcyan
hi Repeat		term=underline	ctermfg=White					guifg=white gui=bold
hi Operator						ctermfg=yellow					guifg=yellow gui=bold
hi Ignore						ctermfg=black					guifg=bg
hi Error		term=reverse	ctermbg=Red ctermfg=White		guibg=Red guifg=White
hi Todo			term=standout	ctermbg=Yellow ctermfg=Black	guifg=Blue guibg=Yellow
hi Visual		term=reverse	cterm=reverse					guibg=bg guifg=fg

hi link String			Constant
hi link Character		Constant
hi link Number			Constant
hi link Boolean			Constant
hi link Float			Number
hi link Conditional		Repeat
hi link Label			Statement
hi link Keyword			Statement
hi link Exception		Statement
hi link Include			PreProc
hi link Define			PreProc
hi link Macro			PreProc
hi link PreCondit		PreProc
hi link StorageClass	Type
hi link Structure		Type
hi link Typedef			Type
hi link Tag				Special
hi link SpecialChar		Special
hi link Delimiter		Special
hi link SpecialComment	Special
hi link Debug			Special
