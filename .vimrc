set nocp
set bs=2
set hlsearch
set incsearch
syn on
set ru
set ai
set ls=2
set title
set is
set sm
set bk
set backupcopy=auto,breakhardlink
filetype plugin on
filetype indent on

if has("gui")
	if has("macunix")
		set guifont=Monaco:h11
	else
		set guifont=Monospace\ 13
	endif
endif
colorscheme 2c
set bg=dark

if v:version >= 603
	set viminfo='20,<50,s10,h
else
	set viminfo='20,\"50,h
endif

if has("macunix")
	set makeprg=bsdmake
endif

if has("multi_byte")
	set enc=utf-8
endif

let CVSCommandDiffOpt='u'
let CVSCommandEdit='split'

set gfm+=%f:\ %l:\ %m

"fun s:Glimpse(...)
"	let s:oldgrep = &grepprg
"	set grepprg=glm
"	call grep a:
"	let grepprg = s:oldgrep
"endfun

com -nargs=* Glimpse let s:oldgrep = &grepprg | set grepprg=glm | grep <args> | let &grepprg = s:oldgrep

set grepprg=grep\ -nH\ $*

set cino=:0,g0,t0,+4,(0,u0,W4
let c_space_errors=1
let c_gnu=1

" Search with *#/ in visual selection mode
vnoremap * y/\V<C-R>=substitute(escape(@@,"/\\"),"\n","\\\\n","ge")<CR><CR>
vnoremap # y?\V<C-R>=substitute(escape(@@,"?\\"),"\n","\\\\n","ge")<CR><CR> 

au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif

runtime ftplugin/man.vim
nmap	K	\K