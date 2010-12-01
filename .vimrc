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
" writing backups is handled below in the autocommand
if v:version >= 700
	set backupcopy=auto,breakhardlink
else
	set backupcopy=auto
endif
set foldmethod=marker
set mouse=a

" long line treatment: wrap on words, use cursor keys to
" move across wrapped lines
nnoremap <C-Up> gk
vnoremap <C-Up> gk
inoremap <C-Up> <C-O>gk
nnoremap <C-Down> gj
vnoremap <C-Down> gj
inoremap <C-Down> <C-O>gj
nnoremap <C-Home> g0
vnoremap <C-Home> g0
inoremap <C-Home> <C-O>g0
nnoremap <C-End> g$
vnoremap <C-End> g$
inoremap <C-End> <C-O>g$

" split vertically when using CTRL-W_f
nmap <C-W><C-F> :vsplit<CR>gf
nmap <C-W>f :vsplit<CR>gf

nmap <Esc>[5;5~ <C-PageUp>
nmap <Esc>[6;5~ <C-PageDown>

let g:NERDLeader = ','

if has("win32")
	let &runtimepath = '~/.vim,' . &runtimepath
end

if exists('+shellslash')
	set shellslash
endif

" add all bundles
call pathogen#runtime_append_all_bundles() 
call pathogen#helptags()

filetype plugin on
filetype indent on

if has("gui")
	if has("macunix")
		set guifont=Monaco:h11
	elseif has("win32")
		set guifont=Terminus:h12
	else
		set guifont=Terminus\ 12
	endif
endif

if has("gui")
	let &guicursor = &guicursor.",a:blinkon0"
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

" let CVSCommandDiffOpt='u'
" let CVSCommandEdit='split'
" let HGCommandEnableBufferSetup=0
" let HGCommandEdit='split'
set statusline=%<%f\ %{VCSCommandGetStatusLine()}%h%m%r%=%-14.(%l,%c%V%)\ %P
let VCSCommandEnableBufferSetup=1

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
let java_highlight_functions='yes'

let g:tex_indent_items = 1
let g:tex_flavor='latex'
let g:Tex_MultipleCompileFormats = 'dvi,pdf'
let g:Tex_DefaultTargetFormat = 'pdf'

if executable('evince')
	let g:Tex_ViewRule_pdf = 'evince'
endif

" Search with *#/ in visual selection mode
vnoremap * y/\V<C-R>=substitute(escape(@@,"/\\"),"\n","\\\\n","ge")<CR><CR>
vnoremap # y?\V<C-R>=substitute(escape(@@,"?\\"),"\n","\\\\n","ge")<CR><CR> 

au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$") | exe "normal g'\"" | endif

""func SetBackupMode(bufname)
""	let rs = system('hammer version ' . shellescape(a:bufname))
""	if v:shell_error == 0
""		let rs = system('ls -lo ' . shellescape(a:bufname) . '|cut -w -f 5 | grep -v -E "no(u)?history"')
""	end
""	if v:shell_error == 0
""		set nowritebackup nobackup
""	else
""		set writebackup backup
""	endif
""endfunc
""
""command DiffOrig vert new | set bt=nofile | r # | 0d_ | diffthis
""	 	\ | wincmd p | diffthis
""
""au BufWritePre,FileAppendPre,FileWritePre * :call SetBackupMode(expand('<afile>'))

func MaybeSessionSave()
	if !empty(v:this_session)
		execute 'silent mksession! ' . v:this_session
	endif
endfunc

au BufWritePost,VimLeavePre * :call MaybeSessionSave()
