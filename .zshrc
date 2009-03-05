PATH=~/bin${PATH:+:}$PATH

SAVEHIST=5000
HISTSIZE=8000
HISTFILE=~/.zshhistory
setopt SHARE_HISTORY
setopt EXTENDED_HISTORY
setopt HIST_EXPIRE_DUPS_FIRST
#setopt HIST_FIND_NO_DUPS
setopt HIST_REDUCE_BLANKS
setopt PUSHD_IGNORE_DUPS

autoload -U colors; colors

typeset -A title
title[start]="\e]0;"
title[end]="\a"

REPORTTIME=10

if [[ $TERM == (xterm|screen)* && $oldterm != $TERM$WINDOWID ]]; then
	SHLVL=1
	export oldterm=$TERM$WINDOWID
fi

#PS1="%(?..%{${fg_bold[red]}%}%?%{$reset_color%} )%(2L.%{${fg_bold[yellow]}%}<%L>%{$reset_color%} .)%B%(#.%{${bg[red]}%}.)%m %(#..%{$fg[green]%})%#%{$reset_color%}%b "
#RPS1=" "
PS1="%{${fg_bold[blue]}%}%(t.DING!.%*)%{$reset_color%} %(2L.%{${fg_bold[yellow]}%}<%L>%{$reset_color%} .)%{${fg[magenta]}%}!%!$reset_color %l %(#.%{${bg[red]}%}.)%B%m%b:%{${fg_bold[green]}%}%~%b
%(?..%{${fg_bold[red]}%}%?%{$reset_color%} )%(#..%{$fg[green]%})%#%{$reset_color%}%b "

if [[ $TERM == (xterm|screen)* ]]; then
	function precmd {
		print -Pn "${title[start]}%n@%m:%~${title[end]}"
	}

	function preexec {
		emulate -L zsh
		local -a cmd; cmd=(${(z)1})
		local -a checkjobs

		case $cmd[1] in
		fg|wait)
			if (( $#cmd == 1 ))
			then
				checkjobs=%+
			else
				checkjobs=$cmd[2]
			fi
			;;
		%*)
			checkjobs=$cmd[1]
			;;
		esac

		print -n "${title[start]}"

		if [[ -n "$checkjobs" ]]
		then
			# from: http://www.zsh.org/mla/workers/2000/msg03990.html
			local -A jt; jt=(${(kv)jobtexts})	# Copy jobtexts for subshell
			builtin jobs -l $checkjobs >>(read num rest
				cmd=(${(z)${(e):-\$jt$num}})
				print -nr "$cmd")
		else
			print -nr "$cmd"
		fi

		print -Pn " | %* | "
		print -Pn "%n@%m:%~"
		print -n "${title[end]}"
	}
fi

if which todo >/dev/null 2>&1; then
	function chpwd {
		# only print stuff in the interactive case
		[[ ! -o interactive ]] && return

		todo
	}
fi

if [[ -r ~/.aliasrc ]]; then
	source ~/.aliasrc
fi

umask 22

if which vim >/dev/null 2>&1; then
	export EDITOR=`which vim`
fi
export PAGER=less
export BLOCKSIZE=K
lesspipe=$(which lesspipe.sh 2>/dev/null) || \
lesspipe=$(which lesspipe 2>/dev/null)
if test -n "$lesspipe"; then
	export LESSOPEN="|$lesspipe %s"
fi

if which keychain >/dev/null 2>&1; then
	keychain -q id_dsa --nogui
	source ~/.keychain/$HOST-sh
fi

bindkey -e
bindkey '\e[H' beginning-of-line
bindkey '\e[F' end-of-line
bindkey '\eOH' beginning-of-line
bindkey '\eOF' end-of-line
bindkey '\e[1~' beginning-of-line
bindkey '\e[4~' end-of-line
autoload -U down-line-or-beginning-search
autoload -U up-line-or-beginning-search
zle -N down-line-or-beginning-search
zle -N up-line-or-beginning-search
bindkey '\e[B' down-line-or-beginning-search
bindkey '\e[A' up-line-or-beginning-search
autoload run-help
bindkey '\eOP' run-help
bindkey '\e[M' run-help
bindkey '\e[1;5D' backward-word
bindkey '\e[1;5C' forward-word
bindkey '\e[3~' delete-char
#WORDCHARS=${WORDCHARS//[\/&.;=]}
autoload -U select-word-style
select-word-style bash
zstyle ':zle:transpose-words' word-style shell
setopt NO_FLOW_CONTROL

autoload -U compinit; compinit

if [[ -r ~/.zshrc.local ]]; then
	source ~/.zshrc.local
fi

cd .
