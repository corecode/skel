typeset -U path
path=(~/bin $path)

if [[ -r ~/.zshenv.local ]]; then
	source ~/.zshenv.local
fi
