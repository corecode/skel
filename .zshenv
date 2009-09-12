typeset -U path
path=(~/bin $path)

export RUBYLIB=$HOME/lib/ruby

if [[ -r ~/.zshenv.local ]]; then
	source ~/.zshenv.local
fi
