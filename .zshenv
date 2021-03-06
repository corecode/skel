typeset -U path
path=(~/bin ~/.local/bin $path)

#: ${LC_CTYPE:=en_DK.UTF-8}
#export LC_CTYPE

if which ruby >/dev/null && which gem >/dev/null; then
        PATH="$(ruby -rrubygems -e 'puts Gem.user_dir')/bin:$PATH"
fi

export RUBYLIB=$HOME/lib/ruby

if [[ -r ~/.zshenv.local ]]; then
	source ~/.zshenv.local
fi

if [[ -n "" && $TERM == eterm-color && -z "$TERMCAP" && -z "$TERMINFO" ]]; then
        # set TERMCAP
        export TERMCAP="eterm-color:li#36:co#103:cl=\E[H\E[J:cd=\E[J:bs:am:xn:cm=\E[%i%d;%dH:nd=\E[C:up=\E[A:ce=\E[K:ho=\E[H:pt:al=\E[L:dl=\E[M:DL=\E[%dM:AL=\E[%dL:cs=\E[%i%d;%dr:sf=^J:dc=\E[P:DC=\E[%dP:IC=\E[%d@:im=\E[4h:ei=\E[4l:mi::so=\E[7m:se=\E[m:us=\E[4m:ue=\E[m:md=\E[1m:mr=\E[7m:me=\E[m:UP=\E[%dA:DO=\E[%dB:LE=\E[%dD:RI=\E[%dC:kl=\EOD:kd=\EOB:kr=\EOC:ku=\EOA:kN=\E[6~:kP=\E[5~:@7=\E[4~:kh=\E[1~:mk=\E[8m:cb=\E[1K:op=\E[39;49m:Co#8:pa#64:AB=\E[4%dm:AF=\E[3%dm:cr=^M:bl=^G:do=^J:le=^H:ta=^I:se=\E[27m:ue=\E24m:kb=^?:kD=^[[3~:sc=\E7:rc=\E8:r1=\Ec:"

        # re-set TERM to force zsh to reload the info
        TERM=eterm-color
fi

[[ -f $HOME/.rvm/environments/default ]] && source $HOME/.rvm/environments/default

export GOPATH=$HOME/go
