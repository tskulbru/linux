alias ls='ls --color=auto'
alias ..='cd ..'
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'
alias pac="yaourt -S"
alias pacs="yaourt -Ss" 
alias pacup="yaourt -Syu --aur"
alias xp='xprop | grep "WM_WINDOW_ROLE\|WM_CLASS" && echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\""'
alias xp2='echo "WM_CLASS(STRING) = \"NAME\", \"CLASS\"" && xprop | grep "WM_WINDOW_ROLE\|WM_CLASS"'

export EDITOR="vim"

reminder(){
	delay=$1
	shift
	(sleep $delay ; zenity --info --text="$*") > /dev/null &
}

extract () {
    if [ -f $1 ] ; then
        case $1 in
            *.tar.bz2)  tar xjf $1      ;;
            *.tar.gz)   tar xzf $1      ;;
            *.bz2)      bunzip2 $1      ;;
            *.rar)      unrar x $1      ;;
            *.gz)       gunzip $1       ;;
            *.tar)      tar xf $1       ;;
            *.tbz2)     tar xjf $1      ;;
            *.tgz)      tar xzf $1      ;;
            *.zip)      unzip $1        ;;
            *.ZIP)      unzip $1        ;;
            *.Z)        uncompress $1   ;;
            *)          echo "'$1' cannot be extracted via extract()" ;;
        esac
    else
        echo "'$1' is not a valid file"
    fi
}

genpasswd() {
    local l=$1
        [ "$l" == "" ] && l=20
        tr -dc A-Za-z0-9_- < /dev/urandom | head -c ${l} | xargs
}

#prompt
#PS1='\w #  '
PS1="┌─[\[\e[36;1m\]\u @ \[\e[32;1m\]\H\[\033[1;37m\]] \n\[\033[1;37m\]└─[\[\033[0;36m\]\w\[\033[1;37m\]]> \[\e[0m\]"

#statuslinje tekst
case "$TERM" in
xterm*|rxvt*)
    PROMPT_COMMAND='echo -ne "\033]0;[${USER}] ${PWD/$HOME/~}\007"'
    ;;
*)
    ;;
esac

if [ -f /etc/bash_completion ]; then
	. /etc/bash_completion
fi
complete -cf sudo
