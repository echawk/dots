#ALIASES
alias cp="cp -i"
alias df='df -h'
alias more=less
alias ytpladl='youtube-dl --extract-audio --audio-format mp3 -o "%(title)s.%(ext)s"' #change to function
alias rm="trash"

# git aliases
alias gplog="git log --all --graph --decorate --stat"
alias gc="git checkout"
alias gap="git add --patch"
alias grp="git reset --patch"
alias gdlc="git reset --hard HEAD^"

# swallow cmds
alias mpv="swallow mpv"
alias zathura="swallow zathura"
alias nsxiv="swallow nsxiv"

alias viewpac="pacman -Qq | fzf --preview 'pacman -Qil {}' --layout=reverse --bind 'enter:execute(pacman -Qil {} | less)'"

# irc
alias irssi="irssi --config=$HOME/.config/irssi/config --home=$HOME/.config/irssi/"
# rss
alias news="sfeed_curses $HOME/.config/sfeed/feeds/*"
#alias news_update="sfeed_update $HOME/.config/sfeed/sfeedrc"
