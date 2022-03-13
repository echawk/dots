# if not running interactively, don't do anything
case "$-" in
    *i*);;
    *) return ;;
esac
[ -f "$XDG_CONFIG_HOME/shell/alias.sh" ] && . "$XDG_CONFIG_HOME/shell/alias.sh"

export PATH=/var/db/kiss/repos/kiss-personal/bin:/usr/lib/ccache/bin:$PATH
export KISS_SU=doas
export KISS_STRIP=0

export KISS_HOOK="/var/db/kiss/repos/kiss-personal/hooks/kiss-timer-hook"
export KISS_HOOK="$KISS_HOOK:/var/db/kiss/repos/kiss-tex/kiss-tex-hook"
export KISS_HOOK="$KISS_HOOK:/var/db/kiss/repos/kiss-personal/hooks/kiss-mangz-hook"
export KISS_HOOK="$KISS_HOOK:/var/db/kiss/repos/kiss-personal/hooks/kiss-window-title-hook"

export KW_HOOK="/var/db/kiss/repos/kiss-personal/hooks/kw-lsp-hook"
export KW_HOOK="$KW_HOOK:/var/db/kiss/repos/kiss-personal/hooks/kw-lang-hook"

KISS_PATH="$HOME/.cache/kiss/hold:$KISS_PATH"

# special funtion to make it obvious when you are inside of a git repo; print out the name of the branch
parse_git_branch() {
    printf "git:%s" "$(git branch 2>/dev/null | sed -e '/^[^*]/d' -e 's~* \(.*\)~(\1)~')"
}

parse_fossil_branch() {
    printf "fossil:%s" "$(fossil branch 2>/dev/null  | grep -E "^\ \*" | cut -d' ' -f3)"
}

extra_ps1_info(){
    # if in a git repo, show currently checked out branch
    git rev-parse >/dev/null 2>&1 && parse_git_branch
    #fossil status >/dev/null 2>&1 && parse_fossil_branch
}

test_microphone() {
    arecord -vvv -f dat /dev/null
}

command -v opam >/dev/null 2>&1 && eval $(opam env)

case "$0" in
    *bash)
        completion="/usr/share/bash-completion/bash_completion"
        [ -r "$completion" ] && . "$completion"

        shopt -s autocd # cd into a directory when you type the directory name
        shopt -s cdspell # try to guess what directory you meant if misspelled
        PS1="\[\033[01;32m\][\u\[\033[01;37m\] \W\[\033[01;32m\]] \[\033[01;34m\]\$(extra_ps1_info)\[\033[01;32m\]%\[\033[00m\] "
        ;;
    *)
        PS1="$USER@$(hostname) $PWD \$(extra_ps1_info)-\$ "
esac

#vi mode for text input
set -o vi
# get rid of the history file
unset HISTFILE
# https://unix.stackexchange.com/questions/72086/ctrl-s-hang-terminal-emulator
stty -ixon
