#!/bin/sh

sys="$(uname)"

# Ensure we source system /etc/profile, if it exists
[ -f /etc/profile ] && . /etc/profile

case "$sys" in
    Darwin);;
    *)
        # Enable wifi if possible
        [ -e "$HOME"/.local/bin/wctl ] && sh "$HOME"/.local/bin/wctl enable
        ;;
esac

# source my environment shell script
[ -f "$HOME"/.config/shell/env.sh ]        && . "$HOME"/.config/shell/env.sh

# Ensure my ssh identities get added to my environment.
if [ -e "$HOME/.ssh/" ]; then
    # Always kill the current ssh agent if it is running.
    # This is particularly useful on macOS.
    if command -v pgrep >/dev/null && command -v pkill >/dev/null; then
        if pgrep ssh-agent >/dev/null; then
            pkill ssh-agent >/dev/null
        fi
    fi
    if command -v ssh-agent > /dev/null 2>&1; then
        eval "$(ssh-agent -s)"
        find "$HOME/.ssh" -name '*.pub' \
            | sed "s/.pub$//" \
            | xargs -I{} ssh-add {}
    fi
fi

case "$sys" in
    Darwin);;
    *)
        # Start up X11 if we are on tty1
        xcmd="$(command -v sx || command -v startx)"
        [ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ] && "$xcmd"
        ;;
esac
