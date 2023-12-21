#!/bin/sh

# Ensure we source system /etc/profile, if it exists
[ -f /etc/profile ] && . /etc/profile

# Enable wifi if possible
[ -e "$HOME"/.local/bin/wctl ] && sh "$HOME"/.local/bin/wctl enable

# source my environment shell script
[ -f "$HOME"/.config/shell/env.sh ]        && . "$HOME"/.config/shell/env.sh
[ -f "$HOME"/.config/shell/github_env.sh ] && . "$HOME"/.config/shell/github_env.sh

# Ensure my ssh identities get added to my environment.
if [ -e "$HOME/.ssh/" ]; then
    if command -v ssh-agent > /dev/null 2>&1; then
        eval "$(ssh-agent -s)"
        find "$HOME/.ssh" -name '*.pub' | sed "s/.pub$//" | xargs -I{} ssh-add {}
    fi
fi

# Start up X11 if we are on tty1
xcmd="$(command -v sx || echo "$(command -v startx)")"
[ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ] && "$xcmd"
