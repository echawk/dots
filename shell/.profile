#!/bin/sh

sys="$(uname)"

# Ensure we source system /etc/profile, if it exists
[ -f /etc/profile ] && . /etc/profile

# source my environment shell script
[ -f "$HOME"/.config/shell/env.sh ]        && . "$HOME"/.config/shell/env.sh

# Ensure my ssh identities get added to my environment.
if [ -e "$HOME/.ssh/" ]; then
    export SSH_AUTH_SOCK="${XDG_CACHE_HOME}/ssh/agent"
    mkdir -p "${XDG_CACHE_HOME}/ssh/"
    # Check for pgrep. May replace with a personal shell script later.
    if command -v pgrep >/dev/null; then
        # Check if ssh-agent is running.
        if ! pgrep ssh-agent >/dev/null; then
            # Check if we even have access to ssh-agent
            if command -v ssh-agent > /dev/null 2>&1; then
                ssh-agent -a "$SSH_AUTH_SOCK"
                find "$HOME/.ssh" -name '*.pub' \
                    | sed "s/.pub$//" \
                    | xargs -I{} ssh-add {} >/dev/null 2>&1
            fi
        fi
        export SSH_AGENT_PID="$(pidof ssh-agent)"
    fi
fi

case "$sys" in
    Darwin)
        # If we have a venv in $HOME, source it.
        # useful on macos.
        if [ -e "$HOME/.venv/" ]; then
            . "$HOME/.venv/bin/activate"
        else
            # If we have python, create the venv & source it.
            if command -v python; then
                python -m venv "$HOME/.venv/" > /dev/null 2>&1
                . "$HOME/.venv/bin/activate"
            fi
        fi
        ;;
    *)
        # Enable wifi if possible
        [ -e "$HOME"/.local/bin/wctl ] && sh "$HOME"/.local/bin/wctl enable

        # Start up X11 if we are on tty1
        xcmd="$(command -v sx || command -v startx)"
        [ -z "$DISPLAY" ] && [ "$(tty)" = /dev/tty1 ] && "$xcmd"
        ;;
esac
