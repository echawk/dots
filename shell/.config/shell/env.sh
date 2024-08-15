# potentially change the order of $PATH to have '.local' stuff to be first
export PATH="$(find "$HOME/.local/bin/" -type d | tr '\n' ':' | sed 's/:*$//'):$PATH"
[ -d "$HOME/.dotnet/" ]             && PATH="$PATH:$HOME/.dotnet"
[ -d "$HOME/.dotnet/tools" ]        && PATH="$PATH:$HOME/.dotnet/tools"
[ -d "$HOME/.local/share/npm/bin" ] && PATH="$PATH:$HOME/.local/share/npm/bin"

# enable homebrew if we are on macOS
case "$(uname)" in
    Darwin)
        eval "$(/opt/homebrew/bin/brew shellenv)"
        ;;
esac

#export EDITOR="nvim" # emacs, vis
export PAGER="less"
export BROWSER="brave" # firefox brave luakit chromium
export READER="emacs"   # mupdf zathura
export FUZZY="fzf"
export WM="stumpwm"     # bspwm dwm i3 herbstluftwm

# FIXME: make this check for the OS as well.
if command -v wpctl; then
    export AUDIO="pipewire"
elif command -v pactl; then
    export AUDIO="puleaudio"
elif command -v sndioctl; then
    export AUDIO="sndio"
else
    export AUDIO="alsa"
fi

#export FILE="fff"
export DISTRO="$(grep "ID"  /etc/os-release | cut -d'=' -f2)" #arch, void, kiss/carbs
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_RUNTIME_DIR="${HOME}/.cache/runtime"
export XDG_STATE_HOME="${HOME}/.local/state"

export CHROME_FLAGS="--ignore-gpu-blocklist --enable-features=VaapiVideoDecoder,CanvasOopRasterization"

# X
#export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"
export _JAVA_AWT_WM_NONREPARENTING=1 # Fix for Java applications in dwm
export MOZ_USE_XINPUT2="1"           # Mozilla smooth scrolling/touchpads.

# Build Flags
case "$(uname)" in
    Linux)
        #export KISS_TMPDIR="/tmp/kiss"
        export MAKEFLAGS=-j12
        export SAMUFLAGS=-j12
        export CFLAGS="-O3 -march=native -mtune=native "
        export CXXFLAGS="$CFLAGS"
        export CMAKE_GENERATOR="Ninja"
        ;;
esac

# Programming Langs
export GOPATH="$XDG_DATA_HOME/go"
export GOPROXY=direct
export CARGO_HOME="$XDG_DATA_HOME/cargo"
#export RUSTC_WRAPPER=sccache
export RUSTFLAGS="-C opt-level=3"
export NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc"
export NODE_REPL_HSTORY="$XDG_DATA_HOME/node_repl_history"

export STACK_ROOT="$XDG_DATA_HOME/stack"
export CABAL_CONFIG="$XDG_CONFIG_HOME/cabal/config"
export CABAL_DIR="$XDG_DATA_HOME/cabal"
export PATH="$CABAL_DIR/bin:$PATH"

# Disable telemetry for dotnet cli.
export DOTNET_CLI_TELEMETRY_OUTPUT=1

export JULIA_DEPOT_PATH="$XDG_DATA_HOME/julia:$JULIA_DEPOT_PATH"
export DISTCC_DIR="$XDG_CONFIG_HOME/distcc"

export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"

# ruby gem env
export GEM_HOME="$XDG_DATA_HOME/ruby/gem"
export GEM_PATH="$XDG_DATA_HOME/ruby/gem"
export GEM_SPEC_CACHE="$XDG_DATA_HOME/ruby/specs"

export BUNDLE_USER_CONFIG="$XDG_CONFIG_HOME"/bundle
export BUNDLE_USER_CACHE="$XDG_CACHE_HOME"/bundle
export BUNDLE_USER_PLUGIN="$XDG_DATA_HOME"/bundle

export SOLARGRAPH_CACHE="$XDG_CACHE_HOME/solargraph"

export NIMBLE_DIR="$XDG_DATA_HOME/nimble"

# ~/ cleanup
export LESS=-R
export LESSHISTFILE="-"
export GNUPGHOME="${XDG_CONFIG_HOME}/gnupg"
export PASSWORD_STORE_DIR="$XDG_DATA_HOME/password-store"
export NOTMUCH_CONFIG="$XDG_CONFIG_HOME/notmuch-config"
export HISTFILE="${XDG_DATA_HOME}/history"
export WGETRC="$XDG_CONFIG_HOME/wget/wgetrc"
export INPUTRC="$XDG_CONFIG_HOME/inputrc"
export GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0"
export QT_QPA_PLATFORMTHEME="qt5ct"
export ANDROID_SDK_HOME="$XDG_DATA_HOME/android"
#export ALSA_CONFIG_PATH="$XDG_CONFIG_HOME/alsa/asoundrc" # uncommenting this line breaks pulse
export WEECHAT_HOME="$XDG_CONFIG_HOME/weechat"
export TMUX_TMPDIR="$XDG_RUNTIME_DIR"
export QT_QPA_PLATFORMTHEME="gtk2" # Have QT use gtk2 theme.
export MPLAYER_HOME="$XDG_CONFIG_HOME"/mplayer
