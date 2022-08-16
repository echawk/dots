# potentially change the order of $PATH to have '.local' stuff to be first
export PATH="$(find "$HOME/.local/bin/" -type d | tr '\n' ':' | sed 's/:*$//'):$PATH"

export EDITOR="nvim" # emacs, vis
export PAGER="less"
export MANPAGER="nvim +Man!"
export BROWSER="chromium" # firefox brave luakit chromium
export READER="zathura"   # mupdf
export FUZZY="fzf"
export WM="dwm"     # bspwm i3 herbstluftwm
export AUDIO="alsa" # pulseaudio pipewire
export FILE="fff"
export DISTRO="$(cat /etc/os-release | grep "ID" | cut -d'=' -f2)" #arch, void, kiss/carbs
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_RUNTIME_DIR="${HOME}/.cache/runtime"

export CHROME_FLAGS="
--ignore-gpu-blocklist
--use-gl=desktop
--enable-zero-copy
--disable-gpu-driver-bug-workarounds
--disable-gpu-driver-workarounds
--enable-accelerated-video-decode
--enable-accelerated-mjpeg-decode
--enable-features=VaapiVideoDecoder,CanvasOopRasterization
--enable-gpu-compositing
--enable-gpu-rasterization
--enable-oop-rasterization
--canvas-oop-rasterization
--enable-raw-draw
"

# X
#export XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority"
export _JAVA_AWT_WM_NONREPARENTING=1 # Fix for Java applications in dwm
export MOZ_USE_XINPUT2="1"           # Mozilla smooth scrolling/touchpads.

# Build Flags
export KISS_TMPDIR="/tmp/kiss"
export MAKEFLAGS=-j16
export SAMUFLAGS=-j16
export CFLAGS="-O3 -march=native -mtune=native "
export CXXFLAGS="$CFLAGS"
export CMAKE_GENERATOR="Ninja"

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


export JULIA_DEPOT_PATH="$XDG_DATA_HOME/julia:$JULIA_DEPOT_PATH"
export DISTCC_DIR="$XDG_CONFIG_HOME/distcc"

export PYTHONSTARTUP="$XDG_CONFIG_HOME/python/pythonrc"

# ruby gem env
export GEM_HOME="$XDG_DATA_HOME/ruby/gem"
export GEM_PATH="$XDG_DATA_HOME/ruby/gem"
export GEM_SPEC_CACHE="$XDG_DATA_HOME/ruby/specs"

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
