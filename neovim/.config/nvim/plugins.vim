if ! filereadable(expand('~/.config/nvim/autoload/plug.vim'))
	echo "Downloading junegunn/vim-plug to manage plugins..."
	silent !mkdir -p ~/.config/nvim/autoload/
	silent !curl "https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim" > ~/.config/nvim/autoload/plug.vim
	autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.config/nvim/plugged')
"Prose plugins
Plug 'junegunn/goyo.vim' "Centers text for writing prose
Plug 'junegunn/limelight.vim' "Focuses current paragraph
Plug 'lukesmithxyz/vimling' "Prose (able to write special characters easily)

"QoL plugins
Plug 'ap/vim-css-color'  "Makes colors their color (ie #00FF00)
"Plug 'norcalli/nvim-colorizer.lua'
Plug 'luochen1990/rainbow'
Plug 'sheerun/vim-polyglot' "Syntax highlighting for basically every language

" Developer focused plugins
Plug 'preservim/nerdtree'
Plug 'editorconfig/editorconfig-vim' " add support for editorconfig
Plug 'airblade/vim-gitgutter'

call plug#end()
