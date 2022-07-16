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
"Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }

" Developer focused plugins
Plug 'preservim/nerdtree'
Plug 'editorconfig/editorconfig-vim' " add support for editorconfig
Plug 'airblade/vim-gitgutter'

"Plug 'somini/vim-autoclose'
"Plug 'tpope/vim-surround' "Automatically adds quotes and parenthesis
"Plug 'Gavinok/vim-troff'
"Plug 'z0mbix/vim-shfmt'
"Plug 'tpope/vim-fugitive'
"Plug 'junegunn/gv.vim'
"Plug 'joshdick/onedark.vim' "onedark colorscheme for vim
"Plug 'ThePrimeagen/vim-be-good' "Plugin to learn vim via games
"Plug 'vimwiki/vimwiki'

" official neovim completion plugins
"Plug 'neovim/nvim-lspconfig' "lsp configuration for default neovim
"Plug 'glepnir/lspsaga.nvim' "Provides some nice helper functions for lsp
"Plug 'hrsh7th/nvim-compe' "nvim completion (for lsp)

"Plug 'mfussenegger/nvim-dap'
"Plug 'mfussenegger/nvim-fzy'

"Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}
call plug#end()
