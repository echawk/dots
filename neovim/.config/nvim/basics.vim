let mapleader =","

set bg=dark
set go=a
set mouse=a
"set nohlsearch "makes it so only the first result is highlighted
set clipboard+=unnamedplus

"if a proj directory has its own vimrc, source it
set exrc
" and do it securely
set secure

" disable swapfile (performance improvement)
set noswapfile

" Some basics:
	nnoremap c "_c
	set nocompatible
	filetype plugin on
	syntax on
	set encoding=utf-8
	set number relativenumber
" Setup column coloring
	set colorcolumn=80
	highlight ColorColumn ctermbg=12
" Enable autocompletion:
	set wildmode=longest,list,full
" Disables automatic commenting on newline:
	autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

" Shortcutting split navigation, saving a keypress:
	map <C-h> <C-w>h
	map <C-j> <C-w>j
	map <C-k> <C-w>k
	map <C-l> <C-w>l

" Splits open at the bottom and right, which is non-retarded, unlike vim defaults.
	set splitbelow splitright

" Automatically deletes all trailing whitespace on save.
	autocmd BufWritePre * %s/\s\+$//eg

" Ensure files are read as what I want:
	"let g:vimwiki_list = [{'path': '~/vimwiki', 'syntax': 'markdown', 'ext': '.md'}]
	autocmd BufRead,BufNewFile /tmp/calcurse*,~/.calcurse/notes/* set filetype=markdown
	autocmd BufRead,BufNewFile *.ms,*.me,*.mom,*.man set filetype=groff
	autocmd BufRead,BufNewFile *.tex set filetype=tex
	autocmd BufRead,BufNewFile *.wiki set filetype=markdown

" Navigating with guides
"	inoremap <leader><leader> <Esc>/<++><Enter>"_c4l
	vnoremap <leader><leader> <Esc>/<++><Enter>"_c4l
	map <leader><leader> <Esc>/<++><Enter>"_c4l

" Replace all is aliased to S.
	nnoremap S :%s//g<Left><Left>

" Display different types of white spaces.
	set list
	set listchars=tab:›\ ,trail:•,
	"extends:#,nbsp:.
	"^^ other possible options

" Compile document, be it groff/LaTeX/markdown/etc.
	map <leader>c :w! \| !compile <c-r>%<CR>

" Make sure the popup window for hints and completion is a reasonable color (darkgray)
	highlight Pmenu ctermbg=darkgray guibg=lightgray

" Nerd tree
	map <leader>n :NERDTreeToggle<CR>
	autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Vim-Rainbow config "work on getting a complete list of files that I would use
	"let g:rainbow_active = 1
	"au FileType c,cpp,tex,python,go,lisp,java,r,scheme,ruby call rainbow#load()
	"au FileType c,cpp,tex,python,go,lisp,java,r,scheme,ruby let g:rainbow_active = 1

" Check file in shellcheck:
"	map <leader>s :!clear && shellcheck %<CR>

" Toggles deoplete auto completion
"	map <leader>a :call deoplete#toggle()<CR>
