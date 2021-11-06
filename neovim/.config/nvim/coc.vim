
Plug 'neoclide/coc.nvim', {'branch': 'release'} " LSP for (neo)vim

" basic settings (from github)
set hidden
set nobackup
set nowritebackup

" decrease the update time from 4 seconds to 300ms
set updatetime=300

" Highlight symbol under cursor on CursorHold
	autocmd CursorHold * silent call CocActionAsync('highlight')

" COC config
	" Use <c-space> to trigger completion.
	if has('nvim')
	  inoremap <silent><expr> <c-space> coc#refresh()
	else
	  inoremap <silent><expr> <c-@> coc#refresh()
	endif
	" Use tab for trigger completion with characters ahead and navigate.
	" NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
	" other plugin before putting this into your config.
	inoremap <silent><expr> <TAB>
	      \ pumvisible() ? "\<C-n>" :
	      \ <SID>check_back_space() ? "\<TAB>" :
	      \ coc#refresh()
	inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

	function! s:check_back_space() abort
  	  let col = col('.') - 1
	  return !col || getline('.')[col - 1]  =~# '\s'
	endfunction

       " Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
       " position. Coc only does snippet and additional edit on confirm.
       " <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
	if exists('*complete_info')
	  inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
	else
	  inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
	endif

	" Make <CR> auto-select the first completion item and notify coc.nvim to
	" format on enter, <cr> could be remapped by other vim plugin
	inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
        	                      \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"

" GoTo code navigation.
	nmap <silent> gd <Plug>(coc-definition)
	nmap <silent> gy <Plug>(coc-type-definition)
	nmap <silent> gi <Plug>(coc-implementation)
	nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
	nnoremap <silent> K :call <SID>show_documentation()<CR>
	nnoremap <silent> gh :call <SID>show_documentation()<CR>

	function! s:show_documentation()
	  if &filetype == 'vim'
	    execute 'h '.expand('<cword>')
	  else
	    call CocAction('doHover')
	  endif
	endfunction

" Make sure the popup window for hints and completion is a reasonable color (darkgray)
	highlight Pmenu ctermbg=darkgray guibg=lightgray

" Symbol renaming.
	nmap <leader>rn <Plug>(coc-rename)
" Formatting selected code.
	xmap <leader>f  <Plug>(coc-format-selected)
	nmap <leader>f  <Plug>(coc-format-selected)

	nnoremap <silent> <leader>co  :<C-u>CocList outline<cr>
	nnoremap <silent> <leader>cs  :<C-u>CocList -I symbols<cr>

" List errors
"	nnoremap <silent> <leader>cl  :<C-u>CocList locationlist<cr>

" restart when tsserver gets wonky
	nnoremap <silent> <leader>cR  :<C-u>CocRestart<CR>

" view all errors
	nnoremap <silent> <leader>cl  :<C-u>CocList locationlist<CR>

" list commands available in tsserver (and others)
"nnoremap <silent> <leader>cc  :<C-u>CocList commands<cr>

" manage extensions
"nnoremap <silent> <leader>cx  :<C-u>CocList extensions<cr>

" rename the current word in the cursor
	"nmap <leader>cr  <Plug>(coc-rename)
	"nmap <leader>cf  <Plug>(coc-format-selected)
	"vmap <leader>cf  <Plug>(coc-format-selected)

" run code actions
	vmap <leader>ca  <Plug>(coc-codeaction-selected)
	nmap <leader>ca  <Plug>(coc-codeaction-selected)
