" Limelight config
	let g:limelight_conceal_ctermfg = 'DarkGray'
	let g:limelight_conceal_guifg = 'DarkGray'
	let g:limelight_default_coefficient = 0.8
	" Goyo Integration
	autocmd! User GoyoEnter Limelight
	autocmd! User GoyoLeave Limelight!

" Goyo plugin makes text more readable when writing prose:
	let g:goyo_width = 80
	map <leader>pr :Goyo  \| set bg=dark \| set linebreak<CR>

" Spell-check set to <leader>o, 'o' for 'orthography':
	map <leader>o :setlocal spell! spelllang=en_us<CR>

" vimling:
	nm <leader>d :call ToggleDeadKeys()<CR>
	imap <leader>d <esc>:call ToggleDeadKeys()<CR>a
	nm <leader>i :call ToggleIPA()<CR>
	imap <leader>i <esc>:call ToggleIPA()<CR>a
	nm <leader>q :call ToggleProse()<CR>

" Enable Goyo by default for mutt writting (this shit is broken for some reason) (Fix) (not fixed)
	" Goyo's width will be the line limit in mutt.
"	autocmd BufRead,BufNewFile /tmp/neomutt* let g:goyo_width=80
"	autocmd BufRead,BufNewFile /tmp/neomutt* :Goyo | set bg=dark
		"<CR> \| set bg=light
"	autocmd BufRead,BufNewFile /tmp/neomutt* let g:goyo_width=80
"	autocmd BufRead,BufNewFile /tmp/neomutt* :Goyo | set bg=light
"	autocmd BufRead,BufNewFile /tmp/neomutt* map ZZ :Goyo\|x!<CR>
"	autocmd BufRead,BufNewFile /tpp/neomutt* map ZQ :Goyo\|q!<CR>
