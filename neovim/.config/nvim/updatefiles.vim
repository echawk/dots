" Update binds when sxhkdrc is updated.
	autocmd BufWritePost *sxhkdrc !pkill -USR1 sxhkd
" Run xrdb whenever Xdefaults or Xresources are updated.
"	autocmd BufWritePost *Xresources,*Xdefaults !xrdb %
