lua fzy = require('fzy')

nnoremap <silent><leader>ff :lua fzy.execute('fd', fzy.sinks.edit_file)<CR>
nnoremap <silent><leader>fb :lua fzy.actions.buffers()<CR>
nnoremap <silent><leader>ft :lua fzy.try(fzy.actions.lsp_tags, fzy.actions.buf_tags)<CR>
nnoremap <silent><leader>fg :lua fzy.execute('git ls-files', fzy.sinks.edit_file)<CR>
nnoremap <silent><leader>fq :lua fzy.actions.quickfix()<CR>
