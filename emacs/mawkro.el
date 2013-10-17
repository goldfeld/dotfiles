;nnoremap m@ :set opfunc=Mawkro<CR>g@
;vnoremap m@ :<C-U>call Mawkro(visualmode(), 1)<CR>
;function! Mawkro(type, ...)
;  let sel_save = &selection
;  let &selection = "inclusive"
;  let reg_save = @@
;
;  silent exe "normal! uyy<C-R>"
;  let original_line = @@
;
;  " taken from ':help g@'
;  if a:0                   | silent exe "normal! `<" . a:type . "`>y"
;  elseif a:type == 'line'  | silent exe "normal! '[V']y"
;  elseif a:type == 'block' | silent exe "normal! `[\<C-V>`]y"
;  else                     | silent exe "normal! `[v`]y"
;  endif
;
;  let lines = split(@@, '\n')
;  let word = 0
;  let char = 0
;  let condition = 1
;  let last_was_space = 0
;
;  while l:condition
;    let now_char = l:lines[0][l:char]
;    let original_char = l:original_line[l:char]
;    let l:condition = l:now_char != "" && l:now_char == l:original_char
;    if l:original_char == " " && !l:last_was_space
;      let l:word += l:word + 1
;      let l:last_was_space = 1
;    else | let l:last_was_space = 0
;    endif
;  endwhile
;
;  silent exe "normal! `["
;  for line in l:lines[1:]
;    silent exe "normal! j0" . l:word . "W."
;  endfor
;
;  let &selection = sel_save
;  let @@ = reg_save
;endfunction
