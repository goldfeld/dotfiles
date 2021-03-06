"{{{1 BUNDLES
" Vundle setup
filetype off
set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Bundle 'gmarik/vundle'

" workflow
Bundle 'jceb/vim-orgmode'
Bundle 'goldfeld/vimdow'
Bundle 'mikewest/vimroom'
Bundle 'goldfeld/vim-remarkable'
Bundle 'pydave/AsyncCommand'

" writing
Bundle 'goldfeld/tnt'

" editing
Bundle 'tpope/vim-surround'
Bundle 'paradigm/TextObjectify'
Bundle 'michaeljsmith/vim-indent-object'
Bundle 'vim-pandoc/vim-markdownfootnotes'
Bundle 'Twinside/vim-haskellConceal'

" files
Bundle 'grep.vim'
Bundle 'goldfeld/vim-fugitive'
"Bundle 'spolu/dwm.vim'

" moving
Bundle 'bkad/CamelCaseMotion'

" syntax
Bundle 'kchmck/vim-coffee-script'
Bundle 'jb55/Vim-Roy'
Bundle 'leafo/moonscript-vim'
Bundle 'Vim-R-plugin'
Bundle 'goldfeld/criticmarkup-vim'
Bundle 'wting/rust.vim'
Bundle 'unc0/vim-gorilla-script'
Bundle 'gkz/vim-ls'

" other
Bundle 'goldfeld/vim-pegword'

"}}}
"{{{ HELPERS
command! -nargs=1 B execute "buffer" (<q-args>)[0]
command! -nargs=1 -complete=file E execute "edit +bdelete\\" bufnr('%') <f-args>
" 'V' is for viewing, when my intent is to quickly view a file then bdelete it.
command! -nargs=1 -complete=file V execute "keepalt edit" <f-args>

function! Rescape(text)
  return escape(a:text, '^$.*+')
endfunction

let g:inbox = fnamemodify('~/leak/.tnt/inbox.tnt', ':p')
let g:inbox_cmd = "awk 'BEGIN { links = 0; } /^### links$/ { links = 1; } "
  \ . "!/^#/ { if (links) print \"L\" NR \" \" $0 }' " . g:inbox
let g:inbox_query = { 'list': 11, 'prompt': 'inbox','query': g:inbox_cmd }

function! InboxHookWrap(cmd)
  if !empty(system(g:inbox_cmd))
    let item = split(dow#pick(g:inbox_query), ' ')
    if empty(l:item) | return | endif
    let [cutoff, markdown] = [l:item[0][1:], l:item[1:]]
    let lines = readfile(g:inbox)
    call writefile(l:lines[: l:cutoff - 2] + l:lines[l:cutoff :], g:inbox)
    let @" = join(l:markdown, ' ')
  endif
  execute a:cmd
endfunction
"}}}
"{{{1 OPTIONS
filetype plugin indent on
set runtimepath^=~/.vim/bundle/ctrlp.vim
color slate
set cursorline
" create a different color area beyond 80 columns.
let &colorcolumn=join(range(81, 201), ",")

"}}}1
"{{{1 BUFLOCAL OPTIONS
augroup filetypeSettings
  autocmd BufRead *pentadactylrc setlocal filetype=vim
augroup END
"}}} {{{1 XMONAD HOOKER my thumb can't take being contracted for Alt all the
"time anymore
nnoremap <silent> c<CR> :silent! !hooker 1<CR>
nnoremap <silent> m<CR> :silent! !hooker 5<CR>
nnoremap <silent> h<CR> :silent! !hooker 7<CR>
nnoremap <silent> r<CR> :silent! !hooker 9 && hooker 33<CR>

nnoremap <silent> <C-W>h :call Wincmd('h', 33)<CR>
nnoremap <silent> <C-W>l :call Wincmd('l', 34)<CR>
nnoremap <silent> <C-W>k :call Wincmd('k', 33)<CR>
nnoremap <silent> <C-W>j :call Wincmd('j', 34)<CR>
function! Wincmd(cmd, hooker)
  if (winwidth(0) + 0.0)/(&columns + 0.0) >= 0.9 | sil! exe "!hooker" a:hooker
  else | sil! exe "wincmd" a:cmd
  endif
endfunction

command! Reload silent! execute '!hooker 9 && hooker 33 && xdotool search'
  \ . ' --onlyvisible --class Chromium-browser key --clearmodifiers ctrl+r'
"}}}
"{{{1 ORG-MODE
augroup orgMode
  autocmd!
  autocmd BufRead,BufNewFile *.tbl.org
    \ vnoremap <buffer> is <Esc>?--<CR>jV/--<CR>k<Esc>:noh<CR>gv

  autocmd BufRead,BufNewFile *.tbl.org
    \ nnoremap <buffer> ]] /--<CR>j:noh<CR>
  autocmd BufRead,BufNewFile *.tbl.org
    \ nnoremap <buffer> ][ /--<CR>k:noh<CR>

  autocmd BufRead,BufNewFile *.tbl.org
    \ nnoremap <buffer> [[ ?--<CR>knj:noh<CR>
  autocmd BufRead,BufNewFile *.tbl.org
    \ nnoremap <buffer> [] ?--<CR>j:noh<CR>
augroup END

nnoremap <leader>.s :call SearchTableRow()<CR>
function! SearchTableRow()
  execute 'normal! 0W"ry$'
  call system('chromium-browser https://duckduckgo.com/?q=g!+rio+'
    \ . substitute(getreg('r'), ' ', '%20', 'g'))
  silent! exe '!hooker 9 && hooker 33'
endfunction
"}}}
"{{{1 LEAKYLL
let g:all_leaks_query = { 'list': 10, 'prompt': 'leak', 'query': '('
  \ . ' cd ~/leak/goldfeld/articles/_posts/ && find `pwd` ! -iname ".*" ;'
  \ . ' cd ~/leak/goldfeld/leaks/_posts/ && find `pwd` ! -iname ".*" ;'
  \ . ' cd ~/leak/void/leaks/_posts/ && find `pwd` ! -iname ".*" ;'
  \ . ' cd ~/leak/.tnt/leaks/_posts/ && find `pwd` ! -iname ".*" ;'
  \ . ' cd ~/leak/.tnt/games/_posts/ && find `pwd` ! -iname ".*" ;'
  \ . ' cd ~/leak/.tnt/stories/_posts/ && find `pwd` ! -iname ".*" ;'
  \ . ' cd ~/leak/void/readings/_posts/ && find `pwd` ! -iname ".*" )' }
nnoremap <silent> <C-T><C-L><C-L>
  \ :call InboxHookWrap('call dow#edit(g:all_leaks_query)')<CR>
nnoremap <silent> <C-T><C-L>l
  \ :call InboxHookWrap('call dow#swap(g:all_leaks_query)')<CR>

nnoremap <C-T><C-L>b :sil! exe '!chromium-browser'
  \ 'http://localhost:4000' . GetPost(getreg('%')).url<CR>

nnoremap <C-T><C-L>w :exe 'norm! i'.getreg('+')<CR>:TNTCreateWebp<CR>
inoremap <C-B><C-L>w <Esc>:exe 'norm! i'.getreg('+')<CR>:TNTCreateWebp<CR>

inoremap <C-B><C-L><C-L> <Esc>:call LinkPost('title')<Cr>
inoremap <C-B><C-L>l <Esc>:call LinkPost()<Cr>

nnoremap <C-T><C-L><C-C> :call Cheatsheet()<CR>
function! Cheatsheet()
  let pick = dow#pick({ 'list': 10, 'prompt': 'cheatsheet',
    \ 'query': "awk '/^[^[]/ {print}' ~/leak/.tnt/cheatsheet" })

  for ref in split(system("awk '/^[[]/ {print}' ~/leak/.tnt/cheatsheet"), '\n')
    let info = split(ref[1:], ']: ')
    let [pattern, url] = [l:info[0], l:info[1]]
    if l:pick =~# l:pattern
      call system('chromium-browser ' . l:url . ' && hooker 9 && hooker 33')
      break
    endif
  endfor
endfunction

augroup LEAKYLL
  autocmd!
  "autocmd BufRead,BufNewFile *.md nnoremap <buffer> <CR> 
augroup END

" TODO auto title-case the post title, ignoring a user-defined dict of stopwords
" scaffold new jekyll leak
let g:leakyll_basedir = '~/leak'
let g:leakyll_default_category = 'leaks'
function! ScaffoldPost(dir, title)
  let path = split(a:dir, '/')
  let dir = l:path[0]
  let category = get(l:path, 1, g:leakyll_default_category)

  let newfile = g:leakyll_basedir . '/' . l:dir . '/' . l:category . '/_posts/'
    \ . strpart(system("date +'%Y-%m-%d-'"), 0, 11)
    \ . substitute(a:title, ' ', '-', 'g') . '.md'

  call writefile(['---', 'layout: leak', 'title: "' . a:title . '"',
    \ 'category: ' . l:category, '---', ''], fnamemodify(l:newfile, ':p'))
  execute "edit" l:newfile
endfunction

command! -nargs=* L call ScaffoldPost(
  \ split(<q-args>, ' ')[0], join(split(<q-args>, ' ')[1:], ' '))
command! -nargs=* LL execute "norm! i[" . join(split(<q-args>, ' ')[1:], ' ')
  \ . "](/" . substitute(<q-args>, ' ', '-', 'g') . ")" | execute "L" <q-args>

function! GetPost(filename, ...)
  let what = get(a:000, 0, ['url'])
  let date_and_slug = fnamemodify(a:filename, ':t:r')

  let info = {}
  if index(l:what, 'url') != -1 | let l:info.url = '/' . l:date_and_slug[0 : 3]
    \ . '/' . l:date_and_slug[11 :] | endif
  if index(l:what, 'title') != -1
    let l:title = dow#chomp(system("awk -F 'title: ' '/^title:/ { print $2 }' "
      \ . a:filename))
    let l:info.title = strpart(l:title, 1, len(l:title) - 2)
  endif

  return l:info
endfunction

function! LinkPost(...)
  let pick = dow#pick(g:all_leaks_query)

  if get(a:000, 0, '') =~# 'title'
    let post = GetPost(l:pick, ['title', 'url'])
    execute 'normal! i[' . l:post.title . '](' . l:post.url . ')'
  else
    let post = GetPost(l:pick)
    execute 'normal! i ' . l:post.url
  endif
endfunction
"}}}
"{{{1 INKSTONE
let g:inkstone_triggers = {}
let g:inkstone_tests = {}
let g:inkstone_last_word = ''
let g:inkstone_last_typed = ''
let g:inkstone_do_replace = 0

"for pair in readfile(fnamemodify('~/inkspree/inkstone/dict', ':p'))
"  let splitpair = split(pair, '\s')
"  let g:inkstone_triggers[splitpair[1]] = splitpair[0]
"  let g:inkstone_tests[splitpair[0]] = splitpair[1]
"endfor

inoremap <C-B><C-I> <Esc>:call ShowWord()<CR>

augroup inkstone
  autocmd!
  autocmd BufRead,BufNewFile *.md inoremap <buffer> <expr> <Space> CheckWord()
  autocmd InsertCharPre *.md let g:inkstone_last_typed .= v:char
augroup END

function! ShowWord()
  normal! bdw
  call setreg('"', g:inkstone_triggers[getreg()])
  let g:inkstone_do_replace = 1
  normal! pa
endfunction

function! CheckWord()
  let line = getline('.')[: col('.')]
  let word = split(l:line, ' ')[-1]
  let insertion = ' ' " a <Space> character

  " we only delete the word if it has been freshly typed, to avoid retesting
  " the user when he is only editing already tested words.
  let freshly_typed = g:inkstone_last_typed[1:] ==# l:word
  if has_key(g:inkstone_triggers, l:word) && l:freshly_typed
    let g:inkstone_last_word = l:word
    let l:insertion = repeat("\<BS>", len(l:word))

  elseif has_key(g:inkstone_tests, l:word)
    let replacement = g:inkstone_last_word " g:inkstone_tests[l:word]
    let l:insertion = repeat("\<BS>", len(l:word)) . l:replacement . ' '
  endif

  let g:inkstone_last_typed = ''
  let g:inkstone_do_replace = 0
  return l:insertion
endfunction
"}}}
"{{{1 SINGLE-FUNCTION FEATURES
nnoremap m@ :set opfunc=Mawkro<CR>g@
vnoremap m@ :<C-U>call Mawkro(visualmode(), 1)<CR>
function! Mawkro(type, ...)
  let sel_save = &selection
  let &selection = "inclusive"
  let reg_save = @@

  silent exe "normal! uyy<C-R>"
  let original_line = @@

  " taken from ':help g@'
  if a:0                   | silent exe "normal! `<" . a:type . "`>y"
  elseif a:type == 'line'  | silent exe "normal! '[V']y"
  elseif a:type == 'block' | silent exe "normal! `[\<C-V>`]y"
  else                     | silent exe "normal! `[v`]y"
  endif

  let lines = split(@@, '\n')
  let word = 0
  let char = 0
  let condition = 1
  let last_was_space = 0

  while l:condition
    let now_char = l:lines[0][l:char]
    let original_char = l:original_line[l:char]
    let l:condition = l:now_char != "" && l:now_char == l:original_char
    if l:original_char == " " && !l:last_was_space
      let l:word += l:word + 1
      let l:last_was_space = 1
    else | let l:last_was_space = 0
    endif
  endwhile

  silent exe "normal! `["
  for line in l:lines[1:]
    silent exe "normal! j0" . l:word . "W."
  endfor

  let &selection = sel_save
  let @@ = reg_save
endfunction

"au! CursorHold *.[ch] nested call PreviewWord()
" This will cause a ":ptag" to be executed for the keyword under the cursor,
" when the cursor hasn't moved for the time set with 'updatetime'.  The "nested"
" makes other autocommands be executed, so that syntax highlighting works in the
" preview window.  The "silent!" avoids an error message when the tag could not
" be found.
" A nice addition is to highlight the found tag, avoid the ":ptag" when there
" is no word under the cursor, and a few other things: >
" (from vim documentation)
function! PreviewWord()
  " don't do this in the preview window
  if &previewwindow | return | endif
  let w = expand("<cword>")		" get the word under cursor
  if w =~ '\a'			" if the word contains a letter

    " Delete any existing highlight before showing another tag
    silent! wincmd P			" jump to preview window
    if &previewwindow			" if we really get there...
      match none			" delete existing highlight
      wincmd p			" back to old window
    endif

    " Try displaying a matching tag for the word under the cursor
    try | exe "ptag " . w
    catch | return
    endtry

    silent! wincmd P			" jump to preview window
    if &previewwindow		" if we really get there...
      " don't want a closed fold
      if has("folding") | silent! .foldopen | endif
      call search("$", "b")		" to end of previous line
      let w = substitute(w, '\\', '\\\\', "")
      call search('\<\V' . w . '\>')	" position cursor on match
      " Add a match highlight to the word at this position
      hi previewWord term=bold ctermbg=green guibg=green
      exe 'match previewWord "\%' . line(".") . 'l\%' . col(".") . 'c\k*"'
      wincmd p			" back to old window
    endif
  endif
endfunction
"}}}
"{{{1 PLUGIN SETTINGS
let g:TNTWebBrowser = 'luakit'
let g:textobjectify_onthefly_same = 1

let g:surround_no_mappings = 1
let g:surround_no_insert_mappings = 1
nmap dx  <Plug>Dsurround
nmap cx  <Plug>Csurround
nmap yx  <Plug>Ysurround
nmap yx  <Plug>YSurround
nmap yxx <Plug>Yssurround
nmap yXx <Plug>YSsurround
nmap rXX <Plug>YSsurround
xmap X   <Plug>VSurround
xmap gX  <Plug>VgSurround
imap <C-G>t <Plug>Isurround
imap <C-G>T <Plug>ISurround

let g:MicroMarks = ['h', 't', 'n', 's', '-']
nnoremap mi :MicroMark<CR>
nnoremap md :MicroMarkClear<CR>
"nnoremap 'c :MicroMarkMatch<CR>
for micromark in g:MicroMarks
  execute "nnoremap '" . micromark . " `" . micromark . "zvzz"
endfor

let g:dow_source = ['dmenu', 'ctrlr']
let g:dow_projects = ['~/leak', '~/goldfeld', '~/void', '~/.vim/bundle',
  \ '~/inkspree', '~/longstorm']
"}}}1
"{{{1 CORE REMAPPINGS
" skip past big lines
nnoremap gj j
nnoremap gk k


" use register 'u' for emulating terminal C-U & C-Y on vim command line.
cnoremap <C-U> <C-\>e(setreg("u", getcmdline())?"":"")<CR>
cnoremap <C-Y> <C-R>u
"}}}
"{{{1 VIM-RESTRAIN
let g:restrain_map = {
  \ "jj": [""],
  \ "kk": [""],
  \ "lw": ["2w"],
  \ "ew": ["3w"],
  \ "hb": ["2b"],
  \ "-b": ["3b"],
  \ "hh ll hl lh": ["%"]
  \ }

"nnoremap <silent> j
"  \ :<C-U>call RestrainCommand('j', "", v:count1, 2)<CR>
"nnoremap <silent> k
"  \ :<C-U>call RestrainCommand('k', "", v:count1, 2)<CR>

" move a line of text using ALT-{j,k}
" bind these to jk and kj (restrained)
nnoremap mj mz:m+<CR>`z
nnoremap mk mz:m-2<CR>`z

nnoremap <silent> h :<C-U>call RestrainCommand('h', "", v:count1)<CR>
nnoremap <silent> l :<C-U>call RestrainCommand('l', "", v:count1)<CR>

"nnoremap <silent> - :<C-U>call RestrainCommand('$', "", v:count)<CR>
" map minus to do a cut short seek in operator pending mode.
let g:SeekCutShortKey = '-'

"nnoremap <silent> _ :<C-U>call RestrainCommand('^', "", v:count)<CR>
" map underscore to do a cut short back seek in operator pending mode.
let g:SeekBackCutShortKey = '_'

nnoremap <silent> b :<C-U>call RestrainCommandPair('b', 'b', '', 'h',
  \ 'normal! 2b')<CR>
nnoremap <silent> w :<C-U>call RestrainCommandPair('w', 'w', '', 'l',
  \ 'normal! 2w')<CR>

nnoremap _ gE
nnoremap - ge

" step back one char so it doesn't include the newline character.
vnoremap $ $h

" TODO map double ^ to do begin of line seek
" TODO map double $ to do end of line seek

augroup restrainCommand
  autocmd!
  autocmd CursorMoved * call CheckCurrentCommand()
augroup END

let g:currentCommand = ''
let g:lastCommand = ''
function! RestrainCommand(cmd, doublePressCmd, ...)
  " the two splat arguments we might get are first a
  " count to pass to the restrained command, and maybe
  " an integer saying at what minimum count we should
  " override the restrainment, that is, ignore it.
  " a:0 represents how many optional arguments we got.
  if a:0 >= 1 | let cnt = a:1
  else | let cnt = ''
  endif

  " if an override was passed and the passed count meets
  " the override value, we set our boolean to that value,
  " which means simply setting it to true.
  if a:0 >= 2 && l:cnt >= a:2 | let countOverride = a:2
  else | let countOverride = 0
  endif

  if g:lastCommand == a:cmd && !l:countOverride
    execute a:doublePressCmd
  else | execute 'normal! ' . l:cnt . a:cmd
  endif
  let g:currentCommand = a:cmd
endfunction

function! RestrainCommandPair(cmd, key, doubleCmd, pair, pairThenCmd)
  if g:lastCommand == a:key | execute a:doubleCmd
  elseif g:lastCommand == a:pair | execute a:pairThenCmd
  else | execute "normal!" a:cmd
  endif
  let g:currentCommand = a:key
endfunction

function! CheckCurrentCommand()
  let g:lastCommand = ''
  if g:currentCommand != ''
    let g:lastCommand = g:currentCommand
  endif
  let g:currentCommand = ''
endfunction
"}}}
"{{{1 LEADER MAPPINGS
" <Leader>w, <Leader>b and <Leader>e are taken by the CamelCaseMotion plugin.

nnoremap <Leader>[ {o
nnoremap <Leader>] }O

nnoremap <Leader>* g*
nnoremap <Leader># g#
nnoremap ]* yi]/<C-\>eRescape('<C-R>"')<CR><CR>
nnoremap [# yi]?<C-\>eRescape('<C-R>"')<CR><CR>

" to go end of textwidth.
nnoremap <Leader>- 81\|

nnoremap <Leader>v :call LoadSession()<CR>

" go to next trailing whitespace
nnoremap <Leader>I /\s$<CR>:noh<CR>a
" output current time and date with year and week, all pretty printed.
nnoremap <silent> <Leader>d :echo DateAndBattery()<CR>
function! DateAndBattery()
  let date = system("date +'[%Yw%V] %b %-e %a <%H:%M>'")
  let battery = system("acpi")
  return l:date . ' ' . l:battery
endfunction

nnoremap <leader>m :TNTVisibleHeadingNext<CR>

nnoremap <Leader>t :call EditOtherExt('.coffee')<CR>
nnoremap <Leader>n :call EditOtherExt('.html')<CR>
nnoremap <Leader>s :call EditOtherExt('.scss')<CR>
function! EditOtherExt(ext)
  let filename = expand('%')
  execute 'e ' split(filename, '\.')[0] . a:ext
endfunction

command! -nargs=0 Sum :5,12!awk '{num = substr($7, 2, length($7) - 4) + substr($8, 2, length($7) - 4); width += num; print} END {print width}'

" convert time since epoch under cursor to readable time.
nnoremap <Leader>.m :echo FromEpoch(expand("<cword>"))<CR>
function! FromEpoch(date)
  let date = strpart(a:date, 0, len(a:date) - 3)
  return system('date --date "Jan 1, 1970 00:00:00 +000 + '.l:date.' seconds"')
endfunction

"}}}
"{{{1 LEADER DOT MAPPINGS
" quickly edit my tnt outline
nnoremap <silent> <Leader>.t :e ~/leak/.tnt/lifethreads.tnt<CR>
" allow left ctrl (which I remap to my Caps Lock key) to act as <Esc> when pressed alone.
nnoremap <silent> <Leader>.x :execute "call system(\"xcape -e 'Control_L=Escape'\")"<CR>
" grab ssh publickey to clipboard.
nnoremap <silent> <Leader>.k :execute "call system(\"xclip -sel clip < ~/.ssh/id_rsa.pub\")"<CR>

" remove swap files for current file.
nnoremap <silent> <Leader>.r :silent! exe "!rm ." .expand('%'). ".sw*"<CR>
" toggle show hidden characters and cursorcolumn
nnoremap <silent> <Leader>.l :set list!<CR>:set cursorcolumn!<CR>

" common searches
nnoremap <Leader>./h /HEAD<CR>
nnoremap <Leader>./c /console<CR>

" quickly edit a vim bundle
nnoremap <silent> <Leader>.b :call Dmenu("edit", "bundle", {
  \ 'query': 'ls $HOME/.vim/bundle/', 'prepend': '$HOME/.vim/bundle/',
  \ 'append': "/README.md" })<CR>

" workaround xmonad glitchy redimensioning of gvim when sourcing my vimrc.
function! SaveDimensions()
  let g:RESETLINES = &lines
  let g:RESETCOLUMNS = &columns
  wviminfo
endfunction
function! ResetDimensions()
  execute "set lines=" . g:RESETLINES
  execute "set columns=" . g:RESETCOLUMNS
endfunction
"}}}1
"{{{1 ^B MAPPINGS
" insert a pair of anything and position in-between
inoremap <C-B><C-A> <C-O>:call InsertPair()<CR>
function! InsertPair()
  let char = nr2char(getchar())
  execute "normal! a" . repeat(l:char, 2)
  startinsert
endfunction

" toggle uppercase/lowercase.
inoremap <C-B>b <Esc>vb~gvova
" same as above but going over underscores.
inoremap <C-B><C-B> <Esc>vB~gvova
" insert two en-dashes (&#8211)
inoremap <C-B><C-D> ––

inoremap <C-B><C-F> <C-O>:AddVimFootnote<CR>

" toggle case of first letter (pascal case).
inoremap <C-B><C-P> <Esc>bv~ea
" toggle case of first letter in each word (title case).
inoremap <C-B><C-T> <Esc>mb:execute
  \ "wh col('.') > 1<Bar>exe 'norm! bv~'<Bar>endw"<CR>`ba
nnoremap <C-B><C-T> :let linelen = len(getline('.'))<CR>mbge:execute
  \ "wh col('.') < linelen<Bar>exe 'norm! wv~e'<Bar>endw"<CR>`b

" toggle uppercase/lowercase of whole line (aka yell)
inoremap <C-B><C-Y> <Esc>v^~gvova
"}}}1
"{{{1 ^T MAPPINGS
" project based configs: https://github.com/timtadh/swork
" B to close a buffer by bufnr (pegword), <C-B> enter purge repl (currently mp)
" G to kill group (git repo), <C-G> for repl -- (pk in projectile)
" L list local (git repo), <C-L> list all (as currently)
nnoremap <silent> <C-C>pf <C-C>:Dow edit prj<CR>
nnoremap <silent> <C-C>pb <C-C>:Dow edit prj buf<CR>
nnoremap <silent> <C-C><C-C> :x<CR>
  \:echom strpart(system("git show $commit \| grep '^    \w'"), 2)<CR>

nnoremap <silent> <C-T>t<C-N> :DowLine swap prj<CR>

nnoremap <silent> <C-T><C-C> :Dowf edit prj buf<CR>
nnoremap <silent> <C-T>c :Dowf swap prj buf<CR>

nnoremap <silent> <C-T><C-S> :DowLine<CR>
nnoremap <silent> <C-T>s :DowSymbol<CR>
nnoremap <silent> <C-T><C-Z> :DowLine #<CR>
nnoremap <silent> <C-T>z :DowSymbol #<CR>

nnoremap <silent> <C-T><C-U> :Dowf edit untracked<CR>
nnoremap <silent> <C-T>c :Dowf swap untracked<CR>

" list projects
" allow me to define here some basic dow projects for which their status will
" always be listed regardless of there being files open, it will still show me
" if there are uncommitted and/or unpushed changes.
nnoremap <C-T><C-P> :DowBuflist<CR>
nnoremap <C-T><C-W> :DowWip<CR>

nnoremap <C-T><C-M> :Dowb<CR>
nnoremap <C-T>m :Dowp<CR>

" set split to double-width, so it can de diffed w/o affecting other splits.
nnoremap <C-T><C-D> :vertical resize 160<CR>
nnoremap <C-T>d :vertical resize 160<CR>:Gdiff<CR><C-W>h
" show buffer list as single echo line, grouped by git repo.
"}}}1
"{{{1 GIT BINDS
"}}}1
"{{{1 LIFEHACKS
command! -nargs=1 -complete=customlist,DayOpt Day call Day(<f-args>)
function! DayOpt(ArgLead, CmdLine, CursorPos)
  if a:ArgLead == 't' | return ['tue', 'thu'] | endif
  if a:ArgLead == 'tu' | return ['tue'] | endif
  if a:ArgLead == 'th' | return ['thu'] | endif
  if a:ArgLead == 's' | return ['sat', 'sun'] | endif
  if a:ArgLead == 'sa' | return ['sat'] | endif
  if a:ArgLead == 'su' | return ['sun'] | endif
  if a:ArgLead == 'm' || a:ArgLead == 'mo' | return ['mon'] | endif
  if a:ArgLead == 'w' || a:ArgLead == 'we' | return ['wed'] | endif
  if a:ArgLead == 'f' || a:ArgLead == 'fr' | return ['fri'] | endif
  if a:ArgLead == 'u' | return ['tue'] | endif
  if a:ArgLead == 'h' | return ['thu'] | endif
  if a:ArgLead == 'n' | return ['sun'] | endif
  if a:ArgLead == 'a' | return ['sat'] | endif
endfunction

function! Day(date)
  if a:date > 31 && a:date < 1 && !match(
    \ ['mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun'], '\c' . a:date)
    return
  endif
  let now = system("date +'%b %-e, %Y 00:00:00 +000 + '")
  let l:now = strpart(l:now, 0, len(l:now) - 1)
  let offset = 86400

  while 1
    let day = system('date --date "' . l:now
      \ . l:offset . ' seconds" ' . "+'%b %-e %a'")
    if match(l:day, '\c\W' . a:date . '\W') != -1
      echo system('date --date "' . l:now
        \ . l:offset . ' seconds" ' . "+'[%Yw%W] %b %-e %a'")
      return
    endif
    let l:offset += 86400
  endwhile
endfunction

command! -nargs=1 Inf call Inform(<f-args>)
function! Inform(data)
  let info = 'No matching info.'
  let otherinfo = []
  if match(['wifi', 'pass'], a:data) != -1 | let l:info = '6106e0ce4f'
  elseif match(['phone', 'tel'], a:data) != -1 | let l:info = '3176-6107'

  elseif match(['ifconfig', 'wlan'], a:data) != -1
    let l:info = "sudo ifconfig wlan0"
    call add(l:otherinfo, "iwlist wlan0 scanning")
    call add(l:otherinfo, "sudo iwconfig wlan0 essid GVT-6E0F")
    call add(l:otherinfo, "sudo dhclient wlan0")

  elseif match(['tar, gz'], a:data) != -1
    let l:info = 'tar xvzf filename.tar.gz'
    call add(l:otherinfo, 'tar it all')
  elseif match(['moon', 'moonc'], a:data) != -1
    let l:info = 'moonc -t "$HOME/.vim/bundle/hudmode-vim/core" .'
    call add(l:otherinfo, 'execute command from ~/goldfeld/hudmode/core')
  elseif match(['restart'], a:data) != -1
    let l:info = 'sudo service network-manager restart'
    call add(l:otherinfo, 'then toggle hardware wireless switch')
  elseif match(['apache', 'stop'], a:data) != -1
    let l:info = 'sudo /etc/init.d/apache2 stop'
    call add(l:otherinfo, 'noop')
  elseif match(['chmod', 'permission', 'executable', 'exe'], a:data) != -1
    let l:info = 'chmod +x filename'
  elseif match(['nodejs', 'node'], a:data) != -1
    let l:info = 'rm -r bin/node bin/node-waf include/node lib/node lib/pkgconfig/nodejs.pc share/man/man1/node.1'
    call add(l:otherinfo, 'noop')


  elseif match(['heroku', 'buildpack'], a:data) != -1
    let appname = input("enter your app's name: ")
    echo "\n"
    let l:info = "heroku create ".l:appname." --stack cedar --buildpack https://github.com/oortcloud/heroku-buildpack-meteorite.git"
    call add(l:otherinfo, "then do 'heroku login'")

  elseif match(['watch', 'inotify'], a:data) != -1
    let l:info = "echo 10000 > /proc/sys/fs/inotify/max_user_watches"
    call add(l:otherinfo, "should 'sudo su' first")
    call add(l:otherinfo, "may also need to pipe to max_user_instances")
    call add(l:otherinfo, "after done, do 'exit'")

  elseif match(['ssh', 'publickey', 'keygen'], a:data) != -1
    let email = input("enter email for publickey: ")
    echo "\n"
    let l:info = 'ssh-keygen -t rsa -C "'.l:email.'"'
    call add(l:otherinfo, "just press enter when prompted for file in which to save")
    call add(l:otherinfo, "use <Leader>.k to xclip the key")
  endif

  let @* = l:info
  let @+ = l:info
  echo l:info
  for other in l:otherinfo
    echo '# '.other
  endfor
  return
endfunction
"}}}
"{{{1 MISC
" http://learnvimscriptthehardway.stevelosh.com/chapters/12.html
" http://learnvimscriptthehardway.stevelosh.com/chapters/14.html
" http://forrst.com/posts/Adding_a_Next_Adjective_to_Vim_Version_2-C4P#comment-land
" http://learnvimscriptthehardway.stevelosh.com/chapters/38.html
" https://github.com/amikula/vim_flashcards/blob/master/all_cards.txt

function! LoadSession()
  if filereadable($HOME . "/.vim/Session.vim")
    execute "source " . $HOME . "/.vim/Session.vim"
  endif
endfunction

" save session on exit.
autocmd VimLeave * nested if (!isdirectory($HOME . "/.vim")) |
  \ call mkdir($HOME . "/.vim") |
  \ endif |
  \ execute "mksession! " . $HOME . "/.vim/Session.vim"

" From vimrc_example.vim distributed with Vim 7.
" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
  \ if line("'\"") > 1 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif

" Put these in an autocmd group, so that we can delete them easily.
augroup vimrcEx
au!
" For all text files set 'textwidth' to 78 characters.
autocmd FileType text setlocal textwidth=78
" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
" Also don't do it when the mark is in the first line, that is the default
" position when opening a file.
autocmd BufReadPost *
  \ if line("'\"") > 1 && line("'\"") <= line("$") |
  \   exe "normal! g`\"" |
  \ endif
augroup END

" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
    \ | wincmd p | diffthis
endif

" meta-vim
augroup HELP
  autocmd!
  autocmd filetype help nnoremap <buffer><C-M> <C-]>
  autocmd filetype help nnoremap <buffer>H <C-T>
  autocmd filetype help nnoremap <buffer>L :tag<CR>
  autocmd filetype help nnoremap <buffer>q :q!<CR>
  autocmd filetype help setlocal nonumber
augroup END
function! ListLeaders()
  silent! redir @r
  silent! nmap <Leader>
  silent! redir END
  silent! new
  silent! put! a
  silent! g/^s*$/d
  silent! %s/^.*,//
  silent! normal ggVg
  silent! sort
  silent! let lines = getline(1,"$")
endfunction

" don't mess up splits when resizing vim
autocmd VimResized * wincmd =
"}}}
"{{{1 UNFINISHED OR UNUSED

"inoremap <C-I> <C-O>:WriterMode<CR>

function! WriterTimestamp()
  let date = system('date +%s%N | cut -b1-13')
  return strpart(l:date, 0, len(l:date) - 1)
endfunction

command! -nargs=0 WriterMode call WriterMode()
function! WriterMode()
  let g:writerModeStart = WriterTimestamp()
  let esc = maparg('<Esc>')
  let bksp = maparg('<Backspace>')
  if l:esc
    let g:writerModeStoreEsc = l:esc
    unmap <Esc>
  endif
  if l:bksp
    let g:writerModeStoreBksp = l:bksp
    unmap <Backspace>
  endif
  inoremap <silent> <Esc> <Esc>:WriterModeEnd<CR>
  inoremap <silent> <Backspace> <NOP>
  startinsert
endfunction

command! -nargs=0 WriterModeEnd call WriterModeEnd()
function! WriterModeEnd()
  let g:writerModeEnd = WriterTimestamp()
  echo 'hey'
  unmap <Esc>
  unmap <Backspace>
  if g:writerModeStoreEsc | execute 'nnoremap | endif
  "echo 'Writing started at '.g:writerModeStart.', '.g:writerModeEnd.' elapsed; X words typed, 180wpm'
endfunction

nnoremap <Backspace> :call Backspace()<CR>
function! Backspace()
  " need to check the current word
  " if we're at the end of it, need to 
  " leave us at the end of the previous.

  " also remap <Enter> to 'ea' or 'eal'

  " get current column
  let cursor = getpos('.')[2]
  " get line text
  let line = getline('.')
  " if we're not at the beginning of the word, go to it.
  if !(l:line[l:cursor - 2] =~ '\s')
    execute "normal! b"
  endif

  let newcursor = getpos('.')[2]
  let delta = l:cursor - l:newcursor
  execute "normal! dawb"

  let max = len(expand("<cword>")) - 1
  let move = 0
  if l:max > l:delta
    let l:move = l:delta
  else
    let l:move = l:max
  endif
  
  execute "normal! ".l:move."l"
endfunction

" vim-around: type an opening ({['"<TAG> etc and press C-S + text object
" to wrap the text around with a closing object.
" have to test it for programming Rust as an alternative to paredit.
let s:around = { '(': ')', '{': '}', '[': ']', '"': '"', "'": "'" }
function! Around(type)
  let pos = col('.')
  let open = getline('.')[l:pos - 1]
  let close = get(s:around, l:open, '')
  if len(l:close)
    normal! "']i" . l:close
  "else we need to deal with a multi-char html tag.
  endif
  call cursor('.', l:pos)
endfunction
inoremap <C-S> <Esc>:set opfunc=Around<CR>g@

" operator-pending
onoremap in( :<C-U>normal! f(vi(<CR>
onoremap in< :<C-U>normal! f<vi<<CR>
onoremap in[ :<C-U>normal! f[vi[<CR>
onoremap ih :<C-U>execute "normal! ?^==\\+$\r:noh\rkvg_"<CR>
onoremap ah :<C-U>execute "normal! ?^==\\+$\r:noh\rg_vk0"<CR>

onoremap aa :<C-U>call AttrTextObj()<CR>

function! AttrTextObj()
  let res = searchpair("\['\"\]", "", "\['\"\]")
  if l:res == 0 || l:res == -1
    normal! f=
    let res = searchpair("\['\"\]", "", "\['\"\]")
    echo res
  endif
  execute 'normal! v' 
endfunction

inoremap <A-C> <A-U>

nnoremap <C-;> yl:execute "normal! f" . @"<CR>
nnoremap <C-:> yl:execute "normal! F" . @"<CR>

"let g:ctrlp_extensions = ['commitdriven']
"noremap <C-T> :CommitDriven<CR>
":noremap <Leader><Leader> :CommitDrivenLeader<CR>

let g:ctrlp_user_command =
  \ ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']
let g:ctrlp_prompt_mappings = {
  \ 'PrtBS()': ['<c-h>', '<c-]>'],
  \ 'PrtCurLeft()': ['<left>'],
  \ }

" go to the function name from within a function
let expr = '\(^fun\S* \)\@<=[^f][^u][^n]\w\+\<Bar>^\w\+'
execute "nnoremap <C-F> ?".expr."<CR>"

nnoremap <silent> <Leader>A :call Sass()<CR>
function! Sass()
  " get current column
  let cursor = getpos('.')[2]
  " get line text
  let line = getline('.')

  let beginning = ''
  let word = expand("<cWORD>")
  if stridx(l:word, '(') != -1
    " if we're not at the beginning of the sass invocation, go to it.
    if !(l:line[l:cursor - 2] =~ '\s')
      let l:beginning = 'B'
    endif
  else
    let l:beginning = 'F(B'
  endif

  execute 'normal! ' . l:beginning . 'vf)"qy'
  let output = system('echo "' . @q . '" | sass -i | xargs echo')
  let @q = strpart(l:output, matchend(l:output, @q) + 1, 7)
  execute 'normal! gv"qp'
endfunction

" can't map <C-I> to anything else since it's the same as <Tab>.
nnoremap <Tab> :CtrlPBuffer<CR>

"nnoremap <Leader>* :set hls<CR>:AutoHighlightToggle<CR>
command! -nargs=0 AutoHighlightToggle call AutoHighlightToggle()
function! AutoHighlightToggle()
  let @/ = ''
  if exists('#auto_highlight')
    au! auto_highlight
    augroup! auto_highlight
    setl updatetime=4000
    echo 'Highlight current word: off'
    return 0
  else
    augroup auto_highlight
      au!
      au CursorHold * let @/ = '\V\<'.escape(expand('<cword>'), '\').'\>'
    augroup end
    setl updatetime=500
    echo 'Highlight current word: ON'
    return 1
  endif
endfunction

":noremap <Space> :LineSeekToggle<CR>
command! -nargs=0 LineSeekToggle call LineSeekToggle()
let LineSeek = 0
let seq = '1234567890'
let qes = '!@#$%^&*()'
let seqlen = len(seq)
function! LineSeekToggle()
  if g:LineSeek == 0
    let g:LineSeek = 1
    let i = 0
    while i < g:seqlen
      execute 'nmap '.g:seq[i].' :<C-U>LineSeek '.g:seq[i].'<CR>'
      execute 'nmap '.g:qes[i].' :<C-U>LineSeekBack '.g:qes[i].'<CR>'
      let i = i + 1
    endwhile
  else
    let g:LineSeek = 0
    let i = 0
    while i < g:seqlen
      execute 'unmap '.g:seq[i]
      execute 'unmap '.g:qes[i]
      let i = i + 1
    endwhile
  endif
endfunction

command! -nargs=0 Streamline call Streamline(v:count)
function! Streamline(target)
  let lnum = line('.')
  let lenlnum = len(l:lnum)
  let relativeness = len(l:lnum) - len(a:target)

  let base = l:lnum[: l:relativeness - 1]
  
  let abstarget = l:base.a:target
  if l:abstarget <= l:lnum
    let l:abstarget = (l:base + 1).a:target
  endif
  execute 'normal! '.l:abstarget.'G'
endfunction

command! -nargs=0 StreamlineBack call StreamlineBack(v:count)

function! StreamlineBack(target)
  let lnum = line('.')
  let lenlnum = len(l:lnum)
  let relativeness = len(l:lnum) - len(a:target)
  let base = l:lnum[: l:relativeness - 1]
  
  let abstarget = l:base.a:target
  if l:abstarget >= l:lnum
    let l:abstarget = (l:base - 1).a:target
  endif
  execute 'normal! '.l:abstarget.'G'
endfunction

command! -nargs=1 LineSeek call LineSeek(<f-args>)
command! -nargs=1 LineSeekBack call LineSeekBack(<f-args>)
function! LineSeek(num)
  let lnum = line(".")
  let lennum = len(l:lnum)

  if l:lnum < 10
    if l:lnum[lennum - 1] >= a:num
      let l:base = 1
    else
      let base = 0
    endif
  else 
    let base = l:lnum[: lennum - 2]
    if l:lnum[lennum - 1] >= a:num
      let l:base = l:base + 1
    endif
  endif
  let dest = l:base . a:num
  execute ":".dest
endfunction
function! LineSeekBack(num)
  let lnum = line(".")
  let lennum = len(l:lnum)
  let num = g:seq[ match(g:qes, a:num) ]

  if l:lnum < 10
    if l:lnum[lennum - 1] <= l:num
      execute ":0"
      return
    else
      let base = 0
    endif
  else 
    let base = l:lnum[: lennum - 2]
    if l:lnum[lennum - 1] <= l:num
      let l:base = l:base - 1
    endif
  endif
  let dest = l:base . l:num
  execute ":".dest
endfunction

" put all this in your .vimrc or a plugin file
command! -nargs=* Stab call Stab()
function! Stab()
  let l:tabstop = 1 * input('set shiftwidth=')

  if l:tabstop > 0
    " do we want expandtab as well?
    let l:expandtab = confirm('set expandtab?', "&Yes\n&No\n&Cancel")
    if l:expandtab == 3
      " abort?
      return
    endif

    let &l:sts = l:tabstop
    let &l:ts = l:tabstop
    let &l:sw = l:tabstop

    if l:expandtab == 1
      setlocal expandtab
    else
      setlocal noexpandtab
    endif
  endif

  " show the selected options
  try
    echohl ModeMsg
    echon 'set tabstop='
    echohl Question
    echon &l:ts
    echohl ModeMsg
    echon ' shiftwidth='
    echohl Question
    echon &l:sw
    echohl ModeMsg
    echon ' sts='
    echohl Question
    echon &l:sts . ' ' . (&l:et ? '  ' : 'no')
    echohl ModeMsg
    echon 'expandtab'
  finally
    echohl None
  endtry
endfunction

nnoremap <leader>.p :call ShowingHNParse()<CR>
function! ShowingHNParse()
  let file = '"'. expand('$HOME/Dropbox/showhn') .'"'
  let output = '"'. expand('$HOME/result') .'"'
  let parsed = system("awk 'BEGIN {RS = ".'"[<>]"'."} NR == 2 {print}' ".file."")
  echo parsed
endfunction

function! Viminder()
  let [date, time] = split(system("date +'%Y%m%d_%T'"), '_')
  let punchcard = '"' . expand('$HOME') . '/punchcard"'
  echo punchcard
  let awk = "awk '"
    \ . "$1 ~ /".date."/ {print ".'"hey"'.", $0;"
      \ . " print ".'"hay"'.", $0 > ".punchcard."}"
    \ . " $1 !~ /".date."/ {print > ".punchcard."}"
    \ . "' ~/punchcard"
  let sys = system(l:awk)
  echo sys
endfunction
"}}}1
"
