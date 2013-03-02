" Vundle setup
filetype off
set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'lordm/vim-browser-reload-linux'

" workflow
Bundle 'jceb/vim-orgmode'
Bundle 'goldfeld/vimdow'

" editing
Bundle 'tpope/vim-surround'

" files
Bundle 'tpope/vim-fugitive'
Bundle 'vim-scripts/LustyJuggler'
"Bundle 'spolu/dwm.vim'

" moving
Bundle 'Lokaltog/vim-easymotion'
Bundle 'goldfeld/vim-seek'

" syntax
Bundle 'kchmck/vim-coffee-script'
Bundle 'leafo/moonscript-vim'
Bundle 'vim-scripts/Vim-R-plugin'
Bundle 'goldfeld/criticmarkup-vim'

" colorschemes
Bundle 'croaker/mustang-vim'
Bundle 'morhetz/gruvbox'

filetype plugin indent on

set runtimepath^=~/.vim/bundle/ctrlp.vim

color slate
"color gruvbox
color mustang

set cursorline
hi CursorLine guibg=#373737 ctermbg=236
hi ColorColumn guibg=#373737 ctermbg=236
hi CursorColumn guibg=#373737 ctermbg=236

if filereadable(expand("~/punchcard"))
  autocmd BufRead,BufNewFile *.coffee,*.js,*.html,*.css setlocal noexpandtab
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.user,*.nupkg,*.dll,*.xml,*.config,*.suo,*.sln,*.asax,*.cs,*.transform,*.ttf,*.ico,*._,*.c,*.h,*.mk,*.js
else
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.user,*.nupkg,*.dll,*.xml,*.config,*.suo,*.sln,*.asax,*.cs,*.transform,*.ttf,*.ico,*._,*.c,*.h,*.mk
endif
set hidden                      " allows switching buffer without saving and keeps undo history.
set tabstop=2                   " number of spaces of tab character
set shiftwidth=2                " number of spaces to (auto)indent
set expandtab
set scrolloff=3                 " keep 3 lines when scrolling
set backspace=indent,eol,start  " allow backspacing over everything in insert mode
set hlsearch                    " hightlight searches
set incsearch                   " do incremental searching
set ignorecase					 			
set smartcase							 			" ignore case of search only if all lowercase
set smarttab                    " insert tabs on start of line according to shiftwidth, not tabstop
set autoread                    " auto reload changed files if there's no conflict
set number
set autoindent
set smartindent

let &colorcolumn=join(range(81, 201), ",")
augroup filetypeSettings
	autocmd!
	autocmd BufRead,BufNewFile *.md setlocal colorcolumn=0
  autocmd BufRead,BufNewFile *.tnt.* setlocal expandtab
augroup END

set laststatus=2
set statusline=
set statusline+=%{fugitive#statusline()}
set statusline+=\ %m
set statusline+=%h
set statusline+=%{GetModifiedBuffers()}

function! GetModifiedBuffers()
	redir @b
	silent! buffers
	redir END
	return system('echo "'.@b.'"' . " | awk '$3 ~ /\\+/ {printf ".'"  " $4 "*"'."}'")
endfunction

" gvim behave like vim: console tabs and no dialogs, menus or scrollbars
set guioptions+=lrbmTLce
set guioptions-=lrbmTLce
set guioptions+=c
" modelines could be exploited maliciously

let mapleader = ","

let g:TNTWebBrowser = 'luakit'

" skip past big lines
nnoremap gj j
nnoremap gk k

 " go to last inserted but don't leave me in insert mode
nnoremap gi gi<Esc>

nnoremap <silent> <Space><Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
nnoremap ge gE
nnoremap Y y$

" insert two en-dashes (&#8211)
inoremap <C-D> ––

nnoremap <silent> j :call RestrainCommand('j', "Streamline")<CR>
nnoremap <silent> k :call RestrainCommand('k', "StreamlineBack")<CR>
nnoremap <silent> h :call RestrainCommand('h', "")<CR>
nnoremap <silent> l :call RestrainCommand('l', "")<CR>

augroup restrainCommand
	autocmd!
	autocmd CursorMoved * call CheckCurrentCommand()
augroup END

let g:currentCommand = ''
let g:lastCommand = ''
function! RestrainCommand(cmd, doublePressCmd)
	if g:lastCommand == a:cmd
		execute a:doublePressCmd
	else
		execute 'normal! '.a:cmd
	endif
	let g:currentCommand = a:cmd
endfunction

function! CheckCurrentCommand()
  let g:lastCommand = ''
  if g:currentCommand != ''
    let g:lastCommand = g:currentCommand
  endif
  let g:currentCommand = ''
endfunction

nnoremap <silent> <Space>w :TNTTriggerSession<CR>
nnoremap <silent> <Space>W :TNTCreateWebpage<CR>
nnoremap <silent> <Space>m :execute "normal! a{>>". TNTTimestamp() ."<<}"<CR>
nnoremap <silent> <Space>t :echo 'cmd-t for folds'<CR>

" go to current heading's next sibling.
noremap <silent> <Space>j :call TNTGoNextSibling()<CR>
" go to current heading's previous sibling.
noremap <silent> <Space>k :call TNTGoPreviousSibling()<CR>
" go to current subtree's heading.
noremap <silent> <Space>h [z
" go to next subtree's heading.
noremap <silent> <Space>n ]zj

" go to first heading of a lower level than the current.
noremap <silent> <Space>l :call TNTGoFirstLower('j')<CR>
" go to current subtree's last open item.
noremap <silent> <Space>e ]z
" go to current tree's (whole tree from root) last open item.
"noremap <silent> <Space>E

" go to 0th-level heading of current fold, or nth-level with a count.
noremap <silent> <Space>H :<C-U>call TNTGoTopLevelHeading(v:count)<CR>
" go back to find the first heading of a lower level than the current.
noremap <silent> <Space>L :call TNTGoFirstLower('k')<CR>

" insert a new parent for the current item. 
"nnoremap <silent> <Space>i
" insert a new absolute parent for the current item, given a count.
" by default, if you don't supply a count, 1 is used, meaning it will insert
" a parent at the level below root--this is more useful in tnt, where you'll
" want to add a new top-level trigger when priorities change--but you can
" change it to another number such as 0 by using `let g:TNTIDefaultCount = 0`.
"nnoremap <silent> <Space>I

" add a new child to the current fold, prepending it to the list. 
"nnoremap <silent> <Space>a
" append a new child to the current fold.
"nnoremap <silent> <Space>A
" prepend a new sibling to the current fold's parent.
"nnoremap <silent> <Space>o
" append a new sibling to the current fold's parent.
"nnoremap <silent> <Space>O

" open all folds on current block.
nnoremap <silent> <Space>bo {v}zo
" close all folds on current block.
nnoremap <silent> <Space>bc {v}zc
" minimize all folds on current block.
nnoremap <silent> <Space>bm {v}zmzc
" open all threads on current block.
nnoremap <silent> <Space>bt {v}zmzo

" demote current heading.
nnoremap <silent> <Space>>> zc>>zo
" promote current heading.
nnoremap <silent> <Space><< zc<<zo
" move current fold down one position.
nnoremap <silent> <Space>mj zcddpzo
" move current fold up one position.
nnoremap <silent> <Space>mk zcddkPzo

function! TNTGoPreviousSibling()
	let column = getpos('.')[2]
	let heading = IndentLevel(line('.'))
	execute 'normal! k'
	let prev = line('.')
	if IndentLevel(prev) != heading || foldclosed(prev) == -1
		while IndentLevel(line('.')) > heading
			execute 'normal! [z'
		endwhile
		execute 'normal! '.column.'|'
		"call cursor(line('.'), column)
	endif
endfunction

function! TNTGoNextSibling()
	let current = line('.')
	execute 'normal! j'
	let next = line('.')
	if IndentLevel(next) > IndentLevel(current)
		execute 'normal! ]zj'
	endif
endfunction

function! TNTGoFirstLower(direction)
  let startindent = IndentLevel(line('.'))
  execute 'normal! ' . a:direction
  let current = line('.')
  let currentindent = IndentLevel(current)
  while currentindent <= startindent
    \ || matchstr(getline(current), g:tntWebpageRegex) != ''
    if currentindent < startindent | return | endif
    execute 'normal! ' . a:direction
    let l:current = line('.')
    let l:currentindent = IndentLevel(current)
  endwhile
endfunction

function! TNTGoTopLevelHeading(upto)
  echo a:upto
  while IndentLevel(line('.')) > a:upto
    execute 'normal! [z'
  endwhile
endfunction

"syn region FoldFocus start="\(\([^\r\n]*[\r\n]\)\{6}\)\@<=\s*\S" end="\[^\r\n]*[\r\n]\(\([^[\r\n]*[\r\n]\)\{2}\)\@="
"hi FoldFocus gui=bold guifg=LimeGreen cterm=bold ctermfg=Green
"hi def link FoldFocus FoldFocus

function! TNTTimestamp()
  if TNTCheckBashUtility('ruby')
    let date = system("ruby -e 'puts Time.now.to_f'")
    return strpart(substitute(l:date, '\.', '', 'g'), 0, len(l:date) - 4)
  else
    let date = system('date +%s%N | cut -b1-13')
    return strpart(l:date, 0, len(l:date) - 1)
  endif
endfunction

function! TNTTimestampN(cmd)
  let date = TNTTimestamp()
  let lnum = line('.')
  execute "normal! ".a:cmd

  execute "s/$/ {>>". l:date ."<<}/"
  execute "normal! 0"
  startinsert
endfunction

function! TNTTimestampI(cmd)
  let date = TNTTimestamp()
  call feedkeys(a:cmd.l:date)
  execute 'normal! '.cmd.l:date
endfunction

" code from Vim The Hard Way
function! IndentLevel(lnum)
  return indent(a:lnum) / &shiftwidth
endfunction
function! NextNonBlankLine(lnum)
  let numlines = line('$')
  let current = a:lnum + 1
  while current <= numlines
    if getline(current) =~? '\v\S' | return current | endif
    let current += 1
  endwhile
  return -2
endfunction

function! TNTFoldExpr(lnum)
  if getline(a:lnum) =~? '\v^\s*$' | return -1 | endif
  let this_indent = IndentLevel(a:lnum)
  let next_indent = IndentLevel(NextNonBlankLine(a:lnum))
  if next_indent == this_indent | return this_indent
  elseif next_indent < this_indent | return this_indent
  elseif next_indent > this_indent | return '>' . next_indent
  endif
endfunction

function! TNTChildren(...)
  let [lnum, filter] = [0, '']
  if a:0 == 0 | return | endif
  if a:0 == 2 | let l:filter = a:2 | endif
  let l:lnum = a:1

  let parent = IndentLevel(l:lnum)
  let i = 1
  let indent = IndentLevel(l:lnum + i)
  let children = []
  if l:filter == ''
    while indent > parent
      if indent == parent + 1 | call add(children, l:lnum + i) | endif
      let i = i + 1
      let indent = IndentLevel(l:lnum + i)
    endwhile

  else
    while indent > parent
      if indent == parent + 1
        if match(getline(l:lnum + i), l:filter) != -1
          call add(children, l:lnum + i)
        endif
      endif
      let i = i + 1
      let indent = IndentLevel(l:lnum + i)
    endwhile
  endif
  return l:children
endfunction

function! TNTHumanDate(line)
  echo a:line
  return
  return substitute(a:line, '{>>\([^<]*\)<<}\s*$', '\=submatch(0)')
endfunction

" go to the function name from within a function
let expr = '\(^fun\S* \)\@<=[^f][^u][^n]\w\+\<Bar>^\w\+'
execute "nnoremap <Leader>f ?".expr."<CR>"

let g:tntWebpageRegex = '^\s*\((\d\d\d\?%)\)\?'
  \ . '\[[^\]]*\]\[[^\]]*\]'
  \ . '\s*\({>>\d*<<}\)\?\s*$'

command! -nargs=0 TNTTriggerSession call TNTTriggerSession(line('.'))
function! TNTTriggerSession(lnum)
	let browser = get(g:, 'TNTWebBrowser', '')
	if !len(browser)
		echom 'Please set your web browser by having e.g."'
			\ . "let g:TNTWebBrowser = 'google-chrome'" . '" in your vimrc.'
		return
	endif
	let webpages = TNTChildren(a:lnum, g:tntWebpageRegex)
	if !len(webpages) | return | endif
	let links = ''
	for page in webpages
		let link = matchstr(getline(page), '\[http\S*\]')
		let l:links = l:links . ' ' . strpart(link, 1, len(link) - 2)
	endfor
	call system(browser.l:links)
endfunction

function! TNTSessionTriggerSymbol(lnum)
	if len(TNTChildren(a:lnum, g:tntWebpageRegex))
		return ':'
	else | return ''
	endif
endfunction

let g:TNTFoldCache = {}
function! TNTFoldText(...)
  if a:0 == 1 | let current = a:1
  else | let current = v:foldstart
  endif
  let line = getline(l:current)
	" the label will be our final folded text

  " a thread begins with a quote followed optionally by pairs of quotes.
  if l:line =~? '^\s*"\([^"]*"[^"]*"\)*[^"]*$'
		let children = len(TNTChildren(l:current))
    let l = matchstr(getline(l:current + 1), '\S[^{]*')
		let lindent = strpart(matchstr(getline(l:current + 1), '^\s*'), 2)
		let prelabel = TNTSessionTriggerSymbol(l:current + 1)
    " make it optional for threads to show their content with a special symbol
    " in front of them, e.g. the double quote or a bang
    "let l:l = substitute(l:l, '\(^\s*\)\@<=\s\S\@=', '!', '')
    "return strpart(l:l, 1)
		if l:line =~ '^\s*"!\d*'
			" get how many chars we should ensure (padding formatting).
			let chars = strpart(matchstr(l:line, '"!\d*'), 2)
			" get the whole thread title up until it's timestamp, and add padding,
			" and add padding.
			let l:label = matchstr(l:line, '"![^{]*') . repeat(' ', chars)
			" extract the actual title and format to the size constraint.
			let l:label = strpart(l:label, 4, chars) . ' '
			return l:lindent . l:prelabel . l:label . l:l . '['.children.']'
		endif
    return l:lindent . l:prelabel . l:l . '['.children.']'

  " a randomizer thread begins with a percent sign (whatever else does?)
  elseif l:line =~? '^\s*%'
    let label = get(g:TNTFoldCache, l:current, '')
    if l:label == ''
      let children = TNTChildren(l:current)
      let number = strpart(TNTTimestamp(), 5) + system('sh -c "echo -n $RANDOM"')
      let random = l:number % len(l:children)
			let child = l:children[random]

			let prelabel = TNTSessionTriggerSymbol(child)
      let label = strpart(TNTFoldText(child), 2)
      let g:TNTFoldCache[l:current] = prelabel . l:label
    endif
    return l:label
	
	" a note begins with a hash, and we'd like to show it's contents' word count.
	elseif l:line =~? '^\s*!\?#'
		let lindent = matchstr(getline(l:current), '^\s*')
		return lindent . matchstr(getline(l:current), '\S[^{]*') . '('
			\ . TNTWordCountRecursive(l:current) . ' words)'
  endif

  return getline(l:current)
endfunction

function! TNTWordCount(lnum)
	" remove one to account for tnt timestamp.
	return len(split(getline(a:lnum), '\s')) - 1
endfunction

function! TNTWordCountRecursive(lnum)
	let wc = get(g:TNTFoldCache, a:lnum, 0)
	if l:wc == 0
		let children = TNTChildren(a:lnum)
		for child in children
			let l:wc += TNTWordCountRecursive(child)
		endfor
		let l:wc += TNTWordCount(a:lnum)
		let g:TNTFoldCache[a:lnum] = l:wc
	endif
	return l:wc
endfunction

augroup TNT
  autocmd!
  au BufRead,BufNewFile *.tnt.* call TNTAutocmds()
  " temporarily switch to manual folding when entering insert mode,
  " so that adjacent folds won't inaverdtently open when we create new folds.
  au InsertEnter *.tnt.* let w:last_fm=&foldmethod | setlocal foldmethod=manual
  au InsertLeave *.tnt.* let &l:foldmethod=w:last_fm
augroup END

function! TNTAutocmds()
  setlocal foldmethod=expr
  setlocal foldexpr=TNTFoldExpr(v:lnum)
  setlocal foldtext=TNTFoldText()
  setlocal foldopen=search,mark,percent,quickfix,tag,undo
  nnoremap <silent> <buffer> o :call TNTTimestampN('o')<CR>
  nnoremap <silent> <buffer> O :call TNTTimestampN('O')<CR>
  "inoremap <silent> <buffer> <CR> :call TNTTimestampI("\<CR>")<CR>
endfunction

command! -nargs=0 TNTCreateWebpage call TNTCreateWebpage()
function! TNTCreateWebpage()
  " get curront column
  let cursor = getpos('.')[2]
  " get text on the character next to current position.
  let next = (getline('.'))[cursor]
  " if we're not at the end of the url.
  let adjust = ''
  if len(next) && !(next =~ '\s') | let l:adjust = 'E' | endif
  execute "normal! " . adjust . "a]\<Esc>yBi["

  " our normal call above used the y operator to capture the url.
  let url = @"

  let engines = { 'google': '\v(google\.com(\....?)?\/search\?q\=)@<=(\S*)' }
  let engine = ''
  let query = ''
  " if our url is a query in one of the listed engines, we treat it differently.
  for pattern in items(engines)
    let query = matchstr(url, pattern[1])
    let engine = pattern[0] . ':'
  endfor

  if query != ''
    " we simply url-decode the query and prepend the engine's name, and we
    " have our title.
    let title = engine . system('echo "' . query . '"'
    \ . ' | echo -e "$(sed ' . "'s/%/\\\\x/g'" . ')"')
  else
    let recordSeparator = "<[\s]*.?[\s]*title[\s]*>"
    " note that for my sanity's sake I treat the closing slash on the title
    " tag as simply any character that doesn't even actually need to be there.
    " you can see I just open and close with <title>, in effect it should not
    " make any difference and spares me from awk/perl/bash escaping nightmare.
    let awkscriptBase = "awk 'BEGIN {RS=" . '"' . recordSeparator . '"}'
      \ . " ; {if (FNR == 2) print "
    " grab the title and make sure we guard against some evil saboteur putting
    " backticks instead of single quotes on his webpage's title.
    let title = substitute(system('curl ' . url . ' | ' . awkscriptBase
      \ . '"<title>"' . ' $0 ' . '"<title>"' . "}'"), '[`"]', "'", 'g')

    " we're gonna try a few tools to decode the html entities.
    if TNTCheckBashUtility('php')
      let decode = " | php -r 'echo html_entity_decode(fgets(STDIN),"
        \ " ENT_NOQUOTES, " . '"UTF-8"' . ");'"
    elseif TNTCheckBashUtility('recode')
      let decode = " | recode HTML_4.0"
    else | let decode = ''
    endif

    " we pass it through awk again since curl gives noisy output, like an ogre.
    let title = system('echo "' . title . '"'
    \ . ' | ' . awkscriptBase . "$0}'" . decode)
  endif

  let title = system('echo "' . title . '"' . " | tr -d '\n*'"
    \ . " | sed -e 's/^[[:space:]\t]*//;s/[[:space:]\t]*$//'")
  " finish the markdown link now that we (hopefully) have the title
  execute "normal! a".title."]["
endfunction

function! TNTCheckBashUtility(name)
  let result = system('hash '.a:name.' 2>/dev/null'
    \ . ' || { echo >&1 "not available"; exit 1; }')
  if result =~ 'not available' | return 0
  else | return 1
  endif
endfunction

function! WriterTimestamp()
  let date = system('date +%s%N | cut -b1-13')
  return strpart(l:date, 0, len(l:date) - 1)
endfunction
"nnoremap <Leader>I :WriterMode<CR>
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
  if g:writerModeStoreEsc | execute 'nnoremap
  "echo 'Writing started at '.g:writerModeStart.', '.g:writerModeEnd.' elapsed; X words typed, 180wpm'
endfunction

nnoremap <Backspace> :call Backspace()<CR>
function! Backspace()
	" need to check the current word
	" if we're at the end of it, need to 
	" leave us at the end of the previous.

	" also remap <Enter> to 'ea' or 'eal'

	" get curront column
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

onoremap <Space> iw

" sacrifice the m mark for my pinkies' sake;
nnoremap mm :
" w mark for quick saving;
nnoremap mw :w<CR>
" v mark for vertical splits
nnoremap mv :vs<CR>
" s mark for horizontal splits
nnoremap ms :sp<CR>

" repurpose the colon as my comma lost to leader.
nnoremap : ,

" visual shifting (relect after shift).
vnoremap < <gv
vnoremap > >gv

nnoremap - $
onoremap - $
" step back one char so it doesn't include the newline character.
vnoremap - $h

 " save file opened without sudo after the fact
cmap w!! w !sudo tee % >/dev/null

noremap <silent> K :execute "normal i".nr2char(getchar())<CR>

" paste from clipboard
"set clipboard=unnamed

nnoremap <Leader>[ {o
nnoremap <Leader>] }O

" to go end of textwidth.
nnoremap <Leader>- 81\|
" pull next line and delete any comment symbols.
nnoremap <Leader>J Jldw

" toggle showing hidden characters
nnoremap <Leader>l :set list!<CR>
" useful for uncommenting lines
nnoremap <Leader>i _wi
" output current time and date with year and week, all pretty printed.
nnoremap <silent> <Leader>d :execute "echo system(\"date +'[%Yw%W] %b %-e %a <%H:%M>'\")"<CR>

command! -nargs=0 Sum :5,12!awk '{num = substr($7, 2, length($7) - 4) + substr($8, 2, length($7) - 4); width += num; print} END {print width}'

" common searches
nnoremap <Leader>/h /HEAD<CR>
nnoremap <Leader>/c /console<CR>

" convert time since epoch under cursor to readable time.
nnoremap <Leader>.m :echo FromEpoch(expand("<cword>"))<CR>
function! FromEpoch(date)
	let date = strpart(a:date, 0, len(a:date) - 3)
	return system('date --date "Jan 1, 1970 00:00:00 +000 + '.l:date.' seconds"')
endfunction

" quickly edit my tnt outline
nnoremap <silent> <Leader>.t :e ~/goldfeld/.tnt/lifethreads.tnt.md<CR>
" quickly edit my vimrc.
nnoremap <silent> <Leader>.v :e ~/goldfeld/dotfiles/vimrc<CR>
" source vimrc to allow live reloading of changes.
nnoremap <silent> <Leader>.V :w<CR>:so $MYVIMRC<CR>
" allow left ctrl (which I remap to my Caps Lock key) to act as <Esc> when pressed alone.
nnoremap <silent> <Leader>.x :execute "call system(\"~/./xcape -e 'Control_L=Escape'\")"<CR>
" grab ssh publickey to clipboard.
nnoremap <silent> <Leader>.k :execute "call system(\"xclip -sel clip < ~/.ssh/id_rsa.pub\")"<CR>

nnoremap <silent> <Esc> :noh<CR><Esc>
" toggle uppercase/lowercase
noremap <Leader>u vb~ea
" toggle capitalize first letter
noremap <Leader>U bv~ea

" operator-pending
onoremap in( :<C-U>normal! f(vi(<CR>
onoremap in< :<C-U>normal! f<vi<<CR>
onoremap in[ :<C-U>normal! f[vi[<CR>
onoremap ih :<C-U>execute "normal! ?^==\\+$\r:noh\rkvg_"<CR>
onoremap ah :<C-U>execute "normal! ?^==\\+$\r:noh\rg_vk0"<CR>

inoremap <A-C> <A-U>

nnoremap <C-;> yl:execute "normal! f" . @"<CR>
nnoremap <C-:> yl:execute "normal! F" . @"<CR>

noremap [q :cprevious<CR>
noremap ]q :cnext<CR>

let g:surround_no_mappings = 1
let g:surround_no_insert_mappings = 1
nnoremap d`  <Plug>Dsurround
nnoremap c`  <Plug>Csurround
nnoremap y`  <Plug>Ysurround
nnoremap y~  <Plug>YSurround
nnoremap y`` <Plug>Yssurround
nnoremap y~` <Plug>YSsurround
nnoremap r~~ <Plug>YSsurround
xnoremap ~   <Plug>VSurround
xnoremap g~  <Plug>VgSurround
inoremap <C-S> 	<Plug>Isurround
inoremap <C-G>` <Plug>Isurround
inoremap <C-G>~ <Plug>ISurround

let g:ctrlp_extensions = ['commitdriven']
let g:seek_enable_jumps = 1

" LustyJuggler plugin
noremap <Tab> :LustyJuggler<CR>
noremap <silent> <Leader><Leader> :LustyJugglePrevious<CR>
let g:LustyJugglerDefaultMapping = 0
let g:LustyJugglerKeyboardLayout = 'dvorak'
let g:LustyJugglerShowKeys = 'a'

" vim-fugitive plugin
nnoremap gs :Gstatus<CR>
nnoremap gb :Gblame<CR>
nnoremap gB :Gbrowse<CR>
nnoremap gl :Glog<CR>
nnoremap gn :Ggrep "<cword>"<CR>
vnoremap gn y:Ggrep <C-R>"<CR>

" same as git add the current file.
nnoremap gt :Gwrite<CR>
" same as git checkout the current file, updating buffer.
nnoremap gx :Gread<CR>
" leave me on the index version, so I can quickly check it and close it.
nnoremap gc :Gdiff<CR><C-W>h
" use 'help index' to see vim's built-in natively mapped keys

noremap <C-T> :CommitDriven<CR>
":noremap <Leader><Leader> :CommitDrivenLeader<CR>

nnoremap <C-N> :LightBeam<CR>
command! -nargs=0 LightBeam call LightBeam()
let lightBeam = 0
function! LightBeam()
	if g:lightBeam == 0
		set cursorcolumn
		let g:lightBeam = 1
	else
		set cursorcolumn!
		let g:lightBeam = 0
	endif
endfunction

nnoremap <Leader>* :set hls<CR>:AutoHighlightToggle<CR>
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

nnoremap <Leader>c :Vimdow Chrome<CR>
nnoremap <Leader>h :Vimdow Luakit<CR>
nnoremap <Leader>s :Vimdow fish<CR>
" and compass
nnoremap <Leader>o :Vimdow coffee<CR>
nnoremap <Leader>m :Vimdow meteor<CR>

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

let reloading = 0
command! -nargs=* AutoReload call AutoReload()
function! AutoReload()
	if l:reloading | ChromeReloadStart<CR>
	else | ChromeReloadStop<CR>
	endif
	:let l:reloading = !l:reloading
endfunction
noremap <Leader>R :AutoReload<CR>
noremap <Leader>r :w<CR>:ChromeReload<CR>

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

command! -nargs=1 Inf call Inform(<f-args>)
function! Inform(data)
  let info = 'No matching info.'
  let otherinfo = []
  if match(['wifi', 'pass'], a:data) != -1 | let l:info = '1251025655'
  elseif match(['phone', 'tel'], a:data) != -1 | let l:info = '3176-6107'

  elseif match(['moon', 'moonc'], a:data) != -1
    let l:info = 'moonc -t "$HOME/.vim/bundle/hudmode-vim/core" .'
    call add(l:otherinfo, 'execute command from ~/goldfeld/hudmode/core')
  elseif match(['restart'], a:data) != -1
    let l:info = 'sudo service network-manager restart'
    call add(l:otherinfo, 'then toggle hardware wireless switch')
  elseif match(['chmod', 'permission', 'executable', 'exe'], a:data) != -1
    let l:info = 'chmod +x filename'
  elseif match(['tar'], a:data) != -1
    let l:info = 'tar xvzf filename.tar.gz'

  elseif match(['heroku', 'buildpack'], a:data) != -1
    let appname = input("enter your app's name: ")
    echo "\n"
    let l:info = "heroku create ".l:appname." --stack cedar --buildpack https://github.com/oortcloud/heroku-buildpack-meteorite.git"
    call add(l:otherinfo, "then do 'heroku login'")

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

nnoremap <leader>.p :call ShowingHNParse()<CR>
function! ShowingHNParse()
	let file = '"'. expand('$HOME/Dropbox/showhn') .'"'
	let output = '"'. expand('$HOME/result') .'"'
	let parsed = system("awk 'BEGIN {RS = ".'"[<>]"'."} NR == 2 {print}' ".file."")
	echo parsed
endfunction

nnoremap <leader>.s :call Viminder()<CR>
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

" http://learnvimscriptthehardway.stevelosh.com/chapters/12.html
" http://learnvimscriptthehardway.stevelosh.com/chapters/14.html
" http://forrst.com/posts/Adding_a_Next_Adjective_to_Vim_Version_2-C4P#comment-land
" http://learnvimscriptthehardway.stevelosh.com/chapters/38.html
" https://github.com/amikula/vim_flashcards/blob/master/all_cards.txt
" 

" Go to last file(s) if invoked without arguments.
autocmd VimLeave * nested if (!isdirectory($HOME . "/.vim")) |
    \ call mkdir($HOME . "/.vim") |
    \ endif |
    \ execute "mksession! " . $HOME . "/.vim/Session.vim"

autocmd VimEnter * nested if argc() == 0 && filereadable($HOME . "/.vim/Session.vim") |
    \ execute "source " . $HOME . "/.vim/Session.vim"

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
