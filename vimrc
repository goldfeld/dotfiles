" Vundle setup
filetype off
set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'lordm/vim-browser-reload-linux'

" workflow
Bundle 'jceb/vim-orgmode'
Bundle 'goldfeld/vimdow'
Bundle 'mikewest/vimroom'
Bundle 'goldfeld/vim-micro'

" writing
Bundle 'goldfeld/tnt'

" editing
Bundle 'tpope/vim-surround'

" files
Bundle 'vim-scripts/grep.vim'
Bundle 'tpope/vim-fugitive'
"Bundle 'spolu/dwm.vim'

" moving
Bundle 'bkad/CamelCaseMotion'
Bundle 'goldfeld/vim-seek'

" syntax
Bundle 'kchmck/vim-coffee-script'
Bundle 'jb55/Vim-Roy'
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
highlight CursorLine guibg=#373737 ctermbg=236
highlight ColorColumn guibg=#373737 ctermbg=236
highlight CursorColumn guibg=#373737 ctermbg=236

if filereadable(expand("~/punchcard"))
  autocmd BufRead,BufNewFile *.coffee,*.js,*.html,*.css setlocal noexpandtab
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.user,*.nupkg,*.dll,*.xml,*.config,*.suo,*.sln,*.asax,*.cs,*.transform,*.ttf,*.ico,*._,*.c,*.h,*.mk,*.js,*/build/*
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
set smartcase                   " ignore case of search only if all lowercase
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

let g:Vimdow = {}
augroup DOW
  autocmd BufRead,BufNewFile *.dow call ReadDow()
   autocmd VimEnter * nested if argc() == 0 && filereadable(".dow") |
     \ call ReadDowFile(".dow")
augroup END

function! ReadDowFile(path)
  let lines = readfile(a:path)
  let bufopen = []

  for line in l:lines
    let char = line[0]
    if l:char == '$'
      let exeline = line[1:]
      call system(line[1:])
      if stridx(line, "grunt") != -1
        let escline = substitute(line[1:], '"', '\\"', 'g')
        execute 'nnoremap <Leader>o :call system("' l:escline '"' ")\<CR>"
      endif
    elseif l:char == '.' || l:char == '/'
      call add(bufopen, line)
    endif
  endfor

  for fname in l:bufopen
    execute "badd" fname
  endfor
endfunction

nnoremap <Leader>c :Vimdow Chrome<CR>
nnoremap <Leader>h :Vimdow http<CR>:Vimdow Luakit<CR>
" working terminal
nnoremap <Leader>s :Vimdow bash<CR>:Vimdow fish<CR>:Vimdow @vitoria<CR>
" server task
nnoremap <Leader>u :Vimdow sudo<CR>:Vimdow meteor<CR>
" compiler task
if get(g:Vimdow, 'grunt', 1)
  nnoremap <Leader>o :Vimdow coffee<CR>:Vimdow grunt<CR>:Vimdow compass<CR>
endif

" skip past big lines
nnoremap gj j
nnoremap gk k

 " go to last inserted but don't leave me in insert mode
nnoremap gi gi<Esc>

nnoremap <silent> <Space><Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
nnoremap Y y$

nnoremap yY ggyG
nnoremap dD ggdG

" insert two en-dashes (&#8211)
inoremap <C-D> ––

nnoremap <silent> j
  \ :<C-U>call RestrainCommand('j', "Streamline", v:count1, 2)<CR>
nnoremap <silent> k
  \ :<C-U>call RestrainCommand('k', "StreamlineBack", v:count1, 2)<CR>

" move a line of text using ALT-{j,k}
" bind these to jk and kj (restrained)
nnoremap <M-j> mz:m+<CR>`z
nnoremap <M-k> mz:m-2<CR>`z

nnoremap <silent> h :<C-U>call RestrainCommand('h', "", v:count1)<CR>
nnoremap <silent> l :<C-U>call RestrainCommand('l', "", v:count1)<CR>

" use minus to do an end of line seek on double press,
" and to do a cut short seek in operator pending mode.
nnoremap <silent> - :<C-U>call RestrainCommand('$', "", v:count)<CR>
let g:SeekCutShortKey = '-'

" use underscore to do a beginning of line seek on double press,
" and to do a cut short back seek in operator pending mode.
nnoremap <silent> _ :<C-U>call RestrainCommand('^', "", v:count)<CR>
let g:SeekBackCutShortKey = '_'

nnoremap <silent> \|
  \ :<C-U>call RestrainCommand("\\|", 'LightBeam', v:count, 1)<CR>

" step back one char so it doesn't include the newline character.
vnoremap - $h

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

function! CheckCurrentCommand()
  let g:lastCommand = ''
  if g:currentCommand != ''
    let g:lastCommand = g:currentCommand
  endif
  let g:currentCommand = ''
endfunction

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

onoremap <Space> iw

" sacrifice the m mark for my pinkies' sake;
nnoremap mm :
" w mark for quick saving;
nnoremap mw :w<CR>
" b mark for closing buffer;
nnoremap mb :bd<CR>
" v mark for vertical splits;
nnoremap mv :vs<CR>
" s mark for horizontal splits.
nnoremap mz :sp<CR>

" repurpose the colon as my comma lost to leader.
nnoremap : ,

" visual shifting (relect after shift).
vnoremap < <gv
vnoremap > >gv

 " save file opened without sudo after the fact
cmap w!! w !sudo tee % >/dev/null

noremap <silent> K :execute "normal i".nr2char(getchar())<CR>

" paste from clipboard
"set clipboard=unnamed

" equivalent to :b#
nnoremap <Leader><Leader> <C-^>

nnoremap <Leader>[ {o
nnoremap <Leader>] }O

" to go end of textwidth.
nnoremap <Leader>- 81\|
" pull next line and delete any comment symbols.
nnoremap <Leader>J Jldw
" delete to first char in line, including current char.
nnoremap <Leader>D d^x

" go to the function name from within a function
let expr = '\(^fun\S* \)\@<=[^f][^u][^n]\w\+\<Bar>^\w\+'
execute "nnoremap <Leader>f ?".expr."<CR>"

nnoremap <Leader>v :call LoadSession()<CR>

" toggle showing hidden characters
nnoremap <Leader>l :set list!<CR>
" useful for uncommenting lines
nnoremap <Leader>i _wi
" output current time and date with year and week, all pretty printed.
nnoremap <silent> <Leader>d :execute "echo system(\"date +'[%Yw%W] %b %-e %a <%H:%M>'\")"<CR>

let g:MicroMarks = ['h', 't', 'n', 's', 'l', 'r' ]
nnoremap mi :MicroMark<CR>
nnoremap md :MicroMarkClear<CR>
nnoremap mc :MicroMarkMatch<CR>

nnoremap me :call EditOtherExt('.coffee')<CR>
nnoremap mu :call EditOtherExt('.html')<CR>
nnoremap ma :call EditOtherExt('.scss')<CR>
function! EditOtherExt(ext)
  let filename = expand('%')
  execute 'e ' split(filename, '\.')[0] . a:ext
endfunction

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
inoremap <C-B>t <Esc>vb~ea
" toggle case of first letter
inoremap <C-B>p <Esc>bv~ea

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
inoremap <C-S>   <Plug>Isurround
inoremap <C-G>` <Plug>Isurround
inoremap <C-G>~ <Plug>ISurround

let g:ctrlp_extensions = ['commitdriven']
let g:ctrlp_user_command =
  \ ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']

let g:seek_enable_jumps = 1
let g:seek_char_aliases =
  \ "[{ ]} 9( 8* 7& 6^ 5% 4$ 3# 2@ 1! 0) \| ;: ,< .> `~ -_ /? =+ '" . '"'

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
noremap <Leader>R :w<CR>:ChromeReload<CR>
noremap <Leader>r :ChromeReload<CR>

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

command! -nargs=1 Day call Day(<f-args>)
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
  if match(['wifi', 'pass'], a:data) != -1 | let l:info = '1251025655'
  elseif match(['phone', 'tel'], a:data) != -1 | let l:info = '3176-6107'

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

" search for current selection when in visual mode.
vnoremap <silent> * :call VisualSearch('f')<CR>
vnoremap <silent> # :call VisualSearch('b')<CR>

" from an idea by Michael Naumann
function! VisualSearch(direction) range
  let l:saved_reg = @"
  execute "normal! vgvy"

  let l:pattern = escape(@", '\\/.*$^~[]')
  let l:pattern = substitute(l:pattern, "\n$", "", "")

  if a:direction == 'b' | execute "normal ?" . l:pattern . "^M"
  elseif a:direction == 'gv'
    call CmdLine("vimgrep " . '/'. l:pattern . '/' . ' **/*.')
  elseif a:direction == 'f' | execute "normal /" . l:pattern . "^M"
  endif

  let @/ = l:pattern
  let @" = l:saved_reg
endfunction

" fix vim-gitgutter for dark background.
autocmd VimEnter * highlight clear SignColumn
