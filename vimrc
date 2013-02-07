" Vundle setup
filetype off
set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'lordm/vim-browser-reload-linux'

" workflow
Bundle 'jceb/vim-orgmode'
Bundle 'goldfeld/vimdow'
Bundle 'longstorm/hudmode-vim'

" editing
Bundle 'tpope/vim-surround'

" files
Bundle 'tpope/vim-fugitive'
Bundle 'vim-scripts/LustyJuggler'
"Bundle 'spolu/dwm.vim'

" moving
Bundle 'Lokaltog/vim-easymotion'
Bundle 'goldfeld/vim-seek'
Bundle 'kshenoy/vim-signature'

" syntax
Bundle 'leafo/moonscript-vim'
Bundle 'kchmck/vim-coffee-script'
Bundle 'tpope/vim-markdown'

" colorschemes
Bundle 'croaker/mustang-vim'

filetype plugin indent on

set runtimepath^=~/.vim/bundle/ctrlp.vim

"colorscheme slate
colorscheme mustang
set cursorline

if filereadable(expand("~/punchcard"))
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.user,*.nupkg,*.dll,*.xml,*.config,*.suo,*.sln,*.asax,*.cs,*.transform,*.ttf,*.ico,*._,*.c,*.h,*.mk,*.js
else
  autocmd BufRead,BufNewFile *.coffee,*.js set expandtab
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.user,*.nupkg,*.dll,*.xml,*.config,*.suo,*.sln,*.asax,*.cs,*.transform,*.ttf,*.ico,*._,*.c,*.h,*.mk
endif
set hidden 											" allows switching buffer without saving and keeps undo history.
set tabstop=2 						 			" number of spaces of tab character
set shiftwidth=2 					 			" number of spaces to (auto)indent
set scrolloff=3						 			" keep 3 lines when scrolling
set backspace=indent,eol,start	" allow backspacing over everything in insert mode
set hlsearch							 			" hightlight searches
set incsearch							 			" do incremental searching
set ignorecase					 			
set smartcase							 			" ignore case of search only if all lowercase
set smarttab 							 			" insert tabs on start of line according to shiftwidth, not tabstop
set autoread                    " auto reload changed files if there's no conflict
set number

let &colorcolumn=join(range(81, 201), ",")
augroup textWidth
	autocmd!
	autocmd BufRead,BufNewFile *.md let &colorcolumn=0
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
	return system('echo "'.@b.'"' . " | awk '$3 ~ /\+/ {printf ".'"  " $4 "*"'."}'")
endfunction

" gvim behave like vim: console tabs and no dialogs, menus or scrollbars
set guioptions+=lrbmTLce
set guioptions-=lrbmTLce
set guioptions+=c
" modelines could be exploited maliciously

let mapleader = ","

noremap h <NOP>
noremap l <NOP>

nnoremap j gj
nnoremap k gk

" skip past big lines
nnoremap gj j
nnoremap gk k

 " go to last inserted but don't leave me in insert mode
nnoremap gi gi<Esc>

":nnoremap <Space> ea
nnoremap ge gE
nnoremap Y y$

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
	if l:line[l:cursor - 2] != ' '
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
" sacrifice the m mark for my pinkies' sake.
nnoremap mm :
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

let g:EasyMotion_leader_key = '<Leader>'
let g:EasyMotion_mapping_w = '<Space>'
let g:EasyMotion_mapping_b = '<Leader><Space>'
":nnoremap f <Leader>f
":nnoremap F <Leader>F

" vim-signature shouldn't touch the m mark which I have remapped.
let g:SignatureIncludeMarks = 'abcdefghijklnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'
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
" leave me on the index version, so I can
" quickly check it and close it with ZZ.
nnoremap gc :Gdiff<CR><C-W>h
nnoremap gl :Ggrep "<cword>"<CR>
vnoremap gl y:Ggrep <C-R>"<CR>

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

nnoremap j :<C-U>Streamline<CR>
nnoremap k :<C-U>StreamlineBack<CR>
" for vi/vim since only gvim distinguishes <S-Space>
nnoremap + :<C-U>StreamlineBack<CR>

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
	if match(['wifi', 'pass'], a:data) != -1 | let info = '1251025655'
	elseif match(['restart'], a:data) != -1
		let info = 'sudo service network-manager restart'
		call add(l:otherinfo, 'then toggle hardware wireless switch')
	elseif match(['phone', 'tel'], a:data) != -1 | let info = '3176-6107'
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

nnoremap <leader>.h :call ShowingHNParse()<CR>
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

" Show HN: Pomohunt - Pomodoro Meets Pro Tips for your Favorite Language/Tool
" halogen design, with red circle for main color/theme
" use same database as Leak, use same collection as hacker.leakmap.com/leak (vim tag)
" small rewards (read book X), medium rewards, great rewards (buy something)
	" allow showing one tip worthy of each reward tier and user picks which 
let viminderCoffee = [
	\ 'gI insert at first columnn',
	\ '<Leader>i insert after first symbol',
	\ "ci['/\"] change inner quote",
	\ '@@',
	\ ':5,10norm! @h ',
	\ 'gi - go back to last edited location',
	\ ':.+1,.+20',
	\ 'o in visual mode to switch between selection ends',
	\ 'O in visual block mode to switch between corners',
	\ '*a to select all in page on gmail',
	\ '*u to select unread in page on gmail',
	\ '*t to select unstarred in page on gmail',
	\ '<C-U> in vi insert to cut to beginning of line.',
	\ '<C-N> new window in dwm',
	\ '<C-C> close window in dwm',
	\ '<C-[J/K]> jump next/prev window in dwm',
	\ '<C-Space> focus current window in dwm',
	\ '<C-M> fullscreen window in dwm (C-Space to get out)',
	\ '*t to select unstarred in page on gmail',
	\ 'ga prints ascii codes of current char',
	\ 'js: callback pattern with this binding - findNodes("hide", nodeLayouter); then inside findNodes do callback_obj[callback_string].call(callback_obj, found)'
  \ ]

let viminderCoffeeIterator = 0
let viminderCoffeeMax = len(g:viminderCoffee)
function! Headsup(context)
	if a:context == 'coffee'
		if g:viminderCoffeeIterator >= g:viminderCoffeeMax
			let g:viminderCoffeeIterator = 0
		else | let g:viminderCoffeeIterator += 1
		endif
		return g:viminderCoffee[ g:viminderCoffeeIterator ]
	endif
endfunction

" Headsup is a tool for when you feel the need to up your game,
" it's always in your statusline, waiting for you to look down on it
" You can let it send back to the server when you improve your mappings
	" It never sends back any code or characters! Only motions and commands,
	" filtering out any search data (replacing it with stock text).
" You can connect to the server to get other people's mappings


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
