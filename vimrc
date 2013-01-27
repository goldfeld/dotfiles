" Vundle setup
set nocompatible
filetype off
set rtp+=~/.vim/bundle/vundle
call vundle#rc()

Bundle 'gmarik/vundle'
Bundle 'jceb/vim-orgmode'
Bundle 'leafo/moonscript-vim'
Bundle 'kchmck/vim-coffee-script'
Bundle 'tpope/vim-fugitive'
Bundle 'Lokaltog/vim-easymotion'
Bundle 'vim-scripts/LustyJuggler'
Bundle 'lordm/vim-browser-reload-linux'

Bundle 'croaker/mustang-vim'

Bundle 'goldfeld/vim-seek'
"Bundle 'goldfeld/vimdow'
filetype plugin indent on

set runtimepath^=~/.vim/bundle/ctrlp.vim

"colorscheme slate
colorscheme mustang
set cursorline

if filereadable(expand("~/vcs.order-management/README.md"))
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.user,*.nupkg,*.dll,*.xml,*.config,*.suo,*.sln,*.asax,*.cs,*.transform,*.ttf,*.ico,*._,*.c,*.h,*.mk,*.js
else
  autocmd BufRead,BufNewFile *.coffee,*.js set expandtab
  set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.user,*.nupkg,*.dll,*.xml,*.config,*.suo,*.sln,*.asax,*.cs,*.transform,*.ttf,*.ico,*._,*.c,*.h,*.mk
endif
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
" gvim behave like vim: console tabs and no dialogs, menus or scrollbars
set guioptions+=lrbmTLce
set guioptions-=lrbmTLce
set guioptions+=c

let mapleader = ","
 " move within virtual (wrapped) lines
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
nnoremap - $
nnoremap <Backspace> dh

" visual shifting (relect after shift)
vnoremap < <gv
vnoremap > >gv

" easy split window navigation.
map <C-H> <C-W>h
map <C-J> <C-W>j
map <C-K> <C-W>k
map <C-L> <C-W>l
 " save file opened without sudo after the fact
cmap w!! w !sudo tee % >/dev/null

noremap <silent> K :execute "normal i".nr2char(getchar())<CR>

" paste from clipboard
"set clipboard=unnamed

nnoremap <Leader>i _wi
nmap <Leader>/h /HEAD<CR>
nmap <Leader>/c /console<CR>
" quickly edit my vimrc.
nmap <silent> <Leader>.v :e $MYVIMRC<CR>
" source vimrc to allow live reloading of changes.
nmap <silent> <Leader>.V :w<CR>:so $MYVIMRC<CR>
" allow left ctrl (which I remap to my Caps Lock key) to act as <Esc> when pressed alone.
nmap <silent> <Leader>.x :execute "call system(\"~/./xcape -e 'Control_L=Escape'\")"<CR>
" grab ssh publickey to clipboard.
nmap <silent> <Leader>.k :execute "call system(\"xclip -sel clip < ~/.ssh/id_rsa.pub\")"<CR>
" output current time and date with year and week, all pretty printed.
nmap <silent> <Leader>d :execute "echo system(\"date +'<%H:%M> %b %e %a [%Yw%W]'\")"<CR>

nnoremap <silent> <Esc> :noh<CR><Esc>
" toggle uppercase/lowercase
noremap <Leader>u vb~ea
" toggle capitalize first letter
noremap <Leader>U bv~ea

" operator-pending
onoremap - $
onoremap in( :<C-U>normal! f(vi(<CR>
onoremap in< :<C-U>normal! f<vi<<CR>
onoremap in[ :<C-U>normal! f[vi[<CR>
onoremap ih :<C-U>execute "normal! ?^==\\+$\r:noh\rkvg_"<CR>
onoremap ah :<C-U>execute "normal! ?^==\\+$\r:noh\rg_vk0"<CR>

inoremap <A-C> <A-U>

noremap [q :cprevious<CR>
noremap ]q :cnext<CR>

let g:EasyMotion_leader_key = '<Leader>'
":nnoremap f <Leader>f
":nnoremap F <Leader>F

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

noremap <Leader>c :Vimdow Chrome<CR>
noremap <Leader>h :Vimdow Luakit<CR>
noremap <Leader>s :Vimdow fish<CR>
noremap <Leader>o :Vimdow coffee<CR> " and compass
noremap <Leader>m :Vimdow meteor<CR>
noremap <Leader>g :Vimdow gedit<CR>
command! -nargs=1 Vimdow call Vimdow(<f-args>)
" TODO make <Leader>o able to launch either coffee (first) or else compass
" also <Leader>s should try fish, else bash
" TODO implement cycling between open vim/gvim instances
"	it should make it as though we were cycling between vim tabs/views
"		the advantage is they can be on different monitors
"	append 'vimdow#X' to vim window 
"		http://vim.wikia.com/wiki/Automatically_set_screen_title
" http://somanov.wordpress.com/2009/12/02/window-shortcuts-for-linux-desktops/
" http://superuser.com/questions/16647/custom-hotkey-shortcut-to-open-bring-to-front-an-app
" http://www.linuxquestions.org/questions/linux-desktop-74/shortcuts-to-switch-to-open-applications-786176/
"
" wmctrl man page http://linux.die.net/man/1/wmctrl
"
" register uppercase as waiting arguments e.g. <Leader>S :VimdowArgs
" fish<CR>
" 	so e.g. both of <Leader>SZ and <Leader>Sz would quit&save vim (:wq) and take me to fish
" 
" translate to riml
function! Vimdow(str)
	" if 'str' is found on more than one window, give lower priority to
	" web browsers (which might just have a page loaded with 'str')
	" also never go to vim itself (since the user is likely to use vim as his
	" only text editor, this is already accounting for files open with 'str')
	call system("wmctrl -a " . a:str)
	" TODO echo a message saying to 'sudo apt-get install wmctrl' if needed
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
  if match(['wifi', 'pass'], a:data) != -1 | let info = '1241025655'
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
	\ 'O in visual block mode to switch between corners'
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
autocmd BufRead,BufNewFile *.coffee,*.js execute "silent! echo Headsup('coffee')"
let sms = 0
function! Hey()
	let g:sms += 1
	return
endfunction
function! Hoy()
	let g:sms += 1
	return
endfunction

autocmd CursorMoved * call Hey()
autocmd CursorMovedI * call Hoy()

nnoremap <Space> :call EasyLines()<CR>
function! EasyLines()
	let v:count = 5
	echo v:count
endfunction
