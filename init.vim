"{{{1 PLUGINS
call plug#begin('~/.vim/plugged')
Plug 'neovim/node-host'

" workflow
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install -all' }
Plug 'junegunn/fzf.vim'
Plug 'justinmk/vim-dirvish'
Plug 'goldfeld/tnt'
Plug 'goldfeld/vim-walker'

" writing
Plug 'godlygeek/tabular'
Plug 'plasticboy/vim-markdown'

" editing
Plug 'junegunn/vim-easy-align'
Plug 'snoe/nvim-parinfer.js'

" languages
Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
Plug 'ElmCast/elm-vim'

" moving
Plug 'goldfeld/vim-seek'
Plug 'easymotion/vim-easymotion'

" git
Plug 'tpope/vim-fugitive'
Plug 'jreybert/vimagit'
Plug 'gregsexton/gitv', { 'on': 'Gitv' }
Plug 'goldfeld/gut'
" slows down scrolling
"Plug 'airblade/vim-gitgutteswitching splitsr'

" other
"Plug 'goldfeld/ctrlr.vim'


" slows down switching splits
"Plug 'vim-airline/vim-airline'
"Plug 'vim-airline/vim-airline-themes'

" nyaovim
Plug 'rhysd/nyaovim-mini-browser'
Plug 'rhysd/nyaovim-popup-tooltip'

" colorschemes
Plug 'freeo/vim-kalisi'
Plug 'croaker/mustang-vim'
Plug 'morhetz/gruvbox'
Plug 'jonathanfilip/vim-lucius'
Plug 'candycode.vim'
Plug 'rainerborene/vim-heroku'
Plug 'sjl/badwolf'
Plug 'Guardian'
Plug 'Lokaltog/vim-distinguished'
Plug 'noahfrederick/Hemisu'
Plug 'Pychimp/vim-luna'
Plug 'Pychimp/vim-sol'
Plug 'altercation/vim-colors-solarized'
call plug#end()
"{{{1 GENERAL
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
runtime macros/matchit.vim
color luna
let mapleader = ","

nnoremap <silent> <Esc> :noh<CR><Esc>
nnoremap <silent> <Space><Space> @=(foldlevel('.')?'za':"\<Space>")<CR>

inoremap hh <Esc>:noh<CR>
inoremap ii <Esc>:noh<CR>
"inoremap <Backspace> <NOP>
nnoremap <C-L> <NOP>
nnoremap ZZ <NOP>

" repurpose the colon as my comma lost to leader.
nnoremap : ,

" go to last inserted 
nnoremap gi '^

nnoremap Y y$
nnoremap yY ggyG
nnoremap dD ggdG

" visual shifting (relect after shift).
vnoremap < <gv
vnoremap > >gv

" make C-U and C-W undoable by using <C-G>u (signal a new change to vim.)
inoremap <C-U> <C-G>u<C-U>
inoremap <C-W> <C-G>u<C-W>

" give up single d visual delete so I can emulate a diff buffer's normal
" mode mappings in visual mode too with linewise control.
vnoremap <silent> dd :delete<CR>
vnoremap <silent> do :diffget<CR>
vnoremap <silent> dp :diffput<CR>
"}}}
"{{{1 SETTINGS
" number of spaces of tab character
set tabstop=2
" number of spaces to (auto)indent
set shiftwidth=2
" put tabs on BOL as per shiftwidth, not tabstop
set smarttab
set expandtab
set smartindent
set autoindent
set textwidth=70
set diffopt+=vertical
set fillchars=vert:\ ,fold:\ 
set foldopen-=block
set conceallevel=2

" allow <C-H> over everything in insert mode
set backspace=indent,eol,start
" keep 3 lines when scrolling
set scrolloff=3
" hightlight searches
set hlsearch
" do incremental searching
set incsearch
set ignorecase                 
" ignore case of search only if all lowercase
set smartcase

set hidden
" reload changed files if there's no conflict
set autoread
set viminfo+=!
" height of the preview window
set previewheight=20

"set wildmenu
"set wildmode="full"
"set wildcharm=<C-Z>
set wildignorecase

" formatoptions t: auto-wrap text using textwidth
set fo+=t

set laststatus=2
set statusline=
set statusline+=%{fugitive#statusline()}
set statusline+=\ %f 
set statusline+=\ %m
set statusline+=%h

" gvim behave like vim: console tabs and no dialogs, menus or scrollbars
set guioptions+=lrbmTLce
set guioptions-=lrbmTLce
set guioptions+=c
" modelines can execute arbitrary code in e.g. set foldexpr
set modelines=0
"}}}
"{{{1 PLUGIN SETTINGS
let g:walker_keepalt = 1
let g:vim_markdown_folding_disabled = 1

let g:seek_char_aliases = '&% $~ #` @^ \| ;: ,< .> -_ /? ' . "'" . '"'
let g:seek_listchars = ','
let g:seek_enable_jumps = 1
let g:seek_use_vanilla_binds_in_diffmode = 1

let g:nyaovim_popup_tooltip_default_mapping = 0
nnoremap <silent> <C-H><C-B> <Plug>(nyaovim-popup-tooltip-open)

nmap - <Plug>(easymotion-s2)

" elm-vim
let g:elm_format_autosave = 1

let g:dirvish_hijack_netrw = 1
"set autochdir                   " keep working dir relative to current file
if !exists("g:Dirvish_Added")
  let g:Dirvish_Added = 1
  au FileType dirvish call s:dirvish_init()
endif
func! s:dirvish_init()
  noremap <buffer><silent> h :call Dirvish_wrap_up('%:h:h')<CR>
  noremap <buffer><silent> o :!mkdir %
  noremap <buffer><silent> a :e %
  nnoremap <buffer><silent> l :<C-U>.call dirvish#open("edit", 0)<CR>
  xnoremap <buffer><silent> l :call dirvish#open("edit", 0)<CR>
  nmap <expr><buffer> <Esc> v:hlsearch?":noh\<CR>":"\<Plug>(dirvish_quit)"
  sort r /[^\/]$/
endfunc
func! Dirvish_wrap_up(path) " automatically seek the directory or file when going up
  let loc = escape(bufname("%"), '\')
  silent! execute "Dirvish " . a:path
  call search(loc)
endfunc
"}}}
"{{{1 TNT & GUT SETTINGS
let g:tnt_root = '~/datav/note'
let g:tnt_workpath = '~/.vim/plugged;'
let g:tnt_force_en_US_dates = 1
let g:tnt_elapsed_minutes_before_new_log_timestamp = 180

let g:gut_gutfiles_path_getter = 'tnt#files#get_project_folder_by_name'
"}}}
"{{{1 BUFLOCAL OPTIONS
augroup filetypeSettings
  autocmd!
  "autocmd BufEnter * call FtColors()

  autocmd FileType make setlocal shiftwidth=8 tabstop=8
  autocmd filetype rust setlocal shiftwidth=4 tabstop=4
  autocmd FileType markdown set fo=tnjcroq
  "autocmd FileType markdown set fo=atnjcroq
  autocmd FileType markdown nnoremap <C-M> :MiniBrowser <C-R><C-P><CR>
  autocmd FileType vim setlocal fdm=marker kp=:help
  autocmd FileType vim nnoremap <buffer> <silent> <C-T><C-M> :w<CR>:so %<CR>

  autocmd BufRead,BufNewFile *.md setlocal textwidth=80
  autocmd BufRead,BufNewFile *.tnt,*.ana setlocal et cc=0 filetype=markdown
augroup END

color mustang
function! FtColors()
  if &diff || match(['perl', 'diff'], &ft) != -1
    if g:colors_name != 'gruvbox' | color gruvbox | endif
  elseif match(['vim'], &ft) != -1
    if g:colors_name != 'luna' | color luna | endif
  elseif match(['html', 'css', 'make'], &ft) != -1
    if g:colors_name != 'distinguished' | color distinguished | endif
  elseif match(['ls'], &ft) != -1
    if g:colors_name != 'candycode' | color candycode | endif
  elseif match(['haskell'], &ft) != -1
    if g:colors_name != 'solarized' | color solarized | endif
  else
    if match(['fzf'], &ft) == -1 && g:colors_name != 'mustang'
      color mustang
    endif
  endif

  highlight ColorColumn guibg=#373737 ctermbg=236 |
  highlight CursorLine guibg=#373737 ctermbg=236 |
  highlight CursorColumn guibg=#373737 ctermbg=236
endfunction
"}}}
"{{{1 TERMINAL
tnoremap <Esc> <C-\><C-N>
tnoremap hh <C-\><C-N>
"}}}
"{{{1 M MAPS
nnoremap mm :
nnoremap me :e 
nnoremap mw :w<CR>
nnoremap mz :sp<CR>
nnoremap mv :vs<CR>
nnoremap mh :vs<CR><C-^>
nnoremap mt :b term<Tab><CR>a
nnoremap mT :vs<CR>:term<CR>

" assist editing a file path under current :term's cwd
nnoremap mE <C-\><C-N>G?@<CR>f:lyt$:e <C-R>"/

" elegant close buffer
nnoremap <silent> mb :<C-U>call Bdelete()<CR>

" maps for executing line under cursor (as VimL or shell)
nnoremap mrr yy:<C-R>"<Backspace>
nnoremap mrj y}:<C-R>"<Backspace>
"nnoremap mrj y}:<C-\>esubstitute(getreg('"'), '\\\^M', '', ''))<CR>

nnoremap mr! yy:!<C-R>"<Backspace>
nnoremap mrt yy<C-W>wpa<CR>
nnoremap mR Y:<C-R>"<Backspace>

" 'more <cWORD>'
nnoremap m* /<C-\>eRescape("<C-R><C-A>")<CR><CR>
nnoremap m# ?<C-\>eRescape("<C-R><C-A>")<CR><CR>
nnoremap mQ :ReturnFromFootnote<CR>

nnoremap mx :x<CR>
  \:echom strpart(system("git show $commit \| grep '^    \w'"), 2)<CR>

" other marks for my other pinky
nnoremap mo O
nnoremap ma A
nnoremap m. zz
nnoremap m, zb
nnoremap m' zt

" save file opened without sudo after the fact
nnoremap mW :!sudo tee % >/dev/null

nnoremap <silent> mp :call PurgeBuffers()<CR>
function! PurgeBuffers()
  let seq = ''
  buffers
  echo "Delete buffers by number: "
  let char = getchar()

  while l:char != 27 && l:char != 3
    " if a number was entered, append it to our sequence.
    if l:char >= 48 && l:char <= 57
      let l:seq = l:seq . nr2char(l:char)
    endif

    " if we've already got two chars entered, or user explicitly presses enter.
    if len(l:seq) > 1 || l:char == 13
      execute "bdelete " l:seq
      let l:seq = ''
      redraw
      buffers
      echo "Delete buffers by number: "
    endif

    let l:char = getchar()
  endwhile
  redraw
endfunction
"}}}
"{{{1 CTRL-H MAPS
nnoremap <silent> <C-H><C-H> <C-^>
nnoremap <silent> <C-H><C-E> :cd %:p:h<CR>:e 
"nnoremap qg :let b:qfbufs = cfirst<CR>
nnoremap <silent> <C-H><C-F> :cwindow<CR>
nnoremap <silent> <C-H><C-G> :WalkerFirst<CR>
nnoremap <silent> <C-H><C-T> :WalkerNext<CR>
nnoremap <silent> <C-H><C-R> :WalkerPrev<CR>
nnoremap <silent> <C-H><C-L> @=(&diff?":diffupd\r":":call CloseQFBufs()\r")<CR>

function! CloseQFBufs()
  " map quickfix dicts to bufnr's, then filter out non-open (listed) buffers.
  let bufs = filter(map(getqflist(), 'v:val.bufnr'), 'getbufvar(v:val, "&bl")')
  let chosenbuf = bufnr('%')
  for listedbuf in l:bufs
    " don't close the quickfix buffer if it's the one we're on.
    if listedbuf != l:chosenbuf | silent! execute "bdelete" listedbuf | endif
  endfor
endfunction
"}}}
"{{{1 CTRL-T MAPS
nnoremap <silent> <C-T><C-N> :cd %:p:h<CR>:GitFiles<CR>
nnoremap <silent> <C-T>n :Dow swap prj<CR>

nnoremap <silent> <C-T><C-T> :Buffers<CR>
nnoremap <silent> <C-T>t :Dow swap buf<CR>

nnoremap <silent> <C-T><C-D> :cd %:p:h<CR>
nnoremap <silent> <C-T><C-S> :<C-U>call tnt#fzf_get_sessions()<CR>
nnoremap <silent> <C-T><C-P> :<C-U>call tnt#fzf_get_sessions('projects')<CR>
nnoremap <silent> <C-T><C-J> :TNTNewEntry<CR>
nnoremap <silent> <C-T><C-R> :<C-U>call tnt#files#pull_reference_item(line('.'))<CR>
nnoremap <silent> <C-T><C-B> :<C-U>call SendToNyao()<CR>
"nnoremap <silent> <C-T>j :TNTNewSubentry<CR>

nnoremap <silent> <C-T><C-I> :e ~/datav/repo/log/inbox/inbox.tnt<CR>
"}}}
"{{{1 CTRL-W MAPS (window cmds)
nnoremap <silent> <C-W><C-B> <C-W><C-C><C-W>b
nnoremap <silent> <C-W><C-H> <C-W><C-C><C-W>h
nnoremap <silent> <C-W><C-J> <C-W><C-C><C-W>j
nnoremap <silent> <C-W><C-K> <C-W><C-C><C-W>k
nnoremap <silent> <C-W><C-L> <C-W><C-C><C-W>l

function! RestoreWindow(cmd)
  let num = winnr()
  execute "normal! " . cmd . num . "<C-W>w"
  "execute 'normal! ' . cmd . num . '<C-W>w'
endfunction
nnoremap <silent> <C-W><C-T> :<C-U>call RestoreWindow("\<C-W>t\<C-W>\<C-C>")<CR>
nnoremap <silent> <C-W><C-T> <C-W>t<C-W><C-C>
"}}}
"{{{1 FZF COMMANDS
command! -nargs=0 Downloads call fzf#run({'source': 'ls ~/Downloads',
\ 'sink': 'e', 'options': '-m', 'down': '40%' })<CR>
"}}}
"{{{1 LEADER MAPS
" pull next line and delete any comment symbols.
nnoremap <Leader>J Jldw
" delete to first char in line, including current char.
nnoremap <Leader>D d^x
nnoremap <Leader>/ :SearchWords 
"}}}
"{{{1 LEADER DOT MAPS
" quickly edit my vimrc.
nnoremap <silent> <Leader>.v :e ~/datav/code/goldfeld/dotfiles/init.vim<CR>
" source vimrc to allow live reloading of changes.
nnoremap <silent> <Leader>.V :w<CR>:so $MYVIMRC<CR>
"}}}
"{{{1 GIT BINDS
" TODO move Gcached to vimdow and make it identify line moves
 " [diff - Highlighting added/deleted lines, ignoring moves, in a patch file - Stack Overflow](http://stackoverflow.com/questions/1380333/highlighting-added-deleted-lines-ignoring-moves-in-a-patch-file)

nnoremap gs :Magit<CR>

" vim-fugitive and gitv plugin mappings
nnoremap gb :Gblame<CR>
nnoremap gB :GbrowseUrl<CR>
nnoremap gll :Gitv<CR>
nnoremap glf :Gitv!<CR>
nnoremap gL :Glog<CR>

nnoremap g/ :Git log -G

nnoremap g* :execute "Ggrep!" expand('<cword>') " -- '*." . &ft . "'"<CR>
vnoremap g* y:execute "Ggrep!" getreg() " -- '*." . &ft . "'"<CR>

nnoremap g# :Ggrep! 
vnoremap g# y:Ggrep! 

" same as git add the current file.
nnoremap gt :Gwrite<CR>
" same as git checkout the current file, updating buffer.
nnoremap gx :Gread<CR>
" leave me on the index version, so I can quickly check it and close it.
nnoremap gc :Gdiff<CR><C-W>h
nnoremap ge :Git 
nnoremap go :Git checkout 
nnoremap gm :Git merge 
" use 'help index' to see vim's built-in natively mapped keys

nnoremap g! :Sgit 
command! -complete=shellcmd -nargs=+ Sgit call s:Bash('git ' . <q-args>, 'git')

command! -complete=shellcmd -nargs=+ S call s:Bash(<q-args>)
function! s:Bash(cmdline, ...)
  echo a:cmdline
  let expanded_cmdline = a:cmdline
  for part in split(a:cmdline, ' ')
    if part[0] =~ '\v[%#<]'
      let expanded_part = shellescape(expand(part))
      let expanded_cmdline = substitute(expanded_cmdline, part, expanded_part, '')
    endif
  endfor

  keepalt split new
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
  if a:0
    execute "setlocal ft=".a:1
  endif

  call setline(1, '$ ' . expanded_cmdline)
  call setline(2,substitute(getline(1),'.','=','g'))
  silent! execute '$read !'. expanded_cmdline
  setlocal nomodifiable
  1
endfunction
"}}}
"{{{1 VIM-COMA
" vim-coma - use the comma as a dead key.
inoremap ,, <Esc>
inoremap ,a <Esc>A
inoremap ,i <Esc>I
inoremap ,x <Esc>l1s
inoremap ,e <Esc>ea
inoremap ,E <Esc>Ea
inoremap ,- <Esc>gea
inoremap ,_ <Esc>gEa
inoremap ,c <C-O>D
inoremap ,d <Esc>D
for deadmap in split('w W b B h l', ' ')
  execute "inoremap ,".deadmap "<C-O>".deadmap
endfor

" use custom mapping for number insertion.
let g:pegword_remap_digits = 0
inoremap ,s <Esc>:Pegword 0<CR>
inoremap ,t <Esc>:Pegword 1<CR>
inoremap ,n <Esc>:Pegword 2<CR>
inoremap ,m <Esc>:Pegword 3<CR>
inoremap ,r <Esc>:Pegword 4<CR>

inoremap ,f <Esc>:call IndentGuideFind(1)<CR>
inoremap ,F <Esc>:call IndentGuideFind(2)<CR>
function! IndentGuideFind(linesUp)
  let line = line('.')
  let text = getline('.') | let trim = matchstr(l:text, '\S.*$')
  let first = matchstr(l:text, '\S') | let empty = empty(l:first)
  if l:empty | let find = nr2char(getchar()) | let repos = 2
  else       | let find = l:first            | let repos = 1 | endif

  let above = line('.') - a:linesUp | if l:above < 1 | return | endif
  let pos = stridx(getline(l:above), l:find) | if l:pos == -1 | return | endif
  call setline(l:line, repeat(' ', l:pos) . l:trim)
  call cursor(l:line, l:pos + l:repos)
  if !l:empty | execute "normal! A\<Space>" | startinsert
  else | startinsert | call cursor(l:line, col('.') + 1) | endif
endfunction
"}}}
"{{{1 FUNCTION FEATURES
function! Bdelete()
  write
  let d = bufnr('%')
  keepalt b #
  exe 'bdelete' d
endfunction

function! SendToNyao()
  let send = expand('%')
  call Bdelete()
  call system('nyaovim ' . fnamemodify(send, ':p'))
endfunction

function! ShowTyping()
  try let char = getchar()
  catch /^Vim:Interrupt$/
    let char = "\<Esc"
  endtry

  if  char == '^\d\+$' || type(char) == 0
    let char = nr2char(char)
  endif
  if char == "\<Esc>" return ''
  endif
  redraw
  return char
endfunction

function! Redraw()
  redraw
  return ''
endfunction

" for <Leader>\ at [[LEADER MAPS]]
" search for words (no substring matches) in sequence.
command! -nargs=* SearchWords call SearchWords(<f-args>)
let s:lastSearchWords = ''
function! SearchWords(...)
  if a:0
    let searchexpr = ''
    for word in a:000
      let l:searchexpr = l:searchexpr . '.*\<' . word . '\>'
    endfor
    let s:lastSearchWords = strpart(l:searchexpr, 2)
  elseif len(s:lastSearchWords) == 0 | return
  endif
  let @/ = s:lastSearchWords
  execute "normal! n"
endfunction

" lifted from vim-fugitive (s:github_url, used for :Gbrowse)
function! GithubUrl(opts, ...) abort
  if a:0 || type(a:opts) != type({})
    return ''
  endif
  let domain_pattern = 'github\.com'
  let domains = exists('g:fugitive_github_domains') ? g:fugitive_github_domains : []
  for domain in domains
    let domain_pattern .= '\|' . escape(split(domain, '://')[-1], '.')
  endfor
  let repo = matchstr(get(a:opts, 'remote'), '^\%(https\=://\|git://\|git@\)\=\zs\('.domain_pattern.'\)[/:].\{-\}\ze\%(\.git\)\=$')
  if repo ==# ''
    return ''
  endif
  let path = substitute(a:opts.path, '^/', '', '')
  if index(domains, 'http://' . matchstr(repo, '^[^:/]*')) >= 0
    let root = 'http://' . s:sub(repo,':','/')
  else
    let root = 'https://' . s:sub(repo,':','/')
  endif
  if path =~# '^\.git/refs/heads/'
    let branch = a:opts.repo.git_chomp('config','branch.'.path[16:-1].'.merge')[11:-1]
    if branch ==# ''
      return root . '/commits/' . path[16:-1]
    else
      return root . '/commits/' . branch
    endif
  elseif path =~# '^\.git/refs/tags/'
    return root . '/releases/tag/' . path[15:-1]
  elseif path =~# '^\.git/refs/remotes/[^/]\+/.'
    return root . '/commits/' . matchstr(path,'remotes/[^/]\+/\zs.*')
  elseif path =~# '.git/\%(config$\|hooks\>\)'
    return root . '/admin'
  elseif path =~# '^\.git\>'
    return root
  endif
  if a:opts.commit =~# '^\d\=$'
    let commit = a:opts.repo.rev_parse('HEAD')
  else
    let commit = a:opts.commit
  endif
  if get(a:opts, 'type', '') ==# 'tree' || a:opts.path =~# '/$'
    let url = substitute(root . '/tree/' . commit . '/' . path, '/$', '', 'g')
  elseif get(a:opts, 'type', '') ==# 'blob' || a:opts.path =~# '[^/]$'
    let url = root . '/blob/' . commit . '/' . path
    if get(a:opts, 'line2') && a:opts.line1 == a:opts.line2
      let url .= '#L' . a:opts.line1
    elseif get(a:opts, 'line2')
      let url .= '#L' . a:opts.line1 . '-L' . a:opts.line2
    endif
  else
    let url = root . '/commit/' . commit
  endif
  return url
endfunction

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
"}}}
" temporary alt maps for nyaovim (not working with ctrl maps)
nnoremap <silent> ms :cd ~/datav/repo/log<CR>:call fzf#run({
\ 'source': 'ls sessions/* inbox/*', 'sink': 'e', 'options': '-m', 'down': '40%' })<CR>
nnoremap <silent> mt :Buffers<CR>
