if &cp | set nocp | endif
let s:cpo_save=&cpo
set cpo&vim
imap <silent> <Plug>IMAP_JumpBack =IMAP_Jumpfunc('b', 0)
imap <silent> <Plug>IMAP_JumpForward =IMAP_Jumpfunc('', 0)
imap <C-Space> 
map! <S-Insert> <MiddleMouse>
vmap <NL> <Plug>IMAP_JumpForward
nmap <NL> <Plug>IMAP_JumpForward
nmap <silent>  :silent noh
nmap ,v :e $MYVIMRC
nmap ,s :source $MYVIMRC
nmap \caL <Plug>CalendarH
nmap \cal <Plug>CalendarV
xmap \nr <Plug>NrrwrgnDo
nmap \dct :DBCompleteTables
map _lang :popup ]LANGUAGES_GHC
map _opt :popup ]OPTIONS_GHC
map _ie :call GHC_MkImportsExplicit()
map _ct :call GHC_CreateTagfile()
map _si :call GHC_ShowInfo()
map _t :call GHC_ShowType(0)
map _T :call GHC_ShowType(1)
map _iqm :call Import(1,1)
map _iq :call Import(0,1)
map _im :call Import(1,0)
map _i :call Import(0,0)
map _. :call Qualify()
map _?2 :call HaskellSearchEngine('hayoo!')
map _?1 :call HaskellSearchEngine('hoogle')
map _?? :let es=g:haskell_search_engines |echo "g:haskell_search_engines" |for e in keys(es) |echo e.' : '.es[e] |endfor
map _? :call Haddock()
nmap gx <Plug>NetrwBrowseX
nnoremap <silent> <Plug>NetrwBrowseX :call netrw#NetrwBrowseX(expand("<cWORD>"),0)
nnoremap <silent> <Plug>CalendarH :cal Calendar(1)
nnoremap <silent> <Plug>CalendarV :cal Calendar(0)
xnoremap <SNR>21_VisualNrrwRgn :call nrrwrgn#VisualNrrwRgn(visualmode())
vmap <silent> <Plug>IMAP_JumpBack `<i=IMAP_Jumpfunc('b', 0)
vmap <silent> <Plug>IMAP_JumpForward i=IMAP_Jumpfunc('', 0)
vmap <silent> <Plug>IMAP_DeleteAndJumpBack "_<Del>i=IMAP_Jumpfunc('b', 0)
vmap <silent> <Plug>IMAP_DeleteAndJumpForward "_<Del>i=IMAP_Jumpfunc('', 0)
nmap <silent> <Plug>IMAP_JumpBack i=IMAP_Jumpfunc('b', 0)
nmap <silent> <Plug>IMAP_JumpForward i=IMAP_Jumpfunc('', 0)
nmap <F6> :!~/src/diku/maplehax/compile %
nmap <F3> :wa|exe "mksession! " . v:this_session
nmap <F2> :wa|exe "mksession! " . v:this_session:so ~/.vim/sessions/
map <S-Insert> <MiddleMouse>
imap <NL> <Plug>IMAP_JumpForward
imap  
iabbr RRR \mathbb{R}
iabbr NNN \mathbb{N}
let &cpo=s:cpo_save
unlet s:cpo_save
set autoindent
set autowrite
set backspace=indent,eol,start
set balloonexpr=GHC_TypeBalloon()
set clipboard=unnamedplus,autoselect,exclude:cons|linux
set cmdheight=3
set complete=.,w,b,u,i
set completeopt=menu,longest
set expandtab
set fileencodings=ucs-bom,utf-8,default,latin1
set formatoptions=croqt
set guifont=DejaVu\ Sans\ Mono\ 10
set guioptions=aegirLt
set helplang=en
set history=1000
set ignorecase
set incsearch
set laststatus=2
set mouse=a
set omnifunc=GHC_CompleteImports
set ruler
set runtimepath=~/.vim,~/.vim/bundle/LaTeX-Suite-aka-Vim-LaTeX,~/.vim/bundle/NrrwRgn,~/.vim/bundle/VimOrganizer,~/.vim/bundle/haskellmode-vim,~/.vim/bundle/lojban,~/.vim/bundle/manual,~/.vim/bundle/vim-colors-solarized,~/.vim/bundle/vim-fugitive,~/.vim/bundle/vim-markdown,/usr/share/vim/vimfiles,/usr/share/vim/vim73,/usr/share/vim/vimfiles/after,~/.vim/after
set scrolloff=5
set shellpipe=2>
set shiftwidth=2
set showmatch
set smartcase
set smartindent
set softtabstop=2
set statusline=%<%f\ %h%m%r%=(%c,%l)%h%m%r\ \ %P
set suffixes=.bak,~,.swp,.o,.info,.aux,.log,.dvi,.bbl,.blg,.brf,.cb,.ind,.idx,.ilg,.inx,.out,.toc
set switchbuf=useopen,usetab,newtab
set tabstop=2
set termencoding=utf-8
set viminfo='1000,f1,:1000,@1000,/1000
set wildmenu
set wildmode=longest:full,full
set window=49
" vim: set ft=vim :
