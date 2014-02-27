call pathogen#infect()
Helptags

let Tlist_Show_One_File = 1
let g:Powerline_stl_path_style = 'full'
let g:Powerline_symbols = 'fancy'
let mapleader = ","
let perl_nofold_packages=1
let perl_include_pod = 1
let perl_extended_vars = 1
let g:startify_files_number = 25 
let g:startify_change_to_dir = 0 
let g:startify_bookmarks = [ '~/.vimrc', '~/.xmonad/xmonad.hs', '~/.bashrc' ]
let g:ctrlp_open_multiple_files = 'i'
let tlist_perl_settings='perl;u:use;p:package;r:role;e:extends;c:constant;a:attribute;s:subroutine;l:label;c:column'
set keywordprg=perldoc\ -f

silent !mkdir -p ~/tmp/.vim/files/info/ > /dev/null 2>&1
set tags=$CTAGS " Define your tag files in your .bashrc file
set viminfo='100,<50,s10,h,n$HOME/tmp/.vim/files/info/viminfo
set laststatus=2
set notitle
set notimeout
set ttimeout
set timeoutlen=50
set foldmethod=marker
set mouse=a
nnoremap <Space> 3<C-d>   " Pagedown by 3 lines pressing Space
nnoremap <S-B> 3<C-y>     " PageUp by 3 lines pressing Shift+B
set backspace=indent,eol,start
set autoindent
set copyindent
set expandtab
set tabstop=4
set shiftwidth=4
set pastetoggle=<F2>
set incsearch 
set hidden " so I change buffers without writing
set showmatch
set wildmenu
set wildmode=list:longest,full
set complete=.,w,b,u,t
set runtimepath^=~/.vim/bundle/ctrlp.vim

filetype plugin indent on
syntax on
let g:solarized_termcolors = '256'
set background=light
colorscheme solarized
hi! link SignColumn LineNr " Git gutter same color as linenumber

au BufRead,BufNewFile *.t set filetype=perl


" Change Color when entering Insert Mode
autocmd InsertEnter * set cursorline
autocmd InsertEnter * highlight  CursorLine ctermbg=4 ctermfg=White

" Revert Color to default when leaving Insert Mode
autocmd InsertLeave * set nocursorline
autocmd InsertLeave * highlight  CursorLine ctermbg=187 ctermfg=Black

" .tt should be classed as html
autocmd BufRead,BufNewFile *.tt set filetype=html

" .psgi should be treated as Perl
autocmd BufRead,BufNewFile *.psgi set filetype=perl

" Allow p at startify screen to open ctrlp 
autocmd FileType startify nnoremap <buffer> p :enew\|CtrlP<cr>

" Allow g at startify screen to open files edited according to git
"autocmd FileType startify nnoremap <buffer> g :args `git status --porcelain \\| sed -ne 's/^ M //p'`<cr>
autocmd FileType startify nnoremap <buffer> g :args `git ls-files -o --exclude-standard -m`<cr>

" Insert a perl debugger stop, so in perl debugger c continues till it hits it
nnoremap <Leader>di o$DB::single = 1;<ESC>:w<CR>

" Jump to next tag
nnoremap <Leader>[ :tprevious<CR>

" Jump to previous tag
nnoremap <Leader>] :tnext<CR>

" <Leader>n to open NERDTree
nnoremap <Leader>n :NERDTree<CR>

nnoremap <leader>b :CtrlPBuffer<cr>

vmap <Leader>d :<C-U>1,'<-1d\|'>+1,$d<CR>gg

map sh :sh<cr>
map wc :!clear;perl -wc %<cr>
map diff :!cd %:h && git diff %:t<cr>
map com :!cd %:h && git pull && git commit %:t<cr>
map rs :!restartapache<cr>
map run :w<cr>:!perl %
map rund :w<cr>:!perl -d %<cr>
" Add a mapping for :bd but will work with a vsplit, so it don't close it
command! BD silent e# | bd#
map <Leader>bd :BD<cr>

" Allow ctrl+down/up to act like mouse scroll
nmap <C-Down> <C-E>
nmap <C-Up> <C-Y>

" F2 in visual mode : Paste toggle (setup above)
" F3 in console mode: Line number toggle
" nnoremap <silent> <F3> :call GitGutterToggle()<CR>:set nonumber!<CR>
nnoremap <silent> <F3> :set nonumber!<CR>
nnoremap <silent> <Leader><F3> :call GitGutterToggle()<CR>
"nnoremap <silent> <F3> :set nonumber!|GitGutterToggle
" F4 in console mode: Gundo for locating code changes
nnoremap <F4> :GundoToggle<CR>
" F5 in console mode: Perltidy
au Filetype perl nmap <silent> <F5> :call DoTidy()<CR>
" F5 in visual  mode: Perltidy
au Filetype perl vmap <silent> <F5> :Tidy<CR>
" F6 in console mode: Hilights search cases
map <silent> <F6> :se invhlsearch<CR>
" F7 in console mode: Comments code
vmap <silent> <F7> ,c <CR>
" F7 in console mode: Comments code 
map <silent> <F7> ,c <CR>
" F9 in console mode: Fold code using markers
map <F9> :call ToggleFolderMarker()<CR>
map <silent> <F10> :CtrlPBuffer<CR>
" F11 in console mode: previous buffer
map <silent> <F11> :bp<CR>
" F12 in console mode: next buffer
map <silent> <F12> :bn<CR>

nnoremap <Leader>s :Startify<CR>

" Paste text from console mode
nnoremap <MiddleMouse> "*p
" shortcusts for copying, mostly will be used in gvim
vmap <C-c> "+yi
vmap <C-x> "+c
vmap <C-v> c<ESC>"+p
imap <C-v> <C-r><C-o>+

" disabling arrows keys so I get used to hjkl
map <up> <nop>
map <down> <nop>
map <left> <nop>
map <right> <nop>

" Perltidy F5 shortcut {{{
"define :Tidy command to run perltidy on visual selection || entire buffer"
command -range=% -nargs=* Tidy <line1>,<line2>!perltidy

"run :Tidy on entire buffer and return cursor to (approximate) original position"
fun DoTidy()
    let Pos = line2byte( line( "." ) ) 
    :Tidy
    exe "goto " . Pos 
endfun

" }}}

" Show Perl subname in vim statusline {{{
" http://blogs.perl.org/users/ovid/2011/01/show-perl-subname-in-vim-statusline.html
if ! exists("g:did_perl_statusline")
    setlocal statusline+=%(\ %{StatusLineIndexLine()}%)
    setlocal statusline+=%=
    setlocal statusline+=%P
    let g:did_perl_statusline = 1
endif

" Show Perl subname in vim statusline
if has( 'perl' )
perl << EOP
    use strict;
    sub current_sub {
        my $curwin = $main::curwin;
        my $curbuf = $main::curbuf;

        my @document = map { $curbuf->Get($_) } 0 .. $curbuf->Count;
        my ( $line_number, $column  ) = $curwin->Cursor;

        my $sub_name = '';
        for my $i ( reverse ( 1 .. $line_number  -1 ) ) {
            my $line = $document[$i];
            if ( $line =~ /^\s*sub\s+(\w+)\b/ ) {
                $sub_name = $1;
                last;
            }
        }
                VIM::DoCommand "let subName='$sub_name '";
    }
EOP

function! StatusLineIndexLine()
  perl current_sub()
  return subName
endfunction
endif " }}}

" Fold code by pressing F9, mapped above {{{
function ToggleFolderMarker()
if exists("g:perl_fold")
    unlet g:perl_fold
    set foldmethod=marker
    set foldlevelstart=1
    e
    normal zM
else
    let g:perl_fold=1
    set foldmethod=syntax
    e
endif
endfunction " }}}

