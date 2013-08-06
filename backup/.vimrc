call pathogen#infect()
call Pl#Theme#InsertSegment('pwd', 'after', 'filename')

let g:Powerline_symbols = 'fancy'
let mapleader = ","
let perl_nofold_packages=1
let perl_include_pod = 1
let perl_extended_vars = 1

set viminfo='100,<50,s10,h,n$HOME/tmp/.vim/files/info/viminfo
set laststatus=2
set notimeout
set ttimeout
set timeoutlen=50
set foldmethod=marker
set background=dark
set mouse=a
set backspace=indent,eol,start
set expandtab
set tabstop=4
set shiftwidth=4
set pastetoggle=<F2>
set incsearch 
set hidden " so I change buffers without writing
set wildmenu
set wildmode=list:longest,full
set complete=.,w,b,u,t
set runtimepath^=~/.vim/bundle/ctrlp.vim

filetype plugin indent on
syntax on
au BufRead,BufNewFile *.t set filetype=perl


" Change Color when entering Insert Mode
autocmd InsertEnter * set cursorline
autocmd InsertEnter * highlight  CursorLine ctermbg=4 ctermfg=White

" Revert Color to default when leaving Insert Mode
autocmd InsertLeave * set nocursorline
autocmd InsertLeave * highlight  CursorLine ctermbg=Black ctermfg=None

map <Leader>b :MiniBufExplorer<cr>
" Switch to buffer N with <Leader>N

nnoremap <Leader>1 :buf 1<CR>
nnoremap <Leader>2 :buf 2<CR>
nnoremap <Leader>3 :buf 3<CR>
nnoremap <Leader>4 :buf 4<CR>
nnoremap <Leader>5 :buf 5<CR>
nnoremap <Leader>6 :buf 6<CR>
nnoremap <Leader>7 :buf 7<CR>
nnoremap <Leader>8 :buf 8<CR>
nnoremap <Leader>9 :buf 9<CR>
nnoremap <Leader>0 :buf 10<CR>

map sh :sh<cr>
map wc :!clear;perl -wc %<cr>
map diff :!cd %:h && git diff %:t<cr>
map com :!cd %:h && git pull && git commit %:t<cr>
map perld :!perl -d %<cr>
map rs :!restartapache<cr>
map run :w<cr>:!perl %
map rund :w<cr>:!perl -d %<cr>

" Allow ctrl+down/up to act like mouse scroll
nmap <C-Down> <C-E>
nmap <C-Up> <C-Y>

" F2 in visual mode : Paste toggle (setup above)
" F3 in console mode: Line number toggle
nnoremap <silent> <F3> :set nonumber!<cr>
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
map <silent> <F8> :TlistToggle<CR>
" F9 in console mode: Fold code using markers
map <F9> :call ToggleFolderMarker()<CR>
map <silent> <F10> :MBEToggle<CR>
" F11 in console mode: previous buffer
map <silent> <F11> :bp<CR>
" F12 in console mode: next buffer
map <silent> <F12> :bn<CR>

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

