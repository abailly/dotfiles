filetype off                  " required

set nu
set tabstop=2
set shiftwidth=2
set expandtab 
set ruler
set background=dark
set ignorecase
set smartcase
set incsearch
set hlsearch
set laststatus=2

" Autofix all whitespace on save
autocmd BufWritePre *.hs :%s/\s\+$//e
" Delete all trailing empty lines on files
autocmd BufWritePre *.hs :%s/\(\s*\n\)\+\%$//e
