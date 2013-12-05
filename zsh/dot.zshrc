#---------------------------------------
# .zshrc for MacOSX
#---------------------------------------

#---------------------------------------
# 文字コード
#---------------------------------------
export LANG=ja_JP.UTF-8
export LC_ALL=ja_JP.UTF-8

#---------------------------------------
# path
#---------------------------------------
export PATH=/usr/local/share/python:/usr/local/bin:/opt/local/bin:$PATH
export MANPATH=/usr/local/share/man:/opt/local/share/man:$MANPATH

#---------------------------------------
# env
#---------------------------------------
# subversion
export SVN_EDITOR='/usr/bin/emacs'
# misc
export PAGER='less -R'
#export PERL_CPANM_OPT="--local-lib=~/Library/perl5"
#export PERL5LIB=~/Library/perl5/lib/perl5:$PERL5LIB
export APPENGINE_SDK_HOME=/usr/local/appengine-java-sdk

#---------------------------------------
# alias
#---------------------------------------
alias ls='ls -wFG'
alias ll='ls -lwFG'
alias la='ls -awFG'
alias lla='ls -alwFG'
alias pa='ps aux'
alias grep='grep --color'
alias pssh='/usr/local/ssh/bin/ssh'
alias cemacs='open -a Emacs'
alias coteditor='open -a CotEditor'
alias ccgl='cc -framework OpenGL -framework GLUT -framework Foundation'

alias -g L="| $PAGER "
alias -g G='| grep '
alias -g N='| nkf'


## mysql
alias mysql51-start='sudo -u mysql /usr/local/mysql51/bin/mysqld_safe &'
alias mysql51='/usr/local/mysql51/bin/mysql'

## ruby
# export RBENV_ROOT=/usr/local/var/rbenv
if which rbenv > /dev/null; then eval "$(rbenv init - zsh)"; fi

#---------------------------------------
# prompt
#---------------------------------------
autoload colors
colors
#PROMPT="%B%n@%m:%~$ %{${reset_color}%}"
PROMPT="%{${fg[green]}%}%B%n@%m:%~$ %{${reset_color}%}"
#RPROMPT="%{${fg[green]}%}[%~]%{${reset_color}%}"
SPROMPT="correct: %R -> %r ?" 

#autoload colors
#colors
#case ${UID} in
#0)
#  PROMPT="%B%{${fg[red]}%}%/#%{${reset_color}%}%b "
#  PROMPT2="%B%{${fg[red]}%}%_#%{${reset_color}%}%b "
#  SPROMPT="%B%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%}%b "
#  [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
#    PROMPT="%{${fg[white]}%}${HOST%%.*} ${PROMPT}"
#  ;;
#*)
#  PROMPT="%{${fg[red]}%}%/%%%{${reset_color}%} "
#  PROMPT2="%{${fg[red]}%}%_%%%{${reset_color}%} "
#  SPROMPT="%{${fg[red]}%}%r is correct? [n,y,a,e]:%{${reset_color}%} "
#  [ -n "${REMOTEHOST}${SSH_CONNECTION}" ] &&
#    PROMPT="%{${fg[white]}%}${HOST%%.*} ${PROMPT}"
#  ;;
#esac

#---------------------------------------
# history
#---------------------------------------
HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

#---------------------------------------
# ヒストリファイルに時刻を記録
#---------------------------------------
setopt extended_history

#---------------------------------------
# ヒストリファイルは上書きでなく追記
#---------------------------------------
setopt append_history

#---------------------------------------
# 直前と同じコマンドは覚えない
#---------------------------------------
setopt hist_ignore_dups

#---------------------------------------
# 重複したヒストリは覚えない
#---------------------------------------
setopt hist_ignore_all_dups

#---------------------------------------
# ヒストリを共有
#---------------------------------------
setopt share_history

#---------------------------------------
# ヒストリを呼び出してから実行する間に編集できる
#---------------------------------------
setopt hist_verify

#---------------------------------------
# コマンドラインの引数で --prefix=/usr などの = 以降でも補完できる
#---------------------------------------
setopt magic_equal_subst

#---------------------------------------
# ディレクトリ名だけでcd
#---------------------------------------
setopt auto_cd

#---------------------------------------
# cd - で補完
#---------------------------------------
setopt auto_pushd

#---------------------------------------
# キーバインド
#---------------------------------------
bindkey -e

#---------------------------------------
# 補完の利用
#---------------------------------------
autoload -Uz compinit
compinit

#---------------------------------------
# 自動修正機能
#---------------------------------------
setopt correct

#---------------------------------------
# 小文字で大文字も補完
#---------------------------------------
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

#---------------------------------------
# 補完候補をなるべく沢山表示
#---------------------------------------
setopt listpacked
