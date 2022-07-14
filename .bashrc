#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

PS1='[\u@\h \W]\$ '

### ALIASES ###

alias ls='exa -alh'
alias dotfiles='/usr/bin/git --git-dir=/home/mikael/.dotfiles/ --work-tree=/home/mikael'
