# .bashrc
# Bash reads and executes commands from this file when invoked as an
# interactive non-login shell, see Bash manual.

# Source global definitions
if [ -f /etc/bashrc ]; then
    . /etc/bashrc
fi

# User specific environment
if ! [[ "$PATH" =~ "$HOME/.local/bin:$HOME/bin:" ]]; then
    PATH="$HOME/.local/bin:$HOME/bin:$PATH"
fi
export PATH

# User prompt
PS1='\[\033[1m\][\u@\h \W]
\$\[\033[0m\] '

# Uncomment the following line if you don't like systemctl's
# auto-paging feature:
# export SYSTEMD_PAGER=

# User specific aliases and functions

# GNU color option for ls
alias ls='ls --color=auto'

# GNU (and BSD) color option for grep.  Note that egrep and fgrep are
# deprecated; use -E and -F options, respectively, to grep instead.
alias grep='grep --color=auto'
alias zgrep='zgrep --color=auto'

