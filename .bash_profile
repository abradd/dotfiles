stty stop undef # to unmap ctrl-s

#Start keychain upon login (this will boot gpg-agent)
#keychain 82B1036E [[ -f $HOME/.keychain/$HOST-sh-gpg ]] && \
                #source  $HOME/.keychain/$HOST-sh-gpg

###To get the console PINentry to play nicely
#export GPG_TTY=$(tty)

source ~/.bashrc
