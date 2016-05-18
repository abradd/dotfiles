export PATH="/Users/links_world/src/bin:/Users/links_world/src:/Users/links_world/anaconda/bin:/usr/local/bin:/Library/Frameworks/Python.framework/Versions/2.7/bin:/usr/bin:/usr/sbin:/bin:/sbin:/Applications/MATLAB_R2014b.app/bin:/usr/bin/env/tmuxomatic:/usr/local/texlive/2014/bin/x86_64-darwin:/usr/local/Cellar/sdl/1.2.15/include:/usr/local/texlive/2014basic/bin/x86_64-darwin"

alias emacs="/usr/local/Cellar/emacs/24.5/Emacs.app/Contents/MacOS/Emacs"

alias lr1="ssh -Y abradd@linrack1.ee.columbia.edu"
alias lr2="ssh -Y abradd@linrack2.ee.columbia.edu"
alias lr3="ssh -Y abradd@linrack3.ee.columbia.edu"
alias lr4="ssh -Y abradd@linrack4.ee.columbia.edu"
alias lr5="ssh -Y abradd@linrack5.ee.columbia.edu"
alias lr6="ssh -Y abradd@linrack6.ee.columbia.edu"
#alias for bioeenx server
alias bioeenx='ssh -Y abradd@bioeenx.ee.columbia.edu'
alias bioeenas='ssh -Y abradd@bioeenas.ee.columbia.edu'
alias bioeecad='ssh -Y abradd@bioeecad.ee.columbia.edu'
alias bioeeserv='ssh -Y abradd@bioeeserv.ee.columbia.edu'
alias mbioeenx='mosh -p 3995 abradd@bioeenx.ee.columbia.edu'
alias fsbioeenx='sshfs abradd@bioeenx.ee.columbia.edu:"/u8/abradd/" ~/bioee/'
alias fslabpc='sshfs abradd@bioeenx.ee.columbia.edu:"/proj1/labpc/" ~/labpc'
alias fstools3='sshfs abradd@bioeenx.ee.columbia.edu:"/tools3/" ~/tools3'

alias scpf="scp abradd@linrack5.ee.columbia.edu"
alias gcc5="/usr/local/Cellar/gcc5/5.1.0/bin/gcc-5"

alias imagej="/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx"
alias firefox="/Applications/Firefox.app/Contents/MacOS/firefox"

newlink () {
osascript -e 'tell application "Firefox" to open location \"$1\"'
}

set -o vi
