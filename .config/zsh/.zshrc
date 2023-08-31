# vim:fileencoding=utf-8:foldmethod=marker:foldlevel=0

# Only read by interactive shells
# i.e only put in stuff that helps with typing commands etc
# https://blog.flowblok.id.au/2013-02/shell-startup-scripts.html

# this has some tips on speeding up zsh
# https://htr3n.github.io/2018/07/faster-zsh/

#: Key Bindings {{{

bindkey -e # emacs key bindings

if [[ "${terminfo[kcbt]}" != "" ]]; then
    bindkey "${terminfo[kcbt]}" reverse-menu-complete   # [Shift-Tab] - move through the completion menu backwards
fi

# highlight tab
zstyle ':completion:*' menu selecto

# fix for navigation keys in IDEA terminal
if [[ "$TERMINAL_EMULATOR" == "JetBrains-JediTerm" ]]; then
    bindkey "∫" backward-word # Option-b
    bindkey "ƒ" forward-word  # Option-f
    bindkey "∂" delete-word   # Option-d
fi

if [ $CT_IS_LINUX -eq 1 ]; then
    # remap capslock to ctrl
    gsettings set org.gnome.desktop.input-sources xkb-options "['ctrl:nocaps']"
fi

#: }}}
#: ZSH History {{{

# see https://zsh.sourceforge.io/Doc/Release/Options.html#History for more info
setopt INC_APPEND_HISTORY        # Write to the history file immediately, not when the shell exits.
setopt SHARE_HISTORY             # Share history between all sessions.
setopt HIST_IGNORE_DUPS          # Don't record an entry that was just recorded again.
setopt HIST_IGNORE_ALL_DUPS      # Remove old duplicate commands
setopt HIST_FIND_NO_DUPS         # Don't show dups when using history command
setopt HIST_IGNORE_SPACE         # Don't add commands starting with 'space' (good for credentials)
setopt HIST_REDUCE_BLANKS        # less blanks

HISTFILE="$ZDOTDIR/.zsh_history"
HISTSIZE=999999999
SAVEHIST=$HISTSIZE

# using autin for a bit as history is always getting trimed for me
eval "$(atuin init zsh --disable-up-arrow)"

#: }}}
#: Prompt {{{

# Prompt https://scriptingosx.com/2019/07/moving-to-zsh-06-customizing-the-zsh-prompt/
# NEWLINE=$'\n' # couldn't get the newline to behave without this
# PROMPT="${NEWLINE}%F{cyan}%~${NEWLINE}%(?.%F{magenta}ᕕ(ᐛ)ᕗ.%F{red}(╯°□°%)╯︵ ┻━┻) %F{normal}"
# # time
# RPROMPT='%F{blue}%*'

eval "$(starship init zsh)"

#: }}}
#: Terminal specific envs and aliases {{{

# add completions to this folder with format _example
# e.g dotty completion zsh > "${ZDOTDIR}/completions/_dotty"
# don't forget to run compinit after
mkdir -p "$ZDOTDIR/completions"
fpath+=( "$ZDOTDIR/completions" )
ssource "$ZDOTDIR/.zsh_plugins.zsh"
ssource "$ZDOTDIR/.aliases.zsh"
ssource ~/.private

if [ $CT_IS_WORK -eq 1 ]; then
  source ~/gdrive/aliases.zsh
  source ~/gdrive/grpcurls.zsh
fi

#: }}}
#: General {{{

# set up colors for ls, fd, tree etc https://github.com/sharkdp/vivid
export LS_COLORS="$(vivid generate dracula)"
export JQ_COLORS="1;30:0;31:0;32:0;35:0;33:1;35:1;35"	# export JQ_COLORS="1;30:0;31:0;32:0;35:0;33:1;35:1;35"

#: }}}
#: Emacs {{{

# https://emacs-lsp.github.io/lsp-mode/page/performance/#use-plists-for-deserialization
export LSP_USE_PLISTS=true

#: }}}
#: FZF {{{

export FZF_DEFAULT_COMMAND="fd --type f --hidden --exclude '{.git}'"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
# quickest way to cd around
export FZF_ALT_C_COMMAND="fd --hidden --type d --exclude '{Library,Music,Applications,Pictures,Unity,VirtualBox VMs,WebstormProjects,Tools,node_modules,.git}' . ${HOME}"


# ssource ~/.fzf.zsh
# [[ $- == *i* ]] && source "$HOMEBREW_PREFIX/opt/fzf/shell/completion.zsh" 2> /dev/null
# source "$HOMEBREW_PREFIX/opt/fzf/shell/key-bindings.zsh"
#: }}}
#: TMUX {{{

source "${ZDOTDIR}/tmux_projects"

#: }}}
#: Language specific {{{

#: kubernetes {{{

# this is huge so better to get when doing k8 stuff
# ssource ~/completions/zsh/kubectl.zsh
# if i alias to k, can do this
# echo 'alias k=kubectl' >>~/.zshrc
# echo 'complete -F __start_kubectl k' >>~/.zshrc

#: }}}

#: gcloud {{{

gcloud() {
    ssource "$HOME/google-cloud-sdk/completion.zsh.inc"
    # The next line updates PATH for the Google Cloud SDK.
    ssource "$HOME/google-cloud-sdk/path.zsh.inc"
}

#: }}}

#: jvm java {{{

# ssource '/usr/local/Cellar/jenv/0.5.3/libexec/libexec/../completions/jenv.zsh'
# https://github.com/jenv/jenv/issues/148 speed up ideas
jenvy() {
    # brew install rlwrap if this fails
    eval "$(jenv init -)"
}

#: }}}

#: ruby {{{

rbenv() {
    eval "$(command rbenv init -)"
    rbenv "$@"
}

#: }}}

#: }}}
#: Profile {{{

# enable this and ~/.zshenv for profiling
# zprof

if [[ "$PROFILE_STARTUP" == true ]]; then
    unsetopt xtrace
    exec 2>&3 3>&-
fi
#: }}}
