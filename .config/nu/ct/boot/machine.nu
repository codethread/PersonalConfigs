use ct/dotty
use ct/brew

export def main [
    --skip-brew # kind of slow and annoying
] {
    dotty link;

    # brew installer
    if not ("/opt/homebrew" | path exists) {
        zsh -c 'bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"'
    }

    if not $skip_brew { brew sync }

    # cool ideas from nix to get touchid
    (echo "
auth       optional       /opt/homebrew/Cellar/pam-reattach/1.3/lib/pam/pam_reattach.so
auth       sufficient     pam_tid.so
    " | str trim | save --force ~/.tmp)
    zsh -c $"sudo mv ~/.tmp /etc/pam.d/sudo_local"

    print 'Files linked'
}
