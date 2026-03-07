let repos = ["idimitrov.dev", "ndlm", "metronome", "hosts.nix"]
def notify_tg [msg: string] {
    let url = $"https://api.telegram.org/bot($env.TG_BOT_TOKEN)/sendMessage"
    let body = {
        chat_id: $env.TG_BOT_CHAT_ID
        text: $msg
        disable_notification: false
    }
    http post --content-type application/json $url $body
}
def update_flake [flake_path: string] {
    cd $flake_path
    mut result = []
    mut err = false
    let commands = [
        { nix flake update }
        { nix flake check --abort-on-warn }
        { git reset }
        { git add flake.lock }
        { git -c commit.gpgsign=false commit -m "auto-update" }
        { git push }
    ]
    for cmd in $commands {
        let src = (view source $cmd)
        let res = do $cmd | complete | merge {src: $src}
        let e = $err or $res.exit_code != 0
        $result = $result | append $res
        $err = $e
        if $e {
            try { notify_tg $"Error updating flake ($flake_path) for command ($src):\n($res.stdout)\n-------\n($res.stderr)" }
            catch { echo $"(ansi red)Error connecting to telegram.(ansi reset)" }
            break
        }
    }
    return {
        result: $result
        err: $err
        flake_path: $flake_path
    }
}
$repos | each {|r| update_flake $"~/src/($r)" }
