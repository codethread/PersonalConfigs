> We measure our lives in the ways we develop and expand our knowledge
> through myriad variations. Nothing can take its place. It’s our very soul...
>
> There is no such thing as perfect in this world ... If something
> is perfect, then there is nothing left. There is no room for imagination.
>
> It is our job to create things more wonderful than anything before them, but
> never to obtain perfection.
>
> ― Kurotsuchi Mayuri

```sh
# Install Dotfiles (this is meant for me)
/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/codethread/PersonalConfigs/main/_boot/boot.sh)"
```

<p align="center">
  <img width="460" src="https://64.media.tumblr.com/9f3abf18b67d35111b2b314463093517/tumblr_n8bzxpd3Kn1qzbqw1o1_400.gif">
</p>

## Scripts

The [scripts](./_scripts) folder contains [Deno](https://deno.com/) scripts[^1] which can be run without dependencies as long as you have Deno installed, e.g `brew install deno`

> [!tip]
> I recommend aliasing these commands with whatever flags you need locally

### [getSlackCreds](./_scripts/getSlackCreds.ts)

Get credentials to run slack cli based tools

```sh
deno run --reload="https://raw.githubusercontent.com/codethread" \
    https://raw.githubusercontent.com/codethread/PersonalConfigs/main/_scripts/getSlackCreds.ts --help
```

> [!note]
> ```sh
> deno run --reload="https://raw.githubusercontent.com/codethread" <script> [...args]
> ```
> will run `<script>` at the specified url, however it will also cache it, so `reload` will always load the latest code hosted in this repo (without downloading all the deps again

[^1]: Deno's secure by default runtime makes it an excellent scripting platform, combined with it's declaritve dependencies mean a script can list all dependencies without consumers needing to care. Only downside is you'll need a lot of --allow-\* flags... unless you are balls to the walls and just pass `--allow-all`, though I recommend against this!
