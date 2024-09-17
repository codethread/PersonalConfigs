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

The \_scripts folder contains [Deno](https://deno.com/) scripts which can be run without dependencies as long as you have Deno installed, e.g `brew install deno`

| script                                       | use                        | help                                                                                                                                   |
| -------------------------------------------- | -------------------------- | -------------------------------------------------------------------------------------------------------------------------------------- |
| [getSlackToken](./_scripts/getSlackToken.ts) | Get creds to run slack cli | `deno run --allow-read --allow-env https://raw.githubusercontent.com/codethread/PersonalConfigs/main/_scripts/getSlackCreds.ts --help` |
