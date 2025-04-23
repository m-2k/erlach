# Erlach
**Fully anonymous imageboard and anonymous comments service**

## Overview

* Erlach – Anonymous imageboard as SPA on the WebSockets and supports BPG images.
* Web site: [erlach.co](https://erlach.co/)
* Tor network: [http://erlachx4ahxagauop5zczffrn2dlqfagcfxyvksbgxus2ktn2kxywpad.onion/](http://erlachx4ahxagauop5zczffrn2dlqfagcfxyvksbgxus2ktn2kxywpad.onion/)


## Features

* Completely anonymous without personalization
* [BPG](https://bellard.org/bpg/) images
* Smart posting
* Canvas rendering
* Single page application (SPA)
* [WebSocket](https://www.rfc-editor.org/rfc/rfc6455.html) transport

![Screenshot](erlach-beta2.2-march-2015.png)


## Requirements

* Unix
* Erlang/OTP 17
* inotify-tools (optional for live code reload)

## Run Erlach

1. Srart attached docker container
```sh
docker pull erlang:17.5.6.4
docker run --rm -it -p 8000:8000 -v "$(pwd)":/app -w /app erlang:17.5.6.4 bash
```
2. Setup Git configuration into docker container and start Erlang app with REPL mode
```sh
git config --system url."https://github.com/".insteadOf git://github.com/
```

3. Patch list of Erlang applications which should be launched, file `./.applist`
```sh
sed -i 's/\[/[xmerl,/; s/,fs//; s/,active//' .applist
```

4. Get dependencies, compile and run Erlach with [mad](https://github.com/synrc/mad/tree/0.9)
```sh
./mad deps compile repl
```

5. Init Erlach database into ERTS terminal
```erlang
utils:init_db().
```

6. Open URL [http://localhost:8000/](http://localhost:8000/) on host system


## Feedback

* Twitter [@erlach_co](https://twitter.com/erlach_co)
* On errors make as issue in this repository

## Credits
* [Namdak Tonpa](https://github.com/5HT/)
