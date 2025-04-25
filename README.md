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

![Screenshot](screenshots/erlach-R3-RC10-september-2017-white.avif)

![Erlach Chat](screenshots/erlach-R3-chat.gif)

![Erlach Search](screenshots/erlach-R3-search.gif)

![Erlach Subscription](screenshots/erlach-R3-subscriptions.gif)


## Requirements

* Unix
* [Erlang/OTP 19](https://github.com/kerl/kerl?tab=readme-ov-file#using-kerl)
* libjpeg-progs
* optipng
* ImageMagick
* [libbpg](https://bellard.org/bpg/)
* inotify-tools (optional for live code reload)

## Run Erlach

1. Srart attached docker container
```sh
docker pull erlang:19.3.6.13
docker run --rm -it -p 8000:8000 -v "$(pwd)":/app -w /app erlang:19.3.6.13 bash
```

2. Install requirements into docker container
```sh
# package manager
cat <<EOF > /etc/apt/sources.list
deb [trusted=yes] http://archive.debian.org/debian/ stretch main contrib non-free
deb [trusted=yes] http://archive.debian.org/debian/ stretch-updates main contrib non-free
deb [trusted=yes] http://archive.debian.org/debian-security/ stretch/updates main contrib non-free
EOF

# install packages from apt
apt-get update
apt-get install -y build-essential libjpeg-progs pkg-config wget ca-certificates cmake
apt-get install -y libjpeg-dev libpng-dev libgif-dev libz-dev
apt-get install -y libsdl-dev libsdl-image1.2-dev optipng imagemagick

# build and install libbpg
wget https://bellard.org/bpg/libbpg-0.9.8.tar.gz -P /tmp
tar -xvzf /tmp/libbpg-0.9.8.tar.gz -C /tmp
cd /tmp/libbpg-0.9.8 && make && make install
```

3. Setup Git configuration into docker container and get dependencies
```sh
git config --system url."https://github.com/".insteadOf git://github.com/
cd /app
./mad deps
```

4. Compile and run Erlach with [mad](https://github.com/synrc/mad/tree/1.9)
```sh
cd /app
./mad compile
./mad repl # for correct logging DO NOT combine arguments like here: ./mad com rep for 
```

5. Init Erlach database into ERTS terminal
```erlang
erlach_db:init_db().
```

6. Open URL [http://localhost:8000/](http://localhost:8000/) on host system


## Feedback

* Twitter [@erlach_co](https://twitter.com/erlach_co)
* On errors make as issue in this repository

## Credits
* [Namdak Tonpa](https://github.com/5HT/)
* [tai](https://github.com/rusjava8/)