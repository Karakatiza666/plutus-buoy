# To compile plutus-apps, limit the number of processes with -j if there is not enough memory in your docker box:
# cabal build all -j1
# Dockerfile of this image

FROM bitnami/minideb:bullseye AS build-nix

RUN apt-get update \
    && apt-get install -y curl

# Nix prerequisites
RUN apt-get install -y bzip2 xz-utils git
RUN mkdir -p /nix /etc/nix \
    && chmod a+rwx /nix \
    && echo 'sandbox = false' > /etc/nix/nix.conf \
    && echo 'build-users-group =' >> /etc/nix/nix.conf \
    && addgroup nixbld

# https://stackoverflow.com/questions/28405902/how-to-set-the-locale-inside-a-debian-ubuntu-docker-container
# Fix locales that are not set up by default in minideb - needed to compile language-javascript package
# ENV DEBIAN_FRONTEND=noninteractive
# RUN apt-get install -y locales \
#     && sed -i '/en_US.UTF-8/s/^# //g' /etc/locale.gen && locale-gen
#     # && update-locale LANG=en_US.UTF-8
#     # && echo 'en_US.UTF-8 UTF-8' | dpkg-reconfigure locales
ENV LC_ALL=en_US.UTF-8 LANG=en_US.UTF-8
# If above didn't work - uncomment above block

# https://stackoverflow.com/questions/67680726/installing-haskells-cabal-or-ghcup-inside-a-dockerfile-wont-work
# Install ghcup
# In Ubuntu and Debian 10 and older use libffi6 instead of libffi7
ENV BOOTSTRAP_HASKELL_NONINTERACTIVE=1
RUN apt-get install -y build-essential libffi-dev libffi7 libgmp-dev libgmp10 libncurses-dev libncurses5 libtinfo5
RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
RUN bash -c "curl -sSL https://get.haskellstack.org/ | sh"

# Add ghcup to PATH
ENV PATH=${PATH}:/root/.local/bin
ENV PATH=${PATH}:/root/.ghcup/bin

# Install cabal
RUN bash -c "ghcup upgrade"
RUN bash -c "ghcup install cabal 3.8.1.0"
RUN bash -c "ghcup set cabal 3.8.1.0"
ENV PATH=${PATH}:/root/.cabal/bin

# Correct GHC version will be installed automatically before build
# # Install GHC
# RUN bash -c "ghcup install ghc 8.10.4"
# RUN bash -c "ghcup set ghc 8.10.4"

# Update Path to include Cabal and GHC exports
RUN bash -c "echo PATH="$HOME/.local/bin:$PATH" >> $HOME/.bashrc"
RUN bash -c "echo export LD_LIBRARY_PATH="/usr/local/lib:$LD_LIBRARY_PATH" >> $HOME/.bashrc"
RUN bash -c "source $HOME/.bashrc"

# Update cabal
RUN bash -c "cabal update"

RUN adduser user --home /home/user --disabled-password --gecos "" --shell /bin/bash

USER user
ENV USER user
WORKDIR /home/user

# Install Nix
RUN touch .bash_profile \
    && curl https://nixos.org/releases/nix/nix-2.11.1/install | sh

USER root
ENV USER root

RUN chmod 777 /root/.bashrc \
    && echo "PATH=$PATH:$(dirname $(find / -name nix-shell -print | head -1))" >> /root/.bashrc

FROM build-nix AS plutus-apps-container

RUN apt-get install -y autoconf make libtool gcc gawk \
    && echo "substituters  = https://hydra.iohk.io https://iohk.cachix.org https://cache.nixos.org/" >> /etc/nix/nix.conf \
    && echo "trusted-public-keys = hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" >> /etc/nix/nix.conf \
    && cd /home/user \
    && git clone https://github.com/Karakatiza666/secp256k1-setup.git \ 
    && chmod +x secp256k1-setup/secp256k1-setup.sh \
    && secp256k1-setup/secp256k1-setup.sh 9c8c4f443c0027c3ed933a3883497131071b1c7e \
    && git clone https://github.com/input-output-hk/plutus-apps \
    && cd plutus-apps \
    && git checkout 172873e87789d8aac623e014eff9a39364c719ae

VOLUME /nix/store
# RUN cd /home/user/plutus-apps && /nix/var/nix/profiles/per-user/user/profile/bin/nix-build --dry-run
RUN cd /home/user/plutus-apps && /nix/var/nix/profiles/per-user/user/profile/bin/nix build -f default.nix plutus-apps.haskell.packages.plutus-pab.components.library --extra-experimental-features nix-command
RUN cd /home/user/plutus-apps && /nix/var/nix/profiles/per-user/user/profile/bin/nix-shell --extra-experimental-features flakes
RUN cabal install yesod-bin
ENV PATH="/root/.cabal/bin:$PATH"
