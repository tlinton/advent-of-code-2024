FROM debian:bookworm-slim AS ghc-builder

RUN apt -y update && \
  apt -y install build-essential curl git libgmp-dev

RUN curl --proto '=https' --tlsv1.2 -sSf -o /usr/local/bin/ghcup https://downloads.haskell.org/~ghcup/0.1.30.0/x86_64-linux-ghcup-0.1.30.0
RUN chmod 755 /usr/local/bin/ghcup

RUN mkdir -p /opt/ghcup/bin
RUN ghcup install ghc 9.4.8 -i /opt/ghcup/ghc
RUN find /opt/ghcup/ghc/bin -mindepth 1 -exec cp -d --preserve=all -t /opt/ghcup/bin '{}' ';'
RUN ghcup install hls 2.9.0.1 -i /opt/ghcup/hls
RUN find /opt/ghcup/hls/bin -mindepth 1 -exec cp -d --preserve=all -t /opt/ghcup/bin '{}' ';'
RUN ghcup install cabal 3.12.1 -i /opt/ghcup/bin
RUN ghcup install stack 3.1.1 -i /opt/ghcup/bin

FROM debian:bookworm-slim

# Set the user to run the container as. This must be the same as the user specified by
# "remoteUser" in devcontainer.json.
ARG REMOTE_USER=vscode

# Only create the user if REMOTE_USER does not already exist.
RUN id -u "${REMOTE_USER:?}" 2> /dev/null || useradd -U "${REMOTE_USER}"
COPY bashrc /home/${REMOTE_USER}/.bashrc
COPY bashrc /root/.bashrc

RUN apt -y update && \
  apt -y install build-essential curl git libgmp-dev

COPY --from=ghc-builder /opt/ghcup /opt/ghcup
RUN ln -s /opt/ghcup/bin/* /usr/local/bin/
