FROM debian:bookworm-slim

RUN apt -y update && \
  apt -y install \
  build-essential \
  curl \
  git \
  libffi-dev \
  libffi8 \
  libgmp-dev \
  libgmp10 \
  libncurses-dev \
  libncurses5 \
  libtinfo5 \
  pkg-config \
  --

# Set the user to run the container as. This must be the same as the user specified by
# "remoteUser" in devcontainer.json.
ARG REMOTE_USER=vscode

# Only create the user if REMOTE_USER does not already exist.
RUN id -u "${REMOTE_USER:?}" 2> /dev/null || useradd -mU "${REMOTE_USER}"
COPY bashrc-root /root/.bashrc

USER ${REMOTE_USER}
COPY bashrc /home/${REMOTE_USER}/.bashrc

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
  BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
  BOOTSTRAP_HASKELL_GHC_VERSION=9.4.8 \
  BOOTSTRAP_HASKELL_CABAL_VERSION=3.12.1 \
  BOOTSTRAP_HASKELL_STACK_VERSION=3.1.1 \
  BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
  BOOTSTRAP_HASKELL_HLS_VERSION=2.9.0 \
  sh
