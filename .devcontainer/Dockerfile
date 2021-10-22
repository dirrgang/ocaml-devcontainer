# [Choice] OCaml version: 3.14, 3.13, 3.12, 3.11
ARG OCAML_VERSION=4.13
FROM ocaml/opam:debian-ocaml-${OCAML_VERSION}

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=$USER_UID

USER root

RUN apt-get update \
  && apt-get -y install --no-install-recommends m4 \
  #
  # Clean up
  && apt-get autoremove -y \
  && apt-get clean -y \
  && rm -rf /var/lib/apt/lists/*

RUN if [ "$USER_GID" != "1000" ] || [ "$USER_UID" != "1000" ]; then \
  groupmod --gid $USER_GID $USERNAME \
  && usermod --uid $USER_UID --gid $USER_GID $USERNAME \
  && chown -R $USER_UID:$USER_GID /home/$USERNAME; \
  fi

USER opam

RUN opam update && \
  opam install \
  ocaml-lsp-server \
  dune \
  utop \
  mparser \
  ocamlformat \
  ounit2 \
  qcheck

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog

ENV PATH=/home/opam/.opam/$VERSION/bin:$PATH

# ** [Optional] Uncomment this section to install additional packages. **
# RUN apk update \
#     && apk add --no-cache <your-package-list-here>
