# [Choice] OCaml version: 3.14, 3.13, 3.12, 3.11
ARG OCAML_VERSION=4.14
FROM ocaml/opam:debian-ocaml-${OCAML_VERSION}

# Avoid warnings by switching to noninteractive
ENV DEBIAN_FRONTEND=noninteractive

USER opam

RUN opam update && \
  opam install \
  ocaml-lsp-server \
  dune \
  utop \
  mparser \
  ocamlformat \
  ounit2 \
  qcheck \
  ocamlformat-rpc

# Switch back to dialog for any ad-hoc use of apt-get
ENV DEBIAN_FRONTEND=dialog

# ENV PATH=/home/opam/.opam/${OCAML_VERSION}/bin:$PATH

# ** [Optional] Uncomment this section to install additional packages. **
# RUN apk update \
#     && apk add --no-cache <your-package-list-here>
