# Loosely based on https://www.fpcomplete.com/blog/2017/12/building-haskell-apps-with-docker
FROM fpco/stack-build:lts-16.8 as build
RUN mkdir /opt/build
WORKDIR /opt/build

 # GHC dynamically links its compilation targets to lib gmp
RUN apt-get update ; apt-get install ca-certificates \
  && apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 8B1DA6120C2BF624 \
  && apt-get update \
  && apt-get install -y \
  && apt-get download libgmp10
# Docker reproducibility

RUN mv libgmp*.deb libgmp.deb

COPY . /opt/build
RUN stack build --system-ghc
RUN mv "$(stack path --local-install-root --system-ghc)/bin" /opt/build/bin

 # Base image for stack build so compiled artifact from previous
# stage should run
FROM debian:bookworm-slim
RUN mkdir -p /opt/executable
WORKDIR /opt/executable

 # Install lib gmp
COPY --from=build /opt/build/libgmp.deb /tmp
RUN dpkg -i /tmp/libgmp.deb && rm /tmp/libgmp.deb

COPY --from=build /opt/build/bin .
COPY --from=build /opt/build/assets .
EXPOSE 8080
CMD ["/opt/executable/links", "8080"]
