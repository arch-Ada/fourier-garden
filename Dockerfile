# ---- Build Fourier Garden -----------------------------------------------
FROM debian:13-slim AS haskell-build

RUN apt-get update && apt-get install -y --no-install-recommends \
    ghc \
    cabal-install \
    libncurses-dev \
    pkg-config \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /src

COPY fourier-garden.cabal cabal.project ./
RUN cabal update

COPY app ./app
COPY src ./src

RUN cabal build exe:fourier-garden \
    && mkdir -p /out \
    && cp "$(cabal list-bin exe:fourier-garden)" /out/fourier-garden


# ---- Browser-demo runtime -----------------------------------------------
FROM debian:13-slim

ARG TARGETARCH

RUN apt-get update && apt-get install -y --no-install-recommends \
    ca-certificates \
    curl \
    tini \
    libgmp10 \
    libncursesw6 \
    libnuma1 \
    && rm -rf /var/lib/apt/lists/*

RUN case "$TARGETARCH" in \
      arm64) TTYD_ARCH="aarch64" ;; \
      amd64) TTYD_ARCH="x86_64" ;; \
      *) echo "Unsupported architecture: $TARGETARCH" >&2; exit 1 ;; \
    esac \
    && curl -fsSL \
       "https://github.com/tsl0922/ttyd/releases/download/1.7.7/ttyd.${TTYD_ARCH}" \
       -o /usr/local/bin/ttyd \
    && chmod +x /usr/local/bin/ttyd

COPY --from=haskell-build /out/fourier-garden /usr/local/bin/fourier-garden

# Make the Docker build fail if any shared library is missing.
RUN ldd /usr/local/bin/fourier-garden \
    && ! ldd /usr/local/bin/fourier-garden | grep -q "not found"

RUN useradd --create-home --uid 10001 demo

USER demo
WORKDIR /home/demo

EXPOSE 7681

ENTRYPOINT ["/usr/bin/tini", "--"]

CMD ["ttyd", \
     "--writable", \
     "--port", "7681", \
     "--interface", "0.0.0.0", \
     "--client-option", "titleFixed=Fourier Garden", \
     "fourier-garden"]