FROM alpine as build

WORKDIR /tmp/build
RUN apk add curl ghc=~8.6.5 musl-dev zlib-dev
RUN curl -sSL https://get.haskellstack.org/ | sh
COPY . /tmp/build/

RUN stack build --system-ghc --copy-bins

FROM alpine as app
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apk add --no-cache gmp libffi ca-certificates

COPY --from=build /root/.local/bin/send-email .
EXPOSE 7777
CMD []
ENTRYPOINT ["/opt/app/send-email"]