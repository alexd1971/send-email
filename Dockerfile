FROM debian:10.4-slim as build

WORKDIR /tmp/build
RUN apt-get update
RUN apt-get install -y curl
RUN curl -sSL https://get.haskellstack.org/ | sh
COPY . /tmp/build/

RUN stack build --copy-bins

FROM debian:10.4-slim as app
RUN mkdir -p /opt/app
WORKDIR /opt/app
RUN apt-get update && apt-get install -y netbase ca-certificates && apt-get autoremove && apt-get clean

COPY --from=build /root/.local/bin/send-email .
EXPOSE 7777
CMD []
ENTRYPOINT ["/opt/app/send-email"]