FROM alpine

EXPOSE 8080
ENV OPAMYES 1
RUN set -ex
RUN apk add --no-cache --purge -U \
       opam sudo make bash gcc musl-dev \
       gmp gmp-dev libressl-dev linux-headers m4 perl zlib-dev git
RUN adduser -D irmin
RUN mkdir /data
RUN chown irmin /data
RUN opam init
RUN opam switch 4.06.1
RUN eval $(opam config env)
RUN opam update
RUN opam pin add digestif.dev https://github.com/mirage/digestif.git
RUN opam pin add checkseum.dev https://github.com/dinosaure/checkseum.git
RUN opam pin add git.dev https://github.com/mirage/ocaml-git.git
RUN opam pin add git-http.dev https://github.com/mirage/ocaml-git.git
RUN opam pin add git-unix.dev https://github.com/mirage/ocaml-git.git
RUN opam pin add irmin.dev https://github.com/mirage/irmin.git
RUN opam pin add irmin-git.dev https://github.com/mirage/irmin.git
RUN opam pin add irmin-unix.dev https://github.com/mirage/irmin.git
RUN opam pin add irmin-resp https://github.com/zshipko/irmin-resp.git
RUN mv ~/.opam/4.06.1/bin/irmin /
RUN mv ~/.opam/4.06.1/bin/irmin-resp /
RUN apk del opam make bash gcc musl-dev gmp-dev libressl-dev \
       linux-headers m4 perl zlib-dev
RUN rm -rf /var/cache/apk/*
RUN rm -rf ~/.opam
USER irmin
WORKDIR /home/irmin
ENTRYPOINT [ "/irmin-resp" ]
VOLUME /data
CMD [ "-a", "0.0.0.0", \
      "-p", "8080", \
      "-s", "git", \
      "-c", "string", \
      "--root", "/data" \
    ]

