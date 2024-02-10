FROM haskell:latest

WORKDIR /app

COPY . /app

RUN cabal update
RUN cabal build sexpr2json
RUN cabal install --installdir=.

EXPOSE 3210

CMD ["./sexpr2json"]