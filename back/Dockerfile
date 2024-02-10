FROM haskell:latest

WORKDIR /app

COPY . /app

RUN cabal update
RUN cabal build all
RUN cabal install --overwrite-policy=always --installdir=.
RUN chmod +x /app/sexpr2json

EXPOSE 3210
 