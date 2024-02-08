FROM haskell:latest

WORKDIR /app

COPY . /app

RUN cabal update

RUN cabal install --only-dependencies

RUN cabal build all

EXPOSE 3210

CMD ["cabal", "run", "sexpr-to-json"]