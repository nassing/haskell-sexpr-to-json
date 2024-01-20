# haskell-sexpr-to-json
Web app that uses haskell to convert S-Expressions to Json.

Requirements: cabal, ghc, npm

Project is available at https://sexpr2json.nassing.fr/

To start backend:
```
cd back
./start
```

To start frontend:
```
cd front
./start
```

Backend and frontend servers will be available at http://localhost:3210/ and http://localhost:3211/ respectively.

If you do not want to use the frontend, you can test the backend with this kind of requests:

```
curl -X POST -H "Content-Type: text/plain;charset=UTF-8" -d '(1 (2A 2.5 2Test3 2..5 \"hello\" my-symbol))' http://localhost:3210/convert
```

This project was made with WSL and probably works with Linux too.