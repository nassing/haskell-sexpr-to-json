# haskell-sexpr-to-json
Web app that uses haskell to convert S-Expressions to Json.

Requirements: cabal & npm

Live demo available at https://sexpr2json.nassing.fr/

To start the backend:
```
cd back
./start
```

Or if you want to use Docker (not recommended as it takes a lot of time to install cabal dependencies):
```
cd back
docker-compose build
docker-compose up
```

To start the frontend:
```
cd front
./start
```

Backend and frontend servers will be available at http://localhost:3210/ and http://localhost:3211/ respectively.

You can run the backend on a custom port with `./start <port>` or `cabal run sexpr-to-json <port>`. If you do, you must also change the environment variable `VITE_API_URL` in the `front/.env` file. The port for the frontend can also be modified in the `front/vite.config.js` file.

If you do not want to use the frontend, you can directly test the backend with requests like this:

```
curl -X POST -H "Content-Type: text/plain;charset=UTF-8" -d '(1 (2A 2.5 2Test3 2..5 \"hello\" my-symbol))' http://localhost:3210/convert
```

This project was made with WSL and probably works with Linux too.

Note: If you are using WSL, you may experience some issues with port forwarding. In that case, run the start files commands manually.