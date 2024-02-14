# haskell-sexpr2json

Haskell web app that converts S-Expressions to Json.

Live demo available at https://sexpr2json.nassing.fr/

## Requirements

You will need cabal and npm to build and run the project.

### Libraries

The following haskell libraries were used in this project :

- `System.Environment.getArgs` to handle command line arguments,
- `Data.Char.isDigit, Text.Read.readMaybe` to parse strings to numbers,
- `Data.List.stripPrefix` to separate strings,
- `Data.Text.Lazy.Text, Data.Text.Lazy.Encoding, Data.Text.Lazy.IO` to handle text and encoding,
- `Web.Scotty, Network.Wai.Middleware.Cors, Network.HTTP.Types.Status` to set up the API.

## Build

Note : The backend is already built so you can skip this step.

To build the backend with the build script :
```
cd back
./build
```

Or, if you want to use Docker (not recommended as the build time can be very long) :
```
cd back
docker-compose build
```

Or, if you want to build it manually :
```
cd back
cabal update
cabal build sexpr2json
cabal install --installdir=.
```

## Run

To start the backend :
```
cd back
./start
```

Or, if you want to use Docker :
```
cd back
docker-compose up
```

Or, if you want to run it manually :
```
cd back
./sexpr2json
```

To start the frontend :
```
cd front
./start
```

Or, if you want to run it manually :
```
cd front
npm install
npm run dev
```

### Ports

Backend and frontend servers will be available by default at http://localhost:3210/ and http://localhost:3211/ respectively.

You can run the backend on a custom port with `./start <port>` or `cabal run sexpr2json <port>`. If you do, you must also change the environment variable `VITE_API_URL` in the `front/.env` file. The port for the frontend can also be modified in the `front/vite.config.js` file.

If you do not want to use the frontend, you can directly test the backend with requests like this :

```
curl -X POST -H "Content-Type: text/plain;charset=UTF-8" -d '(1 (2A 2.5 2Test3 2..5 "hello" my-symbol))' http://localhost:3210/convert
```

Note : If you are using WSL, you may experience some issues with port forwarding when running the project with the scripts. In that case, just run the commands manually.

## Known issues

Due to many backslashes handling issues in Javascript fetch requests, curl requests, bash arguments and even haskell strings, I was unsure of what was actually sent to the server. While the string parsing functions contains code for backslashes handling that should theoretically work, I could not test it properly.