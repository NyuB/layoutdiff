## Deploy locally
+ Build
```bash
make build
```
deploy the site files in **page/**
+ Serve 
```bash
python3 -m http.server --directory page <port>
```
serve the site under localhost:*port*
## Development
### Full pipeline
```bash
make
```
### Test
```bash
make test
```
### Format
```bash
make fmt
```
### Build and deploy
```bash
make build
```
## Requirements

### Build 
+ npm (e.g. via [nvm](https://github.com/nvm-sh/nvm))
+ [elm](https://elm-lang.org/) 
```bash
npm install -g elm
```

### Tests and formatting

+ elm-format 
```bash
npm install -g elm-format
```
+ elm-test 
```bash
npm install -g elm-test
```
+ [tidy](https://www.html-tidy.org/) 
```bash
<package-manager> install html-tidy
```

### Deployment

+ Any static file server, e.g [Python3 http.server module](https://docs.python.org/3/library/http.server.html)
