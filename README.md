## Deploy locally
+ Run
```bash
make
```
to deploy the site files in **page**
+ Run 
```bash
cd page && python3 -m http.server <port>
```
to serve the site under localhost:*port*

## Requirements

### Build 
+ npm (e.g. via **nvm**)
+ elm 
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
npm install -g elm
```
+ tidy 
```bash
<package-manager> install html-tidy
```

### Deployment

+ Any static file server, e.g python3 and its http.server module
