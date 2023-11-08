# Contours diff visualization tool

Simple tool to visualize geometrical polygons, optionally on top of an image

## Usage overview

The website consists of two files [page/index.html](page/index.html) and  the **main.js** build's output (see below for build instructions, or get these from a release).
The site should be able to fetch a 'spec' file specified by url query parameter.
`http://<server>:<port>?specUrl=<mySpec>.json`

Examples of such spec files can be seen under the [page/](page/) folder
For example, assuming a local server serves a directory on port 7777 with the site and the camel.jpg and camelSpec.json files,

```bash
$> ls my_server_directory
camel.jpg camelSpec.json index.html main.js
$> python3 -m http.server --directory my_server_directory 7777
Serving HTTP on :: port 7777 (http://[::]:7777/) ...
```

heading to `http://localhost:7777/?specUrl=camelSpec.json` should load the contours and image corresponding to camelSpec.json in your browser.

![camelSpec screenshot](docs/images/CamelCapture.PNG)

### Note on coordinates referential

All the coordinates (contour and image) are initially assumed to be expressed from the top left corner of the display. You can indicate that the contours are expressed from another corner referential via the dedicated widget.

![bottom left corner selectedt](docs/images/CornerReferential.PNG)


## Deploy locally
+ Build
```bash
make build
```
deploy the site files in [page/](page/)
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
