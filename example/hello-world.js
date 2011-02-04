// hello-world.js

var helloWorldModule = new Restas.Module;

helloWorldModule.routes.main = new Restas.Route;

helloWorldModule.routes.main.handler = function () {
    return "<h1>Hello world!</h1>";
};

Restas.start(helloWorldModule, { port: 8080 });