// hello-world.js

var helloWorldModule = new Restas.Module;

// Hello world!

helloWorldModule.routes.main =  new Restas.Route();

helloWorldModule.routes.main.handler = function () {
    return "<h1>Hello world!</h1>";
};

// Example 1

helloWorldModule.routes.example1 = new Restas.Route( { url: "example1" } );

helloWorldModule.routes.example1.handler = function () {
  return "<form method=\"post\"><input name=\"message\" /><input type=\"submit\" /></form>";
};

helloWorldModule.routes.example1Post = new Restas.Route( { url: "example1", method: "POST" } );

helloWorldModule.routes.example1Post.handler = function () {
    return "not implemeted";
    //return "<div><b>" + 
};


helloWorldModule.start( {port: 8080} );