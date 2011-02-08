// demo2.js
//
// Usage:
// (restas.javascript:execute #P"/path/to/demo2.js")
//
// This file is part of the RESTAS-JAVASCRIPT library, released under Lisp-LGPL.
// See file COPYING for details.
//
// Author: Moskvitin Andrey <archimag@gmail.com>


var helloWorld = new Restas.Module;

helloWorld.context = {
    greeting: "Hello",
    name: "Andrey"
};

helloWorld.routes.main = helloWorld.defineRoute( {url: "hello"} );

helloWorld.routes.main.handler = function () {
    return "<h1>" + this.context.greeting + " " + this.context.name + "</h1>";
};

helloWorld.start();