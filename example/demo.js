// demo.js
//
// Usage:
// (restas.javascript:execute #P"/path/to/demo.js")
//
// This file is part of the RESTAS-JAVASCRIPT library, released under Lisp-LGPL.
// See file COPYING for details.
//
// Author: Moskvitin Andrey <archimag@gmail.com>


var demo = new Restas.Module;

// Hello world!

demo.routes.helloWorld =  demo.defineRoute();

demo.routes.helloWorld.handler = function () {
   return "<h1>Hello world!</h1>";
};

// Simple form

demo.routes.simpleForm = demo.defineRoute( { url: "form" } );

demo.routes.simpleForm.handler = function () {
  return "<form method=\"post\"><input name=\"message\" /><input type=\"submit\" /></form>";
};

demo.routes.simpleFormPost = demo.defineRoute( { url: demo.routes.simpleForm.url,
                                                 method: "POST" } );

demo.routes.simpleFormPost.handler = function () {
    var msg = this.request.post["message"];
    var href = "";
    
    return "<div><b>" + msg + "</b></div>" + "<a href=\"" + href + " \">Try again</a>";
};

// Book with chapters

demo.routes.bookRoot = demo.defineRoute( { url: "book/" } );

demo.routes.bookRoot.handler = function () {
    var html =  "<h1>Index</h1><ul>";

    for (var i = 1; i <= 10; ++i) {
        var href= "chapter-" + i + ".html";
        html += '<li><a href="' + href + '">Chapter ' + i + "</a></li>";
    }

    html += "</ul>";

    return html;
};

demo.routes.chapter = demo.defineRoute( { url: "book/chapter-:(id).html" } );

demo.routes.chapter.handler = function (args) {
    var html = "<h1>Chapter " + args.id + "</h1><ul>";

    for (var i = 1; i <= 10; ++i) {
        var href = "chapter-" + args.id + "-" + i + ".html";
        html += '<li><a href="' + href + '">Chapter ' + args.id + "-" + i + "</a></li>";
    }

    html += '</ul><a href="/' + demo.routes.bookRoot.url +  '">Back to Index</a>';

    return html;
};

demo.routes.subchapter = demo.defineRoute( { url: "book/chapter-:(id1)-:(id2).html" } );

demo.routes.subchapter.handler = function (args) {
    var html = "<h1>Chapter " + args.id1 + "-" + args.id2 + "</h1>";
    html += "<p>This is a chapter " + args.id1 + "-" + args.id2 + "</p>";
    html += '<a href="chapter-' + args.id1 + '.html">Back to Chapter ' + args.id1 + '</a>';

    return html;
};

// Start!

demo.start({port: 8080});