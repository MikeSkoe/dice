'use strict';

var ReactDom = require("react-dom");

function makeContainer(text) {
  var container = document.createElement("div");
  container.className = "container";
  var title = document.createElement("div");
  title.className = "containerTitle";
  title.innerText = text;
  var content = document.createElement("div");
  content.className = "containerContent";
  container.appendChild(title);
  container.appendChild(content);
  document.body.appendChild(container);
  return content;
}

ReactDom.render("Hello!", makeContainer("Blinking Greeting"));

var test = /* Red */0;

exports.test = test;
exports.makeContainer = makeContainer;
/*  Not a pure module */
