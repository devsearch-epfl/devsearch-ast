// 300:../data/crawld/javascript/wycats/handlebars.js/bench/templates/array-each.js
module.exports = {
  context: { names: [{name: "Moe"}, {name: "Larry"}, {name: "Curly"}, {name: "Shemp"}] },
  handlebars: "{{#each names}}{{name}}{{/each}}",
  dust: "{#names}{name}{/names}",
  mustache: "{{#names}}{{name}}{{/names}}",
  eco: "<% for item in @names: %><%= item.name %><% end %>"
};

