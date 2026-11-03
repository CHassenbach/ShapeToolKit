const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

// Exercise the exported callback without a browser, network, Shiny or DOM library.
class Element {
  constructor(tag, namespaceURI) {
    this.tag = tag;
    this.namespaceURI = namespaceURI;
    this.children = [];
    this.style = {};
    this.attributes = {};
    this.textContent = '';
  }
  appendChild(child) {
    child.parent = this;
    this.children.push(child);
  }
  setAttribute(name, value) { this.attributes[name] = value; }
  replaceChildren() { this.children = []; }
  remove() { this.parent.children = this.parent.children.filter(child => child !== this); }
}

const html = fs.readFileSync(process.argv[2], 'utf8');
const match = html.match(/<script type="application\/json" data-for="[^"]+">([\s\S]*?)<\/script>/);
assert.ok(match, 'Export must contain an HTML widget payload');
const widget = JSON.parse(match[1]);
const hook = widget.jsHooks.render[0];
const el = new Element('div');
el.clientWidth = 1000;
const listeners = new Map();
el.on = (name, callback) => listeners.set(name, callback);
el.removeListener = (name, callback) => {
  if (listeners.get(name) === callback) listeners.delete(name);
};
const context = vm.createContext({
  document: {
    createElement: tag => new Element(tag),
    createElementNS: (namespace, tag) => new Element(tag, namespace)
  },
  Plotly: { relayout: (element, layout) => { element.layout = layout; } }
});
const render = vm.runInContext('(' + hook.code + ')', context);
render(el, widget.x, hook.data);
assert.equal(listeners.size, 1);
assert.equal(el.layout['margin.r'], 340);
const text = () => el._specimenPanel.children.map(child => child.textContent).join('\n');
const elements = tag => el._specimenPanel.children.filter(child => child.tag === tag);
const click = key => {
  const trace = widget.x.data.find(trace => trace.customdata && trace.customdata.includes(key));
  assert.ok(trace, 'Row key must belong to a plotted specimen');
  listeners.get('plotly_click')({ points: [{ customdata: key, data: trace }] });
};

click(1);
assert.match(text(), /A alpha/);
assert.match(text(), /Latitude: 0, Longitude: 0/);
assert.equal(elements('svg').length, 1);
assert.equal(elements('svg')[0].children[0].attributes.points, '10,190 190,190 10,10');
assert.match(elements('iframe')[0].src, /marker=0%2C0/);
assert.match(elements('a')[0].href, /specimen\.do\?name=casent0001/);

click(2);
assert.match(text(), /B beta/);
assert.match(text(), /country: two/);
assert.equal(elements('iframe').length, 1);
assert.match(elements('iframe')[0].src, /marker=-12\.5%2C179\.99/);
assert.match(elements('a')[0].href, /casent0002/);

click(3);
assert.match(text(), /B delta/);
assert.match(text(), /Invalid mapped shape/);
assert.match(text(), /Invalid location/);
assert.equal(elements('svg').length, 0);
assert.equal(elements('iframe').length, 0);
const previous = text();
listeners.get('plotly_click')({ points: [{ customdata: 1, data: { type: 'mesh3d' } }] });
listeners.get('plotly_click')({ points: [{ data: { type: 'scatter3d', mode: 'markers' } }] });
assert.equal(text(), previous, 'Hulls and keyless overlays must not select specimens');

hook.data[0].species = '<img src=x onerror=alert(1)>';
click(1);
assert.ok(elements('p').some(child => child.textContent === hook.data[0].species));
assert.equal(elements('img').length, 0, 'Metadata must be rendered as text, not HTML');
render(el, widget.x, hook.data);
assert.equal(el.children.length, 1, 'Rerenders must replace the old panel');
assert.equal(listeners.size, 1, 'Rerenders must replace the old click listener');
console.log('Exported click handler: shape, metadata, map, row identity and rerender checks passed.');
