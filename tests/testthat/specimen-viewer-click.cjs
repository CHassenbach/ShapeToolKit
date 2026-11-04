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
assert.equal(el.layout['margin.l'], 340);
assert.equal(el.layout['margin.r'], undefined, 'Viewer must preserve the legend-side margin');
assert.match(el._specimenPanel.style.cssText, /position:absolute;left:0;/);
assert.match(el._specimenPanel.style.cssText, /border-right:/);
assert.doesNotMatch(el._specimenPanel.style.cssText, /;right:0;/);
const text = () => el._specimenPanel.children.map(child => child.textContent).join('\n');
const elements = tag => el._specimenPanel.children.filter(child => child.tag === tag);
const click = key => {
  const trace = widget.x.data.find(trace => trace.customdata && trace.customdata.includes(key));
  assert.ok(trace, 'Row key must belong to a plotted specimen');
  listeners.get('plotly_click')({ points: [{ customdata: key, data: trace }] });
};
const map = () => elements('svg').find(svg => svg.attributes.viewBox === '0 0 500 270');
const marker = () => map().children.find(child => child.tag === 'circle');
assert.equal(widget.x.specimen_map.length, 2);
assert.ok(widget.x.specimen_map[1].length > 10000, 'World boundaries must be embedded');

click(1);
assert.match(text(), /A alpha/);
assert.match(text(), /Latitude: 0, Longitude: 0/);
assert.equal(elements('svg').length, 2);
assert.equal(elements('svg')[0].children[0].attributes.points, '10,190 190,190 10,10');
assert.equal(Number(marker().attributes.cx), 250);
assert.equal(Number(marker().attributes.cy), 135);
assert.equal(map().children.filter(child => child.tag === 'path').length, 2);
assert.equal(elements('iframe').length, 0, 'Map must not load network content');
assert.match(text(), /Accession ID: COLL-0001/);
assert.equal(elements('a').length, 0, 'Accession IDs must not create external links');

click(2);
assert.match(text(), /B beta/);
assert.match(text(), /country: two/);
assert.ok(Math.abs(Number(marker().attributes.cx) - 477.4412566691) < 1e-8);
assert.ok(Math.abs(Number(marker().attributes.cy) - 156.4347193734) < 1e-8);
assert.match(text(), /Accession ID: COLL-0002/);

click(3);
assert.match(text(), /B delta/);
assert.match(text(), /Invalid mapped shape/);
assert.match(text(), /Invalid location/);
assert.match(text(), /Accession ID: Not available/);
assert.equal(elements('svg').length, 0);
assert.equal(elements('iframe').length, 0);
const previous = text();
listeners.get('plotly_click')({ points: [{ customdata: 1, data: { type: 'mesh3d' } }] });
listeners.get('plotly_click')({ points: [{ data: { type: 'scatter3d', mode: 'markers' } }] });
assert.equal(text(), previous, 'Hulls and keyless overlays must not select specimens');

// Independent reference positions from PROJ's spherical Equal Earth projection.
for (const [lon, lat, expectedX, expectedY] of [
  [-180, 0, 19.9364513858, 135], [180, 0, 480.0635486142, 135],
  [0, 90, 250, 23.0241654716], [0, -90, 250, 246.9758345284],
  [30, 45, 282.8625441413, 61.8803577306],
  [-30, -45, 217.1374558587, 208.1196422694]
]) {
  hook.data[0].longitude = lon;
  hook.data[0].latitude = lat;
  click(1);
  assert.ok(Math.abs(Number(marker().attributes.cx) - expectedX) < 1e-8);
  assert.ok(Math.abs(Number(marker().attributes.cy) - expectedY) < 1e-8);
  assert.match(text(), /Works offline/);
  assert.equal(elements('iframe').length, 0);
}

hook.data[0].species = '<img src=x onerror=alert(1)>';
hook.data[0].accession = '<img src=x onerror=alert(1)>';
click(1);
assert.ok(elements('p').some(child => child.textContent === hook.data[0].species));
assert.ok(elements('p').some(child => child.textContent === 'Accession ID: ' + hook.data[0].accession));
assert.equal(elements('img').length, 0, 'Metadata must be rendered as text, not HTML');
render(el, widget.x, hook.data);
assert.equal(el.children.length, 1, 'Rerenders must replace the old panel');
assert.equal(listeners.size, 1, 'Rerenders must replace the old click listener');
console.log('Exported click handler: offline Equal Earth map, polar/dateline projection, shape, metadata and rerender checks passed.');
