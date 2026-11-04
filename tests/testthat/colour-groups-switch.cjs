const assert = require('node:assert/strict');
const fs = require('node:fs');
const vm = require('node:vm');

class Element {
  constructor(tag) {
    this.tag = tag;
    this.children = [];
    this.style = {};
    this.attributes = {};
    this.listeners = {};
    this.value = '0';
  }
  appendChild(child) { child.parent = this; this.children.push(child); }
  setAttribute(name, value) { this.attributes[name] = value; }
  addEventListener(name, callback) { this.listeners[name] = callback; }
  remove() { this.parent.children = this.parent.children.filter(child => child !== this); }
}

async function main() {
  const html = fs.readFileSync(process.argv[2], 'utf8');
  const match = html.match(/<script type="application\/json" data-for="[^"]+">([\s\S]*?)<\/script>/);
  assert.ok(match);
  const widget = JSON.parse(match[1]);
  const hook = widget.jsHooks.render.find(hook => hook.code.includes('Colour by'));
  const el = new Element('div');
  el.data = widget.x.data;
  el.layout = widget.x.layout;
  el._fullLayout = { scene: { camera: { eye: { x: 2, y: 1, z: 0.8 } } } };
  el._context = { responsive: true };
  const camera = JSON.stringify(el._fullLayout.scene.camera);
  const overlay = { type: 'surface', name: 'Gap overlay', visible: 'legendonly' };
  const grid = { type: 'scatter3d', name: 'morphospace_3d', meta: { shape_toolkit_grouping: 'fixed' } };
  el.data.unshift(grid);
  el.data.push(overlay);
  let reject = false;
  let errorLogged = false;
  const context = vm.createContext({
    document: {
      createElement: tag => new Element(tag),
      createTextNode: text => ({ textContent: text })
    },
    console: { error: () => { errorLogged = true; } },
    Plotly: { react: async (target, traces, layout, config) => {
      if (reject) throw new Error('Test rendering failure');
      assert.equal(config.responsive, true);
      target.data = traces;
      target.layout = layout;
    } }
  });
  const render = vm.runInContext('(' + hook.code + ')', context);
  render(el, widget.x, hook.data);
  const control = () => el._colourGroupControl;
  const select = () => control().children.find(child => child.tag === 'select');
  const status = () => control().children.find(child => child.tag === 'span');
  const change = async value => {
    select().value = String(value);
    select().listeners.change();
    assert.equal(select().disabled, true);
    await new Promise(resolve => setImmediate(resolve));
    assert.equal(select().disabled, false);
  };
  assert.equal(select().children.length, 3);
  await change(1);
  assert.equal(el.layout.legend.title.text, 'region');
  assert.equal(JSON.stringify(el.layout.scene.camera), camera);
  assert.equal(el.data[0], grid);
  assert.equal(el.data.at(-1), overlay);
  assert.deepEqual(Array.from(el.data.filter(trace => trace.type === 'mesh3d'), t => t.name),
    ['East hull', 'West hull']);
  const points = el.data.filter(trace => trace.customdata);
  assert.deepEqual(Array.from(points[0].customdata), [1, 2, 3, 4]);
  assert.deepEqual(Array.from(points[1].customdata), [5, 6, 7, 8]);
  assert.equal(points[0].marker.color, el.data.find(trace => trace.name === 'East hull').facecolor[0]);
  points[0].visible = 'legendonly';
  await change(2);
  assert.equal(el.layout.legend.title.text, 'subfamily');
  assert.ok(el.data.some(trace => trace.name === '(Missing)'));
  await change(1);
  assert.equal(el.data.find(trace => trace.name === 'East').visible, 'legendonly');
  assert.equal(overlay.visible, 'legendonly');
  await change(0);
  assert.equal(el.layout.legend.title.text, 'genus');
  assert.ok(el.data.some(trace => trace.name === 'A hull'));
  assert.equal(JSON.stringify(el.layout.scene.camera), camera);
  reject = true;
  await change(1);
  assert.equal(select().value, '0');
  assert.match(status().textContent, /Unable to change colouring: Test rendering failure/);
  assert.equal(errorLogged, true);
  render(el, widget.x, hook.data);
  assert.equal(el.children.length, 1);
  console.log('Offline colouring: modes, hulls, row keys, camera, overlays, visibility, errors and rerenders passed.');
}

main().catch(error => { console.error(error); process.exitCode = 1; });
