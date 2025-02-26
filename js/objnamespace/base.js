// from catapult

'use strict';

/**
 * The global object.
 * @type {!Object}
 * @const
 */
const global = this.window || this.global;

/** Platform, package, object property, and Event support. */
this.tr = (function() {
  if (global.tr) return global.tr;

  /**
   * Builds an object structure for the provided namespace path,
   * ensuring that names that already exist are not overwritten. For
   * example:
   * 'a.b.c' -> a = {};a.b={};a.b.c={};
   * @param {string} name Name of the object that this file defines.
   * @private
   */
  function exportPath(name) {
    const parts = name.split('.');
    let cur = global;

    for (let part; parts.length && (part = parts.shift());) {
      if (part in cur) {
        cur = cur[part];
      } else {
        cur = cur[part] = {};
      }
    }
    return cur;
  }

  function isExported(name) {
    const parts = name.split('.');
    let cur = global;

    for (let part; parts.length && (part = parts.shift());) {
      if (part in cur) {
        cur = cur[part];
      } else {
        return false;
      }
    }
    return true;
  }

  function isDefined(name) {
    const parts = name.split('.');

    let curObject = global;

    for (let i = 0; i < parts.length; i++) {
      const partName = parts[i];
      const nextObject = curObject[partName];
      if (nextObject === undefined) return false;
      curObject = nextObject;
    }
    return true;
  }

  let panicElement = undefined;
  const rawPanicMessages = [];
  function showPanicElementIfNeeded() {
    if (panicElement) return;

    const panicOverlay = document.createElement('div');
    panicOverlay.style.backgroundColor = 'white';
    panicOverlay.style.border = '3px solid red';
    panicOverlay.style.boxSizing = 'border-box';
    panicOverlay.style.color = 'black';
    panicOverlay.style.display = 'flex';
    panicOverlay.style.height = '100%';
    panicOverlay.style.left = 0;
    panicOverlay.style.padding = '8px';
    panicOverlay.style.position = 'fixed';
    panicOverlay.style.top = 0;
    panicOverlay.style.webkitFlexDirection = 'column';
    panicOverlay.style.width = '100%';

    panicElement = document.createElement('div');
    panicElement.style.webkitFlex = '1 1 auto';
    panicElement.style.overflow = 'auto';
    panicOverlay.appendChild(panicElement);

    if (!document.body) {
      setTimeout(function() {
        document.body.appendChild(panicOverlay);
      }, 150);
    } else {
      document.body.appendChild(panicOverlay);
    }
  }

  function showPanic(panicTitle, panicDetails) {
    if (tr.isHeadless) {
      if (panicDetails instanceof Error) throw panicDetails;
      throw new Error('Panic: ' + panicTitle + ':\n' + panicDetails);
    }

    if (panicDetails instanceof Error) {
      panicDetails = panicDetails.stack;
    }

    showPanicElementIfNeeded();
    const panicMessageEl = document.createElement('div');
    panicMessageEl.innerHTML =
        '<h2 id="message"></h2>' +
        '<pre id="details"></pre>';
    panicMessageEl.querySelector('#message').textContent = panicTitle;
    panicMessageEl.querySelector('#details').textContent = panicDetails;
    panicElement.appendChild(panicMessageEl);

    rawPanicMessages.push({
      title: panicTitle,
      details: panicDetails
    });
  }

  function hasPanic() {
    return rawPanicMessages.length !== 0;
  }
  function getPanicText() {
    return rawPanicMessages.map(function(msg) {
      return msg.title;
    }).join(', ');
  }

  function exportTo(namespace, fn) {
    const obj = exportPath(namespace);
    const exports = fn();

    for (const propertyName in exports) {
      // Maybe we should check the prototype chain here? The current usage
      // pattern is always using an object literal so we only care about own
      // properties.
      const propertyDescriptor = Object.getOwnPropertyDescriptor(exports,
          propertyName);
      if (propertyDescriptor) {
        Object.defineProperty(obj, propertyName, propertyDescriptor);
      }
    }
  }

  /**
   * Initialization which must be deferred until run-time.
   */
  function initialize() {
    if (global.isVinn) {
      tr.isVinn = true;
    } else if (global.process && global.process.versions.node) {
      tr.isNode = true;
    } else {
      tr.isVinn = false;
      tr.isNode = false;
      tr.doc = document;

      tr.isMac = /Mac/.test(navigator.platform);
      tr.isWindows = /Win/.test(navigator.platform);
      tr.isChromeOS = /CrOS/.test(navigator.userAgent);
      tr.isLinux = /Linux/.test(navigator.userAgent);
    }
    tr.isHeadless = tr.isVinn || tr.isNode;
  }

  return {
    initialize,

    exportTo,
    isExported,
    isDefined,

    showPanic,
    hasPanic,
    getPanicText,
  };
})();

tr.initialize();
