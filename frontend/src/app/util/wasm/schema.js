/**
 *
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Copyright (c) KALEIDOS INC
 */

"use strict";

goog.provide("app.util.wasm.schema");

goog.scope(function() {
  console.log('JWAJAJDASJDSA')
  const self = app.util.wasm.schema;

  const DataViewTypes = {
    u1: [1, "getUint8", "setUint8"],
    u2: [2, "getUint16", "setUint16"],
    u4: [4, "getUint32", "setUint32"],
    u8: [8, "getBigUint64", "setBigUint64"],
    s1: [1, "getInt8", "setInt8"],
    s2: [2, "getInt16", "setInt16"],
    s4: [4, "getInt32", "setInt32"],
    s8: [8, "getBigInt64", "setBigInt64"],
    f4: [4, "getFloat32", "setFloat32"],
    f8: [8, "getFloat64", "setFloat64"],
  };

  function createSchemaProperties(propDefs) {
    let offset = 0;
    const properties = new Map();
    for (const [name, propDef] of Object.entries(propDefs)) {
      const [size, getter, setter] = DataViewTypes[propDef.type];
      properties.set(name, {
        type: propDef.type,
        offset: propDef.offset ?? offset,
        size: propDef.size ?? size,
        getter,
        setter,
      });
      offset += size;
    }
    return properties;
  }

  function getSchemaPropertiesSize(properties) {
    return Array.from(properties).reduce((acc, [name, { size }]) => acc + size, 0);
  }

  function createSchemaArray(subSchema, maxSize) {
    const accessors = new Map();
    const schema = {
      set(_, __, ___) {
        return false;
      },
      get(_, property) {
        if (/^[0-9]+$/.test(property)) {
          if (accessors.has(property)) {
            return accessors.get(property);
          }
          const index = parseInt(property, 10);
          if (index >= maxSize) {
            throw new RangeError("offset is out of bounds");
          }
          const byteOffset = index * subSchema.size;
          const byteLength = subSchema.size;
          const subDataView = new DataView(dataView.buffer, byteOffset, byteLength);
          const proxy = createSchemaProxy(subSchema, subDataView);
          accessors.set(proxy);
          return proxy;
        }
      },
    };
    return schema;
  }

  function createSchema(propDefs, { littleEndian = false } = {}) {
    console.log(propDefs)
    const properties = createSchemaProperties(propDefs);
    const size = getSchemaPropertiesSize(properties);
    const schema = {
      size,
      set(target, property, value) {
        if (!properties.has(property)) {
          return false;
        }
        const { type, offset, setter } = properties.get(property);
        target[setter](offset, value, littleEndian);
        return true;
      },
      get(target, property) {
        if (!properties.has(property)) {
          return undefined;
        }
        const { offset, getter } = properties.get(property);
        return target[getter](offset, littleEndian);
      },
    };
    return schema;
  }

  function createSchemaProxy(schema, dataView) {
    const proxy = new Proxy(
      {},
      {
        set(_, name, value) {
          return schema.set(dataView, name, value);
        },
        get(_, name) {
          return schema.get(dataView, name);
        },
      }
    );
    return proxy;
  }

  self.createSchema = createSchema
  self.createSchemaArray = createSchemaArray
  self.createSchemaProxy = createSchemaProxy
})
