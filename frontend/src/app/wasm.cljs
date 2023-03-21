(ns app.wasm
  (:require
   [app.util.wasm :as wasm]
   [app.util.wasm.schema :as schema]
   [promesa.core :as p]))

(defonce instance (atom nil))
(defonce memory (atom nil))
(defonce selrect (atom nil))

(defn load-wasm
  "Loads a WebAssembly module"
  ([uri]
  (load-wasm uri (js-obj)))

  ([uri imports]
  (->
   (p/let [response (js/fetch uri)
           array-buffer (.arrayBuffer response)
           assembly (wasm/instantiate array-buffer imports)]
     assembly)
   (p/catch (fn [error] (prn "error: " error))))))

(defn init-proxies
  [asm]
  (reset! selrect (schema/createSchemaProxy 
          (schema/createSchema 
           (js-obj 
            "x" (js-obj "type" "f4")
            "y" (js-obj "type" "f4")
            "width" (js-obj "type" "f4")
            "height" (js-obj "type" "f4"))) 
           (js/DataView. asm.memory.buffer asm.selRect.value))))

(defn init!
  "Loads WebAssembly module"
  []
  (p/then
   (load-wasm "wasm/resize.debug.wasm")
   (fn [asm]
     (reset! instance asm)
     (reset! memory asm.memory)
     (init-proxies asm))))
