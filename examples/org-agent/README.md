## Build Instructions

Customize the base URL, model, API key and parsing logic(which is dependent on the model used) accordingly and then compile the code into a WASM component using

```
deno task componentize
```

On successful compilation, `dist/` should contain `org_agent.wasm`.

## Usage

```
mkdir sandbox_dir
touch sandbox_dir/knowledge.org
touch sandbox_dir/knowledge.prompt
promptrs run org_agent.wasm sandbox_dir/
```
