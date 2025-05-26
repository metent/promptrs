# promptrs

A tool and engine for creating and running AI agents using embedded Wasmtime. It allows agentic workflows that interact with the filesystem and network in a sandboxed environment. Agents can be written in any language that supports wasi-p2 target.

## Usage
```bash
promptrs run org_agent.wasm sandbox_dir/
```
