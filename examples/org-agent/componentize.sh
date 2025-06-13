swc \
  -C jsc.parser.syntax=typescript \
  -C jsc.parser.decorators=true \
  -C jsc.transform.decoratorVersion='2022-03' \
  -C env.targets.node=23 \
  -d dist \
  --out-file-extension ts \
  src \
&& bun build \
  --target node \
  -e wasi:filesystem/preopens@0.2.5 \
  -e wasi:filesystem/types@0.2.5 \
  -e wasi:clocks/monotonic-clock@0.2.5 \
  --outfile dist/bundle.js \
  dist/src/lib.ts \
&& esbuild \
  --bundle \
  '--external:wasi*' \
  --alias:node:fs=./shim/fs.ts \
  --alias:fs=./shim/fs.ts \
  --platform=node \
  --format=esm \
  --outfile=dist/org_agent.js \
  dist/bundle.js \
&& jco componentize \
  --wit wit/ \
  --world-name ifc \
  --disable http \
  --out dist/org_agent.wasm \
  dist/org_agent.js
